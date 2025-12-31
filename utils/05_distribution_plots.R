# === LOADS & SETUP ============================================================
source(file.path("code", "utils", "load_packages.R"))

packages <- c(
  "ggplot2","dplyr","tidyr","forcats","rlang","purrr","labelled",
  "RColorBrewer","tibble","showtext","ggh4x"
)
load_packages(packages)

showtext_auto()

# === LABEL LOOKUPS ============================================================
question_set_labels <- tibble::tribble(
  ~set,   ~label,
  "a1",   "Early pandemic: how often did you ACCESS information from these sources?",
  "b1",   "Early pandemic: how often did you ENCOUNTER RELIABLE information from these sources?",
  "c1",   "Today: how often do you SEEK info about emerging conditions from these sources?",
  "ppe1c","Factors that informed your thinking about N95 reuse",
  "hfo2c","Factors that informed your thinking about high-flow oxygen",
  "iv3c", "Factors that informed your thinking about ivermectin",
  "b3",   "Why you discontinued an innovation (example you gave)"
)

subscale_titles <- c(
  "subscale1" = "Professional Networks",
  "subscale2" = "News and Social Media",
  "subscale3" = "Academic Publications",
  "subscale4" = "Blogs and Podcasts"
)

# === UTILS ====================================================================
get_set_id <- function(x) sub("(_.*)$", "", x)

get_label <- function(.data, vars) {
  sapply(vars, function(v) attr(.data[[v]], "label"))
}

get_plot_color <- function(question_number, column_names, palette_vec) {
  idx <- match(question_number, column_names)
  palette_vec[(idx - 1) %% length(palette_vec) + 1]
}

palette_list <- list(
  p1 = RColorBrewer::brewer.pal(8, "Accent"),
  p2 = RColorBrewer::brewer.pal(8, "Pastel2"),
  p3 = RColorBrewer::brewer.pal(8, "Set3"),
  p4 = RColorBrewer::brewer.pal(8, "Pastel1"),
  p5 = RColorBrewer::brewer.pal(8, "Set2")
)

# === LIKERT HELPERS -----------------------------------------------------------
get_likert_map <- function(x, decreasing = TRUE) {
  vl <- if (requireNamespace("labelled", quietly = TRUE)) {
    labelled::val_labels(x)
  } else {
    attr(x, "labels", exact = TRUE)
  }
  if (is.null(vl)) return(NULL)
  
  ord    <- order(unname(vl), decreasing = decreasing)
  codes  <- unname(vl)[ord]
  labels <- names(vl)[ord]
  
  list(
    breaks = codes,
    limits = as.character(codes),
    labels = labels
  )
}

build_likert_maps <- function(data, question_numbers) {
  maps <- lapply(question_numbers, function(q) {
    m <- get_likert_map(data[[q]], decreasing = TRUE)
    if (!is.null(m)) {
      m$labels[m$labels == "NA: I did not use this source."] <- "N/A"
      m$limits <- as.character(m$breaks)
    }
    m
  })
  names(maps) <- question_numbers
  maps
}

# --- Re‑assert value_lab levels per question ----------------------------------
reassert_levels_by_question <- function(df, lk_maps) {
  df %>%
    group_split(.data$question_number) %>%
    purrr::map_dfr(function(dfq) {
      q  <- dfq$question_number[1]
      lk <- lk_maps[[q]]
      if (!is.null(lk)) {
        dfq %>% mutate(value_lab = forcats::fct_relevel(as.character(value_lab), lk$labels))
      } else dfq %>% mutate(value_lab = factor(as.character(value_lab)))
    })
}

# === MAIN DRIVER ==============================================================
overview_subsets <- function(
    df,
    question_numbers,
    subset_var = NULL,
    subset_categories = NULL,
    group_var = NULL,
    plot_by = NULL,
    chosen_palette = NULL,
    leg_label = NULL,
    xjust = NULL
) {
  if (!is.null(subset_var)) {
    categories <- if (!is.null(subset_categories)) subset_categories else unique(df[[subset_var]])
    df[[subset_var]] <- factor(df[[subset_var]], levels = categories)
    subset_data <- df[df[[subset_var]] %in% categories, ]
    
    valid_subsets <- subset_data %>%
      group_by(across(all_of(subset_var))) %>%
      filter(n() >= 50) %>%
      ungroup()
    
    if (nrow(valid_subsets) > 0) {
      plot_overview(valid_subsets, question_numbers, group_var, subset_var, plot_by, chosen_palette, leg_label, xjust)
    } else {
      message("No subsets with at least 50 observations.")
    }
  } else {
    plot_overview(df, question_numbers, group_var, subset_var, plot_by, chosen_palette, leg_label, xjust)
  }
}

# === MAIN PLOTTING WORKHORSE ==================================================
plot_overview <- function(
    data,
    question_numbers,
    group_var = NULL,
    subset_var = NULL,
    plot_by = NULL,
    chosen_palette = NULL,
    leg_label = NULL,
    xjust = NULL
) {
  # 1. facet strip labels
  question_labels <- tryCatch({
    labs <- get_label(data, question_numbers)
    names(labs) <- question_numbers
    sapply(labs, function(label) paste(strwrap(label, width = 25), collapse = "\n"))
  }, error = function(e) setNames(question_numbers, question_numbers))
  
  # 2. Likert maps
  lk_maps <- build_likert_maps(data, question_numbers)
  lk_maps <- lk_maps[!vapply(lk_maps, is.null, logical(1))]
  lk_maps <- lk_maps[!duplicated(names(lk_maps))]
  
  # 3. long data
  long_data <- data %>%
    tidyr::pivot_longer(
      cols = all_of(question_numbers),
      names_to = "question_number",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value)
  
  # 4. add value labels
  long_data <- long_data %>%
    group_split(.data$question_number) %>%
    purrr::map_dfr(function(dfq) {
      q  <- dfq$question_number[1]
      lk <- lk_maps[[q]]
      if (!is.null(lk)) {
        dfq %>%
          mutate(
            value_lab = factor(
              lk$labels[match(as.character(value), lk$limits)],
              levels = lk$labels
            )
          )
      } else {
        dfq %>% mutate(value_lab = factor(as.character(value)))
      }
    })
  
  # 5. Counts → percents, complete missing levels
  if (!is.null(subset_var)) {
    long_data <- long_data %>%
      count(!!rlang::sym(subset_var), .data$question_number, .data$value_lab, name = "Frequency") %>%
      group_by(!!rlang::sym(subset_var), .data$question_number) %>%
      group_split() %>%
      purrr::map_dfr(function(dfq) {
        q  <- dfq$question_number[1]
        lk <- lk_maps[[q]]
        if (!is.null(lk)) {
          tidyr::complete(
            dfq,
            !!rlang::sym(subset_var),
            question_number,
            value_lab = lk$labels,
            fill = list(Frequency = 0)
          )
        } else dfq
      }) %>%
      group_by(!!rlang::sym(subset_var), .data$question_number) %>%
      mutate(Percent = round(Frequency / sum(Frequency) * 100, 2)) %>%
      ungroup()
  } else {
    long_data <- long_data %>%
      count(.data$question_number, .data$value_lab, name = "Frequency") %>%
      group_by(.data$question_number) %>%
      group_split() %>%
      purrr::map_dfr(function(dfq) {
        q  <- dfq$question_number[1]
        lk <- lk_maps[[q]]
        if (!is.null(lk)) {
          tidyr::complete(
            dfq,
            question_number,
            value_lab = lk$labels,
            fill = list(Frequency = 0)
          )
        } else dfq
      }) %>%
      group_by(.data$question_number) %>%
      mutate(Percent = round(Frequency / sum(Frequency) * 100, 2)) %>%
      ungroup()
  }
  
  long_data <- reassert_levels_by_question(long_data, lk_maps)
  
  # 6. palette and colors
  pal_name <- chosen_palette %||% "p5"
  palette_vec <- palette_list[[pal_name]]
  if (is.null(palette_vec)) palette_vec <- RColorBrewer::brewer.pal(8, "Set2")
  
  unique_questions <- unique(long_data$question_number)
  colors <- sapply(unique_questions, get_plot_color,
                   column_names = unique_questions, palette_vec = palette_vec)
  names(colors) <- unique_questions
  
  # 7. title
  set_ids <- unique(get_set_id(question_numbers))
  title_labels <- question_set_labels$label[match(set_ids, question_set_labels$set)]
  title_labels <- unique(title_labels[!is.na(title_labels) & nzchar(title_labels)])
  title_text <- if (length(title_labels)) {
    paste(strwrap(paste(title_labels, collapse = " / "), width = 60), collapse = "\n")
  } else NULL
  
  # 8. base plot
  p <- ggplot(long_data, aes(x = value_lab, y = Percent)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    theme(
      text = element_text(size = 25),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = xjust
        # margin = margin(b = 10)
      ),
      strip.text.x = element_text(lineheight = 0.7),
      strip.text.y = element_text(angle = 0),
      panel.spacing = grid::unit(1, "lines"),
      plot.title.position = "plot",
      plot.title = element_text(size = 35, face = "bold")
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(x = " ", y = "Percent of Responders")
  
  if (!is.null(title_text)) {
    p <- p + labs(title = title_text)
  } else {
    p <- p + theme(plot.title = element_blank())
  }
  
  # 9. layout variants
  if (is.null(plot_by)) {
    return(invisible(p))
  } else if (plot_by == "facet") {
    p <- plot_with_facets(p, question_numbers, question_labels, subset_var) +
      scale_fill_manual(values = colors)
    return(invisible(p))
  } else if (plot_by == "color") {
    p <- plot_with_colors(p, question_numbers, question_labels, subset_var, pal_name, leg_label)
    return(invisible(p))
  } else {
    warning("Invalid plot_by value. No special plotting applied.")
    return(invisible(p))
  }
}

# === FACETING HELPERS =========================================================
plot_with_facets <- function(p, question_numbers, question_labels, subset_var) {
  if (is.null(subset_var)) {
    p <- p + aes(fill = question_number) +
      ggh4x::facet_wrap2(
        ~question_number,
        labeller = labeller(question_number = question_labels),
        ncol = 3,
        axes = "all"
      ) +
      theme(
        legend.position = "none",
        axis.ticks.x = element_line()
      )
  } else {
    p <- p + aes(fill = question_number) +
      ggh4x::facet_grid2(
        rows = vars(question_number),
        cols = vars(!!rlang::sym(subset_var)),
        labeller = labeller(question_number = question_labels),
        axes = "all",
        remove_labels = "none"
      ) +
      theme(
        legend.position = "none",
        strip.placement = "outside"
      )
  }
  invisible(p)
}

plot_with_colors <- function(p, question_numbers, question_labels, subset_var, chosen_palette, leg_label) {
  p <- p + aes(fill = !!rlang::sym(subset_var)) +
    ggh4x::facet_wrap2(
      ~question_number,
      labeller = labeller(question_number = question_labels),
      ncol = 3,
      axes = "all"
    ) +
    scale_fill_manual(values = palette_list[[chosen_palette]]) +
    labs(fill = leg_label) +
    theme(
      legend.position = "bottom",
      strip.placement = "outside"
    )
  invisible(p)
}