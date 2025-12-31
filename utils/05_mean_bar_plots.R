# Libraries used: dplyr, tidyr, forcats, stringr, rlang

# Optional for stats: emmeans, broom, effectsize

# Returns columns:
#   x, group, facet, y, ymin (opt), ymax (opt), label (opt), stars (opt), arrow (opt)
# Behavior:
#   kind = "mean_ci"    → expects wide columns like mean, ci_low, ci_high; optional sig stars in `stars_col`
#   kind = "percent"    → expects wide columns e.g. "Recommended" & "Did Not Recommend"

wilson_ci <- function(k, n, conf.level = 0.95) {
  if (n == 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf.level)/2)
  p <- k / n
  denom <- 1 + z^2/n
  center <- (p + z^2/(2*n)) / denom
  halfw  <- (z * sqrt(p*(1-p)/n + z^2/(4*n^2))) / denom
  c(center - halfw, center + halfw)
}

run_stats_group_diffs <- function(
    raw_long,
    x_col, group_col, facet_col, outcome_col,
    model = c("auto","lm","glm"),          # "auto": choose by outcome
    family = gaussian(),                    # used if model = "glm"
    include_interaction = TRUE,            # outcome ~ group * x (or + if FALSE)
    contrast_method = "pairwise",
    adjust_method = "holm",                # which p.adjust governs stars
    effect_size = TRUE,                    # Cohen's d if 2 groups
    x_levels = NULL, group_levels = NULL, facet_levels = NULL
) {
  stopifnot(all(c(x_col, group_col, facet_col, outcome_col) %in% names(raw_long)))
  
  # Coerce + order
  raw <- raw_long %>%
    transmute(
      x     = .data[[x_col]],
      group = as.character(.data[[group_col]]),
      facet = .data[[facet_col]],
      y     = .data[[outcome_col]]
    ) %>%
    mutate(
      x     = if (is.null(x_levels)) factor(x) else factor(x, levels = x_levels),
      group = if (is.null(group_levels)) factor(group) else factor(group, levels = group_levels),
      facet = if (is.null(facet_levels)) factor(facet) else factor(facet, levels = facet_levels)
    )
  
  # Auto-select model if requested
  model <- match.arg(model)
  if (model == "auto") {
    uniq <- unique(na.omit(raw$y))
    model <- if (length(uniq) <= 2 && all(uniq %in% c(0,1))) "glm" else "lm"
  }
  
  facets <- levels(raw$facet)
  results <- vector("list", length(facets))
  
  for (i in seq_along(facets)) {
    fct <- facets[i]
    dfi <- filter(raw, facet == fct)
    
    # Build formula
    if (include_interaction) {
      fmla <- y ~ group * x
    } else {
      fmla <- y ~ group + x
    }
    
    fit  <- if (model == "lm") {
      lm(fmla, data = dfi)
    } else {
      glm(fmla, data = dfi, family = family)
    }
    
    # Pairwise within x
    emm   <- emmeans::emmeans(fit, specs = ~ group | x, type = "response")
    contr <- emmeans::contrast(emm, method = contrast_method, adjust = "none")
    tb    <- broom::tidy(contr) %>% mutate(facet = fct, x = as.character(x))
    
    # Multiplicity (one method governs stars)
    tb[[paste0("p_adj_", adjust_method)]] <- p.adjust(tb$p.value, method = adjust_method)
    tb$stars <- case_when(
      tb[[paste0("p_adj_", adjust_method)]] < 0.001 ~ "***",
      tb[[paste0("p_adj_", adjust_method)]] < 0.01  ~ "**",
      tb[[paste0("p_adj_", adjust_method)]] < 0.05  ~ "*",
      tb[[paste0("p_adj_", adjust_method)]] < 0.10  ~ "†",
      TRUE ~ ""
    )
    
    # Effect size (only if two groups)
    if (isTRUE(effect_size) && nlevels(dfi$group) == 2) {
      es_tbl <- dfi %>%
        group_by(x) %>%
        group_modify(~ {
          eff <- effectsize::cohens_d(y ~ group, data = .x, pooled_sd = TRUE, ci = 0.95)
          as_tibble(eff)[, c("Cohens_d","CI_low","CI_high")]
        }) %>% ungroup() %>%
        rename(d = Cohens_d, d_low = CI_low, d_high = CI_high) %>%
        mutate(x = as.character(x))
      tb <- left_join(tb, es_tbl, by = "x")
    }
    
    results[[i]] <- tb
  }
  
  contrasts <- bind_rows(results)
  
  # One star string per (facet, x)
  star_map <- contrasts %>%
    group_by(facet, x) %>%
    summarise(stars = paste0(unique(stars[nzchar(stars)]), collapse = ""),
              .groups = "drop")
  
  list(contrasts = contrasts, stars = star_map)
}

prepare_bars <- function(
    data,
    x,                # e.g. "subscale" or "pretty_label"
    group,            # e.g. "provtype" or "recommended"
    facet,            # e.g. "question" or "Intervention"
    value,            # numeric column to average (Likert or 0/1)
    facet_levels,     # vector of facet levels to include & order
    x_levels   = NULL,
    group_levels = NULL,
    ci_level   = 0.95,
    # stats control
    model = "auto",               # "auto", "lm", or "glm"
    family = binomial(),          # used if model = "glm" or auto->glm
    include_interaction = TRUE,
    contrast_method = "pairwise",
    adjust_method   = "holm",
    effect_size     = TRUE,
    # label formatter
    label_fun = NULL
) {
  stopifnot(all(c(x, group, facet, value) %in% names(data)))
  
  df <- data %>%
    transmute(
      x     = .data[[x]],
      group = as.character(.data[[group]]),
      facet = .data[[facet]],
      y     = .data[[value]]
    ) %>%
    mutate(
      facet = factor(facet, levels = facet_levels),
      x     = if (is.null(x_levels)) factor(x) else factor(x, levels = x_levels),
      group = if (is.null(group_levels)) factor(group) else factor(group, levels = group_levels)
    ) %>%
    filter(!is.na(facet))  # keep only requested facets
  
  # Detect binary outcome
  uniq <- unique(na.omit(df$y))
  is_binary <- length(uniq) <= 2 && all(uniq %in% c(0,1))
  
  # --- Summary means + CIs
  summary_tbl <- df %>%
    group_by(facet, x, group) %>%
    summarise(
      n    = sum(!is.na(y)),
      mean = mean(y, na.rm = TRUE),
      sd   = sd(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se    = sd / sqrt(pmax(n, 1)),
      tcrit = qt(1 - (1 - ci_level)/2, df = pmax(n - 1, 1)),
      ci_low  = if (!is_binary) mean - tcrit * se else {
        mapply(function(m, n) wilson_ci(k = round(m*n), n = n, conf.level = ci_level)[1], mean, n)
      },
      ci_high = if (!is_binary) mean + tcrit * se else {
        mapply(function(m, n) wilson_ci(k = round(m*n), n = n, conf.level = ci_level)[2], mean, n)
      }
    )
  
  # --- Run pairwise stats within each x and facet
  rs <- run_stats_group_diffs(
    raw_long          = df,
    x_col             = "x",
    group_col         = "group",
    facet_col         = "facet",
    outcome_col       = "y",
    model             = model,
    family            = family,
    include_interaction = include_interaction,
    contrast_method   = contrast_method,
    adjust_method     = adjust_method,
    effect_size       = effect_size,
    x_levels          = levels(df$x),
    group_levels      = levels(df$group),
    facet_levels      = levels(df$facet)
  )
  
  # --- build plotting frame
  plot_df <- summary_tbl %>%
    mutate(
      # nice value labels
      label = if (is.null(label_fun))
        if (is_binary) paste0(round(mean*100, 1), "%") else sprintf("%.2f", mean)
      else label_fun(mean)
    ) %>%
    # attach star string per (facet,x)
    left_join(rs$stars, by = c("facet","x")) %>%
    mutate(stars = tidyr::replace_na(stars, "")) %>%
    rename(ymin = ci_low, ymax = ci_high, y = mean) %>%
    arrange(facet, x, group)
  
  # Keep stats for reporting
  attr(plot_df, "contrasts") <- rs$contrasts
  plot_df
}



plot_bars <- function(
    plot_df,
    orientation = c("horizontal","vertical"),
    fill_values = NULL,
    legend_title = "Group",
    bar_width = 0.7,
    dodge_w   = 0.75,      # ↓ make this smaller (e.g., 0.45) to reduce space between grouped bars
    label_size = 12,
    star_size  = 12,
    family = "Figtree",
    font_color = "gray20",
    # NEW: control the value-axis limits (handled correctly for orientation)
    value_limits = NULL,   # replaces y_limits; use c(1,5) for Likert
    value_breaks = waiver(),
    value_labels = waiver(),
    
    label_offset = 0.3,
    star_offset = 0, # nudge significance stars along the value axis (e.g., 0.15)
    facet_ncol = 1,
    facet_scales = "fixed",
    ci_cap   = 0.25,      # thickness of the caps orthogonal to the value axis
    
    # optional x axis captions
    axis_caps = NULL,            # named character vector: names = x level, values = caption text
    name_face = "bold",          # "plain", "bold", "italic", "bold.italic"
    name_size = 12,              # tick text size (pt)
    cap_face  = "italic",        # face for caption line
    cap_size  = 13,              # point size for caption line
    y_axis_lineheight = 0.2,        # controls spacing between lines of labels
    x_axis_lineheight = 0.8,
    
    # text sizes
    facet_text_size = 40,
    x_text_size = 30,
    legend_title_size = 32,
    legend_text_size = 28
    
) {
  orientation <- match.arg(orientation)
  pd <- position_dodge(width = dodge_w)
  
  # Precompute star & label positions along value axis
  df_star  <- plot_df
  df_star$star_val  <- plot_df$y + star_offset
  
  df_label <- plot_df
  df_label$lab_val  <- plot_df$y + label_offset
  
  star_ann <- plot_df %>%
    dplyr::group_by(facet, x) %>%
    dplyr::summarise(
      stars = paste0(unique(stars[nzchar(stars)]), collapse = ""),
      vmax  = max(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(nzchar(stars)) %>%
    dplyr::mutate(val = vmax + star_offset)  # position along value axis
  
  axis_labels_html <- NULL
  if (!is.null(axis_caps)) {
    # map captions onto *all* x levels (missing → empty string)
    # x_levs <- levels(factor(plot_df$x))
    x_levs <- if (is.factor(plot_df$x)) levels(plot_df$x) else levels(factor(plot_df$x))
    cap_map <- rep("", length(x_levs)); names(cap_map) <- x_levs
    nm <- intersect(names(axis_caps), x_levs)
    cap_map[nm] <- unname(axis_caps[nm])
    
    to_html <- function(name, cap) {
      name_tag <- switch(name_face,
                         "bold"        = paste0("<b>", name, "</b>"),
                         "italic"      = paste0("<i>", name, "</i>"),
                         "bold.italic" = paste0("<b><i>", name, "</i></b>"),
                         name)
      if (nzchar(cap)) {
        cap_style <- paste0(
          "font-style:", if (cap_face %in% c("italic","bold.italic")) "italic" else "normal", ";",
          "font-weight:", if (cap_face %in% c("bold","bold.italic")) "bold" else "normal", ";",
          "font-size:", cap_size, "pt;"
        )
        paste0(name_tag, "<br/><span style='", cap_style, "'>", cap, "</span>")
      } else {
        name_tag
      }
    }
    axis_labels_html <- setNames(mapply(to_html, x_levs, cap_map, USE.NAMES = FALSE), x_levs)
  }
  
  # default palette
  if (is.null(fill_values)) {
    lvls <- levels(factor(plot_df$group))
    fill_values <- setNames(RColorBrewer::brewer.pal(max(3, length(lvls)), "Set2")[seq_along(lvls)], lvls)
  }
  
  # Compute shortened CI endpoints (purely aesthetic)
  df_ci <- plot_df
  if (!is.null(plot_df$ymin) && !is.null(plot_df$ymax) && any(is.finite(plot_df$ymin) | is.finite(plot_df$ymax))) {
    hw <- (plot_df$ymax - plot_df$ymin) / 2
    df_ci$ymin2 <- plot_df$y - hw
    df_ci$ymax2 <- plot_df$y + hw
  } else {
    df_ci$ymin2 <- NA_real_; df_ci$ymax2 <- NA_real_
  }
  
  p <- ggplot(plot_df)
  
  if (orientation == "horizontal") {
    p <- p +
      geom_col(
        aes(y = x, x = y, fill = group), 
        width = bar_width, position = pd, alpha = 0.97) +
      
      ggplot2::geom_errorbarh(
        data = df_ci,
        aes(y = x, xmin = ymin2, xmax = ymax2, group = group),
        height = ci_cap,          # thickness (vertical) of caps
        position = pd,            # same dodge as bars
        linewidth = 0.6,          # line thickness
        color = font_color,
        inherit.aes = FALSE
      ) +
      
      geom_text(
        data = df_label,
        aes(y = x, x = lab_val, label = label, group = group),
        position = pd, hjust = 0, size = label_size, family = family, color = font_color,
        inherit.aes = FALSE
      ) +
      
      geom_text(
        data = star_ann,
        aes(y = x, x = val, label = stars),
        hjust = 0, vjust = 0.5,
        size = star_size, family = family, color = font_color,
        inherit.aes = FALSE
      ) +
      
      scale_x_continuous(oob = scales::squish,
                         breaks = value_breaks, labels = value_labels,
                         expand = expansion(mult = c(0, 0.05))) +
      
      { if (!is.null(value_limits)) coord_cartesian(xlim = value_limits, clip = "on") else NULL } +
      
      labs(x = NULL, y = NULL, fill = legend_title) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = family, color = font_color),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        axis.text.x = ggtext::element_markdown(
          size = x_text_size,
          lineheight = x_axis_lineheight,
          color = "gray30", # font_color
          family = family
        ),
        
        legend.title = element_text(size = legend_title_size),
        legend.text  = element_text(size = legend_text_size)
      )
    
    # compute the intended y order once, preserving an existing factor order
    y_levels <- if (is.factor(plot_df$x)) levels(plot_df$x) else unique(as.character(plot_df$x))
    
    y_lab <- NULL
    if (!is.null(axis_labels_html)) {
      y_lab <- axis_labels_html[y_levels]
    }

    p <- p + scale_y_discrete(
      limits = y_levels,
      labels = if (is.null(y_lab)) waiver() else y_lab,
      drop   = FALSE
    )
    
    if (!is.null(axis_labels_html)) {
      # p <- p + scale_y_discrete(labels = axis_labels_html)
      p <- p + theme(axis.text.y = ggtext::element_markdown(size = name_size,
                                                            lineheight = y_axis_lineheight,
                                                            color = font_color,
                                                            family = family))
    }
    
  } else {  # vertical
    p <- p +
      geom_col(aes(x = x, y = y, fill = group), width = bar_width, position = pd, alpha = 0.97) +
      
      geom_errorbar(
        data = df_ci,
        aes(x = x, ymin = ymin2, ymax = ymax2, group = group),
        width = ci_cap, position = pd, color = font_color, inherit.aes = FALSE
      ) +
      
      geom_text(
        aes(x = x, y = y, label = label, group = group),
        position = pd, vjust = -0.2, size = label_size, family = family, color = font_color
      ) +
      
      geom_text(
        data = star_ann,
        aes(y = x, x = val, label = stars),
        hjust = 0, vjust = 0.5,
        size = star_size, family = family, color = font_color,
        inherit.aes = FALSE
      ) +
      
      scale_y_continuous(breaks = value_breaks, labels = value_labels,
                         expand = expansion(mult = c(0, 0.08))) +
      { if (!is.null(value_limits)) coord_cartesian(ylim = value_limits, clip = "on") else NULL } +
      labs(x = NULL, y = NULL, fill = legend_title) +
      theme_minimal() +
      theme(
        text = element_text(family = family, color = font_color),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    if (!is.null(axis_labels_html)) {
      p <- p + scale_x_discrete(labels = axis_labels_html)
      p <- p + theme(axis.text.x = ggtext::element_markdown(size = name_size,
                                                            lineheight = y_axis_lineheight,
                                                            color = "gray30", # font_color,
                                                            family = family))
    }
  }
  
  if (nlevels(factor(plot_df$facet)) > 1) {
    p <- p + facet_wrap(~ facet, ncol = facet_ncol, scales = facet_scales) +
      theme(
        strip.text   = element_text(size = facet_text_size, face = "bold", color = font_color),
        panel.spacing = unit(2.5, "lines")   # control spacing here
      )
  }
  
  p + scale_fill_manual(values = fill_values, breaks = names(fill_values))
}
