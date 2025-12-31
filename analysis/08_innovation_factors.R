# Creates heat maps and forest plots (un-adjusted odds) to illustrate 
# which factors were specifically cited as influencing clinicians'
# decisions to adopt each innovation.


# =========================================================
# Prep data
# =========================================================

# Source helper functions
source(file.path("code", "utils", "load_packages.R"))

packages <- c(
  "broom", "forcats", "dplyr", "tidyr", "ggplot2", "stringr", 
  "kableExtra", "knitr", "purrr", "scales", "tibble",
  "showtext", "patchwork"
)

load_packages(packages)

# Check which fonts are installed
# systemfonts::system_fonts() |> View()
# If you don’t find the font there, you need to install it. If it’s a google font, navigate to Google Fonts, search for the font, select it to go to its own page, and click on “Download family”. Once it has downloaded, unzip the file, select all the .ttf files and, if you’re on a Windows device, right click and select “Install for all users”. 

# Register the font
# font_add("Figtree", "C:/Users/chuber/AppData/Local/Microsoft/Windows/Fonts/Figtree-VariableFont_wght.ttf")

# Turn on showtext
showtext_auto()

source(file.path("code", "utils", "07_load_logistic_data.R"))
source(file.path("code", "utils", "07_logistic_model_helpers.R"))
source(file.path("code", "utils", "load_custom_fonts.R"))

# Define all columns and their groups
all_columns <- tibble(
  group = rep(c("iv", "hfo", "ppe"), each = 10),
  term = c(paste0("iv3c_", 1:10), paste0("hfo2c_", 1:10), paste0("ppe1c_", 1:10))
) %>%
  mutate(
    item = as.integer(str_extract(term, "(?<=_)\\d+"))
  )

# Get pretty labels for all terms
all_columns <- all_columns %>%
  mutate(
    pretty_label = sapply(term, function(col) {
      label <- attr(survey_results[[col]], "label")
      paste(strwrap(label, width = 30), collapse = "\n")
    })
  )

# Define custom replacements
custom_labels <- c(
  NA, # 1: no change
  "Hospital or organization policy", # 2
  "Colleagues at my institution",    # 3
  "Colleagues at other institutions",# 4
  "ED medical leadership guidance",  # 5
  "ED nursing leadership guidance",  # 6
  NA, # 7: no change
  "Information from social media, online", # 8
  "Anecdotes about patient outcomes", # 9
  NA  # 10: no change
)

# Apply custom labels where available
term_label_lookup_all <- all_columns %>%
  mutate(
    pretty_label = ifelse(!is.na(custom_labels[item]), custom_labels[item], pretty_label)
  ) %>%
  dplyr::select(group, term, item, pretty_label)

# View the combined lookup
print(term_label_lookup_all)

make_summary_table_wide_pval <- function(survey_data, group_name, outcome_var, term_lookup, filter_var, filter_vals) {
  group_terms <- term_lookup %>% filter(group == group_name) %>% pull(term)
  
  # Use unwrapped labels
  term_lookup <- term_lookup %>%
    mutate(pretty_label = str_replace_all(pretty_label, "\n", " "))
  
  # Filter the data
  filtered <- survey_results %>% filter(survey_results[[filter_var]] %in% filter_vals)
  
  # Prepare long data
  long <- filtered %>%
    dplyr::select(all_of(c(outcome_var, group_terms))) %>%
    pivot_longer(cols = all_of(group_terms), names_to = "term", values_to = "value") %>%
    filter(!is.na(value), !is.na(.data[[outcome_var]]))
  
  # Calculate percentages
  summary <- long %>%
    group_by(term, suggested = .data[[outcome_var]]) %>%
    summarise(
      pct = mean(value == 1, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    left_join(term_lookup, by = "term") %>%
    mutate(
      suggested = ifelse(suggested == 1, "Recommended", "Did Not Recommend")
    ) %>%
    dplyr::select(pretty_label, term, suggested, pct)
  
  pval_df <- long %>%
    group_by(term) %>%
    summarise(
      p_value = tryCatch({
        tbl <- table(value == 1, .data[[outcome_var]])
        print(tbl)  # <-- This will print each 2x2 table
        if (all(dim(tbl) == c(2, 2))) {
          chisq.test(tbl, correct = FALSE)$p.value
        } else {
          NA
        }
      }, error = function(e) NA),
      .groups = "drop"
    )
  
  # Merge and pivot
  summary_wide <- summary %>%
    pivot_wider(names_from = suggested, values_from = pct) %>%
    left_join(pval_df, by = "term") %>%
    mutate(
      pval_sig = case_when(
        is.na(p_value) ~ "",
        p_value < 0.001 ~ paste0(formatC(p_value, format = "e", digits = 1), " ***"),
        p_value < 0.01  ~ paste0(signif(p_value, 2), " **"),
        p_value < 0.05  ~ paste0(signif(p_value, 2), " *"),
        TRUE ~ as.character(signif(p_value, 2))
      )
    ) %>%
    dplyr::select(pretty_label, `Recommended`, `Did Not Recommend`, p_value, pval_sig) %>%
    arrange(pretty_label)
  
  summary_wide <- summary_wide %>%
    mutate(
      sig_direction = case_when(
        is.na(p_value) ~ NA_character_,
        p_value < 0.05 & `Recommended` > `Did Not Recommend` ~ "Recommended > Not",
        p_value < 0.05 & `Recommended` < `Did Not Recommend` ~ "Not > Recommended",
        TRUE ~ "ns"
      )
    )
  
  return(summary_wide)
}

# Create summary tables for each group
ppe_summary <- make_summary_table_wide_pval(survey_results, "ppe", "ppe1a", term_label_lookup_all, "ppe1b", c(2, 3))
hfo_summary <- make_summary_table_wide_pval(survey_results, "hfo", "hfo2a", term_label_lookup_all, "hfo2b", c(2, 3))
iv_summary  <- make_summary_table_wide_pval(survey_results, "iv",  "iv3a",  term_label_lookup_all, "iv3b", c(2, 3))

# Helper function to format value with significance
format_value <- function(value, sig) {
  sig_asterisks <- gsub(".*?(\\*+)$", "\\1", sig) # Extract asterisks at end
  sig_asterisks <- ifelse(grepl("\\*", sig_asterisks), sig_asterisks, "") # Only keep asterisks
  value <- round(as.numeric(value), 1)
  paste0(value, sig_asterisks)
}

# Format each column
ppe_col <- mapply(format_value, ppe_summary$Recommended, ppe_summary$pval_sig)
hfo_col <- mapply(format_value, hfo_summary$Recommended, hfo_summary$pval_sig)
iv_col  <- mapply(format_value, iv_summary$Recommended, iv_summary$pval_sig)

plot_group_heatmap_from_summary <- function(summary_data, group_name, plot_title = NULL) {
  library(ggtext)
  library(tidyverse)
  
  # Reshape to long format
  long_data <- summary_data %>%
    pivot_longer(cols = c("Recommended", "Did Not Recommend"),
                 names_to = "recommended_label", values_to = "prop_percent") %>%
    mutate(
      predictor_value = ifelse(recommended_label == "Recommended", 1, 0),
      sig_label = ifelse(recommended_label == "Recommended" & str_detect(pval_sig, "\\*"), "*", ""),
      prop_positive = prop_percent / 100
    )
  
  # Set fill range
  vals <- long_data$prop_positive
  vals <- vals[is.finite(vals) & !is.na(vals)]
  m <- mean(vals)
  s <- sd(vals)
  
  lower_limit <- m - 2 * s
  upper_limit <- m + 2 * s
  
  # Optional bolding: we can say a row is "sig" if it had a sig_label on the Recommended side
  y_labels <- long_data %>%
    filter(recommended_label == "Recommended") %>%
    mutate(is_sig = str_detect(pval_sig, "\\*")) %>%
    distinct(pretty_label, is_sig) %>%
    mutate(label_fmt = ifelse(is_sig, paste0("<b>", pretty_label, "</b>"), pretty_label)) %>%
    { setNames(.$label_fmt, .$pretty_label) }
  
  # Plot
  ggplot(long_data, aes(x = factor(predictor_value), y = pretty_label, fill = prop_positive)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.0f%%", prop_percent), sig_label)), size = 4) +
    scale_x_discrete(
      labels = c("0" = "Did Not Recommend", "1" = "Recommended"),
      position = "top"
    ) +
    scale_fill_gradient2(
      high = "#689268", mid = "gray95", low = "#d73027",
      midpoint = m, limits = c(lower_limit, upper_limit), oob = scales::squish,
      na.value = "gray90", name = "Proportion"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = plot_title
    ) +
    scale_y_discrete(labels = y_labels) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_markdown(size = 11, hjust = 1),
      plot.margin = margin(5, 5, 10, 30),
      legend.position = "none"
    )
}

# Color scale: gray = average, green = ≥2 SD above mean, red = ≤2 SD below mean; more extreme values are shown with the same color.

ppe_plot <- plot_group_heatmap_from_summary(ppe_summary, "ppe", plot_title = "PPE Reuse")
hfo_plot <- plot_group_heatmap_from_summary(hfo_summary, "hfo", plot_title = "High Flow Oxygen")
iv_plot  <- plot_group_heatmap_from_summary(iv_summary,  "iv", plot_title = "Ivermectin")

ppe_plot
hfo_plot
iv_plot

# Add a column to identify the intervention
ppe_summary$Intervention <- "PPE Reuse"
hfo_summary$Intervention <- "High-Flow Oxygen"
iv_summary$Intervention  <- "Ivermectin"

# Combine all into one long data frame
all_summary <- bind_rows(
  ppe_summary %>% dplyr::select(pretty_label, Recommended, pval_sig, sig_direction, Intervention),
  hfo_summary %>% dplyr::select(pretty_label, Recommended, pval_sig, sig_direction, Intervention),
  iv_summary  %>% dplyr::select(pretty_label, Recommended, pval_sig, sig_direction, Intervention)
)

all_summary <- all_summary %>%
  mutate(
    # Extract asterisks from pval_sig
    asterisks = ifelse(grepl("\\*", pval_sig),
                       gsub(".*?(\\*+)$", "\\1", pval_sig),
                       ""),
    # Combine numeric value + asterisks + arrow
    value_fmt = paste0(round(Recommended, 1), asterisks)
  )

# Define your preferred column order for the heatmap
intervention_order <- c("PPE Reuse", "High-Flow Oxygen", "Ivermectin")

# Convert Intervention to an ordered factor
all_summary <- all_summary %>%
  mutate(Intervention = factor(Intervention, levels = intervention_order))

# reorder reasons
reason_order <- c(
  "Information from social media, online",
  "Ease of implementation",
  "Anecdotes about patient outcomes",
  "Necessity or no other option",
  "Evidence from medical literature",
  "Hospital or organization policy",
  "Colleagues at other institutions",
  "Colleagues at my institution",
  "ED nursing leadership guidance",
  "ED medical leadership guidance"
)

all_summary$pretty_label <- factor(all_summary$pretty_label, levels = reason_order)

source(file.path("code", "utils", "05_mean_bar_plots.R"))

# helper to pull star asterisks out of pval_sig
extract_stars <- function(s) {
  ifelse(grepl("\\*", s), sub(".*?(\\*+)$", "\\1", s), "")
}

# Convert one summary table (wide) to the plot_bars schema
summary_to_plotdf <- function(df, facet_label, reason_order = NULL) {
  df %>%
    # keep what's needed
    dplyr::select(pretty_label, `Recommended`, `Did Not Recommend`, pval_sig) %>%
    # reshape to long groups
    pivot_longer(
      cols = c("Recommended", "Did Not Recommend"),
      names_to = "group", values_to = "pct"
    ) %>%
    mutate(
      facet = facet_label,
      # order reasons if you have a preferred order
      x = if (is.null(reason_order)) factor(pretty_label) else factor(pretty_label, levels = reason_order),
      
      # group order for bars/legend
      group = recode(
        group,
        "Recommended" = "Adopters",
        "Did Not Recommend" = "Non-Adopters"
      ),
      
      group = factor(group, levels = c("Non-Adopters", "Adopters")),
      
      # group = factor(group, levels = c("Did Not Recommend", "Recommended")),
      # proportions 0–1 for value axis
      y = pct / 100,
      # value labels on bars
      label = sprintf("%.1f%%", pct),
      # attach stars only to the Recommended row; plot_bars collapses to one star per pair
      stars = ifelse(group == "Adopters", extract_stars(pval_sig), "")
    ) %>%
    dplyr::select(facet, x, group, y, label, stars)
}

plot_df_reasons <- bind_rows(
  summary_to_plotdf(ppe_summary, "PPE Reuse"),
  summary_to_plotdf(hfo_summary, "High-Flow Oxygen"),
  summary_to_plotdf(iv_summary,  "Ivermectin")
)

plot_df_reasons <- plot_df_reasons %>%
  mutate(x = fct_relevel(as.character(x), reason_order)) %>%
         # group = fct_relevel(group, "Non-Adopters", "Adopters")) %>%
  arrange(x, facet, group)

wrap_width <- 20

plot_df_reasons <- plot_df_reasons %>%
  mutate(
    x = str_wrap(as.character(x), width = wrap_width)
  ) 

clinician_justifications <- plot_bars(
  plot_df_reasons,
  orientation   = "horizontal",
  fill_values   = c("Adopters" = "#4F8F4F", "Non-Adopters" = "#BDBDBD"),
  legend_title  = "",
  
  value_limits  = c(0, 1),
  value_breaks  = seq(0, 1, by = 0.2),
  value_labels  = paste0(seq(0, 100, by = 20), "%"),
  
  # Layout
  facet_ncol    = NULL,
  facet_scales  = "free_y",
  dodge_w       = 0.8,
  bar_width     = 0.8,
  
  label_offset  = 0.03,
  star_offset   = 0.24,
  label_size = 12,
  star_size  = 17,
  
  x_axis_lineheight = 0.2,        # controls spacing between lines of labels
  
  cap_size = 30,
  facet_text_size = 38,
  x_text_size = 30,
  legend_text_size = 33
) +
  facet_grid(
    cols = vars(facet),         # ← horizontal facets by intervention
    scales = "free_y",
    space = "free_y"
  ) +
  theme(
    panel.spacing.x = unit(1.3, "lines"),   # space between panels
    axis.text.y = element_text(size = 35, lineheight = 0.3)
  )

clinician_justifications

# ggsave("plots/justifications_for_adoption.png", p_reasons, width = 9, height = 7.3)


# Plot heatmap
reason_percentage_heatmap <- ggplot(all_summary, aes(x = Intervention, y = pretty_label, fill = Recommended)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value_fmt), size = 8, color = "gray10", family = "Nunito") +
  geom_richtext(
    aes(label = ifelse(sig_direction == "Not > Recommended",
                       "<span style='font-family:DejaVu Sans; font-weight:bold;'>&#8595;</span>", 
                       "")),
    fill = NA, label.color = NA, size = 8,
    nudge_x = 0.22
  ) +
  coord_fixed(ratio = 0.23) +
  scale_x_discrete(position = "top") +  # Move x labels to the top
  scale_fill_gradient2(
    low = "#ca3433", mid = "gray95", high = "#689268", 
    
    midpoint = mean(all_summary$Recommended, na.rm = TRUE),
    name = "Cited Factor (%)",
    guide = guide_colorbar(
      barwidth = 7,
      barheight = 0.5,
    )
  ) +
  labs(
    x = NULL, y = NULL,
    title = NULL
    # caption = "Significance: *** p < .001  ** p < .01  * p < .05 <br><span style='font-family:DejaVu Sans; font-weight:bold;'>↓</span>: reason more endorsed by non-users"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Nunito", color = "gray20"),
    axis.text.x = element_text(size = 22, hjust = 0.5),
    axis.text.x.top = element_text(margin = margin(b = 3, t = 5)),  # More space above the labels
    axis.text.y = element_text(size = 21, hjust = 1),
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "bottom",
    legend.text = element_text(size = 22), # color = "gray20"
    legend.title = element_text(size = 22),
    # plot.caption = element_text(size = 22,margin = margin(t = 10, r = 5)),
    plot.caption = element_markdown(
      size = 22,
      lineheight = 0.5
      #margin = margin(t = 5, r = 5),
    ),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) 

reason_percentage_heatmap

# # Save as PNG (background is white by default, but you can specify)
# ggsave(
#   "plots/reason_percentage_heatmap.png",
#   plot = reason_percentage_heatmap,
#   width = 4.5, height = 3, #   width = 7.5, height = 4.5,
#   # units = "in",
#   bg = "white"   # Ensures white background
# )


###### ODDS RATIOS USING FISHER TEST ########

make_summary_table_wide_or_ci <- function(survey_data, group_name, outcome_var, term_lookup, filter_var, filter_vals) {
  group_terms <- term_lookup %>% filter(group == group_name) %>% pull(term)
  
  # Use unwrapped labels
  term_lookup <- term_lookup %>%
    mutate(pretty_label = stringr::str_replace_all(pretty_label, "\n", " "))
  
  # Filter the data
  filtered <- survey_results %>% filter(survey_results[[filter_var]] %in% filter_vals)
  
  # Prepare long data
  long <- filtered %>%
    dplyr::select(all_of(c(outcome_var, group_terms))) %>%
    tidyr::pivot_longer(cols = all_of(group_terms), names_to = "term", values_to = "value") %>%
    filter(!is.na(value), !is.na(.data[[outcome_var]]))
  
  # Calculate percentages
  summary <- long %>%
    group_by(term, suggested = .data[[outcome_var]]) %>%
    summarise(
      pct = mean(value == 1, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    left_join(term_lookup, by = "term") %>%
    mutate(
      suggested = ifelse(suggested == 1, "Recommended", "Did Not Recommend")
    ) %>%
    dplyr::select(pretty_label, term, suggested, pct)
  
  # Calculate p-values, odds ratios, and CIs for each term
  or_pval_df <- long %>%
    group_by(term) %>%
    summarise(
      or = tryCatch({
        tbl <- table(value == 1, .data[[outcome_var]])
        if (all(dim(tbl) == c(2, 2))) {
          fisher.test(tbl)$estimate
        } else {
          NA
        }
      }, error = function(e) NA),
      lower_ci = tryCatch({
        tbl <- table(value == 1, .data[[outcome_var]])
        if (all(dim(tbl) == c(2, 2))) {
          fisher.test(tbl)$conf.int[1]
        } else {
          NA
        }
      }, error = function(e) NA),
      upper_ci = tryCatch({
        tbl <- table(value == 1, .data[[outcome_var]])
        if (all(dim(tbl) == c(2, 2))) {
          fisher.test(tbl)$conf.int[2]
        } else {
          NA
        }
      }, error = function(e) NA),
      p_value = tryCatch({
        tbl <- table(value == 1, .data[[outcome_var]])
        if (all(dim(tbl) == c(2, 2))) {
          fisher.test(tbl)$p.value
        } else {
          NA
        }
      }, error = function(e) NA),
      .groups = "drop"
    )
  
  # Merge and pivot
  summary_wide <- summary %>%
    pivot_wider(names_from = suggested, values_from = pct) %>%
    left_join(or_pval_df, by = "term") %>%
    mutate(
      pval_sig = case_when(
        is.na(p_value) ~ "",
        p_value < 0.001 ~ paste0(formatC(p_value, format = "e", digits = 1), " ***"),
        p_value < 0.01  ~ paste0(signif(p_value, 2), " **"),
        p_value < 0.05  ~ paste0(signif(p_value, 2), " *"),
        p_value < 0.10  ~ paste0(signif(p_value, 2), " +"),
        TRUE ~ as.character(signif(p_value, 2))
      ),
      or_fmt = ifelse(is.na(or), "", sprintf("%.2f", or))
    ) %>%
    dplyr::select(pretty_label, `Recommended`, `Did Not Recommend`, or, lower_ci, upper_ci, pval_sig) %>%
    arrange(pretty_label)
  
  print(names(summary_wide))
  print(head(summary_wide))
  print(or_pval_df)
  
  colnames(summary_wide) <- c("Factor", "Recommended", "Did Not Recommend", "Odds Ratio", "Lower CI", "Upper CI", "p-value (sig)")
  return(summary_wide)
}

ppe_fisher_or <- make_summary_table_wide_or_ci(survey_results, "ppe", "ppe1a", term_label_lookup_all, "ppe1b", c(2, 3))
hfo_fisher_or <- make_summary_table_wide_or_ci(survey_results, "hfo", "hfo2a", term_label_lookup_all, "hfo2b", c(2, 3))
iv_fisher_or  <- make_summary_table_wide_or_ci(survey_results, "iv",  "iv3a",  term_label_lookup_all, "iv3b", c(2, 3))

# # Merge the three summary tables by Factor
summary_fisher_or <- reduce(
  list(
    ppe_fisher_or %>% dplyr::select(Factor, PPE_Recommended = Recommended, PPE_pval = `p-value (sig)`),
    hfo_fisher_or %>% dplyr::select(Factor, HFO_Recommended = Recommended, HFO_pval = `p-value (sig)`),
    iv_fisher_or %>% dplyr::select(Factor, IV_Recommended = Recommended, IV_pval = `p-value (sig)`)
  ),
  full_join, by = "Factor"
)

get_log10_axis_limits <- function(..., or_col = "Odds Ratio", lower_col = "Lower CI", upper_col = "Upper CI") {
  dfs <- list(...)
  all_vals <- unlist(lapply(dfs, function(df) c(df[[or_col]], df[[lower_col]], df[[upper_col]])), use.names = FALSE)
  all_vals <- all_vals[is.finite(all_vals) & !is.na(all_vals) & all_vals > 0]
  
  lower <- min(all_vals, na.rm = TRUE) * 0.9
  upper <- max(all_vals, na.rm = TRUE) * 1.1
  c(lower, upper)
}

plot_limits <- get_log10_axis_limits(ppe_fisher_or, hfo_fisher_or, iv_fisher_or)


# Get full union of Factor names from all 3 summary tables
all_labels <- unique(c(
  ppe_fisher_or$Factor,
  hfo_fisher_or$Factor,
  iv_fisher_or$Factor
))

# Optional: sort alphabetically or by your preferred order
all_labels <- sort(all_labels)

# Compute global min/max of log(OR)
all_log_or_vals <- unlist(lapply(list(ppe_fisher_or, hfo_fisher_or, iv_fisher_or), function(df) {
  log_vals <- log(df[["Odds Ratio"]])
  log_vals[is.finite(log_vals)]
}))

log_or_limits <- range(all_log_or_vals, na.rm = TRUE)

# Define a single shared fill scale with explicit limits
shared_fill_scale <- scale_fill_gradient2(
  low = "#d73027", mid = "gray90", high = "#1a9850",
  midpoint = 0,
  limits = log_or_limits, 
  name = "log(OR)"
)

plot_unadjusted_forest <- function(summary_table, 
                                   or_col = "Odds Ratio",
                                   lower_col = "Lower CI",
                                   upper_col = "Upper CI",
                                   label_col = "Factor",
                                   pval_col = "p-value (sig)",
                                   plot_title = NULL,
                                   x_limits = plot_limits,
                                   show_legend = TRUE,
                                   label_levels = NULL,
                                   fill_scale = shared_fill_scale) {
  
  plot_data <- summary_table %>%
    filter(!is.na(.data[[or_col]])) %>%
    mutate(
      log_or = log(.data[[or_col]]),
      label = .data[[label_col]],
      pval = .data[[pval_col]],
      sig_label = case_when(
        pval < 0.001 ~ "***",
        pval < 0.01 ~ "**",
        pval < 0.05 ~ "*",
        pval < 0.10 ~ "†",
        TRUE ~ ""
      ),
      est_label = paste0(sprintf("%.2f", .data[[or_col]]), sig_label)
    ) %>%
    arrange(.data[[or_col]]) %>%
    mutate(label = factor(label, levels = label_levels))  
  
  ggplot(plot_data, aes(x = .data[[or_col]], y = label)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = .data[[lower_col]], xmax = .data[[upper_col]]), 
                   height = 0.25, color = "gray40") +
    geom_point(aes(fill = log_or), size = 5, shape = 21, color = "black") +
    geom_text(aes(label = est_label, x = .data[[upper_col]] * 1.1), 
              hjust = 0, size = 5, color = "black") +
    scale_x_log10(limits = x_limits) +
    fill_scale +
    labs(x = NULL, y = NULL, subtitle = plot_title) +
    theme_minimal(base_size = 18) +  # bump all text sizes
    theme(
      axis.text.y  = element_text(size = 16),
      axis.text.x  = element_text(size = 14),
      plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = if (show_legend) "bottom" else "none",
      legend.text  = element_text(size = 14),
      legend.title = element_text(size = 16)
    )
}

ppe_or <- plot_unadjusted_forest(
  ppe_fisher_or,
  plot_title = "PPE Reuse",
  x_limits = plot_limits,
  show_legend = FALSE, # set to FALSE if combined
  label_levels = all_labels,
  fill_scale = shared_fill_scale
)

ppe_or

hfo_or <- plot_unadjusted_forest(
  hfo_fisher_or,
  plot_title = "High-Flow Oxygen",
  x_limits = plot_limits,
  show_legend = FALSE, # set to FALSE if combined
  label_levels = all_labels,
  fill_scale = shared_fill_scale
) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

hfo_or

iv_or <- plot_unadjusted_forest(
  iv_fisher_or,
  plot_title = "Ivermectin",
  x_limits = plot_limits,
  show_legend = TRUE,
  label_levels = all_labels,
  fill_scale = shared_fill_scale
) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

iv_or

combined_forest_plot <- (ppe_or | hfo_or | iv_or) +
  # plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center",
      text = element_text(size = 16)
    )
  )

combined_forest_plot <- combined_forest_plot +
  plot_annotation(
    title = "Unadjusted Odds Ratio (log scale)",
    caption = "Error bars: 95% CI. Dashed line: OR = 1. Asterisks indicate significance.",
    theme = theme(
      plot.title   = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 14, hjust = 0.5, margin = margin(t = 10))
    )
  )

combined_forest_plot