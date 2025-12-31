#   The following code contains a set of logistic regressions for each of
# the three innovations featured in the survey: PPE reuse, high-flow
# oxygen, and ivermectin.

# Handle Variables

# Source helper functions
source(file.path("code", "utils", "load_packages.R"))

packages <- c(
  "broom",  
  "dplyr",
  "emmeans",
  "forcats",
  "ggplot2",
  "ggtext",
  "gt", 
  "haven",
  "kableExtra", 
  "mediation", 
  "patchwork", 
  "purrr", 
  "showtext", 
  "stringr", 
  "tibble", 
  "tidyr"
)
load_packages(packages)

source(file.path("code", "utils", "grab_columns.R"))
source(file.path("code", "utils", "07_logistic_model_helpers.R"))
source(file.path("code", "utils", "07_load_logistic_data.R"))


# PPE Reuse ------------------------------------------------------

## All Clinicians
ppe_result <- run_filtered_logistic(
  data = survey_results,
  filter_var = "ppe1b", filter_vals = c(2, 3),
  predictors = ppe_predictors,
  response = "ppe1a"
)

ppe_interact <- summarize_flex_by_role(
  ppe_result,
  flex_var = "ppe1b",
  by_var   = "provtype",
  high     = "3",
  low      = "2"
)

print_flex_summary(ppe_interact, by_label = "Provider")

## Doctors
ppe_doctors_result <- run_filtered_logistic(
  data = df_doctors,
  filter_var = "ppe1b", filter_vals = c(2, 3),
  predictors = ppe_predictors,
  response = "ppe1a",
  remove_predict = c("provtype", "ppe1b * provtype")
)

## Nurses
ppe_nurses_result <- run_filtered_logistic(
  data = df_nurses,
  filter_var = "ppe1b", filter_vals = c(2, 3),
  predictors = ppe_predictors,
  response = "ppe1a",
  remove_predict = c("provtype", "ppe1b * provtype")
)

nurse_predictors <- c("ed_transpar_avg", "org_lead_avg")

ppe_nurses_result <- run_logistic_regression(
  data = df_nurses,
  predictors = nurse_predictors,
  response = "subscale1_avg_b"
)

# High-Flow Oxygen -----------------------------------------------

## All Clinicians
hfo_result <- run_filtered_logistic(
  data = survey_results,
  filter_var = "hfo2b", filter_vals = c(2, 3),
  predictors = hfo_predictors,
  response = "hfo2a"
)

hfo_interact <- summarize_flex_by_role(
  hfo_result,
  flex_var = "hfo2b",
  by_var   = "provtype",
  high     = "3",
  low      = "2"
)
print_flex_summary(hfo_interact, by_label = "Provider")

## Doctors
hfo_doctors_result <- run_filtered_logistic(
  data = df_doctors,
  filter_var = "hfo2b", filter_vals = c(2, 3),
  predictors = hfo_predictors,
  response = "hfo2a",
  remove_predict = c("provtype", "hfo2b * provtype")
)

## Nurses
hfo_nurses_result <- run_filtered_logistic(
  data = df_nurses,
  filter_var = "hfo2b", filter_vals = c(2, 3),
  predictors = hfo_predictors,
  response = "hfo2a",
  remove_predict = c("provtype", "hfo2b * provtype")
)

# Ivermectin ------------------------------------------------------

## All Clinicians
iv_result <- run_filtered_logistic(
  data = survey_results,
  filter_var = "iv3b", filter_vals = c(2, 3),
  predictors = iv_predictors,
  response = "iv3a"
)

iv_interact <- summarize_flex_by_role(
  iv_result,
  flex_var = "iv3b",
  by_var   = "provtype",
  high     = "3",
  low      = "2"
)
print_flex_summary(iv_interact, by_label = "Provider")

## Doctors
iv_doctors_result <- run_filtered_logistic(
  data = df_doctors,
  filter_var = "iv3b", filter_vals = c("2", "3"),
  predictors = iv_predictors,
  response = "iv3a",
  remove_predict = c("provtype", "iv3b * provtype")
)

## Nurses
iv_nurses_result <- run_filtered_logistic(
  data = df_nurses,
  filter_var = "iv3b", filter_vals = c("2", "3"),
  predictors = iv_predictors,
  response = "iv3a",
  remove_predict = c("provtype", "iv3b * provtype")
)


# Odds Ratio Visualizations ----------------------------------------

term_label_lookup <- tibble::tibble(
  term = c(
    "subscale1_avg_a", 
    "subscale2_avg_a", 
    "subscale3_avg_a", 
    "subscale4_avg_a", 
    "age",
    "gender_collapsedFemale", 
    "gender_collapsedOther or Unknown",
    "RUCA_collapsed2", "RUCA_collapsedOther", 
    "ppe1b3",
    "hfo2b3",
    "iv3b3",
    "provtypeMD",
    "ppe1b3:provtypeMD",
    "hfo2b3:provtypeMD",
    "iv3b3:provtypeMD"
  ),
  pretty_label = c(
    "Professional Networks", 
    "News and Social Media", 
    "Academic Publications",
    "Blogs and Podcasts", 
    "Age", 
    "Female (vs. Male)", 
    "Other or Unknown Gender (vs. Male)",
    "Suburban (vs. Urban)", "Exurban/Rural (vs. Urban)",
    "Perceived Flexibility",
    "Perceived Flexibility",
    "Perceived Flexibility",
    "Physician (vs. Nurse)",
    "Physician x Flexibility",
    "Physician x Flexibility",
    "Physician x Flexibility"
  )
)

## PPE Reuse

ppe_or_data <- make_or_data(ppe_result, "PPE Reuse", term_label_lookup, filter_p = "all")
ppe_or_table <- present_or_kable(ppe_or_data, caption = "Adjusted Odds Ratios (all)")

# Show only significant items
ppe_or_sig <- make_or_data(ppe_result, "PPE Reuse", term_label_lookup, filter_p = "sig")
present_or_kable(ppe_or_sig, caption = "Adjusted Odds Ratios (p < 0.05)")

# Show approaching significance
ppe_or_app <- make_or_data(ppe_result, "PPE Reuse", term_label_lookup, filter_p = "approaching")
present_or_kable(ppe_or_app, caption = "Adjusted Odds Ratios (p < 0.10)")

## High-Flow Oxygen
hfo_or_data <- make_or_data(hfo_result, "High-Flow Oxygen", term_label_lookup, filter_p = "all")
hfo_or_table <- present_or_kable(hfo_or_data, "Adjusted Odds Ratios (all)")

## Ivermectin
iv_or_data <- make_or_data(iv_result, "Ivermectin", term_label_lookup, filter_p = "all")
iv_or_table <- present_or_kable(iv_or_data, "Adjusted Odds Ratios (all)")

# Forest Plots

## Basic
models <- list(ppe_result, hfo_result, iv_result)
titles <- c("Decisions to Reuse PPE", "Decisions to Suggest or Order High-Flow Oxygen", "Decisions to Suggest or Prescribe Ivermectin")
filenames <- c("plots/ppe_odds.png", "plots/hfo_odds.png", "plots/iv_odds.png")
plots <- purrr::map2(models, titles, ~plot_forest(.x, term_label_lookup, .y))
plots
# purrr::walk2(plots, filenames, ~ggsave(.y, plot = .x, width = 12, height = 4))

## Pretty

### Select Predictors
ppe_plot <- plot_signif_forest_pretty(ppe_result, term_label_lookup, NULL, show_legend = FALSE, nudge_x_text = 0.2)  # set to TRUE if combining plots
hfo_plot <- plot_signif_forest_pretty(hfo_result, term_label_lookup, NULL, show_legend = FALSE, nudge_x_text = 0.2) # set to FALSE if combining plots
iv_plot  <- plot_signif_forest_pretty(iv_result,  term_label_lookup, NULL, show_legend = FALSE, nudge_x_text = 0.2) # set to FALSE if combining plots
# ggsave("plots/ppe_plot.png", plot = ppe_plot, width = 8, height = 4, units = "in")
# ggsave("plots/hfo_plot.png", plot = hfo_plot, width = 8, height = 2.5, units = "in")
# ggsave("plots/iv_plot.png", plot = iv_plot, width = 8, height = 4, units = "in")

canonical_order <- c(
  "Female (vs. Male)",
  "Age",
  "Exurban/Rural (vs. Urban)",
  "Suburban (vs. Urban)",
  "Blogs and Podcasts",
  "Academic Publications",
  "News and Social Media",
  "Professional Networks",
  "Physician x Flexibility",
  "Physician (vs. Nurse)",
  "Perceived Flexibility"
)


### All Predictors
ppe_plot_all <- plot_signif_forest_pretty(
  ppe_result, term_label_lookup, NULL, show_legend = FALSE, 
  nudge_x_text = 0.28, show_significant_only = FALSE,
  x_title = "PPE Reuse",
  # bold_terms = c("News and Social Media", "Blogs and Podcasts", "Doctor (vs. Nurse)", "Perceived Flexibility", "Suburban (vs. Urban)"),
  remove_terms = c("gender_collapsedOther or Unknown"), # N < 50; hard to interpret due to inclusion of multiple categories
  selected_font = "Nunito",
  size_scale = 0.78,
  x_limits = c(0.01, 20),
  order_terms = canonical_order
) + theme(plot.caption = element_blank())
ppe_plot_all

hfo_plot_all <- plot_signif_forest_pretty(
  hfo_result, term_label_lookup, NULL, show_legend = FALSE, 
  nudge_x_text = 0.28, show_significant_only = FALSE,
  x_title = "High-Flow Oxygen",
  # bold_terms = c("Academic Publications", "Perceived Flexibility", "Doctor (vs. Nurse)"), # bold predictors approaching significance
  remove_terms = c("gender_collapsedOther or Unknown"), # N < 50; hard to interpret due to inclusion of multiple categories
  selected_font = "Nunito",
  size_scale = 0.78,
  x_limits = c(0.5, 5),
  order_terms = canonical_order
) + theme(axis.text.y = element_blank(), plot.caption = element_blank())
hfo_plot_all

iv_plot_all  <- plot_signif_forest_pretty(
  iv_result,  term_label_lookup, NULL, show_legend = FALSE, 
  nudge_x_text = 0.28, show_significant_only = FALSE,
  x_title = "Ivermectin",
  # bold_terms = c( "Doctor (vs. Nurse)", "Professional Networks", "News and Social Media", "Perceived Flexibility"),
  remove_terms = c("gender_collapsedOther or Unknown"), # N < 50; hard to interpret due to inclusion of multiple categories
  selected_font = "Nunito",
  size_scale = 0.78,
  x_limits = c(0.1, 5),
  order_terms = canonical_order
) + theme(axis.text.y = element_blank(), plot.caption = element_blank())
iv_plot_all

# ggsave("plots/ppe_plot_all.png", plot = ppe_plot_all, width = 7, height = 5, units = "in")
# ggsave("plots/hfo_plot_all.png", plot = hfo_plot_all, width = 7, height = 5, units = "in")
# ggsave("plots/iv_plot_all.png", plot = iv_plot_all, width = 7, height = 5, units = "in")

# 1) three-panel figure (no caption)
combined <- (ppe_plot_all | hfo_plot_all | iv_plot_all) +
  plot_layout(widths = c(2.8, .8, 1.45), guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

# 2) bottom strip that acts like a global x-label / bottom title
bottom_title <- ggplot() +
  theme_void() +
  annotate("text", x = 0.5, y = 0.5,
           label = "Adjusted Odds Ratios (log scale)",
           family = "Nunito", size = 13, fontface = "bold", colour = "gray30") +
  xlim(0, 1) + ylim(0, 1) +
  theme(plot.margin = margin(t = 4, r = 0, b = 0, l = 0))

# 3) stack plots over the bottom strip
stacked <- combined / bottom_title + plot_layout(heights = c(1, 0.07))

final <- (ppe_plot_all | hfo_plot_all | iv_plot_all) +
  plot_layout(widths = c(2.8, .8, 1.45), guides = "collect") +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "grey98", color = NA),
      panel.border    = element_rect(color = "grey85", fill = NA, linewidth = 0.6)
    )
  )

final

# ggsave("plots/forest.png", final, width = 10.5, height = 4.5)
