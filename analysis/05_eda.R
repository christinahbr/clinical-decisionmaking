# =========================================================
# Prep code
# =========================================================

# Source helper functions
source(file.path("code", "utils", "load_packages.R"))
source(file.path("code", "utils", "grab_columns.R"))

# Load clean data
survey_results <- readRDS(file.path("data", "clean_data.rds"))

packages = c(
  "ggplot2",
  "dplyr",
  "effectsize",
  "RColorBrewer",
  "ggsignif",
  "haven",
  "forcats",
  "tidyr",
  "stringr",
  "purrr",
  "kableExtra",
  "ggVennDiagram",
  "showtext",
  "sf"
)

load_packages(packages)


a_vars <- get_column_names(survey_results, "a1_1", "a1_15")
b_vars <- get_column_names(survey_results, "b1_1", "b1_15")
c_vars <- get_column_names(survey_results, "c1_1", "c1_15")

info_source_items <- list(a_vars, b_vars, c_vars)



# =========================================================
# Correlations overview
# =========================================================

source(file.path("code", "utils", "05_calc_correlations.R"))
print(corr_long)
print(summary_corr)

# For a vs b:
res_ab <- with(pair_corr, pool_r_fisher(r = r_ab, n = n_ab))
res_ab

# For a vs c:
res_ac <- with(pair_corr, pool_r_fisher(r = r_ac, n = n_ac))
res_ac

cat(fmt_pool(res_ab, "A vs B"), "\n")
cat(fmt_pool(res_ac, "A vs C"), "\n")




# =========================================================
# Information source use/quality distributions
# =========================================================

source(file.path("code", "utils", "05_distribution_plots.R"))

overview_plots <- lapply(info_source_items, function(vars) {
  overview_subsets(
    df = survey_results,
    question_numbers = vars,
    plot_by = "facet",
    xjust = .9
  )
})
overview_plots

provtype_plots <- lapply(info_source_items, function(vars) {
  overview_subsets(
    survey_results,
    vars,
    subset_var = "provtype",
    subset_categories = c("MD", "RN"),
    plot_by = "color",
    chosen_palette = "p1",
    leg_label = "Clinician Type",
    xjust = .9
  )
})
provtype_plots




# =========================================================
# Information source use frequency tables
# Plots frequency tables item by item
# =========================================================

# source(file.path("code", "utils", "05_frequency_tables.R"))
# 
# frequency_tables <- lapply(info_source_items, function(vars) {
#   plot_frequencies(
#     survey_results,
#     vars
#   )
# })
# 
# ## by provider type
# 
# provtype_frequency_tables <- lapply(info_source_items, function(vars) {
#   plot_frequencies(
#     survey_results,
#     vars,
#     subset_var = "provtype",
#     subset_categories = c("MD", "RN")
#   )
# })


# =========================================================
# Organizational attitude distributions
# =========================================================

org_attitude_questions <- c(paste0("b4_", 1:4), paste0("c4_", 1:4))
# attr(survey_results$a4, "label") <- "Teamwork is emphasized"
attr(survey_results$b4_1, "label") <- "ED leadership is accessible"
attr(survey_results$b4_2, "label") <- "ED leadership communicates clearly"
attr(survey_results$b4_3, "label") <- "ED leadership listens to feedback"
attr(survey_results$b4_4, "label") <- "ED leadership is transparent"
attr(survey_results$c4_1, "label") <- "Leadership trusts clinicians’ judgment"
attr(survey_results$c4_2, "label") <- "Leadership supports clinical teams"
attr(survey_results$c4_3, "label") <- "Leadership makes fair decisions"
attr(survey_results$c4_4, "label") <- "Leadership shares timely updates"

## Overview plots
org_attitudes <- overview_subsets(
  df = survey_results,
  question_numbers = org_attitude_questions,
  plot_by = "facet",
  chosen_palette = "p2",
  xjust = .9
)
org_attitudes

## Overview plots by provider type
org_attitudes_by_provtype <- overview_subsets(
  df = survey_results,
  question_numbers = org_attitude_questions,
  subset_var = "provtype",
  subset_categories = c("MD", "RN"),
  plot_by = "color",
  chosen_palette = "p2",
  leg_label = "Clinician Type",
  xjust = .9
)
org_attitudes_by_provtype



# =========================================================
# Adoption of Innovations Table
# =========================================================

# Plot # of clinicians adopting innovations
source(file.path("code", "utils", "05_adoption_table.R"))

adoption_table_pretty


# =========================================================
# Implementation overlap Venn diagram
# =========================================================

# Plot overlap in implementation of innovations
source(file.path("code", "utils", "05_venn_plots.R"))

implementation_venn




# =========================================================
# Implementation use/quality mean bar plots
# =========================================================

source(file.path("code", "utils", "05_mean_bar_plots.R"))


# 1. Define subscale order
subscale_order <- c(
  "News and Social Media",
  "Blogs and Podcasts",
  "Academic Publications",
  "Professional Networks"
)

provtype_order <- c("MD", "RN")

# 2. Create tidy long-format data
tidy_results <- survey_results %>%
  dplyr::select(
    provtype,
    subscale1_avg_a, subscale1_avg_b, # subscale1_avg_c, # uncomment to include current use in figure
    subscale2_avg_a, subscale2_avg_b, # subscale2_avg_c,
    subscale3_avg_a, subscale3_avg_b, # subscale3_avg_c,
    subscale4_avg_a, subscale4_avg_b #, subscale4_avg_c
  ) %>%
  pivot_longer(
    cols = -provtype,
    names_to = c("subscale", "question"),
    names_pattern = "(subscale[0-9]+)_avg_([ab])", # change to "abc" to include current use
    values_to = "score"
  ) %>%
  mutate(
    subscale = recode(subscale, !!!subscale_titles),
    provtype = factor(provtype, levels = provtype_order),
    question = recode(question,
                      "a" = "Use Frequency", # change to "Pandemic Use" (or similar) if including current use
                      "b" = "Perceived Reliability") # replace ) with , if including current use
    # "c" = "Current Use")     # uncomment to include current use in figure
  )

# 3. Define facet (question) order
question_order <- c("Use Frequency", "Perceived Reliability") # or "Pandemic Use", "Perceived Reliability", "Current Use"

# 4. (optional) summarize with t-based 95% CIs
summary_ci <- tidy_results %>%
  group_by(provtype, subscale, question) %>%
  summarise(
    n       = sum(!is.na(score)),
    mean    = mean(score, na.rm = TRUE),
    sd      = sd(score, na.rm = TRUE),
    se      = sd / sqrt(n),
    tcrit   = qt(0.975, df = pmax(n - 1, 1)),
    ci_low  = mean - tcrit * se,
    ci_high = mean + tcrit * se,
    .groups = "drop"
  ) %>%
  mutate(
    question = factor(question, levels = question_order),
    subscale = factor(subscale, levels = subscale_order)
  )

print(summary_ci)

# 5. Build plotting dataframe from RAW data (means, CIs, pairwise stats inside)

plot_df <- prepare_bars(
  data               = tidy_results,
  x                  = "subscale",
  group              = "provtype",
  facet              = "question",
  value              = "score",
  facet_levels       = question_order,
  x_levels           = subscale_order,
  group_levels       = c("MD","RN"),
  ci_level           = 0.95,
  model              = "lm",           # Likert -> lm
  include_interaction= TRUE,
  contrast_method    = "pairwise",
  adjust_method      = "holm",
  effect_size        = TRUE,
  label_fun          = function(m) sprintf("%.2f", m)
)

plot_df <- plot_df %>%
  mutate(
    facet = factor(facet, levels = question_order),
    group = factor(group, levels = c("RN", "MD")),
  )

head(plot_df)

# 6. Plot the data

subscale_caps <- c(
  "Professional Networks" = "96.4% used one/more",
  "News and Social Media" = "80.5% used one/more",
  "Academic Publications" = "96.3% used one/more",
  "Blogs and Podcasts"    = "94.7% used one/more"
)

usage_and_quality <- plot_bars(
  plot_df,
  orientation   = "horizontal",
  fill_values   = c("MD" = "#1f77b4", "RN" = "#ff7f0e"),
  legend_title  = "Clinician type",

  dodge_w       = 0.9,      # smaller = closer bars
  bar_width     = 0.9,      # can bump a tad to keep bars “substantial”

  star_size  = 16,
  star_offset   = 0.5, # pushes stars right
  label_offset = 0.1,

  value_limits  = c(1, 5),
  value_breaks  = 1:5,
  value_labels  = c(
    "1<br>Never",
    "2<br>Rarely",
    "3<br>Sometimes",
    "4<br>Often",
    "5<br>Frequently"
  ),
  facet_ncol    = 3,
  facet_scales  = "fixed",
  
  axis_caps        = subscale_caps,
  name_face        = "bold",
  name_size        = 35,       # match your prior figure sizing
  cap_face         = "italic",
  cap_size         = 30,       # absolute point size for caption line
  y_axis_lineheight = 0.2,
  x_axis_lineheight = 0.4,
  
  facet_text_size = 35,
  x_text_size = 30,
  legend_title_size = 34,
  legend_text_size = 30
)
usage_and_quality

# ggsave("plots/usage_and_quality.png", usage_and_quality, width = 11, height = 3.8)