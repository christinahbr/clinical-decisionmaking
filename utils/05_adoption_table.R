library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(officer)
library(flextable)

# --- Map each innovation to its columns and a pretty name ---
innovation_map <- tribble(
  ~innovation, ~suggest_col, ~flex_col, ~pretty,
  "ppe", "ppe1a", "ppe1b", "PPE reuse",
  "hfo", "hfo2a", "hfo2b", "High-flow oxygen",
  "iv",  "iv3a",  "iv3b",  "Ivermectin"
)

survey_results %>%
  filter(ppe1b %in% c(2,3)) %>%
  count(ppe1a)

survey_results %>%
  count(ppe1a)


# --- Helper: summarize within a subset ---
summarize_subset <- function(df, suggest_col, flex_col = NULL, subset_label = "All clinicians", filter_flex = FALSE) {
  data_sub <- df %>%
    filter(provtype %in% c("RN","MD"))
  
  # Apply flexibility filter only if requested
  if (filter_flex && !is.null(flex_col)) {
    data_sub <- data_sub %>%
      filter(.data[[flex_col]] %in% c(2, 3))
  }
  
  # By group (RN / MD)
  by_group <- data_sub %>%
    group_by(provtype) %>%
    summarise(
      denom = sum(!is.na(.data[[suggest_col]])),
      n     = sum(.data[[suggest_col]] == 1, na.rm = TRUE),
      pct   = ifelse(denom > 0, 100 * n / denom, NA_real_),
      .groups = "drop"
    )
  
  # Total across RN + MD
  total <- data_sub %>%
    summarise(
      provtype = "Total",
      denom = sum(!is.na(.data[[suggest_col]])),
      n     = sum(.data[[suggest_col]] == 1, na.rm = TRUE),
      pct   = ifelse(denom > 0, 100 * n / denom, NA_real_)
    )
  
  bind_rows(by_group, total) %>%
    mutate(subset = subset_label)
}

# --- Build tidy table across innovations ---
adoption_long <-
  innovation_map %>%
  mutate(res = pmap(., function(innovation, suggest_col, flex_col, pretty) {
    
    all_tbl <- summarize_subset(
      survey_results,
      suggest_col = suggest_col,
      subset_label = "All clinicians",
      filter_flex = FALSE
    )
    
    flex_tbl <- summarize_subset(
      survey_results,
      suggest_col = suggest_col,
      flex_col = flex_col,
      subset_label = "Flexible for this innovation",
      filter_flex = TRUE
    )
    
    bind_rows(all_tbl, flex_tbl) %>%
      mutate(innovation_code = innovation, innovation = pretty)
  })) %>%
  pull(res) %>%
  bind_rows()

adoption_table_pretty <-
  adoption_long %>%
  dplyr::select(-denom) %>%
  mutate(
    provtype = factor(provtype, levels = c("RN","MD","Total")),
    subset   = factor(subset, levels = c("All clinicians","Flexible for this innovation")),
    pct = round(pct, 1),
    cell = dplyr::if_else(is.na(n) | is.na(pct), NA_character_, sprintf("%d (%.1f%%)", n, pct))
  ) %>%
  dplyr::select(innovation, subset, provtype, cell) %>%
  tidyr::pivot_wider(names_from = provtype, values_from = cell) %>%
  arrange(factor(innovation, levels = innovation_map$pretty), subset)


adoption_table_pretty %>%
  kable(format = "html", caption = "Adoption Summary Table") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "responsive"),
    position = "center"
  ) %>%
  kable_minimal()


# --- Convert the pretty table to a flextable ---
ft <- flextable(adoption_table_pretty)

# Optional formatting tweaks
ft <- ft %>%
  autofit() %>%
  theme_vanilla() %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "all")

# --- Define output path ---
# If you have a "plots" subfolder in your working directory:
output_path <- file.path("plots", "innovation_adoption_table.docx")

# Or, if using here():
# output_path <- here::here("plots", "innovation_adoption_table.docx")

# --- Save to Word ---
read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = output_path)
