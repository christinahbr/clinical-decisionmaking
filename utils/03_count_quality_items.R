# Get item lists
cat_b <- categorize_columns(survey_results, categories,
                            include_a = FALSE, include_b = TRUE, include_c = FALSE)
cat_a <- categorize_columns(survey_results, categories,
                            include_a = TRUE,  include_b = FALSE, include_c = FALSE)

sub_titles <- c(
  "Leadership and Professional Networks",
  "News and Social Media Platforms",
  "Academic and Professional Publications",
  "Informal Information Sources"
)

sub_list_b <- list(
  subscale1 = cat_b[[sub_titles[1]]],
  subscale2 = cat_b[[sub_titles[2]]],
  subscale3 = cat_b[[sub_titles[3]]],
  subscale4 = cat_b[[sub_titles[4]]]
)
sub_list_a <- list(
  subscale1 = cat_a[[sub_titles[1]]],
  subscale2 = cat_a[[sub_titles[2]]],
  subscale3 = cat_a[[sub_titles[3]]],
  subscale4 = cat_a[[sub_titles[4]]]
)

# 1) Recode to 1–5 (others -> NA) for only the needed columns
recode_1to5 <- function(x) { v <- as.numeric(x); ifelse(v >= 1 & v <= 5, v, NA_real_) }
all_b_items <- unique(unlist(sub_list_b))
all_a_items <- unique(unlist(sub_list_a))

temp_b <- survey_results %>% dplyr::mutate(across(all_of(all_b_items), recode_1to5))
temp_a <- survey_results %>% dplyr::mutate(across(all_of(all_a_items), recode_1to5))

# 2) Per-item 1–5 counts (B)
count_1to5_per_item <- function(df, cols, sub_id, suffix = "_avg_b") {
  if (length(cols) == 0) {
    return(tibble(subscale_id = sub_id, subscale = paste0(sub_id, suffix),
                  item = character(), n_1to5 = integer()))
  }
  df %>%
    dplyr::select(all_of(cols)) %>%
    dplyr::summarise(across(everything(), ~ sum(!is.na(.x)))) %>%
    tidyr::pivot_longer(everything(), names_to = "item", values_to = "n_1to5") %>%
    dplyr::mutate(subscale_id = sub_id,
                  subscale    = paste0(sub_id, suffix),
                  .before = 1)
}

items_counts_b <- dplyr::bind_rows(
  count_1to5_per_item(temp_b, sub_list_b$subscale1, "subscale1"),
  count_1to5_per_item(temp_b, sub_list_b$subscale2, "subscale2"),
  count_1to5_per_item(temp_b, sub_list_b$subscale3, "subscale3"),
  count_1to5_per_item(temp_b, sub_list_b$subscale4, "subscale4")
)

# 3) Per-subscale total 1–5 counts (sum over items) for B and A
subscale_totals_b <- items_counts_b %>%
  dplyr::group_by(subscale_id, subscale) %>%
  dplyr::summarise(n_1to5_total_b = sum(n_1to5), .groups = "drop")

items_counts_a <- dplyr::bind_rows(
  count_1to5_per_item(temp_a, sub_list_a$subscale1, "subscale1", "_avg_a"),
  count_1to5_per_item(temp_a, sub_list_a$subscale2, "subscale2", "_avg_a"),
  count_1to5_per_item(temp_a, sub_list_a$subscale3, "subscale3", "_avg_a"),
  count_1to5_per_item(temp_a, sub_list_a$subscale4, "subscale4", "_avg_a")
)
subscale_totals_a <- items_counts_a %>%
  dplyr::group_by(subscale_id) %>%
  dplyr::summarise(n_1to5_total_a = sum(n_1to5), .groups = "drop")

subscale_b_vs_a_totals <- subscale_totals_b %>%
  dplyr::left_join(subscale_totals_a, by = "subscale_id") %>%
  dplyr::mutate(prop_total_b_over_a =
                  dplyr::if_else(n_1to5_total_a > 0, n_1to5_total_b / n_1to5_total_a, NA_real_))

# 4) Row-based ANY vs ALL per subscale (B) + proportion vs A
row_has_any <- function(df, cols) {
  if (length(cols) == 0) return(rep(FALSE, nrow(df)))
  rowSums(!is.na(df[, cols, drop = FALSE])) > 0
}
row_has_all <- function(df, cols) {
  if (length(cols) == 0) return(rep(FALSE, nrow(df)))
  rowSums(!is.na(df[, cols, drop = FALSE])) == length(cols)
}

row_any_all_summary <- function(df_b, df_a, cols_b, cols_a, sub_id) {
  any_b <- row_has_any(df_b, cols_b); all_b <- row_has_all(df_b, cols_b)
  any_a <- row_has_any(df_a, cols_a); all_a <- row_has_all(df_a, cols_a)
  
  tibble::tibble(
    subscale_id = sub_id,
    subscale_b  = paste0(sub_id, "_avg_b"),
    subscale_a  = paste0(sub_id, "_avg_a"),
    n_rows_any_b = sum(any_b),
    n_rows_all_b = sum(all_b),
    n_rows_any_a = sum(any_a),
    n_rows_all_a = sum(all_a),
    # proportions (B relative to A)
    prop_any_b_over_a = ifelse(n_rows_any_a > 0, n_rows_any_b / n_rows_any_a, NA_real_),
    prop_all_b_over_a = ifelse(n_rows_all_a > 0, n_rows_all_b / n_rows_all_a, NA_real_)
  )
}

per_subscale_any_all <- dplyr::bind_rows(
  row_any_all_summary(temp_b, temp_a, sub_list_b$subscale1, sub_list_a$subscale1, "subscale1"),
  row_any_all_summary(temp_b, temp_a, sub_list_b$subscale2, sub_list_a$subscale2, "subscale2"),
  row_any_all_summary(temp_b, temp_a, sub_list_b$subscale3, sub_list_a$subscale3, "subscale3"),
  row_any_all_summary(temp_b, temp_a, sub_list_b$subscale4, sub_list_a$subscale4, "subscale4")
)

# 5) Overall (across subscales): rows with ANY subscale having 1–5 (B),
#    and proportion vs A's "ANY subscale"
has_any_b <- Reduce(`|`, lapply(sub_list_b, \(cols) row_has_any(temp_b, cols)))
has_any_a <- Reduce(`|`, lapply(sub_list_a, \(cols) row_has_any(temp_a, cols)))

# (Optional) Also, rows where ALL subscales have at least one valid item
has_all_subscales_b <- Reduce(`&`, lapply(sub_list_b, \(cols) row_has_any(temp_b, cols)))
has_all_subscales_a <- Reduce(`&`, lapply(sub_list_a, \(cols) row_has_any(temp_a, cols)))

items_counts_b            # (1) per-item totals (B)
subscale_totals_b         # (2) per-subscale totals (B)
subscale_b_vs_a_totals    # (2 alt) B totals as a proportion of A totals
per_subscale_any_all      # (ANY/ALL) row counts per subscale + proportions vs A
