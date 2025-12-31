compare_subscale_time <- function(df, subscale_prefix) {
  long_df <- df %>%
    pivot_longer(
      cols = matches(paste0("^", subscale_prefix, "(_a|_c)$")),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      time = case_when(
        grepl("_a$", variable) ~ "pandemic",
        grepl("_c$", variable) ~ "present",
        TRUE ~ NA_character_
      ),
      item = subscale_prefix
    ) %>%
    filter(!is.na(time), !is.na(magnet_collapsed), !is.na(value))
  
  # Compute group means
  means <- long_df %>%
    group_by(magnet_collapsed, item, time) %>%
    summarize(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = time, values_from = mean_value)
  
  # Perform t-tests within each magnet_collapsed group
  t_tests <- long_df %>%
    group_by(magnet_collapsed) %>%
    summarize(
      p.value = {
        magnet_data <- filter(long_df, magnet_collapsed == unique(magnet_collapsed))
        n_groups <- length(unique(magnet_data$time[!is.na(magnet_data$value)]))
        if (n_groups == 2 && nrow(magnet_data) > 1) {
          tryCatch(t.test(value ~ time, data = magnet_data)$p.value, error = function(e) NA_real_)
        } else NA_real_
      },
      .groups = "drop"
    )
  
  results <- means %>%
    left_join(t_tests, by = "magnet_collapsed") %>%
    mutate(diff = pandemic - present) %>%
    dplyr::select(magnet_collapsed, item, present, pandemic, diff, p.value)
  
  return(results)
}

# Function 2: Compare pandemic values (_a) between magnet vs non‑magnet nurses
compare_subscale_magnet <- function(df, subscale_prefix) {
  long_df <- df %>%
    pivot_longer(
      cols = matches(paste0("^", subscale_prefix, "_a$")),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(item = subscale_prefix) %>%
    filter(!is.na(value), !is.na(magnet_collapsed))
  
  # Means per magnet_collapsed level
  means <- long_df %>%
    group_by(item, magnet_collapsed) %>%
    summarize(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = magnet_collapsed, values_from = mean_value)
  
  # Perform t-test only if both levels exist
  if (length(unique(long_df$magnet_collapsed)) == 2) {
    t_value <- tryCatch(
      t.test(value ~ magnet_collapsed, data = long_df, na.action = na.omit)$p.value,
      error = function(e) NA_real_
    )
  } else {
    t_value <- NA_real_
  }
  
  results <- means %>%
    mutate(
      p.value = t_value,
      diff = Yes - No
    ) %>%
    dplyr::select(item, No, Yes, diff, p.value)
  
  return(results)
}

adjust_family <- function(models, labels) {
  bind_rows(lapply(seq_along(models), function(i) {
    tidy(models[[i]]) %>%
      mutate(model = labels[i])
  })) %>%
    mutate(
      pval_adj_holm = p.adjust(p.value, method = "holm"),
      pval_adj_fdr  = p.adjust(p.value, method = "BH"),
      sig = case_when(
        pval_adj_holm < 0.001 ~ "***",
        pval_adj_holm < 0.01  ~ "**",
        pval_adj_holm < 0.05  ~ "*",
        pval_adj_holm < 0.10  ~ ".",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(model, term, estimate, std.error, statistic, p.value,
           pval_adj_holm, pval_adj_fdr, sig)
}