# Helpers for 07_logistic_models.R

source(file.path("code", "utils", "load_custom_fonts.R"))

# Predictor lists

ppe_predictors <- c(
  "subscale1_avg_a",
  "subscale2_avg_a",
  "subscale3_avg_a",
  "subscale4_avg_a",
  "age", "gender_collapsed", "RUCA_collapsed",
  "ppe1b * provtype" # flexibility to make decisions
)

hfo_predictors <- c(
  "subscale1_avg_a", 
  "subscale2_avg_a", 
  "subscale3_avg_a", 
  "subscale4_avg_a", 
  "age", "gender_collapsed", "RUCA_collapsed",
  "hfo2b * provtype"
)

iv_predictors <- c(
  "subscale1_avg_a", 
  "subscale2_avg_a", 
  "subscale3_avg_a", 
  "subscale4_avg_a", 
  "age", "gender_collapsed", "RUCA_collapsed",
  "iv3b * provtype"
)

# Logistic Regression Functions
# Small helper
.wald_ci <- function(est, se, level = 0.95) {
  z <- qnorm(1 - (1 - level)/2)
  cbind(lower = est - z*se, upper = est + z*se)
}

run_logistic_regression <- function(data, predictors, response, ci_level = 0.95) {
  data <- data %>% mutate(across(all_of(response), as.factor))
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, data = data, family = binomial)
  
  smry <- summary(model)
  coefs <- smry$coefficients
  est   <- coefs[, "Estimate"]
  se    <- coefs[, "Std. Error"]
  pvals <- coefs[, "Pr(>|z|)"]
  
  # --- Wald CIs on log-odds, then exponentiate for OR CIs (matches emmeans) ---
  ci_log <- .wald_ci(est, se, level = ci_level)
  or     <- exp(est)
  or_ci  <- exp(ci_log)
  
  # Print summary bits (unchanged except CI source)
  print(smry)
  
  sig_idx <- which(pvals < 0.05)
  appr_idx <- which(pvals >= 0.05 & pvals < 0.10)
  
  if (length(sig_idx) > 0) {
    cat("Significant predictors (p < 0.05):\n")
    for (i in sig_idx) {
      predictor_name <- rownames(coefs)[i]
      direction <- ifelse(est[i] > 0, "increase", "decrease")
      cat(predictor_name, ": A unit change in", predictor_name,
          "predicts a", direction, "in the log odds of", response, "by", abs(est[i]), "\n")
      cat("Odds Ratio:", or[i], "\n")
      cat("95% Wald CI for OR: [", or_ci[i, 1], ", ", or_ci[i, 2], "]\n")
      cat("P-value:", pvals[i], "\n")
      cat("Percentage change in odds:", round((or[i]-1)*100, 2), "%\n\n")
    }
  } else cat("No significant predictors found.\n")
  
  if (length(appr_idx) > 0) {
    cat("Predictors approaching significance (0.05 ≤ p < 0.10):\n")
    for (i in appr_idx) {
      predictor_name <- rownames(coefs)[i]
      direction <- ifelse(est[i] > 0, "increase", "decrease")
      cat(predictor_name, ": A unit change in", predictor_name,
          "may predict a", direction, "in the log odds of", response, "by", abs(est[i]), "\n")
      cat("Odds Ratio:", or[i], "\n")
      cat("95% Wald CI for OR: [", or_ci[i, 1], ", ", or_ci[i, 2], "]\n")
      cat("P-value:", pvals[i], "\n")
      cat("Percentage change in odds:", round((or[i]-1)*100, 2), "%\n\n")
    }
  }
  
  return(model)
}

run_linear_regression <- function(data, predictors, response, ci_level = 0.95) {
  # Ensure response is numeric
  data <- data %>% mutate(across(all_of(response), as.numeric))
  
  # Build formula
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = data)
  
  smry <- summary(model)
  coefs <- smry$coefficients
  est   <- coefs[, "Estimate"]
  se    <- coefs[, "Std. Error"]
  pvals <- coefs[, "Pr(>|t|)"]
  
  ci <- .wald_ci(est, se, level = ci_level)
  
  # Print model summary
  print(smry)
  
  sig_idx <- which(pvals < 0.05)
  appr_idx <- which(pvals >= 0.05 & pvals < 0.10)
  
  if (length(sig_idx) > 0) {
    cat("Significant predictors (p < 0.05):\n")
    for (i in sig_idx) {
      predictor_name <- rownames(coefs)[i]
      direction <- ifelse(est[i] > 0, "increase", "decrease")
      cat(predictor_name, ": A one-unit change in", predictor_name,
          "predicts a", direction, "in", response, "by", abs(round(est[i], 3)), "\n")
      cat("95% Wald CI:", "[", round(ci[i, 1], 3), ",", round(ci[i, 2], 3), "]\n")
      cat("P-value:", round(pvals[i], 3), "\n\n")
    }
  } else cat("No significant predictors found.\n")
  
  if (length(appr_idx) > 0) {
    cat("Predictors approaching significance (0.05 ≤ p < 0.10):\n")
    for (i in appr_idx) {
      predictor_name <- rownames(coefs)[i]
      direction <- ifelse(est[i] > 0, "increase", "decrease")
      cat(predictor_name, ": A one-unit change in", predictor_name,
          "may predict a", direction, "in", response, "by", abs(round(est[i], 3)), "\n")
      cat("95% Wald CI:", "[", round(ci[i, 1], 3), ",", round(ci[i, 2], 3), "]\n")
      cat("P-value:", round(pvals[i], 3), "\n\n")
    }
  }
  
  return(model)
}


# Allows data to be filtered by values (specifically by flexibility to make decisions)

run_filtered_logistic <- function(
    data, filters = list(), predictors, response, remove_predict = NULL
) {
  # Optionally drop predictors
  if (!is.null(remove_predict)) {
    predictors <- setdiff(predictors, remove_predict)
  }
  
  # Apply all filters iteratively
  for (f in names(filters)) {
    vals <- filters[[f]]
    data <- data %>% filter(.data[[f]] %in% vals)
  }
  
  # Run logistic regression
  model <- run_logistic_regression(
    data, predictors = predictors, response = response
  )
  return(model)
}

run_filtered_linear <- function(
    data, filter_var, filter_vals, predictors, response, remove_predict = NULL
) {
  if (!is.null(remove_predict)) {
    predictors <- setdiff(predictors, remove_predict)
  }
  
  filtered <- data %>% filter(.data[[filter_var]] %in% filter_vals)
  
  model <- run_linear_regression(
    data = filtered,
    predictors = predictors,
    response = response
  )
  return(model)
}

# Interpret Interaction Terms

# Compute subgroup ORs for flexibility (3 vs 2) by provider
# and the interaction OR (difference in slopes)

summarize_flex_by_role <- function(model, flex_var, by_var = "provtype",
                                   high = "3", low = "2", ci_level = 0.95) {
  # 1) Within-role ORs via emmeans, with Wald CIs on the link scale -> response
  emm <- emmeans(model, reformulate(flex_var), by = by_var)
  contr <- contrast(emm, method = "revpairwise", by = by_var)
  emmtab <- summary(contr, type = "response", infer = c(TRUE, TRUE), level = ci_level) %>%
    dplyr::filter(contrast == paste0(flex_var, high, " / ", flex_var, low)) %>%
    transmute(
      !!by_var := .data[[by_var]],
      or       = odds.ratio,
      ci_low   = asymp.LCL,
      ci_high  = asymp.UCL,
      p_value  = p.value
    )
  
  # 2) Interaction term (same Wald CI recipe)
  coefs <- coef(model); vcv <- vcov(model); cn <- names(coefs)
  name_flex_high <- grep(paste0("^", flex_var, high, "$"), cn, value = TRUE)
  inter_ix <- which(grepl(paste0("(^|:)", flex_var, high, "(:|$)"), cn) &
                      grepl(paste0("(^|:)", by_var, "[^:]*(:|$)"), cn) &
                      grepl(":", cn))
  
  if (length(name_flex_high) == 1 && length(inter_ix) >= 1) {
    name_int <- cn[inter_ix[1]]
    b_int  <- unname(coefs[name_int])
    se_int <- sqrt(vcv[name_int, name_int])
    z <- qnorm(1 - (1 - ci_level)/2)
    
    inter_df <- tibble::tibble(
      term         = name_int,
      inter_or     = exp(b_int),
      inter_ci_low = exp(b_int - z*se_int),
      inter_ci_high= exp(b_int + z*se_int),
      inter_p      = 2 * pnorm(-abs(b_int/se_int))
    )
  } else {
    warning("Could not find expected coefficient names for interaction; skipping interaction OR.")
    inter_df <- tibble::tibble(term = NA_character_,
                               inter_or = NA_real_, inter_ci_low = NA_real_,
                               inter_ci_high = NA_real_, inter_p = NA_real_)
  }
  
  list(within_role = emmtab, interaction = inter_df)
}



print_flex_summary <- function(flex_summary, by_label = "Provider") {
  cat("\nWithin-role flexibility effect (3 vs 2):\n")
  print(flex_summary$within_role %>%
          mutate(across(c(or, ci_low, ci_high), ~round(.x, 3))) %>%
          rename(OR = or, CI_low = ci_low, CI_high = ci_high,
                 p = p_value, !!by_label := !!sym(names(flex_summary$within_role)[1])))
  if (!is.na(flex_summary$interaction$term[1])) {
    cat("\nInteraction (difference in slopes; OR > 1 = weaker reduction for second role):\n")
    print(flex_summary$interaction %>%
            mutate(across(c(inter_or, inter_ci_low, inter_ci_high), ~round(.x, 3))) %>%
            rename(OR = inter_or, CI_low = inter_ci_low, CI_high = inter_ci_high, p = inter_p) %>%
            dplyr::select(OR, CI_low, CI_high, p))
  }
}

# Odds Ratio Functions

get_filtered_or <- function(model, label, term_label_lookup, filter_p = c("all", "sig", "approaching")) {
  filter_p <- match.arg(filter_p)
  
  model_terms <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    left_join(term_label_lookup, by = "term") %>%
    mutate(display_term = coalesce(pretty_label, term))
  
  # Filtering by p-value
  if (filter_p == "sig") {
    model_terms <- filter(model_terms, p.value < 0.05)
  } else if (filter_p == "approaching") {
    model_terms <- filter(model_terms, p.value < 0.10)
  }
  
  # Annotate
  model_terms <- model_terms %>%
    mutate(
      model = label,
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.10 ~ "+",
        TRUE ~ ""
      ),
      OR_label = paste0(sprintf("%.2f", estimate), significance)
    )
  
  # Arrange in the order of the lookup table
  model_terms$display_term <- factor(model_terms$display_term, 
                                     levels = unique(term_label_lookup$pretty_label))
  model_terms <- arrange(model_terms, display_term)
  
  return(model_terms)
}

make_or_data <- function(model, label, term_label_lookup, filter_p = "all") {
  odds_df <- get_filtered_or(model, label, term_label_lookup, filter_p = filter_p) %>%
    dplyr::select(display_term, estimate, conf.low, conf.high, p.value, OR_label) %>%
    dplyr::rename(
      Predictor = display_term,
      OR = estimate,
      `95% CI (Lower)` = conf.low,
      `95% CI (Upper)` = conf.high,
      `P-value` = p.value,
      `OR (Label)` = OR_label
    )
  
  if (nrow(odds_df) == 0) {
    message("No data available for this model and filter.")
    return(NULL)
  }
  
  odds_df
}

# For viewing results in R studio
present_or_kable <- function(or_data, caption) {
  if (is.null(or_data)) {
    return("No data available to display.")
  }
  
  or_data %>%
    knitr::kable(digits = 3, caption = caption, format = "html") %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      font_size = 14,
      position = "center"
    ) %>%
    kableExtra::row_spec(0, bold = TRUE, color = "black", background = "white") %>%
    kableExtra::row_spec(1:nrow(or_data), background = "white")
}

# For html (prettier)
present_or_gt <- function(or_data, caption) {
  if (is.null(or_data)) return("No data available.")
  library(gt)
  or_data %>%
    gt() %>%
    tab_header(title = caption) %>%
    fmt_number(columns = -Predictor, decimals = 3) %>%
    opt_row_striping()
}

# Basic Forest Plot

plot_forest <- function(model, term_label_lookup, plot_title,
                        filter_p = "approaching",
                        x_limits = c(0.08, 8),
                        x_breaks = c(0.1, 0.2, 0.5, 1, 2, 3, 4, 5, 6, 7, 8),
                        x_labels = c("0.1", "0.2", "0.5", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0", "8.0")) {
  
  # Extract ORs and CIs, with p-value filtering
  or_df <- get_filtered_or(model, plot_title, term_label_lookup, filter_p = filter_p)
  
  # Order terms by OR (largest first)
  or_df <- or_df %>%
    dplyr::mutate(display_term = forcats::fct_reorder(display_term, estimate, .desc = TRUE))
  
  # Plot
  ggplot(or_df, aes(y = display_term, x = estimate)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.6) +
    geom_point(shape = 18, size = 4, color = "gray10") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, color = "gray10") +
    geom_text(aes(label = OR_label, x = conf.high), nudge_x = 0.05, hjust = 0, size = 5) +
    scale_x_log10(
      limits = x_limits,
      breaks = x_breaks,
      labels = x_labels
    ) +
    labs(
      title = plot_title,
      x = "Adjusted Odds Ratio (log scale)",
      y = NULL,
      caption = "Significance: *** p < .001  ** p < .01  * p < .05  + p < .10"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.margin = margin(10, 60, 10, 10),
      plot.background = element_rect(fill = "white", color = NA),   # White background
      panel.background = element_rect(fill = "white", color = NA),  # White panel
      text = element_text(color = "gray20")
    ) +
    coord_cartesian(clip = "off")
}

# Pretty Forest Plot

get_all_log_estimates <- function(models, term_label_lookup) {
  purrr::map_df(models, ~ {
    broom::tidy(.x, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term %in% term_label_lookup$term) %>%
      mutate(log_estimate = log(estimate))
  })
}

get_signif_or_pretty <- function(model, label, term_label_lookup, bold_terms = NULL, 
                                 show_significant_only = TRUE, pval_cutoff = 0.10, 
                                 remove_terms = NULL) {
  df <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>% # returns 95% confidence intervals
    filter(term %in% term_label_lookup$term)
  if (show_significant_only) {
    df <- df %>% filter(p.value < pval_cutoff)
  }
  if (!is.null(remove_terms)) {
    df <- df %>% filter(!term %in% remove_terms)
  }
  df %>%
    left_join(term_label_lookup, by = "term") %>%
    mutate(
      model = label,
      OR_label = sprintf("%.2f", estimate),
      log_estimate = log(estimate),
      label_bold = if (!is.null(bold_terms)) pretty_label %in% bold_terms else FALSE,
      display_term = factor(pretty_label, levels = unique(term_label_lookup$pretty_label))
    )
}

plot_signif_forest_pretty <- function(
    model, 
    term_label_lookup, 
    plot_title,
    x_limits = NULL,                     # NULL => auto-range per plot
    bold_terms = NULL,
    show_legend = TRUE,
    show_significant_only = TRUE,
    nudge_x_text = -0.05,
    selected_font = "Nunito",
    remove_terms = NULL,                 # terms to exclude due to limited N
    size_scale = 1,
    order_terms = NULL,                  # canonical order of PRETTY labels
    x_title = NULL
) {
  # Default scaling: smaller in HTML, full size elsewhere
  if (is.null(size_scale)) {
    size_scale <- if (requireNamespace("knitr", quietly = TRUE) && knitr::is_html_output()) 0.7 else 1
  }
  
  # Build data
  df <- get_signif_or_pretty(model, plot_title, term_label_lookup, bold_terms, show_significant_only)
  
  # Optional remove_terms filter
  if (!is.null(remove_terms)) {
    df <- df %>% dplyr::filter(!term %in% remove_terms)
  }
  
  # Enforce canonical order
  if (!is.null(order_terms)) {
    df <- df %>% dplyr::mutate(display_term = stringr::str_trim(display_term))
    
    # Diagnostics: what's missing or extra vs. your canonical vector
    missing <- setdiff(order_terms, unique(df$display_term))
    extra   <- setdiff(unique(df$display_term), order_terms)
    if (length(missing)) message("Not in data: ", paste(missing, collapse = ", "))
    if (length(extra))   message("Not in canonical_order: ", paste(extra, collapse = ", "))
    
    df <- df %>% dplyr::mutate(display_term = factor(display_term, levels = order_terms))
  } else {
    # Fallback: order by effect size
    df <- df %>% dplyr::mutate(display_term = forcats::fct_reorder(display_term, estimate, .desc = TRUE))
  }
  
  # Auto x-limits if not provided
  if (is.null(x_limits)) {
    xmin <- min(df$conf.low,  na.rm = TRUE)
    xmax <- max(df$conf.high, na.rm = TRUE)
    
    # multiplicative padding for log scale
    pad <- 0.10
    xmin <- max(xmin * (1 - pad), 0.05)  # don't go below 0.05
    xmax <- xmax * (1 + pad)
    
    # keep the null line (OR=1) in view
    xmin <- min(xmin, 1)
    xmax <- max(xmax, 1)
    
    x_limits <- c(xmin, xmax)
  }
  
  # log breaks within computed limits
  cand_breaks <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 3, 5, 10, 20)
  x_breaks <- cand_breaks[cand_breaks >= x_limits[1] & cand_breaks <= x_limits[2]]
  if (length(x_breaks) == 0) x_breaks <- 1
  
  # Stars and labels
  df <- df %>%
    dplyr::mutate(
      signif_symbol = dplyr::case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ "†",
        TRUE            ~ ""
      ),
      OR_label_sig = paste0(OR_label, signif_symbol)
    )
  
  # Fill scale range based on log_estimate distribution
  m <- mean(df$log_estimate, na.rm = TRUE)
  s <- stats::sd(df$log_estimate, na.rm = TRUE)
  lower_limit <- m - 2 * s
  upper_limit <- m + 2 * s
  
  # Plot
  p <- ggplot(df, aes(y = display_term, x = estimate)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      height = 0.2, color = "gray60", linewidth = 1 * size_scale
    ) +
    geom_point(aes(fill = log_estimate),
               shape = 21, size = 4 * size_scale, color = "black", stroke = 0.7 * size_scale) +
    geom_text(aes(label = OR_label_sig),
              family = selected_font,
              nudge_x = nudge_x_text, nudge_y = 0.42, hjust = 1, vjust = 0.4,
              size = 14 * size_scale, color = "gray20") +
    scale_x_log10(
      limits = x_limits,
      breaks = x_breaks,
      labels = function(x) ifelse(abs(x) < 1,
                                  gsub("\\.?0+$", "", sprintf("%.2f", x)),
                                  gsub("\\.?0+$", "", sprintf("%.1f", x)))
    ) +
    labs(
      title = plot_title,
      subtitle = x_title,
      x = NULL,
      y = NULL,
      caption = "* p < 0.05, ** p < 0.01, *** p < 0.001, † p < 0.10"
    ) +
    theme_minimal(base_size = 12 * size_scale) +
    theme(
      plot.margin        = margin(t = 10),
      text               = element_text(family = selected_font, color = "gray20"),
      plot.caption       = element_text(size = 40 * size_scale, margin = margin(t = 10, r = 5)),
      axis.text.x        = element_text(size = 40 * size_scale),
      # axis.title.x       = element_text(size = 43 * size_scale, margin = margin(t = 10, b = 6)),
      axis.title.x   = element_blank(),
      plot.title.position = "panel",
      plot.subtitle  = element_text(
        size = 43 * size_scale, 
        hjust = 0.5,               # center across plot
        margin = margin(b = 6, t = 4)
      ),
      axis.text.y        = ggtext::element_markdown(size = 43 * size_scale, margin = margin(l = 5)), # 10
      legend.position    = ifelse(show_legend, "bottom", "none"),
      legend.text        = element_text(size = 14 * size_scale),
      legend.title       = element_text(size = 16 * size_scale),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA)
    ) +
    coord_cartesian(clip = "off") +
    scale_y_discrete(
      limits = order_terms,            # lock order; use rev(order_terms) if you want first at top
      drop   = FALSE,                  # keep empty levels for row alignment across panels
      labels = function(x) {
        if (!is.null(bold_terms)) {
          sapply(x, function(term)
            if (term %in% bold_terms) paste0("<b>", term, "</b>")
            else                      paste0("<span style='color:gray30;'>", term, "</span>")
          )
        } else x
      }
    ) +
    scale_fill_gradient2(
      low = "#ff0800", mid = "gray80", high = "#3BB143",
      midpoint = 0,
      limits = c(-max(abs(c(lower_limit, upper_limit))),
                 max(abs(c(lower_limit, upper_limit)))),   # <- symmetric around 0
      oob = scales::squish,
      name = if (show_legend) "Effect direction" else waiver(),
      labels = function(x) gsub("\\.?0+$", "", sprintf("%.1f", x)),
      guide = if (show_legend) guide_colorbar(
        barwidth = 10 * size_scale,
        barheight = 2 * size_scale
      ) else "none"
    )
  
  return(p)
}
