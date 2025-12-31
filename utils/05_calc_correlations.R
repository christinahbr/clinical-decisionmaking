# Calculate correlations among items

# helper: Fisher z transform and inverse
z_transform   <- function(r) 0.5 * log((1 + r) / (1 - r))
z_inv         <- function(z) (exp(2*z) - 1) / (exp(2*z) + 1)

# vector of indices (assumes a_vars, b_vars, c_vars are aligned 1:1)
idx <- seq_along(a_vars)

# choose method = "pearson" or "spearman"
corr_method <- "pearson"

# per-pair filtering rules (edit if your 0 means "not applicable")
pair_corr <- map_dfr(idx, function(i) {
  a <- a_vars[i]; b <- b_vars[i]; c <- c_vars[i]
  
  # a vs b: filter out rows where b == 0 (plus NAs)
  df_ab <- survey_results %>% filter(!is.na(.data[[a]]), !is.na(.data[[b]]), .data[[b]] != 0)
  r_ab  <- if (nrow(df_ab) > 2) cor(df_ab[[a]], df_ab[[b]], use = "complete.obs", method = corr_method) else NA_real_
  
  # a vs c: (decide your rule) either no 0-filter, or filter c != 0 if analogous to b
  df_ac <- survey_results %>% filter(!is.na(.data[[a]]), !is.na(.data[[c]])) # or add & .data[[c]] != 0
  r_ac  <- if (nrow(df_ac) > 2) cor(df_ac[[a]], df_ac[[c]], use = "complete.obs", method = corr_method) else NA_real_
  
  tibble(
    pair   = i,
    a_var  = a, b_var = b, c_var = c,
    r_ab   = r_ab,
    r_ac   = r_ac,
    n_ab   = nrow(df_ab),
    n_ac   = nrow(df_ac)
  )
})

# Tidy-long table of correlations
corr_long <- pair_corr %>%
  pivot_longer(cols = c(r_ab, r_ac), names_to = "contrast", values_to = "r") %>%
  mutate(contrast = recode(contrast, r_ab = "a vs b", r_ac = "a vs c"))

# Fisher-z average r and 95% CI per contrast
summary_corr <- corr_long %>%
  group_by(contrast) %>%
  summarise(
    k        = sum(!is.na(r)),
    r_mean   = mean(r, na.rm = TRUE),                 # raw mean (report but don't rely on)
    z_mean   = mean(z_transform(r), na.rm = TRUE),    # average in z
    r_z_mean = z_inv(z_mean),                         # back-transformed mean r
    # approximate CI using SE_z = 1/sqrt(n_i - 3) and inverse-variance weighting
    # (simple unweighted CI shown here; for rigor, use weights)
    se_z     = sd(z_transform(r), na.rm = TRUE) / sqrt(k),
    z_low    = z_mean - qnorm(0.975) * se_z,
    z_high   = z_mean + qnorm(0.975) * se_z,
    r_low    = z_inv(z_low),
    r_high   = z_inv(z_high),
    .groups = "drop"
  )

pool_r_fisher <- function(r, n = NULL, conf_level = 0.95) {
  r <- as.numeric(r)
  r <- r[is.finite(r)]                 # drop NA/Inf
  stopifnot(length(r) > 0)
  
  z <- atanh(r)                        # Fisher z
  
  if (!is.null(n)) {
    n <- as.numeric(n)
    keep <- is.finite(n) & n > 3
    r <- r[keep]; z <- z[keep]; n <- n[keep]
    w <- n - 3                         # inverse-variance weight
    z_bar <- sum(w * z) / sum(w)
    se_z  <- 1 / sqrt(sum(w))
  } else {
    z_bar <- mean(z)
    se_z  <- sd(z) / sqrt(length(z))   # approx SE (unweighted)
  }
  
  alpha <- 1 - conf_level
  z_low  <- z_bar + qnorm(alpha/2) * se_z
  z_high <- z_bar + qnorm(1 - alpha/2) * se_z
  
  r_bar  <- tanh(z_bar)
  r_low  <- tanh(z_low)
  r_high <- tanh(z_high)
  
  list(r = r_bar, conf.level = conf_level,
       r_low = r_low, r_high = r_high,
       method = if (is.null(n)) "unweighted" else "inverse-variance")
}

fmt_pool <- function(res, label = "") {
  sprintf("%smean r = %.2f (95%% CI, %.2f–%.2f) [%s]",
          if (nzchar(label)) paste0(label, ": ") else "",
          res$r, res$r_low, res$r_high, res$method)
}