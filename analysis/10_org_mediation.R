############################################################
## 0) Setup
############################################################

source(file.path("code", "utils", "load_packages.R"))
packages <- c(
  "broom","dplyr","forcats","ggplot2","ggtext","gt","haven",
  "kableExtra","mediation","patchwork","purrr","showtext",
  "stringr","tibble","tidyr","lavaan","semPlot"
)
load_packages(packages)

set.seed(20250825)

############################################################
## 1) load data
############################################################

survey_results <- readRDS(file.path("data","clean_data.rds"))

# Convert haven_labelled to factors with value labels
survey_results <- survey_results %>%
  modify_if(~inherits(.,"haven_labelled"), ~haven::as_factor(., levels = "values"))

# Collapse RUCA; collapse gender categories
survey_results <- survey_results %>%
  mutate(
    RUCA_collapsed = if_else(RUCA %in% c(1,2), as.character(RUCA), "Other"),
    gender_collapsed = forcats::fct_collapse(
      addNA(gender),
      `Other or Unknown` = c(
        "Nonbinary","Gender nonconforming",
        "Gender not listed/prefer to self describe (Please specify):",
        "Prefer not to answer", NA
      )
    )
  )

# Provider ref level + RN/MD dummy (RN=0, MD=1, others=NA)
if (is.factor(survey_results$provtype)) {
  survey_results$provtype <- relevel(survey_results$provtype, ref = "RN")
}
survey_results <- survey_results %>%
  mutate(
    prov_md = case_when(
      as.character(provtype) == "MD" ~ 1L,
      as.character(provtype) == "RN" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

############################################################
## 2) Core covariates (age_z, race, gender)
############################################################

# Age z-score
survey_results <- survey_results %>%
  mutate(
    age = suppressWarnings(as.numeric(age)),
    age_z = as.numeric(scale(age))
  )

# Race: White vs POC → numeric dummy (POC=1, White=0, other/missing=NA)
survey_results <- survey_results %>%
  mutate(
    white_or_poc_chr = stringr::str_trim(as.character(white_or_poc)),
    white_or_poc_bin = case_when(
      white_or_poc_chr == "People of Color" ~ "POC",
      white_or_poc_chr == "White" ~ "White",
      TRUE ~ NA_character_
    ),
    white_or_poc_bin = factor(white_or_poc_bin, levels = c("White","POC")),
    race_poc = case_when(
      white_or_poc_bin == "POC"   ~ 1L,
      white_or_poc_bin == "White" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Gender: Male/Female only → numeric dummy (Female=1, Male=0, Other/Unknown=NA)
survey_results <- survey_results %>%
  mutate(
    gender_collapsed_chr = stringr::str_trim(as.character(gender_collapsed)),
    gender_binary = case_when(
      gender_collapsed_chr == "Male"   ~ "Male",
      gender_collapsed_chr == "Female" ~ "Female",
      TRUE ~ NA_character_
    ),
    gender_binary = factor(gender_binary, levels = c("Male","Female")),
    female = case_when(
      gender_binary == "Female" ~ 1L,
      gender_binary == "Male"   ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Quick QA
print(table(survey_results$provtype, useNA = "ifany"))
print(table(survey_results$white_or_poc_bin, useNA = "ifany"))
print(table(survey_results$gender_binary, useNA = "ifany"))
summary(dplyr::select(survey_results, age_z, race_poc, female, prov_md))

############################################################
## 3) handle subscales
############################################################

items_b <- paste0("b4_", 1:4)
items_c <- paste0("c4_", 1:4)
all_items <- c(items_b, items_c)

# Ensure numeric for Likert items
survey_results <- survey_results %>%
  mutate(across(all_of(all_items), ~ suppressWarnings(as.numeric(as.character(.)))))

# Named *_num copies if needed elsewhere
for (v in all_items) survey_results[[paste0(v,"_num")]] <- survey_results[[v]]

# Subscales
survey_results <- survey_results %>%
  mutate(
    LearningClimate     = rowMeans(cbind(b4_1_num, b4_2_num), na.rm = TRUE),
    PolicyDissemination = rowMeans(cbind(b4_3_num, b4_4_num), na.rm = TRUE),
    LeadershipSupport   = rowMeans(cbind(c4_1_num, c4_2_num), na.rm = TRUE),
    Collaboration       = rowMeans(cbind(c4_3_num, c4_4_num), na.rm = TRUE)
  )

############################################################
## 4) Likert plot by provider
############################################################

likert_plot_data <- survey_results %>%
  filter(ppe1b %in% c(2, 3)) %>%
  dplyr::select(provtype, all_of(all_items)) %>%
  pivot_longer(cols = all_of(all_items), names_to = "item", values_to = "response") %>%
  mutate(
    response = factor(response, levels = 1:5,
                      labels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")),
    item = recode(
      item,
      b4_1 = "B4_1: Leadership advocates for clinical innovations",
      b4_2 = "B4_2: Leadership supports continuous learning/technology",
      b4_3 = "B4_3: Leadership shares policies promptly",
      b4_4 = "B4_4: Leadership is transparent about decision-making",
      c4_1 = "C4_1: Org. leadership has a strong clinical vision",
      c4_2 = "C4_2: Org. leadership supports clinical teams",
      c4_3 = "C4_3: I have good relationships with interprofessional teams",
      c4_4 = "C4_4: I collaborate well with interprofessional teams"
    )
  )

likert_plot_prop <- likert_plot_data %>%
  count(provtype, item, response, name = "count") %>%
  group_by(provtype, item) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()



likert_plot <- ggplot(
  likert_plot_prop %>%
    mutate(item_wrapped = str_wrap(item, width = 20)),  # wrap item labels
  aes(x = response, y = prop, fill = response)
) +
  geom_col() +
  facet_grid(item_wrapped ~ provtype) +   # use wrapped item labels
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(
    x = "Response", y = "Proportion",
    title = "Proportion of Likert Responses by Provider Type",
    fill = "Response"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0)   # horizontal facet labels
  )

############################################################
## 5) lavaan mediation with covariates: age_z, race_poc, female, prov_md (RN=0, MD=1)
############################################################

lavaan_model_cov <- '
  # Mediator (adjusted)
  subscale1_avg_b ~ a1*LearningClimate + a2*PolicyDissemination + a3*LeadershipSupport + a4*Collaboration
  subscale1_avg_b ~ c_age*age_z + c_race*race_poc + c_gender*female + c_md*prov_md

  # Outcome (adjusted)
  subscale1_avg_a ~ b*subscale1_avg_b
  subscale1_avg_a ~ d_age*age_z + d_race*race_poc + d_gender*female + d_md*prov_md

  # Indirect effects
  ind_LC := a1*b
  ind_PD := a2*b
  ind_LS := a3*b
  ind_C  := a4*b
  total_ind := ind_LC + ind_PD + ind_LS + ind_C
'

fit_cov0 <- lavaan::sem(
  lavaan_model_cov,
  data      = survey_results,
  estimator = "MLR",
  missing   = "fiml",
  fixed.x   = FALSE   # allows FIML for covariates with NA
)
summary(fit_cov0, standardized = TRUE, fit.measures = TRUE)

############################################################
## 6) plot lavaan results
############################################################

pretty_labels <- c(
  subscale1_avg_b     = "Mediator:\nSubscale 1 (B)",
  subscale1_avg_a     = "Outcome:\nSubscale 1 (A)",
  LearningClimate     = "Learning Climate",
  PolicyDissemination = "Policy Dissemination",
  LeadershipSupport   = "Leadership Support",
  Collaboration       = "Collaboration",
  age_z               = "Age (z)",
  race_poc            = "POC (1=yes)",
  female              = "Female (1=yes)",
  prov_md             = "MD (1=yes)"
)

# ---- 2) semPlot model + lavaan estimates (with std betas) ----
spm <- semPlot::semPlotModel(fit_cov0)

# parameterEstimates() uses std.all; rename to est.std for convenience
pe <- lavaan::parameterEstimates(fit_cov0, standardized = TRUE) %>%
  dplyr::select(lhs, op, rhs, pvalue, std.all, se, z) %>%
  dplyr::rename(est.std = std.all)

# semPlot edges as data.frame
pars_df <- as.data.frame(spm@Pars)

# ---- 2a) Ensure an 'op' column exists on semPlot edges ----
if (!"op" %in% names(pars_df)) {
  # Try to infer directed vs. undirected edges
  if ("arrow" %in% names(pars_df)) {
    # In semPlot, directed paths typically have arrow == 2
    pars_df$op <- ifelse(pars_df$arrow == 2, "~", "~~")
  } else if ("dir" %in% names(pars_df)) {
    pars_df$op <- ifelse(isTRUE(pars_df$dir), "~", "~~")
  } else {
    # Fallback: infer by matching (lhs, rhs) to lavaan regressions
    reg_pairs <- paste(pe$lhs[pe$op == "~"], pe$rhs[pe$op == "~"])
    pars_df$op <- ifelse(paste(pars_df$lhs, pars_df$rhs) %in% reg_pairs, "~", "~~")
  }
}

# ---- 2b) Join semPlot edges with lavaan estimates ----
edge_df <- pars_df %>%
  dplyr::left_join(pe, by = c("lhs","op","rhs"))

# ---- 2c) Style edges: color by significance (regressions only), width by |std beta| ----
edge_cols <- ifelse(edge_df$op == "~" & !is.na(edge_df$pvalue) & edge_df$pvalue < .05,
                    "firebrick", "grey40")

abs_est <- abs(edge_df$est.std)
abs_est[is.na(abs_est)] <- 0
abs_est <- pmin(abs_est, 0.6)                  # clip for nicer aesthetics
edge_w  <- 1 + 3 * abs_est / 0.6               # range ~1–4

# ---- 3) Wrapped node labels ----
node_labels <- pretty_labels[spm@Vars$name]
node_labels <- ifelse(is.na(node_labels), spm@Vars$name, node_labels)
node_labels <- stringr::str_wrap(node_labels, width = 18)

# ---- 4) Plot to screen ----
semPaths(
  spm,
  what = "std", whatLabels = "std",
  style = "ram", layout = "tree", rotation = 2,
  residuals = FALSE, intercepts = FALSE,
  nCharNodes = 0, sizeMan = 9, edge.label.cex = 1.0,
  nodeLabels = node_labels,
  edge.color = edge_cols,
  edge.width = edge_w,
  color = list(latent = "lightblue", manifest = "white", edge = "grey40"),
  mar = c(6,6,6,6)
)

# save png

# if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
# png("outputs/sem_diagram.png", width = 2200, height = 1400, res = 220)
# semPaths(
#   spm,
#   what = "std", whatLabels = "std",
#   style = "ram", layout = "tree", rotation = 2,
#   residuals = FALSE, intercepts = FALSE,
#   nCharNodes = 0, sizeMan = 9, edge.label.cex = 1.0,
#   nodeLabels = node_labels,
#   edge.color = edge_cols,
#   edge.width = edge_w,
#   color = list(latent = "lightblue", manifest = "white", edge = "grey40"),
#   mar = c(6,6,6,6)
# )
# dev.off()

# ---- 6) Quick R² and key paths table ----
# R-squared for mediator & outcome
lavaan::inspect(fit_cov0, "r2")

# Tidy table of regression paths with std betas & p-values
key_paths <- pe %>%
  dplyr::filter(op == "~") %>%
  dplyr::transmute(
    outcome   = lhs,
    predictor = rhs,
    beta_std  = round(est.std, 3),
    se        = round(se, 3),
    z         = round(z, 2),
    p         = signif(pvalue, 3)
  ) %>%
  dplyr::arrange(outcome, dplyr::desc(abs(beta_std)))

key_paths
