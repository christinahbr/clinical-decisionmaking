# Source helper functions
source(file.path("code", "utils", "load_packages.R"))
source(file.path("code", "utils", "04_var_utils.R"))
packages = c(
  "dplyr",
  "tableone",
  "kableExtra",
  "haven",
  "gtsummary",
  "flextable",
  "officer"
)
# Load packages
load_packages(packages)

# Load filtered and cleaned data
survey_results <- readRDS(file.path("data", "clean_data.rds")) %>%
  mutate(years_since_training = as.numeric(years_since_training)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(race = factor(race)) %>%
  mutate(provtype = as_factor(provtype))

# Define demographic questions
demographic_questions <- c(
  "age",
  "years_since_training",
  "gender",
  "race",
  "hispanic",
  "RUCA_regions",
  "magnet",
  "Region"
  )

survey_pretty <- recode_pretty(survey_results)

table_one = tbl_summary(survey_pretty, by = provtype, include = demographic_questions) %>%
  as_flex_table() %>%
  padding(padding.top = 0, part = "all") %>%
  padding(padding.bottom = 0, part = "all") %>%
  fontsize(size = 8) %>%
  width(j = 1, width = 2)

if (!dir.exists("results")) {
  dir.create("results")
}

doc = read_docx() %>%
  body_add_flextable(value = table_one) %>%
  print(target = "results/table_one.docx")
