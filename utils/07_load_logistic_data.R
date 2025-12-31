# Load clean data
survey_results <- readRDS(file.path("data", "clean_data.rds"))

# Convert from haven object, while maintaining numeric values
survey_results <- survey_results %>%
  modify_if(~ inherits(., "haven_labelled"), ~ haven::as_factor(., levels = "values"))

# Set nurses to reference level (my preference for interpreting the results)
survey_results$provtype <- relevel(survey_results$provtype, ref = "RN")

# Create nurse- and doctor-specific data frames
df_nurses <- survey_results %>%
  filter(provtype == "RN")

df_doctors <- survey_results %>%
  filter(provtype == "MD")