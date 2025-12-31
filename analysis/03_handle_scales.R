# Source helper functions
source(file.path("code", "utils", "load_packages.R"))

packages = c(
  "dplyr",
  "haven",
  "tidyr"
)

load_packages(packages)
source(file.path("code", "utils", "grab_columns.R"))
source(file.path("code", "utils", "03_categorize_columns.R"))

# Load clean data
survey_results <- readRDS(file.path("data", "clean_data.rds"))

info_quality <- get_column_names(survey_results, "b1_1", "b1_15")

# Create a temporary data frame to apply the mutation
temp_data_frame <- survey_results %>%
  mutate(across(all_of(info_quality), ~ if_else(.x >= 1 & .x <= 5, .x, NA_real_)))

# Print the count of all values in the columns listed in info_quality
count_values_in_columns(temp_data_frame, info_quality)

# Count item- and subscale-level responses to perceived quality items
source(file.path("code", "utils", "03_count_quality_items.R"))

# List of all unique combinations
combinations <- expand.grid(include_a = c(TRUE, FALSE),
                            include_b = c(TRUE, FALSE),
                            include_c = c(TRUE, FALSE))

# Filter out the combination where all are FALSE
combinations <- combinations[!(combinations$include_a == FALSE & combinations$include_b == FALSE & combinations$include_c == FALSE), ]

# Loop through each combination and process
for (i in 1:nrow(combinations)) {
  include_a <- combinations$include_a[i]
  include_b <- combinations$include_b[i]
  include_c <- combinations$include_c[i]
  
  # Suffix for the combination
  suffix <- paste0(
    if (include_a) "_a" else "",
    if (include_b) "_b" else "",
    if (include_c) "_c" else ""
  )
  
  # Categorize columns based on the current combination
  categorized_columns <- categorize_columns(survey_results, categories, include_a, include_b, include_c)
  
  # Access the categorized columns
  subscale1_items <- categorized_columns[["Leadership and Professional Networks"]]
  subscale2_items <- categorized_columns[["News and Social Media Platforms"]]
  subscale3_items <- categorized_columns[["Academic and Professional Publications"]]
  subscale4_items <- categorized_columns[["Informal Information Sources"]]
  
  # Define column groups and new column names with suffix
  column_groups <- list(
    subscale1_items, subscale2_items, subscale3_items, subscale4_items
  )
  
  new_column_names <- c(
    paste0("subscale1_avg", suffix), paste0("subscale2_avg", suffix), paste0("subscale3_avg", suffix), paste0("subscale4_avg", suffix)
  )
  
  print(new_column_names)

  # Calculate the averages and save them with new column names
  new_columns <- calculate_and_save_averages(temp_data_frame, column_groups, new_column_names)
  
  # Bind new columns to the original survey_results
  survey_results <- bind_cols(survey_results, new_columns)
}

# Define the column groups
ed_attitudes <- paste0("b4_", 1:4) 
ed_transpar <- paste0("b4_", 3:4) 
ed_innov <- paste0("b4_", 1:2)

org_attitudes <- paste0("c4_", 1:4)
org_collab <- paste0("c4_", 3:4)
org_lead <- paste0("c4_", 1:2)

info_pandemic <- get_column_names(survey_results, "a1_1", "a1_15")
info_quality <- get_column_names(survey_results, "b1_1", "b1_15")
info_present <- get_column_names(survey_results, "c1_1", "c1_15")

column_groups <- list(
  ed_attitudes, ed_transpar, ed_innov,
  org_attitudes, org_collab, org_lead,
  info_pandemic, info_quality, info_present)

# Define new column names
new_column_names <- c(
  "ed_attitudes_avg", "ed_transpar_avg", "ed_innov_avg",
  "org_attitudes_avg", "org_collab_avg", "org_lead_avg",
  "info_pandemic_avg", "info_qual_avg", "info_present_avg"
)

# Calculate the averages and save them with new column names
new_columns <- calculate_and_save_averages(temp_data_frame, column_groups, new_column_names)

# Bind new columns to the original survey_results
survey_results <- bind_cols(survey_results, new_columns)

# # Save cleaned data
# saveRDS(survey_results, file.path("data", "clean_data.rds"))