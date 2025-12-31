# Load necessary packages and functions
utils_path <- file.path("code", "utils")
source(file.path(utils_path, "file_paths.R"))
source(file.path(utils_path, "load_packages.R"))
source(file.path(utils_path, "grab_columns.R"))
source(file.path(utils_path, "02_clean_question_text.R"))
source(file.path(utils_path, "02_handle_race_questions.R"))
source(file.path(utils_path, "02_bin_questions.R"))

packages <- c(
  "dplyr",
  "haven",
  "readr",
  "purrr",
  "stringr",
  "openxlsx",
  "forcats"
)

load_packages(packages)

# Load previously filtered data
filtered_data <- readRDS(DATA_RESPONDED_PATH)

to_merge = read.csv(GEO_WITH_RUCA_PATH, colClasses = "character") %>%
  mutate(
    Region = ifelse(Region == "", NA, Region)
  )

print(GEO_WITH_RUCA_PATH)

geo_cols <- c("RUCA", "Region")

filtered_data = dplyr::left_join(filtered_data, dplyr::select(to_merge, c("SRGID", geo_cols)),
  by = c("caseid" = "SRGID"))

# Rename non-descriptive variable names
cleaned_data <- filtered_data %>%
  rename(gender = a5, age = b5, hispanic = d5, training = e5, magnet = g5)

# Ensure age and training are numeric
cleaned_data <- cleaned_data %>%
  mutate(age = as.numeric(age), training = as.numeric(training))

# Determine the range of years in the data, ignoring NA values
min_year <- min(cleaned_data$training, na.rm = TRUE)
max_year <- max(cleaned_data$training, na.rm = TRUE)

# Create decade intervals for age and training
cleaned_data <- cleaned_data %>%
  mutate(
    age_decade = cut(age, breaks = seq(0, 100, by = 10), right = FALSE,
                     labels = paste0(seq(0, 90, by = 10), "-", seq(9, 99, by = 10))),
    train_decade = cut(training,
                       breaks = seq(floor(min_year / 10) * 10, ceiling(max_year / 10) * 10, by = 10),
                       right = FALSE,
                       labels = paste0(seq(floor(min_year / 10) * 10, ceiling(max_year / 10) * 10 - 10, by = 10), "s"))
  )

# Calculate the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Add new columns for years since training and bin by decade
cleaned_data <- cleaned_data %>%
  mutate(
    years_since_training = current_year - training,
    training_decade_bin = cut(years_since_training,
                              breaks = c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, 89, 99, Inf),
                              labels = c("Less than 10 years", "10-19 years", "20-29 years", "30-39 years",
                                         "40-49 years", "50-59 years", "60-69 years", "70-79 years",
                                         "80-89 years", "90-99 years", "100 years or more"), right = FALSE)
  )

demo_vars <- c("gender", "age_decade", "train_decade", "hispanic", "magnet", "RUCA")

# Update race column names and values using handle_race_questions.R
original_race_cols <- cleaned_data[, paste0("c5_", 1:6)]
cleaned_data <- handle_race_questions(cleaned_data, original_race_cols)

# Load the code book and extract labels
codebook <- readr::read_csv(file.path("codebook", "codebook.csv"))
labels <- purrr::map_chr(cleaned_data, safe_label)
labels_df <- tibble(item = names(cleaned_data), question = map_chr(labels, clean_question_text))

# Merge code book with labels dataframe and update the label attributes
merged_labels <- left_join(labels_df, codebook, by = "item")
for (i in seq_len(nrow(merged_labels))) {
  item <- merged_labels$item[i]
  question <- merged_labels$question[i]
  text <- merged_labels$text[i]

  if (item %in% names(cleaned_data)) {
    attr(cleaned_data[[item]], "label") <- question
    if (!is.na(text)) {
      cleaned_data[[paste0(item, "_text")]] <- text
    }
  }
}

cleaned_data <- cleaned_data %>%
  mutate_at(vars(all_of(demo_vars)), haven::as_factor) %>%
  bin_questions()

# Convert specified columns to factors, but exclude 'age'
for (question in demographic_questions) {
  if (question %in% names(cleaned_data) && !is.factor(cleaned_data[[question]]) && question != "age") {
    cleaned_data[[question]] <- as.factor(cleaned_data[[question]])
  } else {
    warning(paste("Column", question, "does not exist in cleaned_data"))
  }
}

# Clean data using grepl within case_when
cleaned_data <- cleaned_data %>%
  mutate(white_or_poc = case_when(
    grepl("White", race, ignore.case = TRUE) ~ "White",
    grepl("Asian|Black|American Indian|Pacific Islander|Multiple races", race, ignore.case = TRUE) ~ "People of Color",
    TRUE ~ "Race not provided"
  )) %>%
  mutate(white_or_poc = factor(white_or_poc, levels = c("White", "People of Color", "Race not provided"))) %>%
  filter(is.na(age) | age >= 18) %>%
  mutate(RUCA_regions = case_when(
    RUCA %in% c(1, 2, 3) ~ "Urban",
    RUCA %in% c(4, 5, 6) ~ "Suburban",
    RUCA %in% c(7, 8, 9, 10) ~ "Rural",
    TRUE ~ NA
  )) %>%
  mutate(RUCA_regions = factor(RUCA_regions, levels = c("Urban", "Suburban", "Rural", "Unknown")))

# Collapse across RUCA codes with a small number of responses, for model stability
cleaned_data <- cleaned_data %>%
  mutate(
    RUCA_collapsed = if_else(RUCA %in% c(1, 2), as.character(RUCA), "Other"),
    gender_collapsed = fct_collapse(
      addNA(gender),
      `Other or Unknown` = c(
        "Nonbinary",
        "Gender nonconforming",
        "Gender not listed/prefer to self describe (Please specify):",
        "Prefer not to answer",
        NA
      )
    )
  )

# Save cleaned data
saveRDS(cleaned_data, file.path("data", "clean_data.rds"))

# save to excel
write.xlsx(cleaned_data, file.path("data", "clean_data.xlsx"))
