
# Define the categories
categories <- list(
  "Leadership and Professional Networks" = c("8", "9", "10", "11", "12", "13"),
  "News and Social Media Platforms" = c("5", "6", "7", "15"),
  "Academic and Professional Publications" = c("1", "2", "14"),
  "Informal Information Sources" = c("3", "4")
)

# Initialize lists to store categorized column names
categorized_columns <- list(
  "Leadership and Professional Networks" = c(),
  "News and Social Media Platforms" = c(),
  "Academic and Professional Publications" = c(),
  "Informal Information Sources" = c()
)

# Function to categorize columns
categorize_columns <- function(data_frame, categories, include_a = TRUE, include_b = TRUE, include_c = TRUE) {
  # Initialize an empty list to store categorized columns
  categorized_columns <- list()
  
  # Determine which prefixes to include
  prefixes <- c()
  if (include_a) prefixes <- c(prefixes, "a")
  if (include_b) prefixes <- c(prefixes, "b")
  if (include_c) prefixes <- c(prefixes, "c")
  
  # Create a pattern for the prefixes followed by '1_'
  prefix_pattern <- paste(paste0(prefixes, "1_"), collapse = "|")
  
  for (category in names(categories)) {
    categorized_columns[[category]] <- c()  # Initialize the category in the list
    for (suffix in categories[[category]]) {
      pattern <- paste0("^(", prefix_pattern, ").*", suffix, "$")
      matching_columns <- grep(pattern, names(data_frame), value = TRUE)
      categorized_columns[[category]] <- c(categorized_columns[[category]], matching_columns)
    }
  }
  return(categorized_columns)
}

# Function to calculate averages of specified columns and save them with new column names
calculate_and_save_averages <- function(data_frame, column_groups, new_column_names) {
  # Check if the length of column_groups and new_column_names are the same
  if(length(column_groups) != length(new_column_names)) {
    stop("The length of column_groups and new_column_names must be the same.")
  }
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  # Loop through each group of columns
  for (i in seq_along(column_groups)) {
    new_column_name <- new_column_names[i]
    
    # Select the columns for the current group
    selected_columns <- column_groups[[i]]
    
    # Calculate the rowwise average for the selected columns
    avg_data <- data_frame %>%
      rowwise() %>%
      mutate(!!new_column_name := mean(c_across(all_of(selected_columns)), na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::select(!!new_column_name) %>%
      mutate(!!new_column_name := ifelse(is.nan(!!sym(new_column_name)), NA, !!sym(new_column_name)))
    
    # Append the result to the list
    results_list[[new_column_name]] <- avg_data
  }
  
  # Combine all results into a single data frame
  combined_results <- bind_cols(results_list)
  
  return(combined_results)
}

# Function to print a count of all values in the columns listed in info_quality
count_values_in_columns <- function(data_frame, columns) {
  # Gather the data into long format
  long_data <- data_frame %>%
    dplyr::select(all_of(columns)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  # Count the occurrences of each value
  value_counts <- long_data %>%
    count(value) %>%
    arrange(desc(n))
  
  # Print the counts
  print(value_counts)
}

# Count non-NA (i.e., 1–5 after recode) per item for a subscale
count_1to5_per_item <- function(df, cols, subscale_label, combo_suffix) {
  if (length(cols) == 0) {
    return(tibble(combo = character(), subscale = character(), item = character(), n_1to5 = integer()))
  }
  df %>%
    dplyr::select(all_of(cols)) %>%
    summarise(across(everything(), ~ sum(!is.na(.x)))) %>%
    tidyr::pivot_longer(everything(), names_to = "item", values_to = "n_1to5") %>%
    mutate(combo = combo_suffix,
           subscale = subscale_label,
           .before = 1)
}