# Define a custom summary function for mean (SD)
summary_function <- function(x) {
  if (is.numeric(x)) {
    mean_sd <- sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    return(mean_sd)
  } else {
    return(NA)
  }
}

# Function to limit levels to the first 5 and convert to numeric for selected columns only
limit_levels <- function(df) {
  level_mapping <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Frequently" = 5)
  
  df[] <- lapply(df, function(column) {
    if (is.factor(column) || is.ordered(column)) {
      # Convert factor levels to numeric values based on the mapping
      column <- as.character(column)
      column <- sapply(column, function(x) ifelse(x %in% names(level_mapping), level_mapping[x], NA))
      return(as.numeric(column))
    } else {
      return(column)
    }
  })
  return(df)
}

# Function to map variable names to their labels
get_variable_labels <- function(df, variables) {
  # Filter the dataframe to include only the specified variables
  df_filtered <- df %>% dplyr::select(all_of(variables))
  
  # Extract labels for the specified variables
  labels <- lapply(names(df_filtered), function(name) attr(df_filtered[[name]], "label"))
  names(labels) <- names(df_filtered)
  labels <- unlist(labels)
  return(labels)
}

# Function to categorize columns by time including only the specified numbers
categorize_columns_by_time <- function(data_frame) {
  time1_columns <- grep(paste0("^a1.*_", pattern), names(data_frame), value = TRUE)
  time2_columns <- grep(paste0("^c1.*_", pattern), names(data_frame), value = TRUE)
  list(time1 = time1_columns, time2 = time2_columns)
}

# General function to prepare data for PCA
prepare_data_for_pca <- function(df, vars, subscale_name, observation_value = NULL) {
  df_subset <- df[, vars] %>%
    dplyr::mutate(dplyr::across(
      -responseid, 
      ~ if (inherits(., "haven_labelled")) {
        as.numeric(haven::as_factor(.))
      } else {
        as.numeric(.)
      }
    ))
  
  df_long <- df_subset %>%
    dplyr::select(responseid, dplyr::all_of(vars)) %>%
    tidyr::pivot_longer(cols = -responseid, names_to = "item", values_to = "value") %>%
    dplyr::mutate(subscale = subscale_name) %>%
    dplyr::mutate(item = stringr::str_extract(item, "\\d+$")) %>%
    dplyr::mutate(observation = ifelse(is.null(observation_value), NA, observation_value))
  
  df_wide <- df_long %>%
    tidyr::pivot_wider(names_from = item, values_from = value) %>%
    dplyr::arrange(responseid, observation) %>%
    dplyr::mutate(dplyr::across(-responseid, as.numeric)) %>%
    dplyr::select(-subscale)
  
  df_wide
}

# List of variable names as character strings
variable_names <- c("a1_1", "a1_2", "a1_3", "a1_4", "a1_5", "a1_6", "a1_7", "a1_8", "a1_9", "a1_10", "a1_11", "a1_12", "a1_13", "a1_14", "a1_15")
