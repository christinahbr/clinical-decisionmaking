source(file.path("code", "utils", "load_packages.R"))

# Function to get column names within a specified range
get_column_names <- function(data, start_col, end_col) {
  start_pos <- which(names(data) == start_col)
  end_pos <- which(names(data) == end_col)
  
  if (length(start_pos) == 0 || length(end_pos) == 0) {
    stop("One or both column names not found in the data.")
  }
  
  col_names <- names(data)[start_pos:end_pos]
  
  return(col_names)
}

# Function to map variable names to their labels
get_variable_labels <- function(df, variables) {
  # Filter the dataframe to include only the specified variables
  df_filtered <- df %>% dplyr::select(variables)
  
  # Extract labels for the specified variables
  labels <- lapply(names(df_filtered), function(name) attr(df_filtered[[name]], "label"))
  names(labels) <- names(df_filtered)
  # labels <- unlist(labels)
  return(labels)
}

# get_variable_labels <- function(df, variables) {
#   # Filter the dataframe to include only the specified variables
#   df_filtered <- df %>% dplyr::select(all_of(variables))
#   
#   # Extract labels for the specified variables and ensure they are strings
#   labels <- sapply(names(df_filtered), function(name) {
#     label <- attr(df_filtered[[name]], "label")
#     if (is.null(label)) {
#       return("")  # Return an empty string if the label is NULL
#     } else {
#       return(as.character(label))  # Ensure the label is a string
#     }
#   })
#   
#   # Convert the named vector to an unnamed character vector
#   labels <- unname(labels)
#   return(labels)
# }

extended_race_questions <- c(
  "white", "black_or_african_american", 
  "american_indian_or_alaska_native",
  "native_hawaiian_or_other_pacific_islander",
  "asian", "prefer_not_to_answer")

demographic_questions_with_extended_race <- c("age_decade", "gender", 
                                              extended_race_questions, 
                                              "hispanic", "RUCA", 
                                              "train_decade", "magnet")

# Define demographic questions
demographic_questions_cat <- c("gender", "race", "hispanic", "RUCA_regions", "magnet")
demographic_questions <- c("age", "gender", "race", "hispanic", "years_since_training", "RUCA_regions", "magnet")
