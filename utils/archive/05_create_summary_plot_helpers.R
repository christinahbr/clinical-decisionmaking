# # Define a color palette for the plots
# palette <- c(
#   "#FF7F7F", "#7FB3FF", "#7FFF7F", "#FFB37F", "#FFD700",
#   "#87CEFA", "#FF69B4", "#7F7FFF", "#FFC0CB", "#7FFFFF",
#   "#DB7093", "#FFE4B5", "#FFFACD", "#E6E6FA", "#D8BFD8"
# )

get_plot_color <- function(question_number, column_names) {
  color_index <- match(question_number, column_names)
  plot_color <- palette[(color_index - 1) %% length(palette) + 1]
  return(plot_color)
}

# print_question_title <- function(
#     df,
#     question_number,
#     question_numbers,
#     header_only = FALSE
# ) {
# 
#   # Contains section text
#   text_column <- paste0(question_number, "_text")
# 
#   # Contains text for each sub-item (e.g. "Facebook")
#   question_label <- attr(df[[question_number]], "label")
# 
#   # Check if the text column exists and print its content
#   if (text_column %in% names(df)) {
# 
#     # Ensure the text column is of character type
#     if (!is.character(df[[text_column]])) {
#       df[[text_column]] <- as.character(df[[text_column]])
#     }
# 
#     # Extract the first non-NA value if possible
#     section_text <- na.omit(df[[text_column]])[1]
# 
#   } else {
#     print(paste(text_column, "does not exist in the dataframe."))
#     section_text <- NA
#   }
# 
#   # section_text <- if (text_column %in% names(df)) df[[text_column]][1] else NA
#   # print(section_text)
# 
#   if (!is.na(section_text) && question_number %in% question_numbers[1] && !header_only) {
#     cat("\n\n#### ", section_text, "\n\n\n")
#     cat("\n\n##### ", question_label, "\n\n\n\n")
#   } else if (header_only == TRUE) {
#     cat("\n\n#### ", section_text, "\n\n\n")
#   } else {
#     cat("\n\n##### ", question_label, "\n\n\n\n")
#   }
# }

# print_subset_title <- function(combination, group_var = NULL, subset_var = NULL) {
#   combination_title <- paste0(
#     # if (!is.null(group_var)) paste0(group_var, ": ", combination[[group_var]], ", ") else "",
#     if (!is.null(subset_var)) paste0(subset_var, ": ", combination[[subset_var]], "s") else ""
#   )
#   cat("\n\n<div style='text-align:center; font-size:14px; font-weight:bold;'>", combination_title, "</div>\n\n\n")
# }

# summarize_question_data <- function(df, question_number, group_var = NULL, subset_var = NULL) {
#   # Reorder responses to ensure levels are correctly set
#   df <- reorder_responses(df, question_number)
# 
#   # Filter out NAs and calculate the frequency of each response
#   filtered_df <- df[!is.na(df[[question_number]]), ]
# 
#   # Grouping by question response, group, and subset if provided
#   group_vars <- c("Response" = question_number)
#   if (!is.null(group_var)) group_vars <- c(group_vars, "Group" = group_var)
#   if (!is.null(subset_var)) group_vars <- c(group_vars, "Subset" = subset_var)
# 
#   data_for_plot <- filtered_df %>%
#     group_by(across(all_of(group_vars))) %>%
#     summarise(Frequency = n(), .groups = 'drop') %>%
#     mutate(
#       Percent = round(Frequency / sum(Frequency) * 100, 2),
#       CumulativePercent = round(cumsum(Frequency) / sum(Frequency) * 100, 2)
#     )
#   # Ensure the data is sorted by the factor levels
#   data_for_plot <- data_for_plot %>%
#     mutate(Response = factor(Response, levels = levels(df[[question_number]]))) %>%
#     arrange(Response)
# 
#   return(data_for_plot)
# }

# create_frequency_table <- function(plot_data, factor_levels) {
#   # Create and style the table
#   table_output <- plot_data %>%
#     # select(Response, Frequency, Percent, CumulativePercent) %>%
#     kable(
#       booktabs = TRUE,
#       format = "html",
#       row.names = FALSE) %>%
#     kable_styling(
#       font_size = 12,
#       latex_options = "hold_position",
#       full_width = FALSE)
# 
#   return(invisible(table_output))
# }

# reorder_responses <- function(df, question_number) {
#   # Ensure the column is a factor with haven labels
#   if (!is.factor(df[[question_number]])) {
#     df[[question_number]] <- as_factor(df[[question_number]])
#   }
#   # Print the original levels for debugging
#   original_levels <- levels(df[[question_number]])
#   # Reverse the factor levels
#   reversed_levels <- rev(original_levels)
#   # Apply the reversed levels to the factor
#   df[[question_number]] <- factor(df[[question_number]], levels = reversed_levels)
#   final_levels <- levels(df[[question_number]])
#   return(df)
# }

print_non_respondents <- function(df, question_number) {
  na_count <- sum(is.na(df[[question_number]]))
  total_responses <- nrow(df)
  na_percent <- round((na_count / total_responses) * 100, 2)
  non_respondent_message <- cat("\nNote: There were", na_count, "non-respondents (", na_percent, "% of total).\n\n\n")
  non_respondents <- count_non_respondents(df, question_number)
  if (non_respondents > 0) {
    non_respondent_percent <- round((non_respondents / total_responses) * 100, 2)
    non_respondent_message <- cat(non_respondent_message, " There were", non_respondents, "participants who did not use this information source (", non_respondent_percent, "% of total).\n\n\n")
  }
  return(invisible(non_respondent_message))
}

count_non_respondents <- function(df, question_number) {
  # Extract the column as a vector
  column_data <- df[[question_number]]
  # Check if the column is a factor
  if (is.factor(column_data)) {
    # Get the levels of the factor
    scale_items <- levels(column_data)
  } else {
    # Get unique items and sort them
    scale_items <- sort(unique(column_data))
  }
  if (length(scale_items) > 5) {
    sixth_level <- scale_items[1]
    non_users <- sum(column_data == sixth_level, na.rm = TRUE)
    return(non_users)
  } else {
    return(0)
  }
}
