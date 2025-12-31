
plot_frequencies <- function(
    df,
    question_numbers,
    subset_var = NULL,
    subset_categories = NULL,
    group_var = NULL
    ) {
  
  # Generate the plot title
  print_question_title(df, question_numbers[1], question_numbers, header_only = TRUE)

  if (!is.null(subset_var)) {
    # Determine the categories to use
    categories <- if (!is.null(subset_categories)) subset_categories else unique(df[[subset_var]])
    
    # Filter the data based on the subset categories
    df <- df[df[[subset_var]] %in% categories, ]
  }
  
  results <- lapply(question_numbers, function(question_number) {
    
    if (!question_number %in% names(df)) {
      print(paste("The question", question_number, "does not exist in the dataset."))
      return(NULL)
    } else {
      
      # Ensure the data has the necessary columns before proceeding
      columns_to_check <- c(group_var, subset_var)
      columns_to_check <- columns_to_check[!is.null(columns_to_check)]

      # Convert haven_labelled columns to character and filter out rows with NA values
      vars_to_process <- c(group_var, subset_var)
      
      for (var in vars_to_process) {
        if (!is.null(var)) {
          df[[var]] <- as.character(df[[var]])
          df <- df[!is.na(df[[var]]), ]
        }
      }
      
      # Get unique combinations of group_var and subset_var
      unique_combinations <- df %>% distinct(across(all_of(columns_to_check)))
      
      # print_question_title(df, question_number, question_numbers)  # Print the overall question title once
      
      lapply(1:nrow(unique_combinations), function(i) {
        combination <- unique_combinations[i, ]
        
        # Print subset title
        print_subset_title(combination, group_var, subset_var)
        
        # Construct filter conditions
        condition <- rep(TRUE, nrow(df))
        if (!is.null(group_var)) {
          condition <- condition & (df[[group_var]] == combination[[group_var]])
        }
        if (!is.null(subset_var)) {
          condition <- condition & (df[[subset_var]] == combination[[subset_var]])
        }
        subset_df <- df[condition, ]
        
        if (nrow(subset_df) >= 20) {
          # Plot all subsets as facets
          non_respondents_text <- capture.output(print_non_respondents(subset_df, question_number))
          cat(non_respondents_text)
          # plot_data <- remove_non_respondents(subset_df, question_number)
          plot_data <- summarize_question_data(subset_df, question_number, group_var, subset_var)
          frequency_table <- create_frequency_table(plot_data, levels(df[[question_number]])) # group_var, subset_var
          print(frequency_table)
        } else {
          cat("This subset included fewer than 20 individuals.")
        }
      })
    }
  })
}


print_question_title <- function(
    df,
    question_number,
    question_numbers,
    header_only = FALSE
) {

  # Contains section text
  text_column <- paste0(question_number, "_text")

  # Contains text for each sub-item (e.g. "Facebook")
  question_label <- attr(df[[question_number]], "label")

  # Check if the text column exists and print its content
  if (text_column %in% names(df)) {

    # Ensure the text column is of character type
    if (!is.character(df[[text_column]])) {
      df[[text_column]] <- as.character(df[[text_column]])
    }

    # Extract the first non-NA value if possible
    section_text <- na.omit(df[[text_column]])[1]

  } else {
    print(paste(text_column, "does not exist in the dataframe."))
    section_text <- NA
  }

  section_text <- if (text_column %in% names(df)) df[[text_column]][1] else NA
  print(section_text)

  if (!is.na(section_text) && question_number %in% question_numbers[1] && !header_only) {
    cat("\n\n#### ", section_text, "\n\n\n")
    cat("\n\n##### ", question_label, "\n\n\n\n")
  } else if (header_only == TRUE) {
    cat("\n\n#### ", section_text, "\n\n\n")
  } else {
    cat("\n\n##### ", question_label, "\n\n\n\n")
  }
}

print_subset_title <- function(combination, group_var = NULL, subset_var = NULL) {
  combination_title <- paste0(
    if (!is.null(group_var)) paste0(group_var, ": ", combination[[group_var]], ", ") else "",
    if (!is.null(subset_var)) paste0(subset_var, ": ", combination[[subset_var]], "s") else ""
  )
  cat("\n\n<div style='text-align:center; font-size:14px; font-weight:bold;'>", combination_title, "</div>\n\n\n")
}

summarize_question_data <- function(df, question_number, group_var = NULL, subset_var = NULL) {
  # Reorder responses to ensure levels are correctly set
  df <- reorder_responses(df, question_number)

  # Filter out NAs and calculate the frequency of each response
  filtered_df <- df[!is.na(df[[question_number]]), ]

  # Grouping by question response, group, and subset if provided
  group_vars <- c("Response" = question_number)
  if (!is.null(group_var)) group_vars <- c(group_vars, "Group" = group_var)
  if (!is.null(subset_var)) group_vars <- c(group_vars, "Subset" = subset_var)

  data_for_plot <- filtered_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(Frequency = n(), .groups = 'drop') %>%
    mutate(
      Percent = round(Frequency / sum(Frequency) * 100, 2),
      CumulativePercent = round(cumsum(Frequency) / sum(Frequency) * 100, 2)
    )
  # Ensure the data is sorted by the factor levels
  data_for_plot <- data_for_plot %>%
    mutate(Response = factor(Response, levels = levels(df[[question_number]]))) %>%
    arrange(Response)

  return(data_for_plot)
}

create_frequency_table <- function(plot_data, factor_levels) {
  # Create and style the table
  table_output <- plot_data %>%
    # select(Response, Frequency, Percent, CumulativePercent) %>%
    kable(
      booktabs = TRUE,
      format = "html",
      row.names = FALSE) %>%
    kable_styling(
      font_size = 12,
      latex_options = "hold_position",
      full_width = FALSE)

  return(invisible(table_output))
}

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


reorder_responses <- function(df, question_number) {
  # Ensure the column is a factor with haven labels
  if (!is.factor(df[[question_number]])) {
    df[[question_number]] <- as_factor(df[[question_number]])
  }
  # Print the original levels for debugging
  original_levels <- levels(df[[question_number]])
  # Reverse the factor levels
  reversed_levels <- rev(original_levels)
  # Apply the reversed levels to the factor
  df[[question_number]] <- factor(df[[question_number]], levels = reversed_levels)
  final_levels <- levels(df[[question_number]])
  return(df)
}