handle_race_questions <- function(df, old_race_cols = NULL) {

  # Initialize an empty character vector to store new column names
  new_race_cols <- character()

  # Step 1: Extract and clean the labels
  race_labels <- lapply(old_race_cols, clean_labels)

  # Step 2: Rename columns and modify values, by each racial group assessed in the survey
  for (i in seq_along(race_labels)) {
    col_name <- paste0("c5_", i)
    race_category <- race_labels[[i]]$cleaned
    original_text <- race_labels[[i]]$original # includes ()s, etc.

    # Cleans text, one racial group at a time
    if (!is.na(race_category)) {
      # Renames the columns to be more descriptive (e.g. changes "cb_1" to "white")
      new_col_name <- clean_category(race_category)
      df <- df %>%
        rename(!!sym(new_col_name) := !!sym(col_name))

      # Append the new column name to the character vector
      new_race_cols <- c(new_race_cols, new_col_name)

      # Convert the column to character to avoid type mismatch issues
      df[[new_col_name]] <- as.character(df[[new_col_name]])

      # Replace 1s with the race_category label
      df[[new_col_name]][df[[new_col_name]] == "1"] <- race_category

      # Replace 0s with NAs
      df[[new_col_name]][df[[new_col_name]] == "0"] <- NA

      # Convert the column back to factor
      df[[new_col_name]] <- factor(df[[new_col_name]])

    } else {
      warning(paste("Skipping column", new_col_name, "due to NA in race_category"))
    }
  }

  print(new_race_cols)
  print(df[, new_race_cols[1]])

  # Extract value labels for updated race items
  race_labels <- lapply(df[, new_race_cols], function(col) {
    labelled::val_labels(col)
  })

  print(race_labels)

  # Create a mapping of race columns to their labels
  race_mapping <- sapply(race_labels, function(lbl) {
    if (length(lbl) > 0) {
      return(names(lbl)[1])
    } else {
      return(NA)
    }
  })
  print(race_mapping)

  # Create a new variable for race
  df <- df %>%
    mutate(race = apply(dplyr::select(., all_of(new_race_cols)), 1, function(row) {
      selected_races <- row[!is.na(row) & row != ""]
      if (length(selected_races) == 1) {
        return(selected_races)
      } else if (length(selected_races) > 1) {
        return("Multiple races")
      } else {
        return(NA)
      }
    }))

  print(df$race)

  # Convert the new race variable to a factor
  df$race <- as.factor(df$race)

  # Return the modified dataframe
  return(df)
}