bin_questions <- function(
    df
) {
  # # Create white_or_poc using grepl within case_when
  df <- df %>%
    mutate(white_or_poc = case_when(
      grepl("White", race, ignore.case = TRUE) ~ "White",
      grepl("Asian|Black|American Indian|Pacific Islander|Multiple races", race, ignore.case = TRUE) ~ "People of Color",
      TRUE ~ "Race not provided"  # Handle any other cases, if necessary
    ))
  
  # # Ensure white_or_poc is a factor with explicit levels
  df$white_or_poc <- factor(df$white_or_poc, levels = c("White", "People of Color", "Race not provided"))
  
  # # Create RUCA_regions with explicit factor levels
  df <- df %>%
    mutate(RUCA_regions = case_when(
      RUCA %in% c(1, 2, 3) ~ "Urban",
      RUCA %in% c(4, 5, 6) ~ "Suburban",
      RUCA %in% c(7, 8, 9, 10) ~ "Rural",
      TRUE ~ "Unknown"  # Handle any other cases, if necessary
    )) %>%
    mutate(RUCA_regions = factor(RUCA_regions, levels = c("Urban", "Suburban", "Rural", "Unknown")))
  
  return(df)
}


