# Ensure strings are properly encoded
clean_string <- function(text) {
  Encoding(text) <- "UTF-8"
  return(text)
}

# Function to clean question labels (e.g. "White")
clean_labels <- function(col) {
  labels <- attr(col, "label")
  # Remove any text following a parenthesis
  cleaned_labels <- gsub("\\(.*", "", labels)
  return(list(original = labels, cleaned = cleaned_labels))
}

# Function to clean and format category strings (e.g. "Black_or_African_American")
clean_category <- function(this_category) {
  # Convert to lowercase
  this_category <- tolower(this_category)
  # Replace spaces with underscores
  this_category <- gsub(" ", "_", this_category)
  # Remove trailing underscores
  this_category <- gsub("_+$", "", this_category)
  return(this_category)
}

# Custom function to clean question text
clean_question_text <- function(question_text) {
  question_text <- clean_string(question_text)
  # Remove all text that begins with a " ("
  clean_text <- str_replace_all(question_text, "\\s*\\(.*", "")
  clean_text <- str_trim(clean_text)
  return(clean_text)
}

# Extract labels from clean_data
safe_label <- function(x) {
  label <- attr(x, "label")
  if (is.null(label)) {
    NA_character_
  } else {
    label
  }
}