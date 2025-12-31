library(dplyr)
library(labelled)

classify_variables <- function(data, variables) {
  var_info <- data.frame(variable = character(), type = character(), stringsAsFactors = FALSE)
  levels_info <- data.frame(variable = character(), level = character(), stringsAsFactors = FALSE)

  for (var in variables) {
    if (var %in% colnames(data)) {
      var_data <- data[[var]]

      if (is.numeric(var_data) && all(var_data %in% c(0, 1))) {
        var_type <- "binary"
      } else if (is.numeric(var_data)) {
        var_type <- "numeric"
      } else if (is.factor(var_data) || is.character(var_data)) {
        unique_levels <- unique(var_data)
        var_type <- "categorical"
        levels_info <- rbind(levels_info, data.frame(variable = var, level = unique_levels, stringsAsFactors = FALSE))
      } else {
        var_type <- "other"
      }

      var_info <- rbind(var_info, data.frame(variable = var, type = var_type, stringsAsFactors = FALSE))
    } else {
      warning(paste("variable", var, "not found in the dataframe."))
    }
  }

  return(list(variable_summary = var_info, categorical_levels = na.omit(levels_info)))
}

load_dict <- function() {
    var_df <- read.csv(file.path("codebook", "var_dict.csv"))
    lev_df <- read.csv(file.path("codebook", "lev_dict.csv"))
    joined_df <- dplyr::left_join(var_df, lev_df, by = "variable")
    list(var = var_df, lev = lev_df, both = joined_df)
}


recode_pretty <- function(data) {
  # Load the variable and level dictionaries
  dicts = load_dict()
  var_dict = dicts$var
  lev_dict = dicts$lev

  # Recode each categorical column based on the pretty_level in lev_dict
  for (var in unique(lev_dict$variable)) {
    if (var %in% names(data) && is.factor(data[[var]])) {
      # Create a named vector for recoding
      pretty_levels <- lev_dict$pretty_level[lev_dict$variable == var]
      original_levels <- lev_dict$level[lev_dict$variable == var]
      recode_map <- setNames(pretty_levels, original_levels)

      # Recode and reorder
      data[[var]] <- factor(data[[var]], levels = original_levels)
      data[[var]] <- dplyr::recode(data[[var]], !!!recode_map)
      data[[var]] <- factor(data[[var]], levels = pretty_levels, ordered = TRUE)
    }
  }

  # Assign labels to each column based on the pretty_variable column in var_dict
  for (i in 1:nrow(var_dict)) {
    var_name <- var_dict$variable[i]
    pretty_var_name <- var_dict$pretty_variable[i]
    if (var_name %in% names(data)) {
      if (!is.null(pretty_var_name) && pretty_var_name != "") {
        # Attempt to apply label and print verification
        print(paste("Setting label for:", var_name, "to", pretty_var_name))
        labelled::var_label(data[[var_name]]) <- pretty_var_name
        print(labelled::var_label(data[[var_name]]))
      }
    }
  }

  return(data)
}