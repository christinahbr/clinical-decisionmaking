source(file.path("code", "utils", "load_custom_fonts.R"))

# Define palette
palette <- c("#FF7F7F", "#7FB3FF", "#7FFF7F", "#FFB37F", "#FFD700",
             "#87CEFA", "#FF69B4", "#7F7FFF", "#FFC0CB", "#7FFFFF",
             "#DB7093", "#FFE4B5", "#FFFACD", "#E6E6FA", "#D8BFD8")

prepare_loadings <- function(loadings, variable_labels, col_titles, threshold = 0.5) {
  variable_names <- paste0("a1_", 1:15)
  
  manual_order <- c(
    "a1_8", "a1_9", "a1_10", "a1_11", 
    "a1_12", "a1_13", 
    "a1_5", "a1_6", "a1_7", 
    "a1_15", "a1_1", "a1_14", "a1_2", "a1_3", "a1_4"
  )
  
  loadings_df <- as.data.frame(unclass(loadings))
  loadings_df$Variable <- variable_names
  
  wrapped_labels <- str_wrap(map_chr(variable_names, ~ variable_labels[[.x]]), width = 35)
  names(wrapped_labels) <- variable_names
  
  loadings_df <- loadings_df %>%
    mutate(Label = wrapped_labels[Variable]) %>%
    arrange(factor(Variable, levels = manual_order))
  
  wrapped_manual_labels <- str_wrap(map_chr(manual_order, ~ variable_labels[[.x]]), width = 35)
  loadings_df <- loadings_df %>%
    mutate(
      Label = factor(Label, levels = rev(wrapped_manual_labels))
    ) %>%
    arrange(factor(Variable, levels = manual_order))
  
  loadings_matrix <- loadings_df %>%
    dplyr::select(-Variable) %>%
    as_tibble(rownames = NULL) %>%
    column_to_rownames(var = "Label")
  
  loadings_tidy <- loadings_matrix %>%
    rownames_to_column(var = "Label") %>%
    pivot_longer(-Label, names_to = "Component", values_to = "Loading") %>%
    mutate(
      Label = factor(Label, levels = levels(loadings_df$Label)),
      Component = factor(Component, levels = names(col_titles)),
      Component = fct_relabel(Component, ~ str_replace_all(str_wrap(col_titles[.], width = 14), "\n", "<br>")),
      Display = ifelse(abs(Loading) >= threshold, sprintf("%.2f", Loading), "")
    )
  
  return(loadings_tidy)
}

# ---- 1) Overview (single panel) ----
plot_loadings_overview <- function(loadings_df, title, s = NULL) {

  ggplot(loadings_df, aes(x = Label, y = Loading, fill = Principal_Component)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = title, x = "Variables", y = "Loadings") +
    theme_minimal(base_size = 12 * s) +
    theme(
      plot.title   = element_text(size = 18 * s, face = "bold"),
      axis.title.x = element_text(size = 14 * s, face = "bold"),
      axis.title.y = element_text(size = 14 * s, face = "bold"),
      axis.text.x  = element_text(size = 12 * s),
      axis.text.y  = element_text(size = 12 * s),
      legend.title = element_text(size = 14 * s),
      legend.text  = element_text(size = 12 * s)
    )
}

# ---- 2) Faceted version ----
plot_loadings_faceted <- function(loadings_df, title, facet_titles, s = NULL) {
  
  loadings_df$Principal_Component <- factor(loadings_df$Principal_Component, labels = facet_titles)
  
  ggplot(loadings_df, aes(x = Label, y = Loading, fill = Label)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = title, x = "Variables", y = "Loadings") +
    theme_minimal(base_size = 14 * s) +
    theme(
      plot.title   = element_text(size = 22 * s, face = "bold"),
      axis.title.x = element_text(size = 18 * s, face = "bold"),
      axis.title.y = element_text(size = 18 * s, face = "bold"),
      axis.text.x  = element_text(size = 14 * s),
      axis.text.y  = element_text(size = 14 * s),
      legend.title = element_text(size = 16 * s),
      legend.text  = element_text(size = 14 * s),
      strip.text   = element_text(size = 16 * s, face = "bold")
    ) +
    facet_wrap(~ Principal_Component)
}

# ---- 3) Wrapper that builds both and passes the scale through ----
plot_loadings_custom <- function(
    loadings,
    title = "Variable Loadings",
    variable_labels,
    facet_titles,
    threshold = 0.5,
    s = NULL
) {
  
  # Ensure labels are named for a1_1..a1_15
  if (is.list(variable_labels)) variable_labels <- unlist(variable_labels, use.names = TRUE)
  stopifnot(all(paste0("a1_", 1:15) %in% names(variable_labels)))
  
  # Prepare tidy loadings with labels and component titles
  loadings_df <- prepare_loadings(
    loadings        = loadings,
    variable_labels = variable_labels,
    col_titles      = facet_titles,
    threshold       = threshold
  )
  
  # Keep only rows with numeric loadings
  loadings_df <- dplyr::filter(loadings_df, !is.na(Loading))
  
  # Match expected column name for faceted plot
  if (!"Principal_Component" %in% names(loadings_df) && "Component" %in% names(loadings_df)) {
    loadings_df <- dplyr::rename(loadings_df, Principal_Component = Component)
  }
  
  plot_overview <- plot_loadings_overview(loadings_df, title, s)
  plot_faceted  <- plot_loadings_faceted(loadings_df, title, facet_titles, s)
  
  list(overview = plot_overview, faceted = plot_faceted)
}





plot_scree_custom <- function(eigenvalues, title = "Scree Plot") {
  
  cumulative_variance <- cumsum(eigenvalues)
  
  data <- data.frame(
    Principal_Component = 1:length(eigenvalues),
    Variance_Explained = eigenvalues,
    Cumulative_Variance = cumulative_variance
  )
  
  ggplot(data) +
    geom_point(aes(x = Principal_Component, y = Variance_Explained, 
                   color = "Marginal Variance Explained")) +
    geom_line(aes(x = Principal_Component, y = Variance_Explained, 
                  color = "Marginal Variance Explained")) +
    geom_point(aes(x = Principal_Component, y = Cumulative_Variance, 
                   color = "Cumulative Variance Explained")) +
    geom_line(aes(x = Principal_Component, y = Cumulative_Variance, 
                  color = "Cumulative Variance Explained")) +
    labs(
      title = title, 
      x = "Principal Component", 
      y = "Variance Explained (%)", 
      color = "Legend"
    ) +
    theme_minimal(base_size = 18) +   # sets a larger baseline
    geom_text(
      aes(x = Principal_Component, y = Variance_Explained, 
          label = round(Variance_Explained, 2)), 
      vjust = -0.5, color = "blue", size = 5
    ) +
    geom_text(
      aes(x = Principal_Component, y = Cumulative_Variance, 
          label = round(Cumulative_Variance, 2)), 
      vjust = 1.5, color = "red", size = 5
    ) +
    theme(
      plot.title   = element_text(size = 24, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(size = 16),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 16)
    ) +
    theme(legend.position = "bottom")
}


plot_biplot_custom <- function(scores, loadings, variable_labels, components = c(1, 2)) {
  
  # Dynamically name the columns
  num_cols <- ncol(scores)
  colnames(scores) <- paste0("PC", 1:num_cols)
  colnames(loadings) <- paste0("PC", 1:num_cols)
  
  # Ensure loadings have rownames for Variable
  loadings_df <- as.data.frame(loadings)
  loadings_df$Variable <- rownames(loadings_df)
  loadings_df$Variable <- as.numeric(loadings_df$Variable)
  
  # Replace Variable names with corresponding labels from variable_labels
  loadings_df <- loadings_df %>%
    dplyr::mutate(Label = map_chr(Variable, ~ variable_labels[[.x]]))
  
  # Wrap text every 20 characters
  loadings_df <- loadings_df %>%
    mutate(Label = str_wrap(Label, width = 20))
  
  # Extract specified components for plotting
  pc_x <- paste0("PC", components[1])
  pc_y <- paste0("PC", components[2])
  
  # Generate dynamic title
  dynamic_title <- paste0("Biplot - Rotated PCA (", pc_x, " vs ", pc_y, ")")
  
  ggplot(scores, aes_string(x = pc_x, y = pc_y)) +
    geom_point(color = "lavender", size = 1, alpha = 0.6) +  # Customize point appearance
    labs(title = dynamic_title, x = pc_x, y = pc_y) +
    theme_light() +  # Use a more visually appealing theme
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank()
    ) +
    geom_segment(
      data = loadings_df, 
      aes(x = 0, y = 0, xend = !!sym(pc_x) * max(scores[[pc_x]], na.rm = TRUE), 
          yend = !!sym(pc_y) * max(scores[[pc_y]], na.rm = TRUE)), 
      arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "red", size = 1  # Customize arrow appearance
    ) +
    geom_label_repel(
      data = loadings_df, 
      aes(x = !!sym(pc_x) * max(scores[[pc_x]], na.rm = TRUE) * 1.1,
          y = !!sym(pc_y) * max(scores[[pc_y]], na.rm = TRUE) * 1.1, 
          label = Label), 
      color = "black",
      fill = "white",
      max.overlaps = 10,  # Adjust this value as needed
      box.padding = 0.5,  # Increase padding around the text labels
      point.padding = 0.5,  # Increase padding around the points
      segment.size = 0.2,  # Adjust the size of the connecting lines
      segment.color = "grey50",  # Set the color of the connecting lines
      label.padding = unit(0.3, "lines"),  # Adjust padding around the labels
      alpha = 0.9  # Set the transparency of the labels
    )
}

# Function to calculate mean and plot distributions
compare_means_and_distributions <- function(data_frame, demographic_var, categorized_columns) {
  
  for (category in names(categorized_columns)) {
    cat("\nCategory:", category, "\n")
    selected_columns <- categorized_columns[[category]]
    demographic_data <- data_frame %>% dplyr::select(all_of(demographic_var))
    selected_data <- data_frame %>% dplyr::select(all_of(selected_columns))
    limited_data <- limit_levels(selected_data)
    combined_data <- bind_cols(demographic_data, limited_data)
    mean_scores <- combined_data %>%
      rowwise() %>%
      mutate(row_mean = mean(c_across(all_of(selected_columns)), na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(!!sym(demographic_var)) %>%
      summarise(mean_value = mean(row_mean, na.rm = TRUE), n = n())
    print(mean_scores)
    
    long_data <- combined_data %>%
      pivot_longer(cols = all_of(selected_columns), names_to = "variable", values_to = "value")
    
    print(head(long_data))
    
    freq_data <- long_data %>%
      group_by(value, !!sym(demographic_var)) %>%
      summarise(n = n(), .groups = 'drop') %>%
      group_by(!!sym(demographic_var)) %>%
      mutate(total = sum(n, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(percent = (n / total) * 100) %>%
      group_by(value, !!sym(demographic_var)) %>%
      summarise(percent = mean(percent), se = sqrt((percent * (100 - percent)) / total), .groups = 'drop')
    
    print(freq_data)
    
    level_mapping <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Frequently" = 5)
    
    pc_plot <- ggplot(freq_data, aes(x = value, y = percent, fill = !!sym(demographic_var))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_errorbar(aes(ymin = percent - se, ymax = percent + se), position = position_dodge(width = 0.9), width = 0.25, color = "black") +
      scale_x_continuous(breaks = unname(level_mapping), labels = names(level_mapping)) +
      scale_fill_manual(values = palette) +
      labs(title = category, x = "Score", y = "Percent Frequency") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    print(pc_plot)
  }
}

# Function to calculate mean and plot comparisons
compare_means_across_categories <- function(data_frame, demographic_var, categorized_columns, palette, plot_title) {
  results_list <- lapply(names(categorized_columns), function(category) {
    cat("\nCategory:", category, "\n")
    selected_columns <- categorized_columns[[category]]
    demographic_data <- data_frame %>% dplyr::select(all_of(demographic_var))
    selected_data <- data_frame %>% dplyr::select(all_of(selected_columns))
    limited_data <- limit_levels(selected_data)
    combined_data <- bind_cols(demographic_data, limited_data)
    mean_se_data <- combined_data %>%
      pivot_longer(cols = all_of(selected_columns), names_to = "variable", values_to = "value") %>%
      group_by(!!sym(demographic_var)) %>%
      summarise(mean_value = mean(value, na.rm = TRUE), se = 1.96 * sd(value, na.rm = TRUE) / sqrt(n()), .groups = 'drop') %>%
      mutate(category = str_wrap(category, width = 20))
    mean_se_data
  })
  
  combined_results <- bind_rows(results_list)
  print(combined_results)
  mean_plot <- ggplot(combined_results, aes(x = category, y = mean_value, fill = !!sym(demographic_var))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), position = position_dodge(width = 0.9), width = 0.25, color = "black") +
    labs(title = plot_title, x = "Variable", y = "Mean Value") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  return(mean_plot)
}

# Function to calculate mean values, standard deviations, and confidence intervals for each category
calculate_category_stats <- function(data, categorized_columns) {
  category_stats <- lapply(names(categorized_columns), function(category) {
    columns <- categorized_columns[[category]]
    question_numbers <- gsub(".*_(\\d+).*", "\\1", columns)
    matched_columns <- names(data)[sapply(names(data), function(x) any(gsub(".*_(\\d+).*", "\\1", x) %in% question_numbers))]
    summary_stats <- data %>% dplyr::select(all_of(matched_columns)) %>%
      summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}"))
    
    mean_value <- mean(as.numeric(summary_stats %>% dplyr::select(ends_with("_mean"))), na.rm = TRUE)
    sd_value <- mean(as.numeric(summary_stats %>% dplyr::select(ends_with("_sd"))), na.rm = TRUE)
    n <- nrow(data)
    se <- sd_value / sqrt(n)
    ci <- qt(0.975, df = n - 1) * se
    labels <- columns[match(question_numbers, gsub(".*_(\\d+).*", "\\1", columns))]
    wrapped_labels <- paste(str_wrap(labels, width = 20), collapse = ", ")
    
    data.frame(category = category, mean_value = mean_value, sd_value = sd_value, ci = ci, labels = wrapped_labels)
  })
  bind_rows(category_stats)
}

# Function to calculate the proportion of non-0 and non-NA responses
calculate_proportion <- function(df, columns) {
  df %>%
    dplyr::select(all_of(columns)) %>%
    pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
    group_by(Question) %>%
    summarize(NonZero_NonNA_Responses = sum(Response != 0 & !is.na(Response)),
              Total_NonNA_Responses = sum(!is.na(Response)),
              Proportion = NonZero_NonNA_Responses / Total_NonNA_Responses)
}

# Function to calculate the mean values
calculate_mean <- function(df, columns) {
  df %>%
    dplyr::select(all_of(columns)) %>%
    pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
    group_by(Question) %>%
    summarize(Mean = mean(Response, na.rm = TRUE))
}

# Function to plot the proportions
plot_proportion <- function(proportion_df, title) {
  ggplot(proportion_df, aes(x = Question, y = Proportion, fill = Question)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = str_wrap(title, width = 50), x = "Questions", y = "Proportion of responses") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Function to plot the mean values
plot_mean <- function(mean_df, title) {
  ggplot(mean_df, aes(x = Question, y = Mean, fill = Question)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = str_wrap(title, width = 50), x = "Questions", y = "Mean Response") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Function to format p-values
format_p_value <- function(p) {
  if (p < 0.001) "p < 0.001" else if (p < 0.01) "p < 0.01" else if (p < 0.05) "p < 0.05" else "n.s."
}

# Custom renaming function for prefix labels
rename_prefixes <- function(colnames, prefix_labels) {
  for (prefix in names(prefix_labels)) {
    colnames <- gsub(paste0("^", prefix), prefix_labels[[prefix]], colnames)
  }
  colnames
}


plot_heatmap_custom <- function(
    loadings_tidy, 
    title = NULL,
    s = 1,
    r = 0.3,
    family = "FigTree",
    legend_title = "Factor Loadings"
) {
  
  heatmap_to_print <- ggplot(loadings_tidy, aes(x = Component, y = Label, fill = Loading)) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = Display),
      size   = 12 * s,       # scaled
      color  = "white",
      family = family
    ) +
    scale_fill_viridis_c(
      option   = "rocket",
      direction = -1,
      limits   = c(-1, 1),
      guide    = guide_colorbar(
        barwidth      = 10 * s,   # scaled
        barheight     = 1  * s,   # scaled
        title.position = "top",
        title.hjust    = 0.5
      )
    ) +
    scale_x_discrete(position = "top", expand = c(0, 0), limits = levels(loadings_tidy$Component)) +
    scale_y_discrete(expand = c(0, 0)) +
    coord_fixed(ratio = r) +
    theme_minimal() +
    theme(
      text = element_text(family = family, color = "gray20"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      axis.text.x.top = ggtext::element_markdown(
        size = 33 * s,         # scaled
        hjust = 0.5, vjust = 0.5,
        lineheight = 0.4,
        margin = margin(b = 5, t = 15)
      ),
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(size = 36 * s, face = "bold", margin = margin(b = 5)),
      
      axis.text.y  = element_text(size = 33 * s, margin = margin(r = 5)),
      axis.title.y = element_text(size = 36 * s, face = "bold", margin = margin(r = 10)),
      
      legend.title = element_text(size = 36 * s, margin = margin(b = 5)),
      legend.text  = element_text(size = 36 * s),
      legend.position = "bottom",
      legend.spacing  = grid::unit(3 * s, "cm"),   # scaled
      
      plot.margin = margin(1, 1, 1, 1, "cm"),
      plot.clip   = "off"
    ) +
    labs(
      x = "Principal Components",
      y = "Information Sources",
      fill = legend_title
    )
  
  if (!is.null(title)) {
    heatmap_to_print <- heatmap_to_print + labs(title = title)
  }
  
  heatmap_to_print
}




