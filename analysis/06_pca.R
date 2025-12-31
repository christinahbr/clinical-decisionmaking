# Source helper functions
source(file.path("code", "utils", "load_packages.R"))
source(file.path("code", "utils", "grab_columns.R"))
source(file.path("code", "utils", "06_pca_plot_functions.R"))
source(file.path("code", "utils", "06_format_columns_PCA.R"))

getOption("repos")
options(repos = c(CRAN = "https://cran.rand.org"))

packages <- c(
  "dplyr",
  "haven",
  "missMDA",
  "psych",
  "tidyr",
  "tibble",
  "purrr",
  "stringr",
  "ggplot2",
  "ggrepel",
  "forcats",
  "showtext",
  "ggtext"
)
load_packages(packages)

survey_results <- readRDS(file.path("data", "clean_data.rds"))

# Check which fonts are installed
# systemfonts::system_fonts() |> View()
# Download and add font like so
# font_add("Figtree", "C:/Users/chuber/AppData/Local/Microsoft/Windows/Fonts/Figtree-VariableFont_wght.ttf")
# Turn on showtext
showtext_auto()

plotting_labels <- list(
  a1_1 = "Peer-Reviewed Journals",
  a1_2 = "Pre-Print Sources",
  a1_3 = "Blogs",
  a1_4 = "Podcasts",
  a1_5 = "Facebook",
  a1_6 = "Twitter / X",
  a1_7 = "Other Social Media",
  a1_8 = "ED Leadership",
  a1_9 = "Hospital Leadership",
  a1_10 = "ED Colleagues",
  a1_11 = "Personal Network",
  a1_12 = "Federal Health Agencies",
  a1_13 = "Local Health Agencies",
  a1_14 = "Society Communications",
  a1_15 = "Online or Print News"
)

subscale_titles <- c(
  "RC1" = "Professional Networks and Authorities",
  "RC2" = "News and Social Media",
  "RC3" = "Academic Publications",
  "RC4" = "Blogs and Podcasts"
)

# # Get the labels for the specified variables
# variable_labels <- get_variable_labels(survey_results, variable_names)

###### LOAD IN DATA ######

# Load clean data
survey_results <- readRDS(file.path("data", "clean_data.rds"))

# Define the variable groups
a_vars <- get_column_names(survey_results, "a1_1", "a1_15")
c_vars <- get_column_names(survey_results, "c1_1", "c1_15")
all_vars <- c("responseid", a_vars, c_vars)

# Prepare data
df_wide_a <- prepare_data_for_pca(survey_results, c("responseid", a_vars), "a", 1)
df_wide_c <- prepare_data_for_pca(survey_results, c("responseid", c_vars), "c", 2)

# Combine the data frames for 'a' and 'c' subscales
df_wide <- dplyr::bind_rows(df_wide_a, df_wide_c)

# Handle missing values by imputing using the mean
ncp_value <- min(2, nrow(df_wide) - 1)

df_imputed <- imputePCA(df_wide %>% dplyr::select(responseid, "1":"15"), ncp = ncp_value)$completeObs %>%
  as.data.frame() %>%
  mutate(across(everything(), as.numeric)) %>%
  dplyr::left_join(survey_results %>% dplyr::select(responseid, provtype, gender), by = "responseid") %>%
  dplyr::left_join(df_wide %>% dplyr::select(responseid, observation), by = "responseid")

# Apply Varimax rotation
cor_matrix <- cor(df_imputed %>% dplyr::select("1":"15"))
eigenvalues <- eigen(cor_matrix)$values
kaiser_criterion <- sum(eigenvalues > 1)
rotated_result <- principal(df_imputed %>% dplyr::select("1":"15"), nfactors = kaiser_criterion, rotate = "varimax")

# Print the results
rotated_result$loadings

# Extract variance explained by each rotated component (in percentage)
rotated_variance_explained <- rotated_result$Vaccounted["Proportion Var", ] * 100

# Print variance explained by the first four components
cat("Variance explained by the first four rotated components (%):\n")
print(round(rotated_variance_explained[1:4], 2))

# Calculate and print cumulative variance explained by the first four components
cumulative_variance_explained <- sum(rotated_variance_explained[1:4])
cat("Cumulative variance explained by the first four rotated components (%):\n")
print(round(cumulative_variance_explained, 2))


threshold <- 0.4
rotated_significant_loadings <- apply(rotated_result$loadings, 2, function(x) which(abs(x) > threshold))

for (i in seq_along(rotated_significant_loadings)) {
  cat(paste("Items loading onto Dimension", i, " (Rotated):\n"))
  print(names(rotated_significant_loadings[[i]]))
  cat("\n")
}

# Function to compute Cronbach's alpha
compute_cronbach_alpha <- function(data, factors) {
  sapply(1:ncol(factors), function(i) {
    selected_vars <- which(abs(factors[, i]) > 0.3)
    if (length(selected_vars) > 1) {
      psych::alpha(data[, selected_vars])$total$raw_alpha
    } else {
      NA
    }
  })
}

rotated_alpha <- compute_cronbach_alpha(df_imputed %>% dplyr::select("1":"15"), rotated_result$loadings)
rotated_alpha

# Manually extract the scores and loadings for rotated PCA
scores_rotated <- as.data.frame(rotated_result$scores[, 1:4])
loadings_rotated <- as.data.frame(rotated_result$loadings[, 1:4])

# Plotting for Rotated PCA
plots <- plot_loadings_custom(
  rotated_result$loadings,
  title = "Variable Loadings - Varimax Rotated PCA",
  variable_labels = plotting_labels,
  facet_titles = subscale_titles,
  s = 1.4
)

plots$overview
plots$faceted

# Calculate the variance explained by each rotated component
rotated_variance_explained <- rotated_result$Vaccounted["Proportion Var", ] * 100  # Convert to percentage

# Plot scree plot for rotated PCA
scree_plot_rotated <- plot_scree_custom(rotated_variance_explained, title = "Varimax Rotated PCA - Variance Explained")
scree_plot_rotated

# Plotting for Rotated PCA, specifying different components
# components_list <- list(c(1, 2), c(1, 3), c(1, 4), c(2, 3), c(2, 4), c(3, 4))
# biplots <- lapply(components_list, function(components) {
#   plot_biplot_custom(scores_rotated, loadings_rotated, variable_labels = variable_labels, components = components)
# })
# 
# # Print all biplots
# #lapply(biplots, print)
# biplots

####### HEATMAP ########

# Step 1: Prepare the tidy data
loadings_tidy <- prepare_loadings(
  loadings = rotated_result$loadings,
  variable_labels = plotting_labels,
  col_titles = subscale_titles,
  threshold = 0.5
)

# Step 2: Plot the heatmap
heatmap_rotated <- plot_heatmap_custom(loadings_tidy, title = NULL)
heatmap_rotated

heatmap_html <- plot_heatmap_custom(loadings_tidy, title = NULL, r = .2, s = 0.6)
heatmap_html

# ggsave(
#   "plots/heatmap_rotated.png",
#   plot = heatmap_rotated,
#   width = 7, height = 7,
#   units = "in",
#   bg = "white"
# )
