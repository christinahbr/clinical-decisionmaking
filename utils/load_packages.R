# Function to load packages
load_packages <- function(packages, repo = "https://cran.rand.org") {
  for (package in packages) {
    message("Loading package: ", package)
    tryCatch(
      {
        if (!require(package, character.only = TRUE)) {
          message("Package ", package, " not found. Installing now...")
          install.packages(package, repos = repo)
          library(package, character.only = TRUE)
        } 
      },
      error = function(e) {
        message("Error loading package: ", package, "\n", e)
      }
    )
  }
}

# # Consistently used packages list
# packages <- c(
#   "readr", # read CSVs
#   "haven", "dplyr", "magrittr", "ggplot2", "stringr",
#   "purrr", # map_chr
#   "forcats", "ggtext", "kableExtra", "tibble", "networkD3",
#   "knitr", "tableone", "readr", "log4r", "broom", "rmarkdown",
#   "data.table", "viridis", "unikn", "mediation", "lmtest",
#   "future.apply", "patchwork", "ggpubr", "gridExtra", "grid", "ggrepel",
#   "tidyr", "FactoMineR", "factoextra", "missMDA", "paran", "psych",
#   "RColorBrewer", "wesanderson", "reshape2", "labelled", "ICSNP",
#   "lavaan", "DiagrammeR", "semPlot", "car", "gtsummary", "glue",
#   "flextable", "officer"
# )
