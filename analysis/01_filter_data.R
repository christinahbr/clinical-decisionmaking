library(ggplot2)
library(dplyr)
library(haven)
library(readxl)

source(file.path("code", "utils", "file_paths.R"))

raw_data <- readRDS(DATA_PATH_STATUS)

filtered_data <- raw_data %>%
  filter(status %in% c("partial", "complete"))

saveRDS(filtered_data, file = DATA_RESPONDED_PATH)
