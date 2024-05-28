# Linear trends of environmental drivers
# Thomas Gjerluff Ager
# 28/10/2022

# Load libraries
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)
library(extrafont)
library(ggpmisc)
library(metR)
library(trend)
library(pals) # Color palette
library(cmsafops)
library(rgdal)
library(broom)
library(rgeos)
library(maptools)
library(ggpubr)

# Font setup
setup_fonts <- function() {
  font_import()
  loadfonts(device = "win")
  windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
}

# Function to load and combine CSV files from a directory
load_csv_files <- function(directory_path, pattern = "*.csv") {
  setwd(directory_path)
  temp <- list.files(pattern = pattern)
  combined_data <- do.call(rbind, lapply(temp, read.csv))
  return(combined_data)
}

# Function to preprocess data
preprocess_data <- function(data) {
  data$time <- lubridate::ymd(data$year, truncated = 2L)
  data$region <- as.factor(data$region)
  data$time <- as.integer(format(data$time, format = "%Y"))
  return(data)
}

# Function to filter regions
filter_regions <- function(data, region_names) {
  filtered_data <- data %>% filter(region %in% region_names)
  return(filtered_data)
}

# Function to calculate trends
calculate_trends <- function(data) {
  slope <- sens.slope(data$mean)$estimates * 10
  p_value <- mk.test(data$mean)$p.value
  mean_value <- mean(data$mean)
  return(list(slope = slope, p_value = p_value, mean_value = mean_value))
}

# Function to process all regions
process_all_regions <- function(data) {
  regions <- unique(data$region)
  results <- list()
  for (region in regions) {
    region_data <- filter_regions(data, region)
    trends <- calculate_trends(region_data)
    results[[region]] <- trends
  }
  return(results)
}

# Example usage
setup_fonts()
directory_path <- "path/to/your/directory"
data <- load_csv_files(directory_path)
data <- preprocess_data(data)

results <- process_all_regions(data)

# Print results for each region
for (region in names(results)) {
  cat("Region:", region, "\n")
  cat("Slope:", results[[region]]$slope, "\n")
  cat("P-value:", results[[region]]$p_value, "\n")
  cat("Mean value:", results[[region]]$mean_value, "\n")
  cat("\n")
}