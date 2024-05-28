# Annual averages - From NetCDF to CSV
# Thomas Gjerluff Ager
# 11/10/2022

# Load libraries
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)
library(metR)
library(trend)
library(pals) # Color palette
library(cmsafops)
library(rgdal)
library(broom)
library(rgeos)
library(maptools)
library(ggpubr)
library(tuple)

# Function to process NetCDF file and calculate annual averages
process_netcdf_annual <- function(file_path, output_csv) {
  # Extract information from the NetCDF
  variable <- GlanceNetCDF(file = file_path)
  print(variable)
  
  # Read the NetCDF
  tx <- ReadNetCDF(file = file_path, vars = c(var = names(variable$vars[2])), out = "data.frame")
  print(tx)
  
  # Filter out NAs
  tx <- tx %>% filter(!is.na(var)) %>% as_tibble()
  print(tx)
  
  # Create annual mean
  tx_annual <- tx %>%
    group_by(time, x, y) %>%
    summarise(annual_tx = mean(var, na.rm = TRUE)) %>%
    ungroup()
  print(tx_annual)
  
  # Calculate mean of variable for each year
  tx_annual_avg <- tx_annual %>%
    group_by(time) %>%
    summarise(n = n(), mean = mean(annual_tx, na.rm = TRUE))
  print(tx_annual_avg)
  
  # Write to CSV
  write.csv(tx_annual_avg, output_csv)
}

# Example usage
file_path <- "path/to/your/netcdf/file.nc"
output_csv <- "output/nw.csv"

process_netcdf_annual(file_path, output_csv)