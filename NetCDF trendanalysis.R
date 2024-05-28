# Trends and significance of NetCDF files
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

# Function to process NetCDF file
process_netcdf <- function(file_path, shapefile_path, shapefile_layer) {
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
  
  # Add ID and select values occurring more than 10 times
  tx_annual <- tibble::rowid_to_column(tx_annual, "ID")
  tx_annual <- tx_annual %>% group_by(x, y) %>% filter(n() > 9)
  
  # Calculate trends
  tx_trend <- tx_annual %>%
    group_by(x, y) %>%
    summarise(
      slope = sens.slope(annual_tx)$estimates * 10,
      sign = mk.test(annual_tx)$p.value,
      std = sd(annual_tx, na.rm = TRUE),
      mean = mean(annual_tx, na.rm = TRUE)
    )
  print(tx_trend)
  
  # Load shapefiles
  regions <- readOGR(dsn = shapefile_path, layer = shapefile_layer, verbose = FALSE)
  plot(regions)
  
  # Convert shapefile to data frame
  regions_fortified <- tidy(regions, region = "layer")
  
  # Plot regression
  rp <- ggplot() +
    geom_tile(data = tx_trend, aes(x = x, y = y, fill = slope)) +
    scale_fill_gradientn(colors = pals::coolwarm(25), name = "Days", limits = c(-30, 30)) +
    geom_polygon(data = regions_fortified, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
    coord_sf(crs = 3411) +
    xlab("") + ylab("") +
    labs(title = "Decadal trend") +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 9, barheight = 0.5, title.position = "right")) +
    theme(legend.position = "bottom")
  
  # Plot standard deviation
  stdp <- ggplot() +
    geom_tile(data = tx_trend, aes(x = x, y = y, fill = std)) +
    scale_fill_gradientn(colors = pals::viridis(25), name = "", limits = c(0, 50)) +
    geom_polygon(data = regions_fortified, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
    coord_sf(crs = 3411) +
    xlab("") + ylab("") +
    labs(title = "Standard deviation") +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 9, barheight = 0.5, title.position = "right")) +
    theme(legend.position = "bottom")
  
  # Plot mean
  meanp <- ggplot() +
    geom_tile(data = tx_trend, aes(x = x, y = y, fill = mean)) +
    scale_fill_gradientn(colors = pals::parula(25), name = "Days", limits = c(225, 365)) +
    geom_polygon(data = regions_fortified, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
    coord_sf(crs = 3411) +
    xlab("") + ylab("") +
    labs(title = "Mean day of freeze") +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 9, barheight = 0.5, title.position = "right")) +
    theme(legend.position = "bottom")
  
  # Plot significant trends
  sigp <- ggplot() +
    geom_point(data = filter(tx_trend, sign < 0.05), aes(x = x, y = y, color = "Sign. trend \n p-value < 0.05"), size = 0.0001, show.legend = TRUE) +
    geom_polygon(data = regions_fortified, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
    scale_color_manual(values = c("black"), name = "") +
    coord_sf(crs = 3411) +
    xlab("") + ylab("") +
    labs(title = "Pixels with significant trends", caption = "Database: NASA Earth Science") +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 9, barheight = 0.5, title.position = "right")) +
    theme(legend.position = "bottom")
  
  # Combine plots
  figure <- ggarrange(meanp, stdp, rp, sigp, ncol = 4, nrow = 1)
  annotate_figure(figure, fig.lab = "Open water period (1979 - 2021)", fig.lab.pos = c("top.left"), fig.lab.size = 28, fig.lab.face = "bold")
  
  return(figure)
}

# Example usage
file_path <- "path/to/your/freezedate.nc"
shapefile_path <- "path/to/your/shapefiles"
shapefile_layer <- "all_regions_psn"

figure <- process_netcdf(file_path, shapefile_path, shapefile_layer)
# Save the figure if needed
# ggsave("output.png", figure, width = 1150, height = 650, units = "px")