#Trends and significance of netcdf files
#Thomas Gjerluff Ager
#11/10/2022

#libs
library(ncdf4)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(lubridate)
library(metR)
library(trend)
library(pals) # color palette
library(cmsafops)
library(rgdal)
library(broom)
library(rgeos)
library(maptools)
library(ggpubr)
library(tuple)
library(data.table)

#set wd
setwd("set path")

file <- "merged.nc"

variable <- GlanceNetCDF(file = file)
variable


#### Create function that runs the sens.slope test over multiple columns ####
sen_slope_test <- function(data, columns) {
  results <- data.frame()
  
  for (col in columns) {
    slope <- sens.slope(data[[col]])
    sign <- mk.test(data[[col]])
    results <- rbind(results, data.frame(column = col, estimate = slope$estimate * 10, sign = sign$p.value, con95 = slope$conf.int * 10))
  }
  
  return(results)
}

#Create column vector for function input
col = c('NW','W','SW','NE','E','SE')



#### liquid ####

# Reading the NetCDF

df <- ReadNetCDF(file = file,
                 vars = c(MAR_ice = names(variable$vars[1]), 
                          RACMO_ice = (names(variable$vars[2])), 
                          MAR_land = (names(variable$vars[3])),
                          RACMO_land = (names(variable$vars[4]))
                 ),# extraction of variables.
                 out = "data.frame") # output object
df


#sum to yearly basis

df_annual <- df %>%
  mutate(time = as_date(time)) %>% # convert the dates into lubridate format
  mutate(year = year(time)) %>% # create the "year" column to perform the annual aggregate
  group_by(year, ROI) %>% # we group by year and ROI
  summarise(across(c(MAR_ice, RACMO_ice, MAR_land, RACMO_land), sum)) %>% #Should it be sum or mean? It is m3 per sec, and i think it would make the most sense to take the mean and convert it to Gt yr-1
  ungroup()
df_annual

# create df for land, ice and total full period

liquid_1958_2020 <- df_annual[df_annual$year >= 1958 & df_annual$year < 2021, ] 
liquid_1958_2020$land_avg <- (liquid_1958_2020$MAR_land + liquid_1958_2020$RACMO_land)/2
liquid_1958_2020$ice_avg <- (liquid_1958_2020$MAR_ice + liquid_1958_2020$RACMO_ice)/2
liquid_1958_2020$liquid_total_avg <- (liquid_1958_2020$land_avg + liquid_1958_2020$ice_avg) 
liquid_1958_2020 <- subset(liquid_1958_2020, ROI != "")


liquid_dfs <- list()

for (i in 7:9) {
  cols <- c(1, 2, i)
  new_df <- liquid_1958_2020[, cols]
  liquid_dfs[[paste0("df_", names(liquid_1958_2020)[i])]] <- new_df
}

df_land_avg <- spread(liquid_dfs[[1]], key = ROI, value = land_avg)
#df_land_avg_gt <- df_land_avg[, 2:7] * 31.536 #Assuming 1.000 kg / m3 for water
#df_land_avg_gt['year'] <- df_land_avg['year']

df_ice_avg <- spread(liquid_dfs[[2]], key = ROI, value = ice_avg)
#df_ice_avg_gt <- df_ice_avg[, 2:7] * 31.536 #Assuming 1.000 kg / m3 for water
#df_ice_avg_gt['year'] <- df_ice_avg['year']

df_liquid_avg <- spread(liquid_dfs[[3]], key = ROI, value = liquid_total_avg)
#df_liquid_avg_gt <- df_liquid_avg[, 2:7] * 31.536 #Assuming 1.000 kg / m3 for water
#df_liquid_avg_gt['year'] <- df_liquid_avg['year']


#### Solid ####

df_solid <- ReadNetCDF(file = file,
                       vars = c(solid = (names(variable$vars[5])),
                                solid_error = (names(variable$vars[6]))
                       ),# extraction of variables.
                       out = "data.frame") # output object
df_solid


#sum to yearly basis

df_solid_annual <- df_solid %>%
  mutate(time = as_date(time)) %>% # convert the dates into lubridate format
  mutate(year = year(time)) %>% # create the "year" column to perform the annual aggregate
  group_by(year, ROI) %>% # we group by year and ROI
  summarise(across(c(solid, solid_error), sum)) %>%
  ungroup()
df_solid_annual

# select 1987 - 2022, split into solid and solid error, and remove NaN.
solid_1987_2022 <- df_solid_annual[df_solid_annual$year >= 1987 & df_solid_annual$year <= 2022, ] 
solid_1987_2022 <- solid_1987_2022[,c(1,2,3)]
solid_1987_2022 <- subset(solid_1987_2022, ROI != "")

solid_error_1987_2022 <- df_solid_annual[df_solid_annual$year >= 1987 & df_solid_annual$year <= 2022, ] 
solid_error_1987_2022 <- solid_error_1987_2022[,c(1,2,4)]
solid_error_1987_2022 <- subset(solid_error_1987_2022, ROI != "")


df_solid_wide <- spread(solid_1987_2022, key = ROI, value = solid)

df_solid_error <- spread(solid_error_1987_2022, key = ROI, value = solid_error)


#### Sens slope ####
#liquid is in Gt!!
liq1950 <- as.data.frame(sen_slope_test(df_liquid_avg,col))
#liq1950 <- as.data.frame(sen_slope_test(df_liquid_avg_gt,col))
land1950 <- as.data.frame(sen_slope_test(df_land_avg,col))
#land1950 <- as.data.frame(sen_slope_test(df_land_avg_gt,col))
ice1950 <- as.data.frame(sen_slope_test(df_ice_avg,col))
#ice1950 <- as.data.frame(sen_slope_test(df_ice_avg_gt,col))
solid <- as.data.frame(sen_slope_test(df_solid_wide,col))

df_liq1979 <- df_liquid_avg[df_liquid_avg$year >= 1979,]
#df_liq1979 <- df_liquid_avg_gt[df_liquid_avg_gt$year >= 1979,]
liq1979 <- as.data.frame(sen_slope_test(df_liq1979,col))

df_ice1979 <- df_ice_avg[df_ice_avg$year >= 1979,]
#df_ice1979 <- df_ice_avg_gt[df_ice_avg_gt$year >= 1979,]
ice1979 <- as.data.frame(sen_slope_test(df_ice1979,col))

df_land1979 <- df_land_avg[df_land_avg$year >= 1979,]
#df_land1979 <- df_land_avg_gt[df_land_avg_gt$year >= 1979,]
land1979 <- as.data.frame(sen_slope_test(df_land1979,col))

df_liq1951_1978 <- df_liquid_avg[df_liquid_avg$year >= 1951 & df_liquid_avg$year <= 1978,]
liq1951_1978 <- as.data.frame(sen_slope_test(df_liq1951_1978, col))

df_ice1951_1978 <- df_ice_avg[df_ice_avg$year >= 1951 & df_ice_avg$year <= 1978,]
ice1951_1978 <- as.data.frame(sen_slope_test(df_ice1951_1978, col))

df_land1951_1978 <- df_land_avg[df_land_avg$year >= 1951 & df_land_avg$year <= 1978,]
land1951_1978 <- as.data.frame(sen_slope_test(df_land1951_1978, col))



# Add prefix to column names for each data frame
names(liq1950)[-1] <- paste0("liq_1950", names(liq1950)[-1])
names(land1950)[-1] <- paste0("land_1950", names(land1950)[-1])
names(ice1950)[-1] <- paste0("ice_1950", names(ice1950)[-1])
names(liq1979)[-1] <- paste0("liq_1979", names(liq1979)[-1])
names(land1979)[-1] <- paste0("land_1979", names(land1979)[-1])
names(ice1979)[-1] <- paste0("ice_1979", names(ice1979)[-1])
names(liq1951_1978)[-1] <- paste0("liq_1951_1978", names(liq1951_1978)[-1])
names(land1951_1978)[-1] <- paste0("land_1951_1978", names(land1951_1978)[-1])
names(ice1951_1978)[-1] <- paste0("ice_1951_1978", names(ice1951_1978)[-1])
names(solid)[-1] <- paste0("solid_", names(solid)[-1])


# Merge stats
merged_stat <- Reduce(function(x, y) merge(x, y, by = "column"), list(liq1950, land1950, ice1950, liq1979, land1979, ice1979, solid,liq1951_1978, land1951_1978, ice1951_1978))

print(merged_stat)
#### plot ####

#plot data
  #add solid data to liquid
ggplot() + 
  geom_line(data = liquid_1958_2020, mapping = aes(year,liquid_total_avg, color = "Liquid")) +
  geom_line(data = solid_1987_2022, mapping = aes(year,solid, color = "Solid")) +
  scale_color_manual(name = "Discharge source:",
                     values = c("Liquid" = "black", "Solid" = "red"), 
                     labels = c("Liquid", "Solid")) +
  facet_wrap(~factor(ROI, levels=c('NW','NE','W','E','SW','SE')), ncol = 2) +
  labs(x = 'Year', y = 'Discharge (Gt / yr)') +
  theme(axis.line = element_line(size = 0.6),
        axis.title=element_text(size=14, family = "Times New Roman"),
        axis.text.x = element_text(size=12, angle=0, color = "black", family = "Times New Roman"),
        axis.text.y = element_text(size=12, angle=0, color = "black", family = "Times New Roman"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(family = "Times New Roman", face = "bold", size = 20, hjust = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.text=element_text(size=12),
        legend.position = "right", # move the legend inside the plot area
        legend.box = "horizontal", # arrange the legend items horizontally
        plot.margin = unit(c(1, 1, 2, 1), "cm"))
