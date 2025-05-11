# rm(list = ls()) # clear environment
{
  library(raster)
  library(sp)
  library(tidyverse)
  library(rgdal)
  library(sf)
  library(stars)
  library(naniar)
  library(terra)
  library(gstat)
  library(colorRamps)
  library(pbapply)
  library(plotly)
}

# Creating color ramp for temperature plot
red_ramp <- colorRampPalette(c("#F5F500", "#F5B800", "#F57A00", "#F53D00", "#F50000"))(100)

# TEMPERATURE DATA IMPORT -----------------------------------------------
# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import average maximum temperature file names
WorldClim_temperature_filenames_2002_2021 <- list.files('Raw Data/Climatological Data/Temperature/WorldClim/wc2.1_cruts4.06_2.5m_tmax_2000-2021/', pattern = '.tif')[25:264]

WorldClim_temperature_raster_extraction <- function(index, plot = NULL){
  WorldClim_temperature_raster <- raster(paste0('Raw Data/Climatological Data/Temperature/WorldClim/wc2.1_cruts4.06_2.5m_tmax_2000-2021/', WorldClim_temperature_filenames_2002_2021[index]))
  raster_name <- str_extract(WorldClim_temperature_filenames_2002_2021[index], "\\d{4}-\\d{2}") # extract date from tif file
  names(WorldClim_temperature_raster) <- paste("AMT", raster_name) # AMT for Average Maximum Temperature
  large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation (This step was done to accelerate processing time!)
  temperature_raster_crop <- crop(WorldClim_temperature_raster, large_extent) # crop raster to extent
  values(temperature_raster_crop) <- ifelse(values(temperature_raster_crop)<1, NA, values(temperature_raster_crop)) # convert huge negative values (e.g, -9999 to NA)
  temperature_raster_proj <- projectRaster(temperature_raster_crop, 
                                             crs = crs(roi_trans), # project raster to EPSG:32734 (WGS 84 / UTM zone 34S)
                                             res = 30, # upsample spatial resolution to 30x30m
                                             method = 'ngb') # nearest neighbour preserves the original values 
  temperature_raster_cropTMNR <- crop(temperature_raster_proj, roi_trans) # crop to study area
  temperature_raster_maskTMNR <- mask(temperature_raster_cropTMNR, roi_trans) # mask to study area
  
  if(plot==T){
    plot(temperature_raster_maskTMNR, 
         main = names(temperature_raster_maskTMNR), 
         col = red_ramp)
  }
  return(temperature_raster_maskTMNR)
}

# Processing Worldclim temperature data extraction
WorldClim_temperature_raster_list <- pblapply(seq_along(WorldClim_temperature_filenames_2002_2021), function(x){
  WorldClim_temperature_raster_extraction(index = x, plot = T)
})

WorldClim_temperature_raster_extraction(240, plot = T)
# # Saving results
# {
#   # Save object
#   save(WorldClim_temperature_raster_list,
#        file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Temperature/WorldClim/WorldClim_temperature_raster_list.Rdata')
# 
# 
#   # Save rasters in one folder on local machine or hard drive
#   Save_WorldClim_temperature_raster <- function(index, path){
# 
#     file_path <- paste0(path, gsub("\\.", " ", names(WorldClim_temperature_raster_list[[index]])))
# 
#     return(writeRaster(WorldClim_temperature_raster_list[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
# 
#   pblapply(seq_along(WorldClim_temperature_raster_list), function(x){
#     Save_WorldClim_temperature_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Temperature/WorldClim/')
# 
#   })
# }


# COMBINE 2022-2023 SENTINEL 3 LST TO WORLDCLIM DATA ----------------------
# Modify the extent of S3_LST for consistency to WorldClim data
S3_LST_2022_2023_temperature_raster_stack <- stack(S3_LST_rasters_list[1:24])
extent(S3_LST_2022_2023_temperature_raster_stack) <- extent(WorldClim_temperature_raster_list[[1]]) # assign appropriate extent for consistency

S3_LST_2022_2023_temperature_raster_list <- unstack(S3_LST_2022_2023_temperature_raster_stack) # unstack rasters
WorldClim_S3LST_temperature_raster_list <- append(WorldClim_temperature_raster_list, S3_LST_2022_2023_temperature_raster_list) # combine rasters

# # Save object
# save(WorldClim_S3LST_temperature_raster_list,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Temperature/Combined temperature data/WorldClim_S3LST_temperature_raster_list.Rdata')


# EDA for temperature data ------------------------------------------------
# Trim data from 2014 to 2023
WorldClim_S3LST_2014_2023_temperature_raster_list <- WorldClim_S3LST_temperature_raster_list[145:264]

# TEMPORAL ANALYSIS
# Calculate the median value of temperature per raster
median_temperature_values <- pbsapply(seq_along(WorldClim_S3LST_2014_2023_temperature_raster_list), function(index){
  values(WorldClim_S3LST_2014_2023_temperature_raster_list[[index]]) |>
    na.omit() |>
    median()
})


# Add the median values to a dataframe
temp_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                          median_temp = median_temperature_values) 


temp_EDA_df$year <- year(temp_EDA_df$date) # extract year from date and create a year column

# Find the maximum median temperature value for each year
max_median_temp_df <- temp_EDA_df %>%
  group_by(year) %>%
  summarise(median_temp = max(median_temp)) %>%
  select(median_temp) %>%
  left_join(temp_EDA_df) %>%
  select(-year) %>%
  rename(max_median_temp = median_temp) %>%
  distinct() %>%  # remove repetitive rows
  arrange(date) %>%
  slice(-c(6,8,10)) # manually remove incorrect maximum


# Find the minimum median temperature value for each year
min_median_temp_df <- temp_EDA_df %>%
  group_by(year) %>%
  summarise(median_temp = min(median_temp)) %>%
  select(median_temp) %>%
  left_join(temp_EDA_df) %>%
  select(-year) %>%
  rename(min_median_temp = median_temp)%>%
  distinct() %>% # remove repetitive rows
  arrange(date) %>%
  slice(-c(1,3,4,6,7,9,10,11,13,14,16,19,10,22))

# Plot median value for temperature
median_temperature_plot <- ggplot(temp_EDA_df, aes(x = date, y = median_temp, color = median_temp)) +
  geom_line(linewidth = .8) +
  geom_point(data = max_median_temp_df, aes(x = date, y = max_median_temp), color = 'deeppink', size = 1) +
  geom_text(data = max_median_temp_df, aes(x = date, y = max_median_temp, label = format(date, '%Y-%m')), 
            vjust = -1, color = "deeppink", size = 2) +  # Label the max points)
  geom_point(data = min_median_temp_df, aes(x = date, y = min_median_temp), color = 'salmon', size = 1) +
  geom_text(data = min_median_temp_df, aes(x = date, y = min_median_temp, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradient(low = "yellow", high = 'red', guide = 'none', name = 'Median Temperature (°C)') +
  ylab('Median AMT (°C)') +
  xlab('Period') +
  theme_light() +
  theme(legend.position = 'bottom',
        legend.title= element_text(size = 9),
        legend.text = element_text(size = 7))

median_temperature_plot
# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/median_temperature_plot.pdf", 
       plot = median_temperature_plot, width = 6.56, height = 3.5)

# SPATIAL ANALYSIS
{
  stats <- median # stats to be calculated from temperature data
  
  # 2014
  temp_stack_2014 <- stack(lapply(1:12, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2014 temperature raster series
  median_temp_raster_2014 <- calc(temp_stack_2014, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2014@file@name <- '2014 Series' # rename raster
  
  # 2015
  temp_stack_2015 <- stack(lapply(13:24, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2015 temperature raster series
  median_temp_raster_2015 <- calc(temp_stack_2015, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2015@file@name <- '2015 Series' # rename raster
  
  # 2016
  temp_stack_2016 <- stack(lapply(25:36, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2016 temperature raster series
  median_temp_raster_2016 <- calc(temp_stack_2016, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2016@file@name <- '2016 Series' # rename raster
  
  # 2017
  temp_stack_2017 <- stack(lapply(37:48, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2017 temperature raster series
  median_temp_raster_2017 <- calc(temp_stack_2017, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2017@file@name <- '2017 Series' # rename raster
  
  # 2018
  temp_stack_2018 <- stack(lapply(49:60, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2018 temperature raster series
  median_temp_raster_2018 <- calc(temp_stack_2018, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2018@file@name <- '2018 Series' # rename raster
  
  # 2019
  temp_stack_2019 <- stack(lapply(61:72, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2019 temperature raster series
  median_temp_raster_2019 <- calc(temp_stack_2019, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2019@file@name <- '2019 Series' # rename raster
  
  # 2020
  temp_stack_2020 <- stack(lapply(73:84, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2020 temperature raster series
  median_temp_raster_2020 <- calc(temp_stack_2020, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2020@file@name <- '2020 Series' # rename raster
  
  # 2021
  temp_stack_2021 <- stack(lapply(85:96, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2021 temperature raster series
  median_temp_raster_2021 <- calc(temp_stack_2021, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2021@file@name <- '2021 Series' # rename raster
  
  # 2022
  temp_stack_2022 <- stack(lapply(97:108, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2022 temperature raster series
  median_temp_raster_2022 <- calc(temp_stack_2022, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2022 <- resample(median_temp_raster_2022, median_temp_raster_2014, method ='ngb') # resample to match dimension of WorldClim data
  median_temp_raster_2022@file@name <- '2022 Series' # rename raster
  
  # 2023
  temp_stack_2023 <- stack(lapply(109:120, function(x) {WorldClim_S3LST_2014_2023_temperature_raster_list[[x]]})) # stack the 2023 temperature raster series
  median_temp_raster_2023 <- calc(temp_stack_2023, fun = stats) # calculate median or mean value of the stack
  median_temp_raster_2023 <- resample(median_temp_raster_2023, median_temp_raster_2014, method ='ngb') # resample to match dimension of WorldClim data
  median_temp_raster_2023@file@name <- '2023 Series' # rename raster
  
}

# stack all the median temperature plot
median_temp_stack_2014_to_2023 <- stack(median_temp_raster_2014,
                                        median_temp_raster_2015,
                                        median_temp_raster_2016,
                                        median_temp_raster_2017,
                                        median_temp_raster_2018,
                                        median_temp_raster_2019,
                                        median_temp_raster_2020,
                                        median_temp_raster_2021,
                                        median_temp_raster_2022,
                                        median_temp_raster_2023)

names(median_temp_stack_2014_to_2023) <- c('Median AMT 2014', 
                                           'Median AMT 2015',
                                           'Median AMT 2016',
                                           'Median AMT 2017',
                                           'Median AMT 2018',
                                           'Median AMT 2019',
                                           'Median AMT 2020',
                                           'Median AMT 2021',
                                           'Median AMT 2022',
                                           'Median AMT 2023') # rename raster stack
# save(median_temp_stack_2014_to_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_temp_stack_2014_to_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_temp_stack_2014_to_2023.Rdata')

# Converting stack into a dataframe
median_temp_stack_df <- as.data.frame(projectRaster(median_temp_stack_2014_to_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), xy  = T) %>%
  melt(id.vars= c('x','y'), na.rm = T) %>%
  as_tibble() %>%
  # Find the range of temp value for each median series of each year
  mutate(variable = as.factor(case_when(variable == 'Median.AMT.2014'~ paste0('2014 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2014),1), ' ºC to ', round(maxValue(median_temp_raster_2014),1), ' ºC'),
                                        variable == 'Median.AMT.2015'~paste0('2015 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2015),1), ' ºC to ', round(maxValue(median_temp_raster_2015),1), ' ºC'),
                                        variable == 'Median.AMT.2016'~paste0('2016 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2016),1), ' ºC to ', round(maxValue(median_temp_raster_2016),1), ' ºC'),
                                        variable == 'Median.AMT.2017'~paste0('2017 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2017),1), ' ºC to ', round(maxValue(median_temp_raster_2017),1), ' ºC'),
                                        variable == 'Median.AMT.2018'~paste0('2018 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2018),1), ' ºC to ', round(maxValue(median_temp_raster_2018),1), ' ºC'),
                                        variable == 'Median.AMT.2019'~paste0('2019 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2019),1), ' ºC to ', round(maxValue(median_temp_raster_2019),1), ' ºC'),
                                        variable == 'Median.AMT.2020'~paste0('2020 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2020),1), ' ºC to ', round(maxValue(median_temp_raster_2020),1), ' ºC'),
                                        variable == 'Median.AMT.2021'~paste0('2021 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2021),1), ' ºC to ', round(maxValue(median_temp_raster_2021),1), ' ºC'),
                                        variable == 'Median.AMT.2022'~paste0('2022 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2022),1), ' ºC to ', round(maxValue(median_temp_raster_2022),1), ' ºC'),
                                        variable == 'Median.AMT.2023'~paste0('2023 Series\n Median AMT Range from ', round(minValue(median_temp_raster_2023),1), ' ºC to ', round(maxValue(median_temp_raster_2023),1), ' ºC'))))

head(median_temp_stack_df)
str(median_temp_stack_df)

median_temp_map <- ggplot()+
  geom_raster(data = median_temp_stack_df, aes(x = x, y = y, fill = value))+
  scale_fill_gradientn(colours = red_ramp, 'Median\n AMT\n (ºC)',
                       breaks = seq(min(median_temp_stack_df$value),max(median_temp_stack_df$value),length.out = 11),
                       labels = round(seq(min(median_temp_stack_df$value),max(median_temp_stack_df$value),length.out = 11),2)) +
  facet_wrap(~ variable, nrow = 4, ncol = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  theme_bw()+
  theme(panel.grid.major= element_blank(),
        strip.text = element_text(size=5),
        legend.position = 'right')+
  guides(fill = guide_colorbar(barwidth = .4, barheight = 30))

# save plot
ggsave('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/median_temperature_map.pdf', 
       plot = median_temp_map, width = 6.56, height = 8)



# # Find the range in full range in the temperature timeframe
# zlim_temp <- range(c(minValue(median_temp_stack_2014_to_2023), 
#                      maxValue(median_temp_stack_2014_to_2023)))
# # Function to extract temperature range and print neatly
# temp_range <- function(data){
#   range <- round(range(c(minValue(data), maxValue(data))),1)
#   return(paste('Median AMT Range from ', range[1], 'ºC to', range[2], 'ºC'))
# }
# 
# # plot the median temperature rasters for each year
# { 
#   
#   # par(mar = c(bottom, left, top, right))
#   # par(mar = c(8.0, 3, 1.3, 0.1)) # customised margin
#   # par(mfrow = c(3,4)) # layout control
#   par(mar = c(4, 3, 1.8, 0.5)) # customised margin
#   par(mfrow = c(4,3)) # layout control
#   plot(projectRaster(median_temp_raster_2014, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2014@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2014), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2015, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2015@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2015), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2016, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2016@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2016), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2017, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2017@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2017), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2018, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2018@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2018), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2019, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2019@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2019), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2020, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2020@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2020), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2021, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2021@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2021), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2022, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_temp_raster_2022@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2022), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_temp_raster_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = red_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_temp, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = T,
#        horizontal = F, # make legend horizontal or vertical
#        legend.shrink = 1, # stretch or compress legend
#        axis.args = list(cex.axis = .6))
#   #mgp = c(3, 0.2, 0)), # adjust legend lable size and position to ticks
#   # legend.args = list(text = "Median \nTotal \nPrecipitation \n(mm)", side = 4, cex = .5)) # add legend title and adjust size
#   title(main=median_temp_raster_2023@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(temp_range(median_temp_raster_2023), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
# }
