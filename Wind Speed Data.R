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
  library(ggspatial)
  library(cowplot)
}

wind_color_ramp <- colorRampPalette(c('steelblue','khaki','red'))(100) # define a color ramp for wind speed

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

windspeed_filenames <- list.files('Raw Data/Climatological Data/Near Surface Wind Speed') # read file names

windspeed_raster_func <- function(file, index, plot = NULL){
  windspeed_raster <- raster(paste0('Raw Data/Climatological Data/Near Surface Wind Speed/', file[index])) # read raster from filenames
  raster_name <- paste0('ANSWS ',format(as.Date(str_extract(file[index], "\\d{8}"), format = '%Y%m%d'), '%Y-%m')) # extract date from tif file
  names(windspeed_raster) <- raster_name # remame raster
  large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
  windspeed_raster_crop <- crop(windspeed_raster, large_extent) # crop raster to extent
  windspeed_raster_proj <- projectRaster(windspeed_raster_crop, 
                                         crs = crs(roi_trans),
                                         res = 30, # downsample spatial resolution to 30x30m
                                         method = 'bilinear')
  # crop and mask raster to study area only
  windspeed_raster_projcropmask_TMNR <- windspeed_raster_proj |>
    crop(roi_trans) |>
    mask(roi_trans)
  
  if(plot==T){
    # plot this for both ngb and biliear to show difference!
    plot(windspeed_raster_projcropmask_TMNR, col = wind_color_ramp, main = raster_name)
    plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, add = T)
  }
  return(windspeed_raster_projcropmask_TMNR)

}
windspeed_raster_func(windspeed_filenames, 2, T)
# Processing windspeed data extraction
windspeed_raster_list <- pblapply(seq_along(windspeed_filenames), function(x){windspeed_raster_func(file = windspeed_filenames, index = x, plot = T)})

# # This is the option where we are not interpolating but using that one value to represent the whole raster.
# # Must make sure that ngb is used in projectRaster above for this to work.
# xx <- calc(windspeed_raster_projcropmask_TMNR, function(x) ifelse(is.na(x),
#                                                            values(windspeed_raster_projcropmask_TMNR) |> 
#                                                              na.omit() |> 
#                                                              unique(),
#                                                             x))
# 
# plot(mask(xx, roi_trans), col = wind_color_ramp, main = raster_name)

# # Saving results
# {
#   # Save object
#   save(windspeed_raster_list,
#        file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Wind Speed/windspeed_raster_list.Rdata')
# 
# 
#   # Save rasters in one folder on local machine or hard drive
#   Save_windspeed_raster <- function(index, path){
# 
#     file_path <- paste0(path, gsub("\\.", " ", names(windspeed_raster_list[[index]])))
# 
#     return(writeRaster(windspeed_raster_list[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
# 
#   pblapply(seq_along(windspeed_raster_list), function(x){
#     Save_windspeed_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Wind Speed/')
# 
#   })
# }

windspeed_filenames_2014_2023 <- windspeed_filenames[145:264]
windspeed_raster_list_nearest_neighbour <- pblapply(seq_along(windspeed_filenames_2014_2023), function(x){windspeed_raster <- raster(paste0('Raw Data/Climatological Data/Near Surface Wind Speed/', windspeed_filenames_2014_2023[x]))
raster_name <- paste0('ANSWS ',format(as.Date(str_extract(windspeed_filenames_2014_2023[x], "\\d{8}"), format = '%Y%m%d'), '%Y-%m')) # extract date from tif file
names(windspeed_raster) <- raster_name # remame raster
large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
windspeed_raster_crop <- crop(windspeed_raster, large_extent) # crop raster to extent
windspeed_raster_proj <- projectRaster(windspeed_raster_crop,
                                       crs = crs(roi_trans),
                                       res = 30, # downsample spatial resolution to 30x30m
                                       method = 'ngb')
# crop and mask raster to study area only
windspeed_raster_projcropmask_TMNR <- windspeed_raster_proj |>
  crop(roi_trans) |>
  mask(roi_trans)

plot(windspeed_raster_projcropmask_TMNR, col = wind_color_ramp, main = str_extract(windspeed_filenames_2014_2023[[x]],"\\d{6}"))
plot(roi_trans,
     col = 'transparent', border = 'black', lwd = 1, add = T)

return(windspeed_raster_projcropmask_TMNR)})


# Show raw wind speed data
raster_name <- paste0('ANSWS ',format(as.Date(str_extract(windspeed_filenames_2014_2023[1], "\\d{8}"), format = '%Y%m%d'), '%Y-%m')) # extract date from tif file
names(windspeed_raster) <- raster_name # remame raster
large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
windspeed_raster_crop <- crop(windspeed_raster, large_extent) # crop raster to extent
# windspeed_raster_proj <- projectRaster(windspeed_raster_crop,
#                                        crs = crs(roi_trans),
#                                        res = 30, # downsample spatial resolution to 30x30m
#                                        method = 'ngb')
# # crop and mask raster to study area only
# windspeed_raster_projcropmask_TMNR <- windspeed_raster_proj |>
#   crop(roi_trans) |>
#   mask(roi_trans)
# par(mar = c(4, 3, 1.8, 0.5)) # customised margin
plot(windspeed_raster_crop|>rast(), col = wind_color_ramp, xlab = 'Longitude', ylab = 'Latitude')
plot(spTransform(roi_trans, '+proj=longlat +datum=WGS84 +no_defs '),
     col = 'transparent', border = 'black', lwd = 1, add = T)

# EDA for ANSWS data ----------------------------------------------

# TEMPORAL ANALYSIS using the windspeed as is
# Calculate the median value of ANSWS per raster
median_ANSWS_values <- pbsapply(seq_along(windspeed_raster_list_nearest_neighbour), function(index){
  values(windspeed_raster_list_nearest_neighbour[[index]]) |>
    na.omit() |>
    median()
})

# Add the median values to a dataframe
ANSWS_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                        median_ANSWS = median_ANSWS_values) 


ANSWS_EDA_df$year <- year(ANSWS_EDA_df$date) # extract year from date and create a year column
ANSWS_EDA_df$month <- month(ANSWS_EDA_df$date) # extract year from date and create a month column

# creating boxplot
# boxplot(ANSWS_EDA_df$median_ANSWS ~ ANSWS_EDA_df$month, outline = T, xlab = 'Month', ylab = 'ANSWS (m/s)')

ANSWS_nn_boxplot <- ggplot(ANSWS_EDA_df, aes(x = factor(month), y = median_ANSWS)) +
  geom_boxplot(fill = 'khaki') +
  xlab('Month') +
  ylab(' ANSWS (m/s)') +
  theme_light() +
  ggtitle('Raw Data')+
  theme(plot.title = element_text(size = 7))
  
  
# Find the maximum median ANSWS value for each year
max_median_ANSWS_df <- ANSWS_EDA_df %>%
  group_by(year) %>%
  summarise(median_ANSWS = max(median_ANSWS)) %>%
  select(median_ANSWS) %>%
  left_join(ANSWS_EDA_df) %>%
  select(-year) %>%
  rename(max_median_ANSWS = median_ANSWS)

# Find the minimum median ANSWS value for each year
min_median_ANSWS_df <- ANSWS_EDA_df %>%
  group_by(year) %>%
  summarise(median_ANSWS = min(median_ANSWS)) %>%
  select(median_ANSWS) %>%
  left_join(ANSWS_EDA_df) %>%
  select(-year) %>%
  rename(min_median_ANSWS = median_ANSWS)

# Plot median value for ANSWS
median_ANSWS_plot_nn <- ggplot(ANSWS_EDA_df, aes(x = date, y = median_ANSWS, color = median_ANSWS)) +
  geom_line(linewidth = .8) +
  geom_point(data = max_median_ANSWS_df, aes(x = date, y = max_median_ANSWS), color = 'deeppink', size = 1) +
  geom_text(data = max_median_ANSWS_df, aes(x = date, y = max_median_ANSWS, label = format(date, '%Y-%m')), 
            vjust = -1, color = "deeppink", size = 2) +  # Label the max points)
  geom_point(data = min_median_ANSWS_df, aes(x = date, y = min_median_ANSWS), color = 'salmon', size = 1) +
  geom_text(data = min_median_ANSWS_df, aes(x = date, y = min_median_ANSWS, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradientn(colours = c('steelblue','khaki','red'), guide = 'none', name = 'Average NSWS (m/s)') +
  ylab(' ANSWS (m/s)') +
  xlab('Period') +
  theme_light() +
  ggtitle('Raw Data') +
  theme(legend.position = 'bottom',
        legend.title= element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 7))

median_ANSWS_plot_nn

# # Save above plot
# ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/median_ANSWS_plot.pdf", 
#        plot = median_ANSWS_plot, width = 6.56, height = 3.5)


####################################################################################
windspeed_raster_list_2014_2023_bilinear <- windspeed_raster_list[145:264]
# TEMPORAL ANALYSIS using the windspeed with bilinear interpolation
# Calculate the median value of ANSWS per raster
median_ANSWS_values <- pbsapply(seq_along(windspeed_raster_list_2014_2023_bilinear), function(index){
  values(windspeed_raster_list_2014_2023_bilinear[[index]]) |>
    na.omit() |>
    median()
})

# Add the median values to a dataframe
ANSWS_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                           median_ANSWS = median_ANSWS_values) 


ANSWS_EDA_df$year <- year(ANSWS_EDA_df$date) # extract year from date and create a year column
ANSWS_EDA_df$month <- month(ANSWS_EDA_df$date) # extract year from date and create a month column

# creating boxplot
# boxplot(ANSWS_EDA_df$median_ANSWS ~ ANSWS_EDA_df$month, outline = T, xlab = 'Month', ylab = 'ANSWS (m/s)')
ANSWS_bilnr_boxplot <- ggplot(ANSWS_EDA_df, aes(x = factor(month), y = median_ANSWS)) +
  geom_boxplot(fill = 'khaki') +
  xlab('Month') +
  ylab('Median  ANSWS (m/s)') +
  theme_light() +
  ggtitle('Bilinear Interpolation Applied')+
  theme(plot.title = element_text(size = 7))

# Find the maximum median ANSWS value for each year
max_median_ANSWS_df <- ANSWS_EDA_df %>%
  group_by(year) %>%
  summarise(median_ANSWS = max(median_ANSWS)) %>%
  select(median_ANSWS) %>%
  left_join(ANSWS_EDA_df) %>%
  select(-year) %>%
  rename(max_median_ANSWS = median_ANSWS)

# Find the minimum median ANSWS value for each year
min_median_ANSWS_df <- ANSWS_EDA_df %>%
  group_by(year) %>%
  summarise(median_ANSWS = min(median_ANSWS)) %>%
  select(median_ANSWS) %>%
  left_join(ANSWS_EDA_df) %>%
  select(-year) %>%
  rename(min_median_ANSWS = median_ANSWS)

# Plot median value for ANSWS
median_ANSWS_plot_bilnr <- ggplot(ANSWS_EDA_df, aes(x = date, y = median_ANSWS, color = median_ANSWS)) +
  geom_line(linewidth = .8) +
  geom_point(data = max_median_ANSWS_df, aes(x = date, y = max_median_ANSWS), color = 'deeppink', size = 1) +
  geom_text(data = max_median_ANSWS_df, aes(x = date, y = max_median_ANSWS, label = format(date, '%Y-%m')), 
            vjust = -1, color = "deeppink", size = 2) +  # Label the max points)
  geom_point(data = min_median_ANSWS_df, aes(x = date, y = min_median_ANSWS), color = 'salmon', size = 1) +
  geom_text(data = min_median_ANSWS_df, aes(x = date, y = min_median_ANSWS, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradientn(colours = c('steelblue','khaki','red'), guide = 'none', name = 'Median Average NSWS (m/s)') +
  ylab('Median  ANSWS (m/s)') +
  xlab('Period') +
  theme_light() +
  ggtitle('Bilinear Interpolation Applied') +
  theme(legend.position = 'bottom',
        legend.title= element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 7))

median_ANSWS_plot_bilnr

Windspeed_plots <- cowplot::plot_grid(ANSWS_nn_boxplot,median_ANSWS_plot_nn,
                                      ANSWS_bilnr_boxplot, median_ANSWS_plot_bilnr,
                                      nrow = 2, ncol = 2)

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/Windspeed_plots.pdf",
       plot = Windspeed_plots, width = 6.56, height = 7)


ANSWS_dates <- seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month")

# SPATIAL ANALYSIS
{
  stats <- median # stats to be calculated from ANSWS data
  
  # 2014
  ANSWS_stack_2014 <- stack(lapply(which(year(ANSWS_dates)=='2014'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2014 ANSWS raster series
  median_ANSWS_raster_2014 <- calc(ANSWS_stack_2014, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2014@file@name <- '2014 Series' # rename raster
  
  # 2015
  ANSWS_stack_2015 <- stack(lapply(which(year(ANSWS_dates)=='2015'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2015 ANSWS raster series
  median_ANSWS_raster_2015 <- calc(ANSWS_stack_2015, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2015@file@name <- '2015 Series' # rename raster
  
  # 2016
  ANSWS_stack_2016 <- stack(lapply(which(year(ANSWS_dates)=='2016'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2016 ANSWS raster series
  median_ANSWS_raster_2016 <- calc(ANSWS_stack_2016, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2016@file@name <- '2016 Series' # rename raster
  
  # 2017
  ANSWS_stack_2017 <- stack(lapply(which(year(ANSWS_dates)=='2017'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2017 ANSWS raster series
  median_ANSWS_raster_2017 <- calc(ANSWS_stack_2017, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2017@file@name <- '2017 Series' # rename raster
  
  # 2018
  ANSWS_stack_2018 <- stack(lapply(which(year(ANSWS_dates)=='2018'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2018 ANSWS raster series
  median_ANSWS_raster_2018 <- calc(ANSWS_stack_2018, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2018@file@name <- '2018 Series' # rename raster
  
  # 2019
  ANSWS_stack_2019 <- stack(lapply(which(year(ANSWS_dates)=='2019'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2019 ANSWS raster series
  median_ANSWS_raster_2019 <- calc(ANSWS_stack_2019, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2019@file@name <- '2019 Series' # rename raster
  
  # 2020
  ANSWS_stack_2020 <- stack(lapply(which(year(ANSWS_dates)=='2020'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2020 ANSWS raster series
  median_ANSWS_raster_2020 <- calc(ANSWS_stack_2020, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2020@file@name <- '2020 Series' # rename raster
  
  # 2021
  ANSWS_stack_2021 <- stack(lapply(which(year(ANSWS_dates)=='2021'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2021 ANSWS raster series
  median_ANSWS_raster_2021 <- calc(ANSWS_stack_2021, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2021@file@name <- '2021 Series' # rename raster
  
  # 2022
  ANSWS_stack_2022 <- stack(lapply(which(year(ANSWS_dates)=='2022'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2022 ANSWS raster series
  median_ANSWS_raster_2022 <- calc(ANSWS_stack_2022, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2022@file@name <- '2022 Series' # rename raster
  
  # 2023
  ANSWS_stack_2023 <- stack(lapply(which(year(ANSWS_dates)=='2023'), function(x) {windspeed_raster_list_2014_2023_bilinear[[x]]})) # stack the 2023 ANSWS raster series
  median_ANSWS_raster_2023 <- calc(ANSWS_stack_2023, fun = stats) # calculate median or mean value of the stack
  median_ANSWS_raster_2023@file@name <- '2023 Series' # rename raster
  
}

# stack all the median ANSWS plot
median_ANSWS_stack_2014_to_2023 <- stack(median_ANSWS_raster_2014,
                                        median_ANSWS_raster_2015,
                                        median_ANSWS_raster_2016,
                                        median_ANSWS_raster_2017,
                                        median_ANSWS_raster_2018,
                                        median_ANSWS_raster_2019,
                                        median_ANSWS_raster_2020,
                                        median_ANSWS_raster_2021,
                                        median_ANSWS_raster_2022,
                                        median_ANSWS_raster_2023)

names(median_ANSWS_stack_2014_to_2023) <- c('Median ANSWS 2014', 
                                           'Median ANSWS 2015',
                                           'Median ANSWS 2016',
                                           'Median ANSWS 2017',
                                           'Median ANSWS 2018',
                                           'Median ANSWS 2019',
                                           'Median ANSWS 2020',
                                           'Median ANSWS 2021',
                                           'Median ANSWS 2022',
                                           'Median ANSWS 2023') # rename raster stack
# save(median_ANSWS_stack_2014_to_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_ANSWS_stack_2014_to_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_ANSWS_stack_2014_to_2023.Rdata')

# Converting stack into a dataframe
median_ANSWS_stack_df <- as.data.frame(projectRaster(median_ANSWS_stack_2014_to_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), xy  = T) %>%
  melt(id.vars= c('x','y'), na.rm = T) %>%
  as_tibble() %>%
  # Find the range of ANSWS value for each median series of each year
  mutate(variable = as.factor(case_when(variable == 'Median.ANSWS.2014'~ paste0('2014 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2014),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2014),1), ' m/s'),
                                        variable == 'Median.ANSWS.2015'~paste0('2015 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2015),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2015),1), ' m/s'),
                                        variable == 'Median.ANSWS.2016'~paste0('2016 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2016),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2016),1), ' m/s'),
                                        variable == 'Median.ANSWS.2017'~paste0('2017 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2017),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2017),1), ' m/s'),
                                        variable == 'Median.ANSWS.2018'~paste0('2018 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2018),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2018),1), ' m/s'),
                                        variable == 'Median.ANSWS.2019'~paste0('2019 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2019),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2019),1), ' m/s'),
                                        variable == 'Median.ANSWS.2020'~paste0('2020 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2020),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2020),1), ' m/s'),
                                        variable == 'Median.ANSWS.2021'~paste0('2021 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2021),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2021),1), ' m/s'),
                                        variable == 'Median.ANSWS.2022'~paste0('2022 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2022),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2022),1), ' m/s'),
                                        variable == 'Median.ANSWS.2023'~paste0('2023 Series\n Median ANSWS Range from ', round(minValue(median_ANSWS_raster_2023),1), ' m/s to ', round(maxValue(median_ANSWS_raster_2023),1), ' m/s'))))

head(median_ANSWS_stack_df)
str(median_ANSWS_stack_df)

median_ANSWS_map <- ggplot()+
  geom_raster(data = median_ANSWS_stack_df, aes(x = x, y = y, fill = value))+
  scale_fill_gradientn(colours = wind_color_ramp, 'Median\n ANSWS (m/s)',
                       breaks = seq(min(median_ANSWS_stack_df$value),max(median_ANSWS_stack_df$value),length.out = 11),
                       labels = round(seq(min(median_ANSWS_stack_df$value),max(median_ANSWS_stack_df$value),length.out = 11),2)) +
  facet_wrap(~ variable, nrow = 4, ncol = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  theme_bw()+
  theme(panel.grid.major= element_blank(),
        strip.text = element_text(size=5),
        legend.position = 'right')+
  guides(fill = guide_colorbar(barwidth = .4, barheight = 30))

# save plot
ggsave('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/median_ANSWS_map.pdf', 
       plot = median_ANSWS_map, width = 6.56, height = 8)



# # Find the range in full range in the ANSWS timeframe
# zlim_ANSWS <- range(c(minValue(median_ANSWS_stack_2014_to_2023), 
#                      maxValue(median_ANSWS_stack_2014_to_2023)))
# # Function to extract ANSWS range and print neatly
# ANSWS_range <- function(data){
#   range <- round(range(c(minValue(data), maxValue(data))),1)
#   return(paste('Median  ANSWS Range from ', range[1], 'm/s', 'to', range[2], 'm/s'))
# }
# 
# 
# # plot the median ANSWS rasters for each year
# { 
#   
#   # par(mar = c(bottom, left, top, right))
#   # par(mar = c(8.0, 3, 1.3, 0.1)) # customised margin
#   # par(mfrow = c(3,4)) # layout control
#   par(mar = c(4, 3, 1.8, 0.5)) # customised margin
#   par(mfrow = c(4,3)) # layout control
#   plot(projectRaster(median_ANSWS_raster_2014, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2014@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2014), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2015, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2015@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2015), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2016, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2016@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2016), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2017, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2017@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2017), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2018, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2018@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2018), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2019, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2019@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2019), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2020, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2020@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2020), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2021, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2021@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2021), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2022, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_ANSWS_raster_2022@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2022), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_ANSWS_raster_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = wind_color_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_ANSWS, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = T,
#        horizontal = F, # make legend horizontal or vertical
#        legend.shrink = 1, # stretch or compress legend
#        axis.args = list(cex.axis = .6))
#   #mgp = c(3, 0.2, 0)), # adjust legend lable size and position to ticks
#   # legend.args = list(text = "Median \nANSWS", side = 4, cex = .5)) # add legend title and adjust size
#   title(main=median_ANSWS_raster_2023@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(ANSWS_range(median_ANSWS_raster_2023), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
# }
# 
# 
