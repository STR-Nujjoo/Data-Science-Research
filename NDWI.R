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
}

# Colour ramp for NDWI
NDWI_colour_ramp <- colorRampPalette(c("brown", "yellow", "green"))(100)

# Create a function to compute 
NDWI_function <- function(data, index, plot = NULL){
  data_name <- data[[index]]@file@name # extract name from raster
  data <- clamp(data[[index]], 0, 1) # clamp value from 0 to 1- THIS STEP IS VERY IMPORTANT FOR THE VI CALC TO BE CORRECT!!!
  NIR <- data[[4]] # select NIR band
  G <- data[[2]] # select G band
  
  NDWI <- (G-NIR) /(G+NIR) # NDWI computation
  NDWI_reproj <- projectRaster(NDWI, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb') # projecting the raster for the sake of axis labels to be shown as lon-lat
  NDWI@file@name <- paste('NDWI:', as.Date(data_name, format = "%Y%m%d")) # rename raster
  
  if(plot == T){
    # Visualisation of NDWI rasters
    plot(NDWI_reproj,
         col = NDWI_colour_ramp,
         main = NDWI@file@name)
  }
  
  return(NDWI)
} 

NDWI_function(trimmed_df_aerial_imagery_after_interpolation,1,T)
# Calculating NDWI for all aerial imagery BEFORE INTERPOLATION
NDWI_rasters_before_interpolation <- pblapply(seq_along(all_aerial_imagery_before_interpolation),
                                              function(x){NDWI_function(data = all_aerial_imagery_before_interpolation,
                                                                        index = x,
                                                                        plot = T)})

# Calculating NDWI for all aerial imagery AFTER INTERPOLATION
NDWI_rasters_after_interpolation <- pblapply(seq_along(trimmed_df_aerial_imagery_after_interpolation),
                                             function(x){NDWI_function(data = trimmed_df_aerial_imagery_after_interpolation,
                                                                       index = x,
                                                                       plot = T)})


# Save object
save(NDWI_rasters_before_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2002-2023 (without interpolation)/NDWI_rasters_before_interpolation.Rdata')
save(NDWI_rasters_after_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2014-2023 (with interpolation)/NDWI_rasters_after_interpolation.Rdata')

# Save rasters in one folder on local machine or hard drive
{
  Save_raster <- function(data, index, path){
    
    file_path <- paste0(path, data[[index]]@file@name)
    
    return(writeRaster(data[[index]],
                       filename = file_path, format = "GTiff", overwrite = TRUE))
  }
  
  # Bulk Save!!!!
  pblapply(seq_along(NDWI_rasters_before_interpolation),
           function(x) {Save_raster(data = NDWI_rasters_before_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2002-2023 (without interpolation)/')})
  
  # Bulk Save!!!!
  pblapply(seq_along(NDWI_rasters_after_interpolation),
           function(x) {Save_raster(data = NDWI_rasters_after_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2014-2023 (with interpolation)/')})
  
}

# EDA for NDWI data -------------------------------------------------------
# TEMPORAL ANALYSIS
# Calculate the median value of NDWI per raster

median_NDWI_values <- pbsapply(seq_along(NDWI_rasters_after_interpolation), function(index){
  values(NDWI_rasters_after_interpolation[[index]]) |>
    na.omit() |>
    median()
})

# Extracting dates from NDWI rasters
NDWI_dates <- pbsapply(seq_along(NDWI_rasters_after_interpolation), function(index){sub("NDWI: ", "", NDWI_rasters_after_interpolation[[index]]@file@name)}) %>% 
  as.Date("%Y-%m-%d")

# Add the median values to a dataframe
NDWI_EDA_df <- data.frame(date = NDWI_dates,
                          median_NDWI = median_NDWI_values) 


NDWI_EDA_df$year <- year(NDWI_EDA_df$date) # extract year from date and create a year column

# Find the maximum median NDWI value for each year
max_median_NDWI_df <- NDWI_EDA_df %>%
  group_by(year) %>%
  summarise(median_NDWI = max(median_NDWI)) %>%
  select(median_NDWI) %>%
  left_join(NDWI_EDA_df) %>%
  select(-year) %>%
  rename(max_median_NDWI = median_NDWI)

# Find the minimum median NDWI value for each year
min_median_NDWI_df <- NDWI_EDA_df %>%
  group_by(year) %>%
  summarise(median_NDWI = min(median_NDWI)) %>%
  select(median_NDWI) %>%
  left_join(NDWI_EDA_df) %>%
  select(-year) %>%
  rename(min_median_NDWI = median_NDWI)

# Plot median value for NDWI
median_NDWI_plot <- ggplot(NDWI_EDA_df, aes(x = date, y = median_NDWI, color = median_NDWI)) +
  geom_line(linewidth = .8) +
  geom_point(data = max_median_NDWI_df, aes(x = date, y = max_median_NDWI), color = 'deeppink', size = 1) +
  geom_text(data = max_median_NDWI_df, aes(x = date, y = max_median_NDWI, label = format(date, '%Y-%m')), 
            vjust = -1, color = "deeppink", size = 2) +  # Label the max points)
  geom_point(data = min_median_NDWI_df, aes(x = date, y = min_median_NDWI), color = 'salmon', size = 1) +
  geom_text(data = min_median_NDWI_df, aes(x = date, y = min_median_NDWI, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradientn(colours = c("brown", "yellow", "green"), guide = 'none', name = 'Median NDWI') +
  ylab('Median NDWI') +
  xlab('Period') +
  theme_light() +
  theme(legend.position = 'bottom',
        legend.title= element_text(size = 9),
        legend.text = element_text(size = 7))

median_NDWI_plot

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/median_NDWI_plot.pdf", 
       plot = median_NDWI_plot, width = 6.56, height = 3.5)

# SPATIAL ANALYSIS
{
  stats <- median # stats to be calculated from NDWI data
  
  # 2014
  NDWI_stack_2014 <- stack(lapply(which(year(NDWI_dates)=='2014'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2014 NDWI raster series
  median_NDWI_raster_2014 <- calc(NDWI_stack_2014, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2014@file@name <- '2014 Series' # rename raster
  
  # 2015
  NDWI_stack_2015 <- stack(lapply(which(year(NDWI_dates)=='2015'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2015 NDWI raster series
  median_NDWI_raster_2015 <- calc(NDWI_stack_2015, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2015@file@name <- '2015 Series' # rename raster
  
  # 2016
  NDWI_stack_2016 <- stack(lapply(which(year(NDWI_dates)=='2016'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2016 NDWI raster series
  median_NDWI_raster_2016 <- calc(NDWI_stack_2016, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2016@file@name <- '2016 Series' # rename raster
  
  # 2017
  NDWI_stack_2017 <- stack(lapply(which(year(NDWI_dates)=='2017'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2017 NDWI raster series
  median_NDWI_raster_2017 <- calc(NDWI_stack_2017, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2017@file@name <- '2017 Series' # rename raster
  
  # 2018
  NDWI_stack_2018 <- stack(lapply(which(year(NDWI_dates)=='2018'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2018 NDWI raster series
  median_NDWI_raster_2018 <- calc(NDWI_stack_2018, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2018@file@name <- '2018 Series' # rename raster
  
  # 2019
  NDWI_stack_2019 <- stack(lapply(which(year(NDWI_dates)=='2019'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2019 NDWI raster series
  median_NDWI_raster_2019 <- calc(NDWI_stack_2019, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2019@file@name <- '2019 Series' # rename raster
  
  # 2020
  NDWI_stack_2020 <- stack(lapply(which(year(NDWI_dates)=='2020'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2020 NDWI raster series
  median_NDWI_raster_2020 <- calc(NDWI_stack_2020, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2020@file@name <- '2020 Series' # rename raster
  
  # 2021
  NDWI_stack_2021 <- stack(lapply(which(year(NDWI_dates)=='2021'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2021 NDWI raster series
  median_NDWI_raster_2021 <- calc(NDWI_stack_2021, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2021@file@name <- '2021 Series' # rename raster
  
  # 2022
  NDWI_stack_2022 <- stack(lapply(which(year(NDWI_dates)=='2022'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2022 NDWI raster series
  median_NDWI_raster_2022 <- calc(NDWI_stack_2022, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2022@file@name <- '2022 Series' # rename raster
  
  # 2023
  NDWI_stack_2023 <- stack(lapply(which(year(NDWI_dates)=='2023'), function(x) {NDWI_rasters_after_interpolation[[x]]})) # stack the 2023 NDWI raster series
  median_NDWI_raster_2023 <- calc(NDWI_stack_2023, fun = stats) # calculate median or mean value of the stack
  median_NDWI_raster_2023@file@name <- '2023 Series' # rename raster
  
}

# stack all the median NDWI plot
median_NDWI_stack_2014_to_2023 <- stack(median_NDWI_raster_2014,
                                        median_NDWI_raster_2015,
                                        median_NDWI_raster_2016,
                                        median_NDWI_raster_2017,
                                        median_NDWI_raster_2018,
                                        median_NDWI_raster_2019,
                                        median_NDWI_raster_2020,
                                        median_NDWI_raster_2021,
                                        median_NDWI_raster_2022,
                                        median_NDWI_raster_2023)

# Find the range in full range in the NDWI timeframe
zlim_NDWI <- range(c(minValue(median_NDWI_stack_2014_to_2023), 
                     maxValue(median_NDWI_stack_2014_to_2023)))
# Function to extract NDWI range and print neatly
NDWI_range <- function(data){
  range <- round(range(c(minValue(data), maxValue(data))),1)
  return(paste('Median NDWI Range from ', range[1], 'to', range[2]))
}


# plot the median NDWI rasters for each year
{ 
  
  # par(mar = c(bottom, left, top, right))
  # par(mar = c(8.0, 3, 1.3, 0.1)) # customised margin
  # par(mfrow = c(3,4)) # layout control
  par(mar = c(4, 3, 1.8, 0.5)) # customised margin
  par(mfrow = c(4,3)) # layout control
  plot(projectRaster(median_NDWI_raster_2014, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2014@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2014), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2015, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2015@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2015), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2016, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2016@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2016), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2017, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2017@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2017), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2018, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2018@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2018), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2019, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2019@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2019), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2020, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2020@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2020), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2021, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2021@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2021), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2022, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_NDWI_raster_2022@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2022), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_NDWI_raster_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
       col = NDWI_colour_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim_NDWI, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = T,
       horizontal = F, # make legend horizontal or vertical
       legend.shrink = 1, # stretch or compress legend
       axis.args = list(cex.axis = .6))
  #mgp = c(3, 0.2, 0)), # adjust legend lable size and position to ticks
  # legend.args = list(text = "Median \nNDWI", side = 4, cex = .5)) # add legend title and adjust size
  title(main=median_NDWI_raster_2023@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(NDWI_range(median_NDWI_raster_2023), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
}
