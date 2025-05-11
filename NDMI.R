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

# Colour ramp for NDMI
NDMI_colour_ramp <- colorRampPalette(c("brown", "yellow", "green"))(100)

# Create a function to compute 
NDMI_function <- function(data, index, plot = NULL){
  data_name <- data[[index]]@file@name # extract name from raster
  data <- clamp(data[[index]], 0, 1) # clamp value from 0 to 1- THIS STEP IS VERY IMPORTANT FOR THE VI CALC TO BE CORRECT!!!
  NIR <- data[[4]] # select NIR band
  SWIR1 <- data[[5]] # select SWIR1 band
  
  NDMI <- (NIR-SWIR1) /(NIR+SWIR1) # NDMI computation
  NDMI_reproj <- projectRaster(NDMI, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb') # projecting the raster for the sake of axis labels to be shown as lon-lat
  NDMI@file@name <- paste('NDMI:', as.Date(data_name, format = "%Y%m%d")) # rename raster
  
  if(plot == T){
    # Visualisation of NDMI rasters
    plot(NDMI_reproj,
         col = NDMI_colour_ramp,
         main = NDMI@file@name)
  }
  
  return(NDMI)
} 

NDMI_function(trimmed_df_aerial_imagery_after_interpolation,1,T)
# Calculating NDMI for all aerial imagery BEFORE INTERPOLATION
NDMI_rasters_before_interpolation <- pblapply(seq_along(all_aerial_imagery_before_interpolation),
                                              function(x){NDMI_function(data = all_aerial_imagery_before_interpolation,
                                                                        index = x,
                                                                        plot = T)})

# Calculating NDMI for all aerial imagery AFTER INTERPOLATION
NDMI_rasters_after_interpolation <- pblapply(seq_along(trimmed_df_aerial_imagery_after_interpolation),
                                             function(x){NDMI_function(data = trimmed_df_aerial_imagery_after_interpolation,
                                                                       index = x,
                                                                       plot = T)})


# Save object
save(NDMI_rasters_before_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDMI 2002-2023 (without interpolation)/NDMI_rasters_before_interpolation.Rdata')
save(NDMI_rasters_after_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDMI 2014-2023 (with interpolation)/NDMI_rasters_after_interpolation.Rdata')

# Save rasters in one folder on local machine or hard drive
{
  Save_raster <- function(data, index, path){
    
    file_path <- paste0(path, data[[index]]@file@name)
    
    return(writeRaster(data[[index]],
                       filename = file_path, format = "GTiff", overwrite = TRUE))
  }
  
  # Bulk Save!!!!
  pblapply(seq_along(NDMI_rasters_before_interpolation),
           function(x) {Save_raster(data = NDMI_rasters_before_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDMI 2002-2023 (without interpolation)/')})
  
  # Bulk Save!!!!
  pblapply(seq_along(NDMI_rasters_after_interpolation),
           function(x) {Save_raster(data = NDMI_rasters_after_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDMI 2014-2023 (with interpolation)/')})
  
}

load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDMI 2014-2023 (with interpolation)/NDMI_rasters_after_interpolation.Rdata')
# EDA for NDMI data -------------------------------------------------------
# TEMPORAL ANALYSIS
# Calculate the median value of NDMI per raster

median_NDMI_values <- pbsapply(seq_along(NDMI_rasters_after_interpolation), function(index){
  values(NDMI_rasters_after_interpolation[[index]]) |>
    na.omit() |>
    median()
})

# Extracting dates from NDMI rasters
NDMI_dates <- pbsapply(seq_along(NDMI_rasters_after_interpolation), function(index){sub("NDMI: ", "", NDMI_rasters_after_interpolation[[index]]@file@name)}) %>% 
  as.Date("%Y-%m-%d")

# Add the median values to a dataframe
NDMI_EDA_df <- data.frame(date = NDMI_dates,
                          median_NDMI = median_NDMI_values) 


NDMI_EDA_df$year <- year(NDMI_EDA_df$date) # extract year from date and create a year column

# Find the maximum median NDMI value for each year
max_median_NDMI_df <- NDMI_EDA_df %>%
  group_by(year) %>%
  summarise(median_NDMI = max(median_NDMI)) %>%
  select(median_NDMI) %>%
  left_join(NDMI_EDA_df) %>%
  select(-year) %>%
  rename(max_median_NDMI = median_NDMI)

# Find the minimum median NDMI value for each year
min_median_NDMI_df <- NDMI_EDA_df %>%
  group_by(year) %>%
  summarise(median_NDMI = min(median_NDMI)) %>%
  select(median_NDMI) %>%
  left_join(NDMI_EDA_df) %>%
  select(-year) %>%
  rename(min_median_NDMI = median_NDMI)

# Plot median value for NDMI
median_NDMI_plot <- ggplot(NDMI_EDA_df, aes(x = date, y = median_NDMI, color = median_NDMI)) +
  geom_line(linewidth = .8) +
  geom_point(data = max_median_NDMI_df, aes(x = date, y = max_median_NDMI), color = 'deeppink', size = 1) +
  geom_text(data = max_median_NDMI_df, aes(x = date, y = max_median_NDMI, label = format(date, '%Y-%m')), 
            vjust = -1, color = "deeppink", size = 2) +  # Label the max points)
  geom_point(data = min_median_NDMI_df, aes(x = date, y = min_median_NDMI), color = 'salmon', size = 1) +
  geom_text(data = min_median_NDMI_df, aes(x = date, y = min_median_NDMI, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradientn(colours = c("brown", "yellow", "green"), guide = 'none', name = 'Median NDMI') +
  ylab('Median NDMI') +
  xlab('Period') +
  theme_light() +
  theme(legend.position = 'bottom',
        legend.title= element_text(size = 9),
        legend.text = element_text(size = 7))

median_NDMI_plot

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/median_NDMI_plot.pdf", 
       plot = median_NDMI_plot, width = 6.56, height = 3.5)

# SPATIAL ANALYSIS
{
  stats <- median # stats to be calculated from NDMI data
  
  # 2014
  NDMI_stack_2014 <- stack(lapply(which(year(NDMI_dates)=='2014'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2014 NDMI raster series
  median_NDMI_raster_2014 <- calc(NDMI_stack_2014, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2014@file@name <- '2014 Series' # rename raster
  
  # 2015
  NDMI_stack_2015 <- stack(lapply(which(year(NDMI_dates)=='2015'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2015 NDMI raster series
  median_NDMI_raster_2015 <- calc(NDMI_stack_2015, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2015@file@name <- '2015 Series' # rename raster
  
  # 2016
  NDMI_stack_2016 <- stack(lapply(which(year(NDMI_dates)=='2016'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2016 NDMI raster series
  median_NDMI_raster_2016 <- calc(NDMI_stack_2016, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2016@file@name <- '2016 Series' # rename raster
  
  # 2017
  NDMI_stack_2017 <- stack(lapply(which(year(NDMI_dates)=='2017'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2017 NDMI raster series
  median_NDMI_raster_2017 <- calc(NDMI_stack_2017, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2017@file@name <- '2017 Series' # rename raster
  
  # 2018
  NDMI_stack_2018 <- stack(lapply(which(year(NDMI_dates)=='2018'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2018 NDMI raster series
  median_NDMI_raster_2018 <- calc(NDMI_stack_2018, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2018@file@name <- '2018 Series' # rename raster
  
  # 2019
  NDMI_stack_2019 <- stack(lapply(which(year(NDMI_dates)=='2019'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2019 NDMI raster series
  median_NDMI_raster_2019 <- calc(NDMI_stack_2019, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2019@file@name <- '2019 Series' # rename raster
  
  # 2020
  NDMI_stack_2020 <- stack(lapply(which(year(NDMI_dates)=='2020'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2020 NDMI raster series
  median_NDMI_raster_2020 <- calc(NDMI_stack_2020, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2020@file@name <- '2020 Series' # rename raster
  
  # 2021
  NDMI_stack_2021 <- stack(lapply(which(year(NDMI_dates)=='2021'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2021 NDMI raster series
  median_NDMI_raster_2021 <- calc(NDMI_stack_2021, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2021@file@name <- '2021 Series' # rename raster
  
  # 2022
  NDMI_stack_2022 <- stack(lapply(which(year(NDMI_dates)=='2022'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2022 NDMI raster series
  median_NDMI_raster_2022 <- calc(NDMI_stack_2022, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2022@file@name <- '2022 Series' # rename raster
  
  # 2023
  NDMI_stack_2023 <- stack(lapply(which(year(NDMI_dates)=='2023'), function(x) {NDMI_rasters_after_interpolation[[x]]})) # stack the 2023 NDMI raster series
  median_NDMI_raster_2023 <- calc(NDMI_stack_2023, fun = stats) # calculate median or mean value of the stack
  median_NDMI_raster_2023@file@name <- '2023 Series' # rename raster
  
}

# stack all the median NDMI plot
median_NDMI_stack_2014_to_2023 <- stack(median_NDMI_raster_2014,
                                        median_NDMI_raster_2015,
                                        median_NDMI_raster_2016,
                                        median_NDMI_raster_2017,
                                        median_NDMI_raster_2018,
                                        median_NDMI_raster_2019,
                                        median_NDMI_raster_2020,
                                        median_NDMI_raster_2021,
                                        median_NDMI_raster_2022,
                                        median_NDMI_raster_2023)

names(median_NDMI_stack_2014_to_2023) <- c('Median NDMI 2014', 
                                           'Median NDMI 2015',
                                           'Median NDMI 2016',
                                           'Median NDMI 2017',
                                           'Median NDMI 2018',
                                           'Median NDMI 2019',
                                           'Median NDMI 2020',
                                           'Median NDMI 2021',
                                           'Median NDMI 2022',
                                           'Median NDMI 2023') # rename raster stack
# save(median_NDMI_stack_2014_to_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_NDMI_stack_2014_to_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_NDMI_stack_2014_to_2023.Rdata')

# Converting stack into a dataframe
median_NDMI_stack_df <- as.data.frame(projectRaster(median_NDMI_stack_2014_to_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), xy  = T) %>%
  melt(id.vars= c('x','y'), na.rm = T) %>%
  as_tibble() %>%
  # Find the range of NDMI value for each median series of each year
  mutate(variable = as.factor(case_when(variable == 'Median.NDMI.2014'~ paste0('2014 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2014),1), ' to ', round(maxValue(median_NDMI_raster_2014),1)),
                                        variable == 'Median.NDMI.2015'~paste0('2015 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2015),1), ' to ', round(maxValue(median_NDMI_raster_2015),1)),
                                        variable == 'Median.NDMI.2016'~paste0('2016 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2016),1), ' to ', round(maxValue(median_NDMI_raster_2016),1)),
                                        variable == 'Median.NDMI.2017'~paste0('2017 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2017),1), ' to ', round(maxValue(median_NDMI_raster_2017),1)),
                                        variable == 'Median.NDMI.2018'~paste0('2018 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2018),1), ' to ', round(maxValue(median_NDMI_raster_2018),1)),
                                        variable == 'Median.NDMI.2019'~paste0('2019 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2019),1), ' to ', round(maxValue(median_NDMI_raster_2019),1)),
                                        variable == 'Median.NDMI.2020'~paste0('2020 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2020),1), ' to ', round(maxValue(median_NDMI_raster_2020),1)),
                                        variable == 'Median.NDMI.2021'~paste0('2021 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2021),1), ' to ', round(maxValue(median_NDMI_raster_2021),1)),
                                        variable == 'Median.NDMI.2022'~paste0('2022 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2022),1), ' to ', round(maxValue(median_NDMI_raster_2022),1)),
                                        variable == 'Median.NDMI.2023'~paste0('2023 Series\n Median NDMI Range from ', round(minValue(median_NDMI_raster_2023),1), ' to ', round(maxValue(median_NDMI_raster_2023),1)))))

head(median_NDMI_stack_df)
str(median_NDMI_stack_df)

median_NDMI_map <- ggplot()+
  geom_raster(data = median_NDMI_stack_df, aes(x = x, y = y, fill = value))+
  scale_fill_gradientn(colours = NDMI_colour_ramp, 'Median\n NDMI',
                       breaks = seq(min(median_NDMI_stack_df$value),max(median_NDMI_stack_df$value),length.out = 11),
                       labels = round(seq(min(median_NDMI_stack_df$value),max(median_NDMI_stack_df$value),length.out = 11),2)) +
  facet_wrap(~ variable, nrow = 4, ncol = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  theme_bw()+
  theme(panel.grid.major= element_blank(),
        strip.text = element_text(size=5),
        legend.position = 'right')+
  guides(fill = guide_colorbar(barwidth = .4, barheight = 30))

# save plot
ggsave('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/median_NDMI_map.pdf', 
       plot = median_NDMI_map, width = 6.56, height = 8)



# # Find the range in full range in the NDMI timeframe
# zlim_NDMI <- range(c(minValue(median_NDMI_stack_2014_to_2023), 
#                      maxValue(median_NDMI_stack_2014_to_2023)))
# # Function to extract NDMI range and print neatly
# NDMI_range <- function(data){
#   range <- round(range(c(minValue(data), maxValue(data))),1)
#   return(paste('Median NDMI Range from ', range[1], 'to', range[2]))
# }
# 
# 
# # plot the median NDMI rasters for each year
# { 
#   
#   # par(mar = c(bottom, left, top, right))
#   # par(mar = c(8.0, 3, 1.3, 0.1)) # customised margin
#   # par(mfrow = c(3,4)) # layout control
#   par(mar = c(4, 3, 1.8, 0.5)) # customised margin
#   par(mfrow = c(4,3)) # layout control
#   plot(projectRaster(median_NDMI_raster_2014, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2014@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2014), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2015, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2015@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2015), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2016, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2016@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2016), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2017, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2017@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2017), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2018, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2018@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2018), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2019, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2019@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2019), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2020, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2020@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2020), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2021, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2021@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2021), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2022, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_NDMI_raster_2022@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2022), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_NDMI_raster_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = NDMI_colour_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim_NDMI, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = T,
#        horizontal = F, # make legend horizontal or vertical
#        legend.shrink = 1, # stretch or compress legend
#        axis.args = list(cex.axis = .6))
#   #mgp = c(3, 0.2, 0)), # adjust legend lable size and position to ticks
#   # legend.args = list(text = "Median \nNDMI", side = 4, cex = .5)) # add legend title and adjust size
#   title(main=median_NDMI_raster_2023@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(NDMI_range(median_NDMI_raster_2023), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
# }
