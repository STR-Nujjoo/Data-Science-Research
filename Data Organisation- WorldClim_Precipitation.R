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

# Creating color ramp for precipitation plot
blue_ramp <- colorRampPalette(c("#E7FBFF", "#C6DBFF", "#6BAED6", "#2171B5", "#08306B"))(20)


# PRECIPITATION DATA IMPORT -----------------------------------------------
# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import WorldClim precipitation file names
WorldClim_precipitation_filenames_2002_2021 <- list.files('Raw Data/Climatological Data/Precipitation/WorldClim/wc2.1_cruts4.06_2.5m_prec_2000-2021/', pattern = '.tif')[25:264]

WorldClim_precipitation_raster_extraction <- function(index, plot = NULL){
  WorldClim_precipitation_raster <- raster(paste0('Raw Data/Climatological Data/Precipitation/WorldClim/wc2.1_cruts4.06_2.5m_prec_2000-2021/', WorldClim_precipitation_filenames_2002_2021[index]))
  raster_name <- str_extract(WorldClim_precipitation_filenames_2002_2021[index], "\\d{4}-\\d{2}") # extract date from tif file
  names(WorldClim_precipitation_raster) <- paste("TP", raster_name) # TP for Total Precipitation
  large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation (This step was done to accelerate processing time!)
  precipitation_raster_crop <- crop(WorldClim_precipitation_raster, large_extent) # crop raster to extent
  values(precipitation_raster_crop) <- ifelse(values(precipitation_raster_crop)<1, NA, values(precipitation_raster_crop)) # convert huge negative values (e.g, -9999 to NA)
  precipitation_raster_proj <- projectRaster(precipitation_raster_crop, 
                                             crs = crs(roi_trans), # project raster to EPSG:32734 (WGS 84 / UTM zone 34S)
                                             res = 30, # downsample spatial resolution to 30x30m
                                             method = 'ngb') # nearest neighbour preserves the original values 
  precipitation_raster_cropTMNR <- crop(precipitation_raster_proj, roi_trans) # crop to study area
  precipitation_raster_maskTMNR <- mask(precipitation_raster_cropTMNR, roi_trans) # mask to study area
  
  if(plot==T){
    plot(precipitation_raster_maskTMNR, 
         main = names(precipitation_raster_maskTMNR), 
         col = blue_ramp)
  }
  return(precipitation_raster_maskTMNR)
}

# Processing Worldclim precipitation data extraction
WorldClim_precipitation_raster_list <- pblapply(seq_along(WorldClim_precipitation_filenames_2002_2021), function(x){
  WorldClim_precipitation_raster_extraction(index = x, plot = T)
})



# # Saving results
# {
#   # # Save object
#   # save(WorldClim_precipitation_raster_list,
#   #      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/WorldClim/WorldClim_precipitation_raster_list.Rdata')
#   
#   
#   # Save rasters in one folder on local machine or hard drive
#   Save_WorldClim_precipitation_raster <- function(index, path){
#     
#     file_path <- paste0(path, gsub("\\.", " ", names(WorldClim_precipitation_raster_list[[index]])))
#     
#     return(writeRaster(WorldClim_precipitation_raster_list[[index]], 
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
#   
#   pblapply(seq_along(WorldClim_precipitation_raster_list), function(x){
#     Save_WorldClim_precipitation_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/WorldClim/')
#     
#   })
# }


# IDW alternative method (can ignore!)
{
  # WorldClim_raster_to_point_prec <- function(index, plot = T){
  #   # Import precipitation rasters
  #   total_precipitation_raster <- raster(paste0('Raw Data/Climatological Data/Precipitation/WorldClim/wc2.1_cruts4.06_2.5m_prec_2010-2021/', total_precipitation_filenames_2014_2021[index]))
  #   raster_name <- str_extract(total_precipitation_filenames_2014_2021[index], "\\d{4}-\\d{2}") # extract date from tif file
  #   names(total_precipitation_raster) <- paste("prec", raster_name)
  #   # e <- drawExtent()
  #   large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
  #   total_precipitation_raster_crop <- crop(total_precipitation_raster, large_extent) # crop raster to extent
  #   total_precipitation_raster_proj <<- projectRaster(total_precipitation_raster_crop, crs = crs(roi_trans)) # project raster to EPSG:32734 (WGS 84 / UTM zone 34S)
  #   precipitation_df <- as.data.frame(total_precipitation_raster_proj, xy = T) # extract data into a data frame
  #   colnames(precipitation_df) <- c('x', 'y', 'total_precipitation') # rename column
  #   precipitation_df_NA_omit <- na.omit(precipitation_df) # omit NAs rows
  #   # Convert normal data frame to spatial point data frame
  #   precipitation_raster_to_point <- as_Spatial(st_as_sf(precipitation_df_NA_omit, 
  #                                                        coords = c("x", "y"), 
  #                                                        crs = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs"))
  #   
  #   if(plot == T){
  #     plot(total_precipitation_raster_proj, main = paste(raster_name, 'Total Precipitation'))
  #     plot(roi_trans, col = 'transparent', border = 'red', lwd = 1, add = T)
  #     plot(precipitation_raster_to_point, add = T, pch = 3, col = 'black')
  #   }
  #   
  #   return(precipitation_raster_to_point)
  # }
  # WorldClim_raster_to_point_prec(1)
  # total_precipitation_raster_to_point_list <- pblapply(1, WorldClim_raster_to_point_prec)
  # 
  # # Example of the plot and what we're extracting
  # plot(total_precipitation_raster_proj, main = paste(raster_name, 'Total Precipitation'))
  # plot(roi_trans, col = 'transparent', border = 'red', lwd = 1, add = T)
  # plot(total_precipitation_raster_to_point_list[[1]], add = T, pch = 3, col = 'black')
  # 
  # # IDW ---------------------------------------------------------------------
  # 
  # # Creating 30x30 spatial resolution grid with defined large extent above
  # IDW_precipitation_grid <- expand.grid(
  #   x = seq(
  #     from = extent(total_precipitation_raster_proj)@xmin,
  #     to = extent(total_precipitation_raster_proj)@xmax,
  #     by = 30 # cell size in m 
  #   ),
  #   y = seq(
  #     from = extent(total_precipitation_raster_proj)@ymin,
  #     to = extent(total_precipitation_raster_proj)@ymax,
  #     by = 30 # cell size in m
  #   )
  # )
  # 
  # coordinates(IDW_precipitation_grid) <- ~ x + y # cast it into a SpatialPoints object
  # proj4string(IDW_precipitation_grid) <- proj4string(total_precipitation_raster_to_point_list[[1]]) # assign appropriate spatial reference system to grid
  # gridded(IDW_precipitation_grid) <- T # cast grid from SpatialPoints object into SpatialPixels object
  # 
  # 
  # # IDW function
  # IDW_WorldClim_prec <- function(index, plot = T){
  #   # IDW model
  #   beta <- 2 
  #   IDW_total_precipitation <- gstat::gstat(formula = total_precipitation ~ 1, # interpolate based on total precipitation
  #                                    data = total_precipitation_raster_to_point_list[[index]], 
  #                                    nmax = length(total_precipitation_raster_to_point_list[[index]]), 
  #                                    set = list(idp = beta))
  #   
  #   IDW_total_precipitation_pred <- predict(IDW_total_precipitation, IDW_precipitation_grid) # IDW interpolation using IDW model
  #   
  #   IDW_total_precipitation_raster <- raster(IDW_total_precipitation_pred) # convert IDW data into raster
  #   IDW_total_precipitation_raster_TMNR <- crop(IDW_total_precipitation_raster, roi_trans) # crop data to study area extent
  #   IDW_total_precipitation_raster_TMNR <- mask(IDW_total_precipitation_raster_TMNR, roi_trans) # mask data to study area extent
  #   names(IDW_total_precipitation_raster_TMNR) <- paste("IDW", str_extract(total_precipitation_filenames_2014_2021[index], "\\d{4}-\\d{2}")) # rename IDW output
  #   
  #   if(plot == T){
  #     plot(IDW_total_precipitation_raster_TMNR,
  #          col = blue_ramp(5),
  #          main = paste(str_extract(total_precipitation_filenames_2014_2021[index], "\\d{4}-\\d{2}"), 
  #                       "IDW Total Precipitation (mm)"))
  #   }
  #   
  #   return(IDW_total_precipitation_raster_TMNR)
  # }
  # 
  # total_precipitation_IDW_list <- pblapply(1:1, FUN = IDW_WorldClim_prec)
  # 
  # # example of the IDW precipitation plot outside the function
  # plot(total_precipitation_IDW_list[[1]],
  #      col = blue_ramp(5),
  #      main = paste(str_extract(total_precipitation_filenames_2014_2021[1], "\\d{4}-\\d{2}"), 
  #                   "IDW Total Precipitation (mm)"))
  
}


# COMBINE 2022-2023 CHIRPS DATA TO WORLDCLIM DATA -------------------------
# Modify the extent of CHIRPS for consistency to WorldClim data
CHIRPS_2022_2023_precipitation_raster_stack <- stack(CHIRPS_precipitation_raster_list[241:264])
extent(CHIRPS_2022_2023_precipitation_raster_stack) <- extent(WorldClim_precipitation_raster_list[[1]]) # assign appropriate extent for consistency

CHIRPS_2022_2023_precipitation_raster_list <- unstack(CHIRPS_2022_2023_precipitation_raster_stack) # unstack rasters
WorldClimCHIRPS_precipitation_raster_list <- append(WorldClim_precipitation_raster_list, CHIRPS_2022_2023_precipitation_raster_list) # combine rasters

# Save object
save(WorldClimCHIRPS_precipitation_raster_list,
     file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/Combined precipitation data/WorldClimCHIRPS_precipitation_raster_list.Rdata')


# EDA for precipitation data ----------------------------------------------
# Trim data from 2014 to 2023
WorldClimCHIRPS_2014_2023_precipitation_raster_list <- WorldClimCHIRPS_precipitation_raster_list[145:264]

# TEMPORAL ANALYSIS
# Calculate the median value of precipitation per raster
median_precipitation_values <- pbsapply(seq_along(WorldClimCHIRPS_2014_2023_precipitation_raster_list), function(index){
  values(WorldClimCHIRPS_2014_2023_precipitation_raster_list[[index]]) |>
    na.omit() |>
    median()
})


# Add the median values to a dataframe
prec_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                          median_prec = median_precipitation_values) 


prec_EDA_df$year <- year(prec_EDA_df$date) # extract year from date and create a year column

# Find the maximum median precipitation value for each year
max_median_prec_df <- prec_EDA_df %>%
  group_by(year) %>%
  summarise(median_prec = max(median_prec)) %>%
  select(median_prec) %>%
  left_join(prec_EDA_df) %>%
  select(-year) %>%
  rename(max_median_prec = median_prec)

# Find the minimum median precipitation value for each year
min_median_prec_df <- prec_EDA_df %>%
  group_by(year) %>%
  summarise(median_prec = min(median_prec)) %>%
  select(median_prec) %>%
  left_join(prec_EDA_df) %>%
  select(-year) %>%
  rename(min_median_prec = median_prec)

# Plot median value for precipitation
median_precipitation_plot <- ggplot(prec_EDA_df, aes(x = date, y = median_prec, color = median_prec)) +
  geom_line(linewidth = .8) +
  geom_point(data = max_median_prec_df, aes(x = date, y = max_median_prec), color = 'deeppink', size = 1) +
  geom_text(data = max_median_prec_df, aes(x = date, y = max_median_prec, label = format(date, '%Y-%m')), 
            vjust = -1, color = "deeppink", size = 2) +  # Label the max points)
  geom_point(data = min_median_prec_df, aes(x = date, y = min_median_prec), color = 'salmon', size = 1) +
  geom_text(data = min_median_prec_df, aes(x = date, y = min_median_prec, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradient(low = "lightskyblue", high = 'darkblue', guide = 'none', name = 'Median Precipitation (mm)') +
  ylab('Median TP (mm)') +
  xlab('Period') +
  theme_light() +
  theme(legend.position = 'bottom',
        legend.title= element_text(size = 9),
        legend.text = element_text(size = 7))

median_precipitation_plot

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/median_precipitation_plot.pdf", 
       plot = median_precipitation_plot, width = 6.56, height = 3.5)


# SPATIAL ANALYSIS
{
  stats <- median # stats to be calculated from precipitation data
  
  # 2014
  prec_stack_2014 <- stack(lapply(1:12, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2014 precipitation raster series
  median_prec_raster_2014 <- calc(prec_stack_2014, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2014@file@name <- '2014 Series' # rename raster
  
  # 2015
  prec_stack_2015 <- stack(lapply(13:24, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2015 precipitation raster series
  median_prec_raster_2015 <- calc(prec_stack_2015, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2015@file@name <- '2015 Series' # rename raster
  
  # 2016
  prec_stack_2016 <- stack(lapply(25:36, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2016 precipitation raster series
  median_prec_raster_2016 <- calc(prec_stack_2016, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2016@file@name <- '2016 Series' # rename raster
  
  # 2017
  prec_stack_2017 <- stack(lapply(37:48, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2017 precipitation raster series
  median_prec_raster_2017 <- calc(prec_stack_2017, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2017@file@name <- '2017 Series' # rename raster
  
  # 2018
  prec_stack_2018 <- stack(lapply(49:60, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2018 precipitation raster series
  median_prec_raster_2018 <- calc(prec_stack_2018, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2018@file@name <- '2018 Series' # rename raster
  
  # 2019
  prec_stack_2019 <- stack(lapply(61:72, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2019 precipitation raster series
  median_prec_raster_2019 <- calc(prec_stack_2019, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2019@file@name <- '2019 Series' # rename raster
  
  # 2020
  prec_stack_2020 <- stack(lapply(73:84, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2020 precipitation raster series
  median_prec_raster_2020 <- calc(prec_stack_2020, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2020@file@name <- '2020 Series' # rename raster
  
  # 2021
  prec_stack_2021 <- stack(lapply(85:96, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2021 precipitation raster series
  median_prec_raster_2021 <- calc(prec_stack_2021, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2021@file@name <- '2021 Series' # rename raster
  
  # 2022
  prec_stack_2022 <- stack(lapply(97:108, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2022 precipitation raster series
  median_prec_raster_2022 <- calc(prec_stack_2022, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2022@file@name <- '2022 Series' # rename raster
  
  # 2023
  prec_stack_2023 <- stack(lapply(109:120, function(x) {WorldClimCHIRPS_2014_2023_precipitation_raster_list[[x]]})) # stack the 2023 precipitation raster series
  median_prec_raster_2023 <- calc(prec_stack_2023, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2023@file@name <- '2023 Series' # rename raster
  
}

# stack all the median precipitation plot
median_prec_stack_2014_to_2023 <- stack(median_prec_raster_2014,
                                        median_prec_raster_2015,
                                        median_prec_raster_2016,
                                        median_prec_raster_2017,
                                        median_prec_raster_2018,
                                        median_prec_raster_2019,
                                        median_prec_raster_2020,
                                        median_prec_raster_2021,
                                        median_prec_raster_2022,
                                        median_prec_raster_2023)

names(median_prec_stack_2014_to_2023) <- c('Median TP 2014', 
                                          'Median TP 2015',
                                          'Median TP 2016',
                                          'Median TP 2017',
                                          'Median TP 2018',
                                          'Median TP 2019',
                                          'Median TP 2020',
                                          'Median TP 2021',
                                          'Median TP 2022',
                                          'Median TP 2023') # rename raster stack
# save(median_prec_stack_2014_to_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_prec_stack_2014_to_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/Median Rasters/median_prec_stack_2014_to_2023.Rdata')

# Converting stack into a dataframe
median_prec_stack_df <- as.data.frame(projectRaster(median_prec_stack_2014_to_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), xy  = T) %>%
  melt(id.vars= c('x','y'), na.rm = T) %>%
  as_tibble() %>%
  # Find the range of prec value for each median series of each year
  mutate(variable = as.factor(case_when(variable == 'Median.TP.2014'~ paste0('2014 Series\n Median TP Range from ', round(minValue(median_prec_raster_2014),1), ' mm to ', round(maxValue(median_prec_raster_2014),1), ' mm'),
                                        variable == 'Median.TP.2015'~paste0('2015 Series\n Median TP Range from ', round(minValue(median_prec_raster_2015),1), ' mm to ', round(maxValue(median_prec_raster_2015),1), ' mm'),
                                        variable == 'Median.TP.2016'~paste0('2016 Series\n Median TP Range from ', round(minValue(median_prec_raster_2016),1), ' mm to ', round(maxValue(median_prec_raster_2016),1), ' mm'),
                                        variable == 'Median.TP.2017'~paste0('2017 Series\n Median TP Range from ', round(minValue(median_prec_raster_2017),1), ' mm to ', round(maxValue(median_prec_raster_2017),1), ' mm'),
                                        variable == 'Median.TP.2018'~paste0('2018 Series\n Median TP Range from ', round(minValue(median_prec_raster_2018),1), ' mm to ', round(maxValue(median_prec_raster_2018),1), ' mm'),
                                        variable == 'Median.TP.2019'~paste0('2019 Series\n Median TP Range from ', round(minValue(median_prec_raster_2019),1), ' mm to ', round(maxValue(median_prec_raster_2019),1), ' mm'),
                                        variable == 'Median.TP.2020'~paste0('2020 Series\n Median TP Range from ', round(minValue(median_prec_raster_2020),1), ' mm to ', round(maxValue(median_prec_raster_2020),1), ' mm'),
                                        variable == 'Median.TP.2021'~paste0('2021 Series\n Median TP Range from ', round(minValue(median_prec_raster_2021),1), ' mm to ', round(maxValue(median_prec_raster_2021),1), ' mm'),
                                        variable == 'Median.TP.2022'~paste0('2022 Series\n Median TP Range from ', round(minValue(median_prec_raster_2022),1), ' mm to ', round(maxValue(median_prec_raster_2022),1), ' mm'),
                                        variable == 'Median.TP.2023'~paste0('2023 Series\n Median TP Range from ', round(minValue(median_prec_raster_2023),1), ' mm to ', round(maxValue(median_prec_raster_2023),1), ' mm'))))

head(median_prec_stack_df)
str(median_prec_stack_df)

median_prec_map <- ggplot()+
  geom_raster(data = median_prec_stack_df, aes(x = x, y = y, fill = value))+
  scale_fill_gradientn(colours = blue_ramp, 'Median\n TP',
                       breaks = seq(min(median_prec_stack_df$value),max(median_prec_stack_df$value),length.out = 11),
                       labels = round(seq(min(median_prec_stack_df$value),max(median_prec_stack_df$value),length.out = 11),2)) +
  facet_wrap(~ variable, nrow = 4, ncol = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  theme_bw()+
  theme(panel.grid.major= element_blank(),
        strip.text = element_text(size=5),
        legend.position = 'right')+
  guides(fill = guide_colorbar(barwidth = .4, barheight = 30))

# save plot
ggsave('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/median_precipitation_map.pdf', 
       plot = median_prec_map, width = 6.56, height = 8)

# # Find the range in full range in the precipitation timeframe
# zlim_prec <- range(c(minValue(median_prec_stack_2014_to_2023), 
#                 maxValue(median_prec_stack_2014_to_2023)))
# # Function to extract precipitation range and print neatly
# prec_range <- function(data){
#   range <- round(range(c(minValue(data), maxValue(data))),1)
#   return(paste('Median TP Range from ', range[1], 'mm to', range[2], 'mm'))
# }
# 
# 
# # plot the median precipitation rasters for each year
# { 
#   
#   # par(mar = c(bottom, left, top, right))
#   # par(mar = c(8.0, 3, 1.3, 0.1)) # customised margin
#   # par(mfrow = c(3,4)) # layout control
#   par(mar = c(4, 3, 1.8, 0.5)) # customised margin
#   par(mfrow = c(4,3)) # layout control
#   plot(projectRaster(median_prec_raster_2014, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2014@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2014), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2015, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2015@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2015), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2016, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2016@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2016), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2017, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2017@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2017), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2018, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2018@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2018), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2019, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2019@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2019), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2020, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2020@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2020), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2021, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2021@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2021), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2022, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = F)
#   title(main=median_prec_raster_2022@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2022), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
#   plot(projectRaster(median_prec_raster_2023, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb'), 
#        col = blue_ramp,
#        xlab = '',
#        ylab = '',
#        zlim = zlim, # this makes a better global representation of the data in terms of colour
#        cex.main = .9,
#        cex.axis = .6,
#        legend = T,
#        horizontal = F, # make legend horizontal or vertical
#        legend.shrink = 1, # stretch or compress legend
#        axis.args = list(cex.axis = .6))
#   #mgp = c(3, 0.2, 0)), # adjust legend lable size and position to ticks
#   # legend.args = list(text = "Median \nTotal \nPrecipitation \n(mm)", side = 4, cex = .5)) # add legend title and adjust size
#   title(main=median_prec_raster_2023@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
#   mtext(prec_range(median_prec_raster_2023), side = 3, cex = .5, line = .1) # add subtitle
#   title(ylab="Latitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
#   title(xlab = "Longitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
