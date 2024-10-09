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
}

# Creating color ramp for precipitation plot
blue_ramp <- colorRampPalette(c("#E7FBFF", "#C6DBFF", "#6BAED6", "#2171B5", "#08306B"))(20)

# PRECIPITATION DATA IMPORT -----------------------------------------------
# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import precipitation file names
CHIRPS_precipitation_filenames_2002_2023 <- list.files('Raw Data/Climatological Data/Precipitation/CHIRPS', pattern = '.tif')

CHIRPS_precipitation_raster_extraction <- function(index, plot = NULL){
  
  precipitation_raster <- raster(paste0('Raw Data/Climatological Data/Precipitation/CHIRPS/', CHIRPS_precipitation_filenames_2002_2023[index]))
  raster_name <- str_extract(CHIRPS_precipitation_filenames_2002_2023[index], "\\d{4}.\\d{2}") # extract date from tif file
  names(precipitation_raster) <- paste("TP", raster_name)
  # e <- drawExtent()
  large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
  precipitation_raster_crop <- crop(precipitation_raster, large_extent) # crop raster to extent
  values(precipitation_raster_crop) <- ifelse(values(precipitation_raster_crop)<1, NA, values(precipitation_raster_crop)) # convert huge negative values (e.g, -9999 to NA)
  precipitation_raster_proj <- projectRaster(precipitation_raster_crop, crs = crs(roi_trans), res = 30, method = 'ngb') # project raster to EPSG:32734 (WGS 84 / UTM zone 34S)
  precipitation_raster_cropTMNR <- crop(precipitation_raster_proj, roi_trans)
  precipitation_raster_maskTMNR <- mask(precipitation_raster_cropTMNR, roi_trans)
  
  if(plot==T){
    plot(precipitation_raster_maskTMNR, 
         main = names(precipitation_raster_maskTMNR), 
         col = blue_ramp)
  }
  
  return(precipitation_raster_maskTMNR)
}

# Processing CHIRPS precipitation data extraction
CHIRPS_precipitation_raster_list <- pblapply(seq_along(CHIRPS_precipitation_filenames_2002_2023), function(x){
  CHIRPS_precipitation_raster_extraction(index = x, plot = T)
})

# IDW alternative method (can ignore!)
{
  # CHIRPS_raster_to_point_prec <- function(index, plot = T){
  #   # Import precipitation rasters
  #   index <- index
  #   precipitation_raster <- raster(paste0('Raw Data/Climatological Data/Precipitation/CHIRPS/', precipitation_filenames_2014_2023[index]))
  #   raster_name <- str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}") # extract date from tif file
  #   names(precipitation_raster) <- paste("prec", raster_name)
  #   # e <- drawExtent()
  #   large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
  #   precipitation_raster_crop <- crop(precipitation_raster, large_extent) # crop raster to extent
  #   precipitation_raster_proj <- projectRaster(precipitation_raster_crop, crs = crs(roi_trans)) # project raster to EPSG:32734 (WGS 84 / UTM zone 34S)
  #   values(precipitation_raster_proj) <- ifelse(values(precipitation_raster_proj)<1, NA, values(precipitation_raster_proj)) # convert huge negative values (e.g, -9999 to NA)
  #   precipitation_df <- as.data.frame(precipitation_raster_proj, xy = T) # extract data into a data frame
  #   colnames(precipitation_df) <- c('x', 'y', 'precipitation') # rename column
  #   precipitation_df_NA_omit <- na.omit(precipitation_df) # omit NAs rows
  #   # Convert normal data frame to spatial point data frame
  #   precipitation_raster_to_point <- as_Spatial(st_as_sf(precipitation_df_NA_omit, 
  #                                                        coords = c("x", "y"), 
  #                                                        crs = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs"))
  # 
  #     if(plot == T){
  #     plot(precipitation_raster_proj, main = paste(raster_name, 'Precipitation'))
  #     plot(roi_trans, col = 'transparent', border = 'red', lwd = 1, add = T)
  #     plot(precipitation_raster_to_point, add = T, pch = 3, col = 'black')
  #   }
  # 
  #   return(precipitation_raster_to_point)
  # }
  # 
  # precipitation_raster_to_point_list <- pblapply(seq_along(precipitation_filenames_2014_2023), CHIRPS_raster_to_point_prec)
  # 
  # # Individual Visualisation example- simply run function again with appropriate index
  # CHIRPS_raster_to_point_prec(index = 1)
  # 
  # # IDW ---------------------------------------------------------------------
  # 
  # # Creating 30x30 spatial resolution grid with defined large extent above
  # IDW_precipitation_grid <- expand.grid(
  #   x = seq(
  #     from = extent(precipitation_raster_proj)@xmin,
  #     to = extent(precipitation_raster_proj)@xmax,
  #     by = 30 # cell size in m 
  #   ),
  #   y = seq(
  #     from = extent(precipitation_raster_proj)@ymin,
  #     to = extent(precipitation_raster_proj)@ymax,
  #     by = 30 # cell size in m
  #   )
  # )
  # 
  # coordinates(IDW_precipitation_grid) <- ~ x + y # cast it into a SpatialPoints object
  # proj4string(IDW_precipitation_grid) <- proj4string(precipitation_raster_to_point_list[[1]]) # assign appropriate spatial reference system to grid
  # gridded(IDW_precipitation_grid) <- T # cast grid from SpatialPoints object into SpatialPixels object
  # 
  # # IDW functions
  # 
  # PRECIPITATION_IDW <-function(index, beta_vector){
  #   IDW_CHIRPS_optimal_beta <- function(index, beta){
  #     rast_data <- precipitation_raster_to_point_list[[index]]
  #     beta <- beta
  #     g <- gstat::gstat(formula = precipitation ~ 1, # interpolate based on total precipitation
  #                       data = rast_data, 
  #                       nmax = length(rast_data), 
  #                       set = list(idp = beta))
  #     set.seed(1)
  #     cv_list <- pblapply(seq(2,10, by = 1), function(x) {gstat.cv(g, nfold = x)}) # generate CV using different folds
  #     SSR <- sapply(1:length(cv_list), function (x) {sum((cv_list[[x]]@data$residual)^2)}) # calculate sum of square of the residuals for each nfold
  #     
  #     return(cbind(nfold = seq(2,10, by = 1), beta, SSR))
  #   } 
  #   
  #   beta_vector <- beta_vector 
  #   # iterate IDW model over different betas
  #   IDW_CHIRPS_optimal_beta_list <- pblapply(beta_vector, function(x){IDW_CHIRPS_optimal_beta(index = index, beta = x)})
  #   result <- do.call(rbind, IDW_CHIRPS_optimal_beta_list) |> as.data.frame()
  #   
  #   IDW_CHIRPS_prec <- function(index, plot = T){
  #     # IDW model
  #     rast_data <- precipitation_raster_to_point_list[[index]]
  #     opt_beta <- result$beta[which.min(result$SSR)] # save optimal beta with lowest sum of square of the residuals
  #     # run optimal model with optimal number of folds and optimal beta
  #     IDW_precipitation <- gstat::gstat(formula = precipitation ~ 1, # interpolate based on total precipitation
  #                                       data = rast_data, 
  #                                       nmax = length(rast_data), 
  #                                       set = list(idp = opt_beta))
  #     
  #     IDW_precipitation_pred <- predict(IDW_precipitation, IDW_precipitation_grid) # IDW interpolation using optimal IDW model
  #     
  #     IDW_precipitation_raster <- raster(IDW_precipitation_pred) # convert IDW data into raster
  #     IDW_precipitation_raster_TMNR <- crop(IDW_precipitation_raster, roi_trans) # crop data to study area extent
  #     IDW_precipitation_raster_TMNR <- mask(IDW_precipitation_raster_TMNR, roi_trans) # mask data to study area extent
  #     names(IDW_precipitation_raster_TMNR) <- paste("IDW", str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}")) # rename IDW output
  #     
  #     if(plot == T){
  #       plot(IDW_precipitation_raster_TMNR,
  #            col = blue_ramp,
  #            main = paste(str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}"), 
  #                         "IDW Precipitation (mm)"))
  #     }
  #     
  #     return(IDW_precipitation_raster_TMNR)
  #   }
  #   
  #   precipitation_IDW_list <- IDW_CHIRPS_prec(index = index)
  #   
  #   return(precipitation_IDW_list)
  # }
  # 
  # # Execute function to compute IDW precipitation rasters over the collection of data
  # IDW_precipitation_rasters <- pblapply(1:length(precipitation_filenames_2014_2023), 
  #                                       function(x) {PRECIPITATION_IDW(index = x, beta_vector = 2:5)})
  # 
  # # # Save object
  # # save(IDW_precipitation_rasters,
  # #      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/CHIRPS/IDW_CHIRPS_precipitation_rasters.Rdata')
  # 
  # 
  # # Save rasters in one folder on local machine or hard drive
  # {
  #   Save_IDW_precipitation_raster <- function(index, path){
  #     
  #     file_path <- paste0(path, gsub("\\.", " ", 'Prec.' |> paste0(names(IDW_precipitation_rasters[[index]]))))
  #     
  #     return(writeRaster(IDW_precipitation_rasters[[index]], 
  #                        filename = file_path, format = "GTiff", overwrite = TRUE))
  #   }
  #     # # Bulk save
  #     # pblapply(seq_along(IDW_precipitation_rasters),
  #     #          function(x) {Save_IDW_precipitation_raster(index = x, 
  #     #                                                     path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/CHIRPS/')})
  # 
  # }
  
}

# # Saving results
# {
#   # Save object
#   save(CHIRPS_precipitation_raster_list,
#        file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/CHIRPS/CHIRPS_precipitation_raster_list.Rdata')
# 
# 
#   # Save rasters in one folder on local machine or hard drive
#   Save_CHIRPS_precipitation_raster <- function(index, path){
# 
#     file_path <- paste0(path, gsub("\\.", " ", names(CHIRPS_precipitation_raster_list[[index]])))
# 
#     return(writeRaster(CHIRPS_precipitation_raster_list[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
# 
#   pblapply(seq_along(CHIRPS_precipitation_raster_list), function(x){
#     Save_CHIRPS_precipitation_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/CHIRPS/')
# 
#   })
# }

# EDA for precipitation data ----------------------------------------------

# TEMPORAL ANALYSIS
# Calculate the median value of precipitation per raster
median_precipitation_values <- pbsapply(seq_along(precipitation_filenames_2014_2023), function(index){
  values(IDW_precipitation_rasters[[index]]) |>
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
                        vjust = -1, color = "deeppink", size = 2.1) +  # Label the max points)
  geom_point(data = min_median_prec_df, aes(x = date, y = min_median_prec), color = 'salmon', size = 1) +
  geom_text(data = min_median_prec_df, aes(x = date, y = min_median_prec, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradient(low = "lightskyblue", high = 'darkblue', guide = 'none', name = 'Median Precipitation (mm)') +
  ylab('Median Precipitation (mm)') +
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
  stats <- mean # stats to be calculated from precipitation data
  
  # 2014
  prec_stack_2014 <- stack(lapply(1:12, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2014 precipitation raster series
  median_prec_raster_2014 <- calc(prec_stack_2014, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2014@file@name <- '2014 Series' # rename raster
  
  # 2015
  prec_stack_2015 <- stack(lapply(13:24, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2015 precipitation raster series
  median_prec_raster_2015 <- calc(prec_stack_2015, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2015@file@name <- '2015 Series' # rename raster
  
  # 2016
  prec_stack_2016 <- stack(lapply(25:36, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2016 precipitation raster series
  median_prec_raster_2016 <- calc(prec_stack_2016, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2016@file@name <- '2016 Series' # rename raster
  
  # 2017
  prec_stack_2017 <- stack(lapply(37:48, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2017 precipitation raster series
  median_prec_raster_2017 <- calc(prec_stack_2017, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2017@file@name <- '2017 Series' # rename raster
  
  # 2018
  prec_stack_2018 <- stack(lapply(49:60, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2018 precipitation raster series
  median_prec_raster_2018 <- calc(prec_stack_2018, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2018@file@name <- '2018 Series' # rename raster
  
  # 2019
  prec_stack_2019 <- stack(lapply(61:72, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2019 precipitation raster series
  median_prec_raster_2019 <- calc(prec_stack_2019, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2019@file@name <- '2019 Series' # rename raster
  
  # 2020
  prec_stack_2020 <- stack(lapply(73:84, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2020 precipitation raster series
  median_prec_raster_2020 <- calc(prec_stack_2020, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2020@file@name <- '2020 Series' # rename raster
  
  # 2021
  prec_stack_2021 <- stack(lapply(85:96, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2021 precipitation raster series
  median_prec_raster_2021 <- calc(prec_stack_2021, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2021@file@name <- '2021 Series' # rename raster
  
  # 2022
  prec_stack_2022 <- stack(lapply(97:108, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2022 precipitation raster series
  median_prec_raster_2022 <- calc(prec_stack_2022, fun = stats) # calculate median or mean value of the stack
  median_prec_raster_2022@file@name <- '2022 Series' # rename raster
  
  # 2023
  prec_stack_2023 <- stack(lapply(109:120, function(x) {IDW_precipitation_rasters[[x]]})) # stack the 2023 precipitation raster series
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

# Find the range in full range in the precipitation timeframe
zlim <- range(c(minValue(median_prec_stack_2014_to_2023), 
                maxValue(median_prec_stack_2014_to_2023)))

# Function to extract precipitation range and print neatly
prec_range <- function(data){
  range <- round(range(c(minValue(data), maxValue(data))),1)
  return(paste('Median TP Range from ', range[1], 'mm to', range[2], 'mm'))
}


# plot the median precipitation rasters for each year
{ 

  # par(mar = c(bottom, left, top, right))
  # par(mar = c(8.0, 3, 1.3, 0.1)) # customised margin
  # par(mfrow = c(3,4)) # layout control
  par(mar = c(4, 3, 1.8, 0.5)) # customised margin
  par(mfrow = c(4,3)) # layout control
  plot(projectRaster(median_prec_raster_2014, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2014@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2014), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2015, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2015@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2015), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2016, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2016@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2016), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2017, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2017@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2017), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2018, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2018@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2018), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2019, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2019@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2019), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2020, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2020@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2020), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2021, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2021@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2021), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2022, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = F)
  title(main=median_prec_raster_2022@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2022), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
  plot(projectRaster(median_prec_raster_2023, crs = "+proj=longlat +datum=WGS84 +no_defs"), 
       col = blue_ramp,
       xlab = '',
       ylab = '',
       zlim = zlim, # this makes a better global representation of the data in terms of colour
       cex.main = .9,
       cex.axis = .6,
       legend = T,
       horizontal = F, # make legend horizontal or vertical
       legend.shrink = 1, # stretch or compress legend
       axis.args = list(cex.axis = .6))
                         #mgp = c(3, 0.2, 0)), # adjust legend lable size and position to ticks
       # legend.args = list(text = "Median \nTotal \nPrecipitation \n(mm)", side = 4, cex = .5)) # add legend title and adjust size
  title(main=median_prec_raster_2023@file@name, line=1.1, cex.main=.8) # make main title label closer to the top of the plot
  mtext(prec_range(median_prec_raster_2023), side = 3, cex = .5, line = .1) # add subtitle
  title(ylab="Longitude", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Latitude", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  
}
dev.off()










































