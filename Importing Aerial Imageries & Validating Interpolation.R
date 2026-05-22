# rm(list = ls()) # clear environment

{
  library(raster)
  library(sp)
  library(tidyverse)
  library(rgdal)
  library(sf)
}

# # Read 1 image to check if path is correct
# image <- brick(paste0('Raw Data/Aerial Imagery/Landsat 7/', L7SR_image_collection[72]))
# as.data.frame(image, xy = TRUE) # converting raster to data frame- the xy is the coordinates

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# LANDSAT 7 ---------------------------------------------------------------
# Read in Landsat 7 files names in the R environment
L7SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Landsat 7',
                                    pattern = '.tif')

# Function to read in all possible Landsat 7 imagery downloaded and plot the image
# NOTE: If title is not showing up, clear all previous plots from the console and run again
L7SR_image_collection_visualisation <- function(index){
  image <- brick(paste0('Raw Data/Aerial Imagery/Landsat 7/', L7SR_image_collection[index]))
  
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  plotRGB(mask_image, r=3 , g=2 , b=1, 
          stretch = 'lin', 
          margin = T, 
          main = paste0("Index: ", 
                        which(L7SR_image_collection==L7SR_image_collection[index]), "\n",
                        "Date: ", as.Date(str_extract(L7SR_image_collection[index], "\\d{8}"),
                                          format = "%Y%m%d")))
  plot(roi_trans, col = 'transparent', border = 'red', lwd = 2, add = T)
  return(mask_image)
}

L7_with_plot <- lapply(1:length(L7SR_image_collection), L7SR_image_collection_visualisation) # plot imageries

# LANDSAT 8 ---------------------------------------------------------------
# Read in Landsat 8 files names in the R environment
L8SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Landsat 8',
                                    pattern = '.tif')

# Function to read in all possible Landsat 8 imagery downloaded and plot the image
L8SR_image_collection_visualisation <- function(index){
  image <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[index]))
  
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  plotRGB(mask_image, r=4 , g=3 , b=2, 
          stretch = 'lin', 
          margin = T, 
          main = paste0("Index: ", 
                        which(L8SR_image_collection==L8SR_image_collection[index]), "\n",
                        "Date: ", as.Date(str_extract(L8SR_image_collection[index], "\\d{8}"),
                                          format = "%Y%m%d")))
  plot(roi_trans, col = 'transparent', border = 'red', lwd = 2, add = T)
  return(mask_image)
}

L8_with_plot  <- lapply(1:length(L8SR_image_collection), L8SR_image_collection_visualisation) # plot imageries

# LANDSAT 9 ---------------------------------------------------------------
# Read in Landsat 9 files names in the R environment
L9SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Landsat 9',
                                    pattern = '.tif')

# Function to read in all possible Landsat 9 imagery downloaded and plot the image
L9SR_image_collection_visualisation <- function(index){
  image <- brick(paste0('Raw Data/Aerial Imagery/Landsat 9/', L9SR_image_collection[index]))
  
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  plotRGB(mask_image, r=4 , g=3 , b=2, 
          stretch = 'lin', 
          margin = T, 
          main = paste0("Index: ", 
                        which(L9SR_image_collection==L9SR_image_collection[index]), "\n",
                        "Date: ", as.Date(str_extract(L9SR_image_collection[index], "\\d{8}"),
                                          format = "%Y%m%d")))
  plot(roi_trans, col = 'transparent', border = 'red', lwd = 2, add = T)
  return(mask_image)
}

L9_with_plot  <- lapply(1:length(L9SR_image_collection), L9SR_image_collection_visualisation) # plot imageries

# SENTINEL 2 TOA ----------------------------------------------------------
# Read in Sentinel 2 TOA files names in the R environment
S2TOA_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Sentinel 2 TOA',
                                    pattern = '.tif')

# Function to read in all possible Sentinel 2 TOA imagery downloaded and plot the image
S2TOA_image_collection_visualisation <- function(index){
  image <- brick(paste0('Raw Data/Aerial Imagery/Sentinel 2 TOA/', S2TOA_image_collection[index]))
  
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  plotRGB(mask_image, r=4 , g=3 , b=2, 
          stretch = 'lin', 
          margin = T, 
          main = paste0("Index: ", 
                        which(S2TOA_image_collection==S2TOA_image_collection[index]), "\n",
                        "Date: ", as.Date(str_extract(S2TOA_image_collection[index], "\\d{8}"),
                                          format = "%Y%m%d")))
  plot(roi_trans, col = 'transparent', border = 'red', lwd = 2, add = T)
  return(mask_image)
}
S2TOA_with_plot  <- lapply(1:length(S2TOA_image_collection), S2TOA_image_collection_visualisation) # plot imageries

# SENTINEL 2 SR -----------------------------------------------------------
# Read in Sentinel 2 SR files names in the R environment
S2SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Sentinel 2 SR',
                                     pattern = '.tif')

# Function to read in all possible Sentinel 2 SR imagery downloaded and plot the image
S2SR_image_collection_visualisation <- function(index){
  image <- brick(paste0('Raw Data/Aerial Imagery/Sentinel 2 SR/', S2SR_image_collection[index]))
  
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  plotRGB(mask_image, r=4 , g=3 , b=2, 
          stretch = 'lin', 
          margin = T, 
          main = paste0("Index: ", 
                        which(S2SR_image_collection==S2SR_image_collection[index]), "\n",
                        "Date: ", as.Date(str_extract(S2SR_image_collection[index], "\\d{8}"),
                                          format = "%Y%m%d")))
  plot(roi_trans, col = 'transparent', border = 'red', lwd = 2, add = T)
  return(mask_image)
}
S2SR_with_plot <- lapply(1:length(S2SR_image_collection), S2SR_image_collection_visualisation) # plot imageries

# INTERPOLATION METHODS AND STRATEGIES ------------------------------------

# Import the relevant imagery for the test interpolation 
l8_20171010 <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[38]))
l8_20180911 <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[47]))
l8_20181013_original <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[48])) # target image to interpolate and check which interpolated method is the best
l8_20181114 <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[49]))
l8_20191016 <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[57]))
  
# Horizontal interpolation

# # LOOPING METHOD BUT NOT NECESSARY!

# # Perform linear interpolation for each pixel
# # Create an empty raster to store the interpolated image
# interpolated_img <- raster(ncol = ncol(l8_20181013_original), nrow = nrow(l8_20181013_original),
#                            xmn = xmin(l8_20181013_original), xmx = xmax(l8_20181013_original),
#                            ymn = ymin(l8_20181013_original), ymx = ymax(l8_20181013_original))
# interpolated_img <- brick(interpolated_img)

# # Linear interpolation formula for each band
# for (i in 1:nlayers(l8_20181013_original)) {
#   prev_image_band <- raster(l8_20180911, layer = i)
#   following_image_band <- raster(l8_20181114, layer = i)
# 
#   interpolated_band <- (prev_image_band + following_image_band) / 2  # Simple average
#   interpolated_img <- addLayer(interpolated_img, interpolated_band)
# }

l8_20181013_interpoltedH <- (l8_20180911+l8_20181114)/2 # simple average
summary(l8_20181013_interpoltedH)
summary(l8_20181013_original)

names(l8_20181013_interpoltedH) <- names(l8_20181013_original) # rename band layers
l8_20181013_original_df <- as.data.frame(l8_20181013_original, xy=T) # convert to dataframe
summary(l8_20181013_original_df)
head(l8_20181013_original_df)

l8_20181013_interpoltedH_df <- as.data.frame(l8_20181013_interpoltedH, xy = T) # convert to dataframe
summary(l8_20181013_interpoltedH_df)
head(l8_20181013_interpoltedH_df)

# Normal summary function
interpolated_image_summary <- function(raster1_as_df_orig, raster2_as_df_inter, bands){
  raster1_as_df_orig <- raster1_as_df_orig %>% replace(is.na(.), 0) # convert NA to 0
  raster2_as_df_inter <- raster2_as_df_inter %>% replace(is.na(.), 0) # convert NA to 0
  
  min_pix_val_orig <- min(raster1_as_df_orig[,bands]) # minimum pixel value for original imagery
  min_pix_val_inter <- min(raster2_as_df_inter[,bands]) # minimum pixel value for interpolated imagery
  max_pix_val_orig <- max(raster1_as_df_orig[,bands]) # maximum pixel value for original imagery
  max_pix_val_inter <- max(raster2_as_df_inter[,bands]) # maximum pixel value for interpolated imagery
  mean_pix_val_orig <- mean(raster1_as_df_orig[,bands]) # mean pixel value for original imagery
  mean_pix_val_inter <- mean(raster2_as_df_inter[,bands]) # mean pixel value for interpolated imagery
  sd_pix_val_orig <- sd((raster1_as_df_orig[,bands])) # standard deviation pixel value for original imagery
  sd_pix_val_inter <- sd((raster2_as_df_inter[,bands])) # # standard deviation pixel value for interpolated imagery
  rmse <- sqrt(mean((raster1_as_df_orig[,bands]-raster2_as_df_inter[,bands])^2)) # RMSE
  
  # Return a summary list of minimum, maximum, mean, standard deviation of both original and interpolated imagery. Plus RMSE for interpolated Imagery.
  return(list(min_pix_val_orig = min_pix_val_orig,
              min_pix_val_inter = min_pix_val_inter,
              max_pix_val_orig = max_pix_val_orig,
              max_pix_val_inter = max_pix_val_inter,
              mean_pix_val_orig = mean_pix_val_orig,
              mean_pix_val_inter = mean_pix_val_inter,
              sd_pix_val_orig = sd_pix_val_orig,
              sd_pix_val_inter = sd_pix_val_inter,
              rmse = rmse))
}

# Summary statistics for each bands for horizontal interpolation
all_summary_horizontal_average_interpolation <- rbind(sapply(3:ncol(l8_20181013_interpoltedH_df), 
       function(x){interpolated_image_summary(l8_20181013_original_df,
                                           l8_20181013_interpoltedH_df,
                                           x)}))

colnames(all_summary_horizontal_average_interpolation) <- names(l8_20181013_interpoltedH) # rename columns

# Original:
# Net minimum pixel value for colour composite bands of original imagery
horiz_orig_natural_colour_composite_net_min_pix_val <- (all_summary_horizontal_average_interpolation[,4]$min_pix_val_orig + # red
                                                          all_summary_horizontal_average_interpolation[,3]$min_pix_val_orig + # green
                                                          all_summary_horizontal_average_interpolation[,2]$min_pix_val_orig) / 3 # blue
                                                          
# Net maximum pixel value for colour composite bands of original imagery
horiz_orig_natural_colour_composite_net_max_pix_val <- (all_summary_horizontal_average_interpolation[,4]$max_pix_val_orig + # red
                                                          all_summary_horizontal_average_interpolation[,3]$max_pix_val_orig + # green
                                                          all_summary_horizontal_average_interpolation[,2]$max_pix_val_orig) / 3 # blue

# Net mean pixel value for colour composite bands of original imagery
horiz_orig_natural_colour_composite_net_mean_pix_val <- (all_summary_horizontal_average_interpolation[,4]$mean_pix_val_orig + # red
                                                          all_summary_horizontal_average_interpolation[,3]$mean_pix_val_orig + # green
                                                          all_summary_horizontal_average_interpolation[,2]$mean_pix_val_orig) / 3 # blue

# Net standard deviation pixel value for colour composite bands of original imagery
horiz_orig_natural_colour_composite_net_sd_pix_val <- (all_summary_horizontal_average_interpolation[,4]$sd_pix_val_orig + # red
                                                          all_summary_horizontal_average_interpolation[,3]$sd_pix_val_orig + # green
                                                          all_summary_horizontal_average_interpolation[,2]$sd_pix_val_orig) / 3 # blue



# Interpolated: 
# Net minimum pixel value for colour composite bands of interpolated imagery
horiz_inter_natural_colour_composite_net_min_pix_val <- (all_summary_horizontal_average_interpolation[,4]$min_pix_val_inter + # red
                                                          all_summary_horizontal_average_interpolation[,3]$min_pix_val_inter + # green
                                                          all_summary_horizontal_average_interpolation[,2]$min_pix_val_inter) / 3 # blue

# Net maximum pixel value for colour composite bands of interpolated imagery
horiz_inter_natural_colour_composite_net_max_pix_val <- (all_summary_horizontal_average_interpolation[,4]$max_pix_val_inter + # red
                                                          all_summary_horizontal_average_interpolation[,3]$max_pix_val_inter + # green
                                                          all_summary_horizontal_average_interpolation[,2]$max_pix_val_inter) / 3 # blue

# Net mean pixel value for colour composite bands of interpolated imagery
horiz_inter_natural_colour_composite_net_mean_pix_val <- (all_summary_horizontal_average_interpolation[,4]$mean_pix_val_inter + # red
                                                           all_summary_horizontal_average_interpolation[,3]$mean_pix_val_inter + # green
                                                           all_summary_horizontal_average_interpolation[,2]$mean_pix_val_inter) / 3 # blue

# Net standard deviation pixel value for colour composite bands of interpolated imagery
horiz_inter_natural_colour_composite_net_sd_pix_val <- (all_summary_horizontal_average_interpolation[,4]$sd_pix_val_inter + # red
                                                         all_summary_horizontal_average_interpolation[,3]$sd_pix_val_inter + # green
                                                         all_summary_horizontal_average_interpolation[,2]$sd_pix_val_inter) / 3 # blue


# Calculate Net RMSE for colour composite bands
horiz_inter_natural_colour_composite_net_RMSE <- (all_summary_horizontal_average_interpolation[,4]$rmse + # red
                                                    all_summary_horizontal_average_interpolation[,3]$rmse + # green
                                                    all_summary_horizontal_average_interpolation[,2]$rmse) / 3 # blue

# Comparison plot between original and interpolated plot
{
  par(mfrow = c(1, 2))
  plotRGB(mask(l8_20181013_original, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .7,
          main = paste0('Original Aerial Imagery \n Net Minimum Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_sd_pix_val,3)))
  plotRGB(mask(l8_20181013_interpoltedH, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .7,
          main = paste0('Interpolated Aerial Imagery \n Net Minimum Pixel Value: ', 
                        round(horiz_inter_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(horiz_inter_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(horiz_inter_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(horiz_inter_natural_colour_composite_net_sd_pix_val,3),
                        '\n Net RMSE: ',
                        round(horiz_inter_natural_colour_composite_net_RMSE,3)))
}

dev.off()

# Vertical interpolation
l8_20181013_interpoltedV <- (l8_20171010+l8_20191016)/2 # simple average
summary(l8_20181013_interpoltedV)
summary(l8_20181013_original)

l8_20181013_interpoltedV_df <- as.data.frame(l8_20181013_interpoltedV, xy = T) # convert to dataframe
summary(l8_20181013_interpoltedV_df)
head(l8_20181013_interpoltedV_df)

# Summary statistics for each bands for vertical interpolation
all_summary_vertical_average_interpolation <- rbind(sapply(3:ncol(l8_20181013_interpoltedV_df), 
                                                          function(x){interpolated_image_summary(l8_20181013_original_df,
                                                                                                 l8_20181013_interpoltedV_df,
                                                                                                 x)}))

colnames(all_summary_vertical_average_interpolation) <- names(l8_20181013_interpoltedV) # rename columns

# Interpolated: 
# Net minimum pixel value for colour composite bands of interpolated imagery
vertic_inter_natural_colour_composite_net_min_pix_val <- (all_summary_vertical_average_interpolation[,4]$min_pix_val_inter + # red
                                                            all_summary_vertical_average_interpolation[,3]$min_pix_val_inter + # green
                                                            all_summary_vertical_average_interpolation[,2]$min_pix_val_inter) / 3 # blue

# Net maximum pixel value for colour composite bands of interpolated imagery
vertic_inter_natural_colour_composite_net_max_pix_val <- (all_summary_vertical_average_interpolation[,4]$max_pix_val_inter + # red
                                                            all_summary_vertical_average_interpolation[,3]$max_pix_val_inter + # green
                                                            all_summary_vertical_average_interpolation[,2]$max_pix_val_inter) / 3 # blue

# Net mean pixel value for colour composite bands of interpolated imagery
vertic_inter_natural_colour_composite_net_mean_pix_val <- (all_summary_vertical_average_interpolation[,4]$mean_pix_val_inter + # red
                                                             all_summary_vertical_average_interpolation[,3]$mean_pix_val_inter + # green
                                                             all_summary_vertical_average_interpolation[,2]$mean_pix_val_inter) / 3 # blue

# Net standard deviation pixel value for colour composite bands of interpolated imagery
vertic_inter_natural_colour_composite_net_sd_pix_val <- (all_summary_vertical_average_interpolation[,4]$sd_pix_val_inter + # red
                                                           all_summary_vertical_average_interpolation[,3]$sd_pix_val_inter + # green
                                                           all_summary_vertical_average_interpolation[,2]$sd_pix_val_inter) / 3 # blue

# Calculate Net RMSE for colour composite bands
vertic_inter_natural_colour_composite_net_RMSE <- (all_summary_vertical_average_interpolation[,4]$rmse + # red
                                                     all_summary_vertical_average_interpolation[,3]$rmse + # green
                                                     all_summary_vertical_average_interpolation[,2]$rmse)/3 # blue



# Comparison plot between original and interpolated plot
{
  par(mfrow = c(1, 2))
  plotRGB(mask(l8_20181013_original, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .7,
          main = paste0('Original Aerial Imagery \n Net Minimum Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_sd_pix_val,3)))
  plotRGB(mask(l8_20181013_interpoltedV, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .7,
          main = paste0('Interpolated Aerial Imagery \n Net Minimum Pixel Value: ', 
                        round(vertic_inter_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(vertic_inter_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(vertic_inter_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(vertic_inter_natural_colour_composite_net_sd_pix_val,3),
                        '\n Net RMSE: ',
                        round(vertic_inter_natural_colour_composite_net_RMSE,3)))
}

# Weighted interpolation

{
  weights_combn1 <- c('Previous month-same year imagery (left)' = 0.25, 
                      'Following month-same year imagery (right)' = 0.25, 
                      'Previous year-same month imagery (top)' = 0.25, 
                      'Following year-same month imagery (bottom)' = 0.25) # equal weights
  
  weights_combn2 <- c('Previous month-same year imagery (left)' = 0.50, 
                      'Following month-same year imagery (right)' = 0.25, 
                      'Previous year-same month imagery (top)' = 0.15, 
                      'Following year-same month imagery (bottom)' = 0.10) # varying weights with different focus
  
  weights_combn3 <- c('Previous month-same year imagery (left)' = 0.50, 
                      'Following month-same year imagery (right)' = 0.25, 
                      'Previous year-same month imagery (top)' = 0.10, 
                      'Following year-same month imagery (bottom)' = 0.15) # varying weights with different focus
  
  weights_combn4 <- c('Previous month-same year imagery (left)' = 0.25, 
                      'Following month-same year imagery (right)' = 0.50, 
                      'Previous year-same month imagery (top)' = 0.15, 
                      'Following year-same month imagery (bottom)' = 0.10) # varying weights with different focus
  
  weights_combn5 <- c('Previous month-same year imagery (left)' = 0.25, 
                      'Following month-same year imagery (right)' = 0.50, 
                      'Previous year-same month imagery (top)' = 0.10, 
                      'Following year-same month imagery (bottom)' = 0.15) # varying weights with different focus
  
  weights_combn6 <- c('Previous month-same year imagery (left)' = 0.10, 
                      'Following month-same year imagery (right)' = 0.15, 
                      'Previous year-same month imagery (top)' = 0.50, 
                      'Following year-same month imagery (bottom)' = 0.25) # varying weights with different focus
  
  weights_combn7 <- c('Previous month-same year imagery (left)' = 0.15, 
                      'Following month-same year imagery (right)' = 0.10, 
                      'Previous year-same month imagery (top)' = 0.50, 
                      'Following year-same month imagery (bottom)' = 0.25) # varying weights with different focus
  
  weights_combn8 <- c('Previous month-same year imagery (left)' = 0.10, 
                      'Following month-same year imagery (right)' = 0.15, 
                      'Previous year-same month imagery (top)' = 0.25, 
                      'Following year-same month imagery (bottom)' = 0.50) # varying weights with different focus
  
  weights_combn9 <- c('Previous month-same year imagery (left)' = 0.15, 
                      'Following month-same year imagery (right)' = 0.10, 
                      'Previous year-same month imagery (top)' = 0.25, 
                      'Following year-same month imagery (bottom)' = 0.50) # varying weights with different focus
}

# Weight combination lists
weights <- list(weights_combn1, weights_combn2, weights_combn3,
                weights_combn4, weights_combn5, weights_combn6,
                weights_combn7, weights_combn8, weights_combn9)


optimal_weights <- function(WC){
  # Applying weighted average
  l8_20181013_interpoltedW <- (l8_20180911 * weights[[WC]][1]) + 
    (l8_20181114 * weights[[WC]][2]) +
    (l8_20171010 * weights[[WC]][3]) +
    (l8_20191016 * weights[[WC]][4])
  
  summary(l8_20181013_interpoltedW)
  summary(l8_20181013_original)
  
  
  l8_20181013_interpoltedW_df <- as.data.frame(l8_20181013_interpoltedW, xy = T) # convert to dataframe
  summary(l8_20181013_interpoltedW_df)
  head(l8_20181013_interpoltedW_df)
  
  # Summary statistics for each bands for weighted interpolation
  all_summary_weighted_average_interpolation <- rbind(sapply(3:ncol(l8_20181013_interpoltedW_df), 
                                                          function(x){interpolated_image_summary(l8_20181013_original_df,
                                                                                              l8_20181013_interpoltedW_df,
                                                                                              x)}))
  
  colnames(all_summary_weighted_average_interpolation) <- names(l8_20181013_interpoltedW) # rename columns
  
  # Calculate Net RMSE for colour composite bands
  weighted_inter_natural_colour_composite_net_RMSE <- (all_summary_weighted_average_interpolation[,4]$rmse + # red
                                                         all_summary_weighted_average_interpolation[,3]$rmse + # green
                                                         all_summary_weighted_average_interpolation[,2]$rmse)/3 # blue
  
  
  return(Net_RMSE = weighted_inter_natural_colour_composite_net_RMSE)
}


RMSE_for_different_weights <- sapply(1:length(weights), function(x) {optimal_weights(x)})
names(RMSE_for_different_weights) <- c('WC1', 'WC2', 'WC3', 'WC4', 'WC5', 'WC6', 'WC7', 'WC8', 'WC9')
plot(RMSE_for_different_weights, xaxt = 'n', type = 'b',
     xlab = 'Weight Combinations',
     ylab = 'Net RMSE')
axis(1, at = 1:9) # 2nd weight combinations gave the best result

# Applying weighted average
l8_20181013_interpoltedW <- (l8_20180911 * weights[[which(RMSE_for_different_weights == min(RMSE_for_different_weights))]][1]) + 
  (l8_20181114 * weights[[which(RMSE_for_different_weights == min(RMSE_for_different_weights))]][2]) +
  (l8_20171010 * weights[[which(RMSE_for_different_weights == min(RMSE_for_different_weights))]][3]) +
  (l8_20191016 * weights[[which(RMSE_for_different_weights == min(RMSE_for_different_weights))]][4])

summary(l8_20181013_interpoltedW)
summary(l8_20181013_original)
names(l8_20181013_interpoltedW) <- names(l8_20181013_original) # rename band layers


l8_20181013_interpoltedW_df <- as.data.frame(l8_20181013_interpoltedW, xy = T) # convert to dataframe
summary(l8_20181013_interpoltedW_df)
head(l8_20181013_interpoltedW_df)

# Summary statistics for each bands for weighted interpolation
all_summary_weighted_average_interpolation <- rbind(sapply(3:ncol(l8_20181013_interpoltedW_df), 
                                                           function(x){interpolated_image_summary(l8_20181013_original_df,
                                                                                                  l8_20181013_interpoltedW_df,
                                                                                                  x)}))

colnames(all_summary_weighted_average_interpolation) <- names(l8_20181013_interpoltedW) # rename columns

# Interpolated:
# Net minimum pixel value for colour composite bands of interpolated imagery
weighted_inter_natural_colour_composite_net_min_pix_val <- (all_summary_weighted_average_interpolation[,4]$min_pix_val_inter + # red
                                                              all_summary_weighted_average_interpolation[,3]$min_pix_val_inter + # green
                                                              all_summary_weighted_average_interpolation[,2]$min_pix_val_inter) / 3 # blue

# Net maximum pixel value for colour composite bands of interpolated imagery
weighted_inter_natural_colour_composite_net_max_pix_val <- (all_summary_weighted_average_interpolation[,4]$max_pix_val_inter + # red
                                                              all_summary_weighted_average_interpolation[,3]$max_pix_val_inter + # green
                                                              all_summary_weighted_average_interpolation[,2]$max_pix_val_inter) / 3 # blue

# Net mean pixel value for colour composite bands of interpolated imagery
weighted_inter_natural_colour_composite_net_mean_pix_val <- (all_summary_weighted_average_interpolation[,4]$mean_pix_val_inter + # red
                                                               all_summary_weighted_average_interpolation[,3]$mean_pix_val_inter + # green
                                                               all_summary_weighted_average_interpolation[,2]$mean_pix_val_inter) / 3 # blue

# Net standard deviation pixel value for colour composite bands of interpolated imagery
weighted_inter_natural_colour_composite_net_sd_pix_val <- (all_summary_weighted_average_interpolation[,4]$sd_pix_val_inter + # red
                                                             all_summary_weighted_average_interpolation[,3]$sd_pix_val_inter + # green
                                                             all_summary_weighted_average_interpolation[,2]$sd_pix_val_inter) / 3 # blue


# Calculate Net RMSE for colour composite bands
weighted_inter_natural_colour_composite_net_RMSE <- (all_summary_weighted_average_interpolation[,4]$rmse + # red
                                                       all_summary_weighted_average_interpolation[,3]$rmse + # green
                                                       all_summary_weighted_average_interpolation[,2]$rmse)/3 # blue


# Comparison plot between original and interpolated plot
{
  par(mfrow = c(1, 2))
  plotRGB(mask(l8_20181013_original, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          main = 'Original Aerial Imagery')
  plotRGB(mask(l8_20181013_interpoltedW, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          main = paste0('Interpolated Aerial Imagery \n Net RMSE: ', 
                        round(weighted_inter_natural_colour_composite_net_RMSE,3)))
}


# Combined plot
{
  par(mfrow = c(2, 2))
  plotRGB(mask(l8_20181013_original, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = paste0('Original Aerial Imagery \n Net Minimum Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(horiz_orig_natural_colour_composite_net_sd_pix_val,3)),
          sub = '(a)')
  
  plotRGB(mask(l8_20181013_interpoltedH, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = paste0('Net Minimum Pixel Value: ', 
                        round(horiz_inter_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(horiz_inter_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(horiz_inter_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(horiz_inter_natural_colour_composite_net_sd_pix_val,3),
                        '\n Net RMSE: ',
                        round(horiz_inter_natural_colour_composite_net_RMSE,3)),
          sub = '(b)')
  mtext("Across-Months Interpolation using Simple Average", side = 3, cex = .5)
  
  plotRGB(mask(l8_20181013_interpoltedV, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = paste0('Net Minimum Pixel Value: ', 
                        round(vertic_inter_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(vertic_inter_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(vertic_inter_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(vertic_inter_natural_colour_composite_net_sd_pix_val,3),
                        '\n Net RMSE: ',
                        round(vertic_inter_natural_colour_composite_net_RMSE,3)),
          sub = '(c)')
  mtext("Across-Years Interpolation using Simple Average", side = 3, cex = .5)
  
  plotRGB(mask(l8_20181013_interpoltedW, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = paste0('Net Minimum Pixel Value: ', 
                        round(weighted_inter_natural_colour_composite_net_min_pix_val,3),
                        '\n Net Maximum Pixel Value: ',
                        round(weighted_inter_natural_colour_composite_net_max_pix_val,3),
                        '\n Net Mean Pixel Value: ',
                        round(weighted_inter_natural_colour_composite_net_mean_pix_val,3),
                        '\n Net Std Dev Pixel Value: ',
                        round(weighted_inter_natural_colour_composite_net_sd_pix_val,3),
                        '\n Net RMSE: ',
                        round(weighted_inter_natural_colour_composite_net_RMSE,3)),
          sub = '(d)')
  mtext("Weighted Interpolation", side = 3, cex = .5)
}

# Replot the above to insert in thesis
{
  par(mfrow = c(2, 2))
  par(mar = c(0.2, 0.1, 1.8, 0.1))
  plotRGB(mask(l8_20181013_original, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = 'Original Satellite Imagery \n 2018-10-13')
  plotRGB(mask(l8_20181013_interpoltedH, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = 'Horizontally Interpolated Satellite Imagery \n 2018-10-13 \n Net RMSE: 0.015')
  plotRGB(mask(l8_20181013_interpoltedV, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = 'Vertically Interpolated Satellite Imagery \n 2018-10-13 \n Net RMSE: 0.019')
  plotRGB(mask(l8_20181013_interpoltedW, roi_trans), r=4 , g=3 , b=2,
          margin = T,
          stretch = 'lin',
          cex.main = .6,
          main = 'Weighted Interpolated Satellite Imagery \n 2018-10-13 \n Net RMSE: 0.014')
  
}


# Creating a function to quickly compute all prompted metrics for the interpolated imagery
Interpolation_Metrics <- function(data, original = NULL, r, g, b){
  # Original Imagery won't have RMSE as a metric
  if(original==T){
    # Net minimum pixel value for colour composite bands of interpolated imagery
    orig_natural_colour_composite_net_min_pix_val <- (data[,r]$min_pix_val_orig + # red
                                                         data[,g]$min_pix_val_orig + # green
                                                         data[,b]$min_pix_val_orig) / 3 # blue
    
    # Net maximum pixel value for colour composite bands of interpolated imagery
    orig_natural_colour_composite_net_max_pix_val <- (data[,r]$max_pix_val_orig + # red
                                                         data[,g]$max_pix_val_orig + # green
                                                         data[,b]$max_pix_val_orig) / 3 # blue
    
    # Net mean pixel value for colour composite bands of interpolated imagery
    orig_natural_colour_composite_net_mean_pix_val <- (data[,r]$mean_pix_val_orig + # red
                                                          data[,g]$mean_pix_val_orig + # green
                                                          data[,b]$mean_pix_val_orig) / 3 # blue
    
    # Net standard deviation pixel value for colour composite bands of interpolated imagery
    orig_natural_colour_composite_net_sd_pix_val <- (data[,r]$sd_pix_val_orig + # red
                                                        data[,g]$sd_pix_val_orig + # green
                                                        data[,b]$sd_pix_val_orig) / 3 # blue
    
    return(list(Net_Min = orig_natural_colour_composite_net_min_pix_val,
                Net_Max = orig_natural_colour_composite_net_max_pix_val,
                Net_Mean = orig_natural_colour_composite_net_mean_pix_val,
                Net_sd = orig_natural_colour_composite_net_sd_pix_val))
    
  }else{
    # Net minimum pixel value for colour composite bands of interpolated imagery
    inter_natural_colour_composite_net_min_pix_val <- (data[,r]$min_pix_val_inter + # red
                                                         data[,g]$min_pix_val_inter + # green
                                                         data[,b]$min_pix_val_inter) / 3 # blue
    
    # Net maximum pixel value for colour composite bands of interpolated imagery
    inter_natural_colour_composite_net_max_pix_val <- (data[,r]$max_pix_val_inter + # red
                                                         data[,g]$max_pix_val_inter + # green
                                                         data[,b]$max_pix_val_inter) / 3 # blue
    
    # Net mean pixel value for colour composite bands of interpolated imagery
    inter_natural_colour_composite_net_mean_pix_val <- (data[,r]$mean_pix_val_inter + # red
                                                          data[,g]$mean_pix_val_inter + # green
                                                          data[,b]$mean_pix_val_inter) / 3 # blue
    
    # Net standard deviation pixel value for colour composite bands of interpolated imagery
    inter_natural_colour_composite_net_sd_pix_val <- (data[,r]$sd_pix_val_inter + # red
                                                        data[,g]$sd_pix_val_inter + # green
                                                        data[,b]$sd_pix_val_inter) / 3 # blue
    
    # Calculate Net RMSE for colour composite bands
    inter_natural_colour_composite_net_RMSE <- (data[,r]$rmse + # red
                                                  data[,g]$rmse + # green
                                                  data[,b]$rmse)/3 # blue
    
    return(list(Net_Min = inter_natural_colour_composite_net_min_pix_val,
                Net_Max = inter_natural_colour_composite_net_max_pix_val,
                Net_Mean = inter_natural_colour_composite_net_mean_pix_val,
                Net_sd = inter_natural_colour_composite_net_sd_pix_val,
                Net_RMSE = inter_natural_colour_composite_net_RMSE))
    
  }
  
}

# Validating other sequence of imageriess
L8_20190218 <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[52])) 
L8_20190306_original <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[53])) 
L8_20190306_original_df <- as.data.frame(L8_20190306_original, xy = T) # convert to dataframe
L8_20190407 <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[54])) 

# Horizontal interpolation
L8_20190306_interpoltedH <- (L8_20190218+L8_20190407)/2 # simple average
L8_20190306_interpoltedH_df <- as.data.frame(L8_20190306_interpoltedH, xy = T) # convert to dataframe

# Summary statistics for each bands for horizontal interpolation
all_summary_L8_20190306_interpoltedH <- rbind(sapply(3:ncol(L8_20190306_interpoltedH_df), 
                                                           function(x){interpolated_image_summary(L8_20190306_original_df,
                                                                                                  L8_20190306_interpoltedH_df,
                                                                                                  x)}))

colnames(all_summary_L8_20190306_interpoltedH) <- names(L8_20190306_original) # rename columns
L8_20190306_original_metrics <- Interpolation_Metrics(all_summary_L8_20190306_interpoltedH,T, r = 4, g = 3, b = 2)
L8_20190306_interpolated_metrics <- Interpolation_Metrics(all_summary_L8_20190306_interpoltedH,F, r = 4, g = 3, b = 2)

# Horizontal Interpolation
L9_20220914_original <- trimmed_df_aerial_imagery_after_interpolation[[119]]
L9_20220914_original_df <- as.data.frame(L9_20220914_original, xy = T) # convert to dataframe

L9_20220823 <- trimmed_df_aerial_imagery_after_interpolation[[118]]
L9_20221022 <- trimmed_df_aerial_imagery_after_interpolation[[120]]

L9_20220914_interpolatedH <- (L9_20220823+L9_20221022)/2 # simple average
L9_20220914_interpolatedH_df <- as.data.frame(L9_20220914_interpolatedH, xy = T) # convert to dataframe
# Summary statistics for each bands for horizontal interpolation
all_summary_L9_20220914_interpoltedH <- rbind(sapply(3:ncol(L9_20220914_interpolatedH_df), 
                                                     function(x){interpolated_image_summary(L9_20220914_original_df,
                                                                                            L9_20220914_interpolatedH_df,
                                                                                            x)}))
colnames(all_summary_L9_20220914_interpoltedH) <- names(L9_20220914_original)
L9_20220914_original_metrics <- Interpolation_Metrics(all_summary_L9_20220914_interpoltedH,T, r = 3, g = 2, b = 1)
L9_20220914_interpolated_metrics <- Interpolation_Metrics(all_summary_L9_20220914_interpoltedH,F,r = 3, g = 2, b = 1)

# Vertical Interpolation
L9_20180404 <- trimmed_df_aerial_imagery_after_interpolation[[61]]
L9_20190407_original <- trimmed_df_aerial_imagery_after_interpolation[[74]]
L9_20190407_original_df <- as.data.frame(L9_20190407_original, xy = T) # convert to dataframe
L9_20200409 <- trimmed_df_aerial_imagery_after_interpolation[[86]]

L9_20190407_interpolatedV <- (L9_20180404+L9_20200409)/2 # simple average
L9_20190407_interpolatedV_df <- as.data.frame(L9_20190407_interpolatedV, xy = T) # convert to dataframe

# Summary statistics for each bands for vertical interpolation
all_summary_L9_20190407_interpolatedV <- rbind(sapply(3:ncol(L9_20190407_interpolatedV_df), 
                                                     function(x){interpolated_image_summary(L9_20190407_original_df,
                                                                                            L9_20190407_interpolatedV_df,
                                                                                            x)}))
colnames(all_summary_L9_20190407_interpolatedV) <- names(L9_20190407_original)
L9_20190407_original_metrics <- Interpolation_Metrics(all_summary_L9_20190407_interpolatedV,T, r = 3, g = 2, b = 1)
L9_20190407_interpolated_metrics <- Interpolation_Metrics(all_summary_L9_20190407_interpolatedV,F,r = 3, g = 2, b = 1)



# Vertical Interpolation
L8_20140425 <- trimmed_df_aerial_imagery_after_interpolation[[5]]
L8_20150412_original <- trimmed_df_aerial_imagery_after_interpolation[[22]]
L8_20150412_original_df <- as.data.frame(L8_20150412_original, xy = T) # convert to dataframe
S2_20160406 <- trimmed_df_aerial_imagery_after_interpolation[[35]]

L8_20150412_interpolatedV <- (L8_20140425+S2_20160406)/2 # simple average
L8_20150412_interpolatedV_df <- as.data.frame(L8_20150412_interpolatedV, xy = T) # convert to dataframe

# Summary statistics for each bands for vertical interpolation
all_summary_L8_20150412_interpolatedV <- rbind(sapply(3:ncol(L8_20150412_interpolatedV_df), 
                                                      function(x){interpolated_image_summary(L8_20150412_original_df,
                                                                                             L8_20150412_interpolatedV_df,
                                                                                             x)}))

colnames(all_summary_L8_20150412_interpolatedV) <- names(L8_20150412_original)
L8_20150412_original_metrics <- Interpolation_Metrics(all_summary_L8_20150412_interpolatedV,T, r = 3, g = 2, b = 1)
L8_20150412_interpolated_metrics <- Interpolation_Metrics(all_summary_L8_20150412_interpolatedV,F,r = 3, g = 2, b = 1)

par(mar = c(0.2, 0.1, 1.8, 0.1))
plotRGB(L8_20150412_original, r=3 , g=2 , b=1,
        margin = T,
        stretch = 'lin',
        cex.main = .6)

plotRGB(L8_20150412_interpolatedV, r=3 , g=2 , b=1,
        margin = T,
        stretch = 'lin',
        cex.main = .6)












# # Check if the images have the same extent and resolution
# if (!compareRaster(xfloat, x)) {
#   stop("The images do not have the same extent and resolution.")
# }