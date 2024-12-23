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

RH_color_ramp <- colorRampPalette(c('wheat','cyan','purple'))(100) # define a color ramp for relative humidity

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

RH_filenames <- list.files('Raw Data/Climatological Data/Relative Humidity')  # read file names

RH_raster_func <- function(file, index, plot = NULL){
  RH_raster <- raster(paste0('Raw Data/Climatological Data/Relative Humidity/', file[index])) # read raster from filenames
  raster_name <- paste0('ARH ',format(as.Date(str_extract(file[index], "\\d{8}"), format = '%Y%m%d'), '%Y-%m')) # extract date from tif file
  names(RH_raster) <- raster_name # remame raster
  large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
  RH_raster_crop <- crop(RH_raster, large_extent) # crop raster to extent
  RH_raster_proj <- projectRaster(RH_raster_crop, 
                                  crs = crs(roi_trans),
                                  res = 30, # downsample spatial resolution to 30x30m
                                  method = 'ngb')
  # crop and mask raster to study area only
  RH_raster_projcropmask_TMNR <- RH_raster_proj |>
    crop(roi_trans) |>
    mask(roi_trans)
  
  if(plot == T){
    plot(RH_raster_projcropmask_TMNR, col = RH_color_ramp, main = raster_name)
    plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, add = T)
  }
  return(RH_raster_projcropmask_TMNR)
}

RH_raster_func(RH_filenames, 1, T)
# Processing relative humidity data extraction
RH_raster_list <- pblapply(seq_along(RH_filenames), function(x){RH_raster_func(file = RH_filenames, index = x, plot = T)})

# # Saving results
# {
#   # Save object
#   save(RH_raster_list,
#        file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Relative humidity/RH_raster_list.Rdata')
# 
# 
#   # Save rasters in one folder on local machine or hard drive
#   Save_RH_raster <- function(index, path){
# 
#     file_path <- paste0(path, gsub("\\.", " ", names(RH_raster_list[[index]])))
# 
#     return(writeRaster(RH_raster_list[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
# 
#   pblapply(seq_along(RH_raster_list), function(x){
#     Save_RH_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Relative humidity/')
# 
#   })
# }


# EDA for RH data ----------------------------------------------
# Trim data from 2014 to 2023
RH_raster_list_2014_2023 <- RH_raster_list[145:264]

# TEMPORAL ANALYSIS
# Calculate the median value of RH per raster
median_RH_values <- pbsapply(seq_along(RH_raster_list_2014_2023), function(index){
  values(RH_raster_list_2014_2023[[index]]) |>
    na.omit() |>
    median()
})


# Add the median values to a dataframe
RH_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                          median_RH = median_RH_values) 


RH_EDA_df$year <- year(RH_EDA_df$date) # extract year from date and create a year column

# Find the maximum median RH value for each year
max_median_RH_df <- RH_EDA_df %>%
  group_by(year) %>%
  summarise(median_RH = max(median_RH)) %>%
  select(median_RH) %>%
  left_join(RH_EDA_df) %>%
  select(-year) %>%
  rename(max_median_RH = median_RH)

# Find the minimum median RH value for each year
min_median_RH_df <- RH_EDA_df %>%
  group_by(year) %>%
  summarise(median_RH = min(median_RH)) %>%
  select(median_RH) %>%
  left_join(RH_EDA_df) %>%
  select(-year) %>%
  rename(min_median_RH = median_RH)

# Plot median value for RH
median_RH_plot <- ggplot(RH_EDA_df, aes(x = date, y = median_RH, color = median_RH)) +
  geom_line(linewidth = .8) +
  geom_point(data = max_median_RH_df, aes(x = date, y = max_median_RH), color = 'deeppink', size = 1) +
  geom_text(data = max_median_RH_df, aes(x = date, y = max_median_RH, label = format(date, '%Y-%m')), 
            vjust = -1, color = "deeppink", size = 2) +  # Label the max points)
  geom_point(data = min_median_RH_df, aes(x = date, y = min_median_RH), color = 'salmon', size = 1) +
  geom_text(data = min_median_RH_df, aes(x = date, y = min_median_RH, label = format(date, '%Y-%m')), 
            vjust = 1.5, color = 'salmon', size = 2) +  # Label the max points)
  geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
  scale_color_gradientn(colours = c('wheat','cyan','purple'), guide = 'none', name = 'Median Average RH (%)') +
  ylab('Median ARH (%)') +
  xlab('Period') +
  theme_light() +
  theme(legend.position = 'bottom',
        legend.title= element_text(size = 9),
        legend.text = element_text(size = 7))

median_RH_plot

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/median_RH_plot.pdf", 
       plot = median_RH_plot, width = 6.56, height = 3.5)




