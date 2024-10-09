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

