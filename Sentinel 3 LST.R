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

# SENTINEL 3 LST ----------------------------------------------------------

# Creating color ramp for temperature plot
red_ramp <- colorRampPalette(c("#F5F500", "#F5B800", "#F57A00", "#F53D00", "#F50000"))(100)

# import filenames
S3_LST_filenames <- list.files('Raw Data/Climatological Data/Temperature/Sentinel 3 LST/', pattern = '.tiff')

# Function to read all raster
S3_LST_rasters <- function(file, index, plot = NULL){
  # read raster
  S3_LST <- raster(paste0('Raw Data/Climatological Data/Temperature/Sentinel 3 LST/', file[index]))
  S3_LST_name <- sub(".*?(\\d{4}\\.\\d{2}).*", "\\1", names(S3_LST)) # rename layer for plotting
  
  # NOTE: Temperature values are in Kelvin!
  S3_LST_proj_crop_mask <- projectRaster(S3_LST, crs = crs(roi_trans), 
                                         res = 30, # downsample spatial resolution to 30x30m
                                         method = 'ngb') |>
    crop(roi_trans) |> # crop raster to study area
    mask(roi_trans)  # mask raster to study area
  
  # Convert LST rasters from Kelvin to ºC
  S3_LST_proj_crop_mask_degC <- S3_LST_proj_crop_mask-273.15
  
  if(plot==T){
    # Visualise LST
    plot(S3_LST_proj_crop_mask_degC, col = red_ramp, main = S3_LST_name)
    plot(roi_trans, col = 'transparent', border = 'black', add = T)
  }
  return(S3_LST_proj_crop_mask_degC)
}

# Apply function to read all LST rasters
S3_LST_rasters_list <- pblapply(seq_along(S3_LST_filenames), function(x){S3_LST_rasters(file = S3_LST_filenames, index = x, plot = T)})

# # Save object
# save(S3_LST_rasters_list, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Temperature/Sentinel 3 LST/S3_LST_rasters_list.Rdata')
# 
# # Save rasters in one folder on local machine or hard drive
# Save_S3_LST_temperature_raster <- function(data, index, path){
# 
#     file_path <- paste0(path, gsub('\\.', ' ',paste0('LST ', sub(".*?(\\d{4}\\.\\d{2}).*", "\\1", 
#                                                                  names(S3_LST_rasters_list[[index]])))))
#     return(writeRaster(data[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
# }
# 
# pblapply(seq_along(S3_LST_rasters_list), function(x){Save_S3_LST_temperature_raster(data = S3_LST_rasters_list, 
#                                                                                     index = x,
#                                                                                     path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Temperature/Sentinel 3 LST/')
# })

