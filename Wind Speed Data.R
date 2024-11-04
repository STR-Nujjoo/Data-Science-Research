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

# Saving results
{
  # Save object
  save(windspeed_raster_list,
       file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Wind Speed/windspeed_raster_list.Rdata')


  # Save rasters in one folder on local machine or hard drive
  Save_windspeed_raster <- function(index, path){

    file_path <- paste0(path, gsub("\\.", " ", names(windspeed_raster_list[[index]])))

    return(writeRaster(windspeed_raster_list[[index]],
                       filename = file_path, format = "GTiff", overwrite = TRUE))
  }

  pblapply(seq_along(windspeed_raster_list), function(x){
    Save_windspeed_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Wind Speed/')

  })
}


windspeed_raster_list[[1]]

