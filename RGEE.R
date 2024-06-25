# RGEE RESOURCES: https://csaybar.github.io/rgee-examples/
# rm(list = ls()) # clear environment
# Install python
library(reticulate)
# Unset the RETICULATE_PYTHON environment variable
Sys.unsetenv("RETICULATE_PYTHON")

Sys.which("python")   # system default
Sys.which("python3")  # is a V3 installed?

use_python(Sys.which("python3"), required = T)  # use it

# #check
# # use the standard Python numeric library
# np <- reticulate::import("numpy", convert = FALSE)
# # do some array manipulations with NumPy
# a <- np$array(c(1:4))
# print(a)  # this should be a Python array
# print(py_to_r(a))  # this should be an R array
# (sum <- a$cumsum())
# # convert to R explicitly at the end
# print(py_to_r(sum))

# Install rgee package
library(rgee)
rgee::ee_install()
ee_check()

# Initialising the GEE interface
ee_clean_pyenv() # Remove reticulate system variables
rgee::ee_install_upgrade()
ee_Initialize()

# install.packages(c("sf", "geojsonio"))
{
  library(sf)
  library(geojsonio)
  library(tidyverse)
  library(stars)
  library(stringr)
}


# IMPORTING TMNR SHAPE FILE -----------------------------------------------

roi <- read_sf('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_ee <- sf_as_ee(roi) # converting shape file to earth engine environment

# LANDSAT 7 SR IMAGE COLLECTION -------------------------------------------
# Filter landsat image collection
# Function that gives all the available aerial imagery at a specific % cloud cover
L7_collection <- function(cloud_cover_land){
  L7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$
    filterDate('2002-01-01', '2013-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('WRS_ROW', 'equals', 84)$
    filterMetadata('WRS_PATH', 'equals', 175)$
    filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
  
  return(L7$aggregate_array('system:index')$getInfo())
}

l7SR_5_collection_ID <-  L7_collection(5) # all the imageries available at 5% LCC
l7SR_10_collection_ID <-  L7_collection(10) # all the imageries available at 10% LCC
l7SR_15_collection_ID <-  L7_collection(15) # all the imageries available at 15% LCC
l7SR_20_collection_ID <-  L7_collection(20) # all the imageries available at 20% LCC
l7SR_25_collection_ID <-  L7_collection(25) # all the imageries available at 25% LCC
l7SR_30_collection_ID <-  L7_collection(30) # all the imageries available at 30% LCC
l7SR_35_collection_ID <-  L7_collection(35) # all the imageries available at 35% LCC
l7SR_40_collection_ID <-  L7_collection(40) # all the imageries available at 40% LCC
l7SR_45_collection_ID <-  L7_collection(45) # all the imageries available at 45% LCC
l7SR_50_collection_ID <-  L7_collection(50) # all the imageries available at 50% LCC
l7SR_55_collection_ID <-  L7_collection(55) # all the imageries available at 55% LCC
l7SR_60_collection_ID <-  L7_collection(60) # all the imageries available at 60% LCC
l7SR_65_collection_ID <-  L7_collection(65) # all the imageries available at 65% LCC


# Extract the date part and convert to date class
l7SR_5_collection_date <- as.Date(str_extract(l7SR_5_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_10_collection_date <- as.Date(str_extract(l7SR_10_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_15_collection_date <- as.Date(str_extract(l7SR_15_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_20_collection_date <- as.Date(str_extract(l7SR_20_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_25_collection_date <- as.Date(str_extract(l7SR_25_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_30_collection_date <- as.Date(str_extract(l7SR_30_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_35_collection_date <- as.Date(str_extract(l7SR_35_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_40_collection_date <- as.Date(str_extract(l7SR_40_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_45_collection_date <- as.Date(str_extract(l7SR_45_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_50_collection_date <- as.Date(str_extract(l7SR_50_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_55_collection_date <- as.Date(str_extract(l7SR_55_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_60_collection_date <- as.Date(str_extract(l7SR_60_collection_ID, "\\d{8}"), format = "%Y%m%d")
l7SR_65_collection_date <- as.Date(str_extract(l7SR_65_collection_ID, "\\d{8}"), format = "%Y%m%d")

# The -1 from the code below is to obtain the imageries with the correct earth engine index which starts with 0
which((l7SR_10_collection_date %in% l7SR_5_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_10_collection_date[which((l7SR_10_collection_date %in% l7SR_5_collection_date) == F)] # which date are those imageries from the 10% LCC threshold

which((l7SR_15_collection_date %in% l7SR_10_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_15_collection_date[which((l7SR_15_collection_date %in% l7SR_10_collection_date) == F)] # which date are those imageries from the 15% LCC threshold

which((l7SR_20_collection_date %in% l7SR_15_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_20_collection_date[which((l7SR_20_collection_date %in% l7SR_15_collection_date) == F)] # which date are those imageries from the 20% LCC threshold

which((l7SR_25_collection_date %in% l7SR_20_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_25_collection_date[which((l7SR_25_collection_date %in% l7SR_20_collection_date) == F)] # which date are those imageries from the 25% LCC threshold

which((l7SR_30_collection_date %in% l7SR_25_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_30_collection_date[which((l7SR_30_collection_date %in% l7SR_25_collection_date) == F)] # which date are those imageries from the 30% LCC threshold

which((l7SR_35_collection_date %in% l7SR_30_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_35_collection_date[which((l7SR_35_collection_date %in% l7SR_30_collection_date) == F)] # which date are those imageries from the 35% LCC threshold

which((l7SR_40_collection_date %in% l7SR_35_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_40_collection_date[which((l7SR_40_collection_date %in% l7SR_35_collection_date) == F)] # which date are those imageries from the 40% LCC threshold

which((l7SR_45_collection_date %in% l7SR_40_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_45_collection_date[which((l7SR_45_collection_date %in% l7SR_40_collection_date) == F)] # which date are those imageries from the 45% LCC threshold

which((l7SR_50_collection_date %in% l7SR_45_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_50_collection_date[which((l7SR_50_collection_date %in% l7SR_45_collection_date) == F)] # which date are those imageries from the 50% LCC threshold

which((l7SR_55_collection_date %in% l7SR_50_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_55_collection_date[which((l7SR_55_collection_date %in% l7SR_50_collection_date) == F)] # which date are those imageries from the 55% LCC threshold

which((l7SR_60_collection_date %in% l7SR_55_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_60_collection_date[which((l7SR_60_collection_date %in% l7SR_55_collection_date) == F)] # which date are those imageries from the 60% LCC threshold

which((l7SR_65_collection_date %in% l7SR_60_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l7SR_65_collection_date[which((l7SR_65_collection_date %in% l7SR_60_collection_date) == F)] # which date are those imageries from the 65% LCC threshold

# Function to visualise the desired imagery from the available collection
L7_CollectionVis <- function(cloud_cover_land, image_index, tile_vis, roi_vis, roishp_vis){
  # Filter landsat image collection
  L7 <<- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$
    filterDate('2002-01-01', '2013-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('WRS_ROW', 'equals', 84)$
    filterMetadata('WRS_PATH', 'equals', 175)$
    filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
  
  IDs <- L7$aggregate_array('system:index')$getInfo()
  # ee_print(L7)
  
  # Apply scaling factors
  L7applyScaleFactors <- function(image) {
    opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
    thermalBand <- image$select('ST_B6')$multiply(0.00341802)$add(149.0)
    image$addBands(opticalBands, NULL, TRUE)$addBands(thermalBand, NULL, TRUE)
  }
  
  L7 <-  L7$map(L7applyScaleFactors)
  # Select specific image index
  L7_index <- image_index
  L7_selectedImage <- ee$Image(L7$toList(L7$size())$get(L7_index))
  #L7_selectedImage$getInfo()
  
  # Visualise selected image
  L7_visualisation <- list(bands = c('SR_B3', 'SR_B2', 'SR_B1'), min = 0.0, max = 0.3)
  
  # Add layers to the map
    Map$centerObject(roi_ee, zoom = 13) 
    Map$addLayer(L7_selectedImage, L7_visualisation, 'Landsat 7: whole designated area', shown = tile_vis) +
    Map$addLayer(L7_selectedImage$clip(roi_ee), L7_visualisation, paste('Landsat 7: TMNR: ', IDs[image_index+1]), 
                 shown = roi_vis) +
    Map$addLayer(roi_ee, name = 'ROI', shown = roishp_vis)
}

# Change the parameters below for visualise the desired aerial imagery with the selected % land cloud cover
{
  L7_ee_image_index <- 1 # image index in earth engine env. (note: starts from 0)
  LCC <- 65 # percentage cloud cover
  
  # Call L7 imagery from its collection using function defined above
  L7_CollectionVis(cloud_cover_land = LCC,
                   image_index = L7_ee_image_index,
                   tile_vis = F,
                   roi_vis = T,
                   roishp_vis = F)
}

L7_collectionID <- L7$aggregate_array('system:index')$getInfo() # all the imageries available

(L7_available_imageries <- length(L7_collectionID)) # the size of collection

(L7_image_ID <- L7_collectionID[L7_ee_image_index+1]) # ID of the currently visualised imagery


# EXPORT LANDSAT 7 SR IMAGERIES -------------------------------------------

{

  # Function to export Landsat 7 imagery
  L7_Collection_Export <- function(cloud_cover_land, export_folder = folder_name) {
    # Filter Landsat 7 image collection
    L7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$
      filterDate('2002-01-01', '2013-12-31')$
      filterBounds(roi_ee)$
      filterMetadata('WRS_ROW', 'equals', 84)$
      filterMetadata('WRS_PATH', 'equals', 175)$
      filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
    
    # Get list of image IDs
    L7id <- L7$aggregate_array('system:index')

    # Function to apply scaling factors
    L7applyScaleFactors <- function(image) {
      opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
      thermalBand <- image$select('ST_B6')$multiply(0.00341802)$add(149.0)
      image$addBands(opticalBands, NULL, TRUE)$addBands(thermalBand, NULL, TRUE)
    }

    # Apply scaling factors to the collection
    L7 <- L7$map(L7applyScaleFactors)

    # Function to export each image to Google Drive
    export_image <- function(image_id) {
      image <- L7$filter(ee$Filter$eq('system:index', image_id))$first()
      task <- ee_image_to_drive(
        image = image$toFloat(),
        description = as.character(image_id),
        folder = export_folder,
        scale = 30,
        region = roi_ee$geometry(),
        crs = 'EPSG:32734', # WGS 84 / UTM zone 34S
        maxPixels = 1e13,
        fileFormat = "GeoTIFF",
        formatOptions = list(cloudOptimized = TRUE)
      )
      task$start()
    }

    # Evaluate the image IDs and export each one
    L7id$getInfo() %>% lapply(export_image)
  }

  # Run the function to export Landsat 7 imagery
  L7_Collection_Export(cloud_cover_land = 65, export_folder = 'RGEE Landsat Collection')

}



# LANDSAT 8 SR IMAGE COLLECTION -------------------------------------------
# Filter landsat image collection
# Function that gives all the available aerial imagery at a specific % cloud cover
L8_collection <- function(cloud_cover_land){
  L8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
    filterDate('2013-01-01', '2021-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('WRS_ROW', 'equals', 84)$
    filterMetadata('WRS_PATH', 'equals', 175)$
    filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
  
  return(L8$aggregate_array('system:index')$getInfo())
}

l8SR_5_collection_ID <-  L8_collection(5) # all the imageries available at 5% LCC
l8SR_10_collection_ID <-  L8_collection(10) # all the imageries available at 10% LCC
l8SR_15_collection_ID <-  L8_collection(15) # all the imageries available at 15% LCC
l8SR_20_collection_ID <-  L8_collection(20) # all the imageries available at 20% LCC
l8SR_25_collection_ID <-  L8_collection(25) # all the imageries available at 25% LCC
l8SR_30_collection_ID <-  L8_collection(30) # all the imageries available at 30% LCC
l8SR_35_collection_ID <-  L8_collection(35) # all the imageries available at 35% LCC
l8SR_40_collection_ID <-  L8_collection(40) # all the imageries available at 40% LCC
l8SR_45_collection_ID <-  L8_collection(45) # all the imageries available at 45% LCC
l8SR_50_collection_ID <-  L8_collection(50) # all the imageries available at 50% LCC
l8SR_55_collection_ID <-  L8_collection(55) # all the imageries available at 55% LCC
l8SR_60_collection_ID <-  L8_collection(60) # all the imageries available at 60% LCC
l8SR_65_collection_ID <-  L8_collection(65) # all the imageries available at 65% LCC

# Extract the date part and convert to date class
l8SR_5_collection_date <- as.Date(str_extract(l8SR_5_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_10_collection_date <- as.Date(str_extract(l8SR_10_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_15_collection_date <- as.Date(str_extract(l8SR_15_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_20_collection_date <- as.Date(str_extract(l8SR_20_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_25_collection_date <- as.Date(str_extract(l8SR_25_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_30_collection_date <- as.Date(str_extract(l8SR_30_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_35_collection_date <- as.Date(str_extract(l8SR_35_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_40_collection_date <- as.Date(str_extract(l8SR_40_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_45_collection_date <- as.Date(str_extract(l8SR_45_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_50_collection_date <- as.Date(str_extract(l8SR_50_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_55_collection_date <- as.Date(str_extract(l8SR_55_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_60_collection_date <- as.Date(str_extract(l8SR_60_collection_ID, "\\d{8}"), format = "%Y%m%d")
l8SR_65_collection_date <- as.Date(str_extract(l8SR_65_collection_ID, "\\d{8}"), format = "%Y%m%d")


# The -1 from the code below is to obtain the imageries with the correct earth engine index which starts with 0
which((l8SR_10_collection_date %in% l8SR_5_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_10_collection_date[which((l8SR_10_collection_date %in% l8SR_5_collection_date) == F)] # which date are those imageries from the 10% LCC threshold

which((l8SR_15_collection_date %in% l8SR_10_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_15_collection_date[which((l8SR_15_collection_date %in% l8SR_10_collection_date) == F)] # which date are those imageries from the 15% LCC threshold

which((l8SR_20_collection_date %in% l8SR_15_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_20_collection_date[which((l8SR_20_collection_date %in% l8SR_15_collection_date) == F)] # which date are those imageries from the 20% LCC threshold

which((l8SR_25_collection_date %in% l8SR_20_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_25_collection_date[which((l8SR_25_collection_date %in% l8SR_20_collection_date) == F)] # which date are those imageries from the 25% LCC threshold

which((l8SR_30_collection_date %in% l8SR_25_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_30_collection_date[which((l8SR_30_collection_date %in% l8SR_25_collection_date) == F)] # which date are those imageries from the 30% LCC threshold

which((l8SR_35_collection_date %in% l8SR_30_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_35_collection_date[which((l8SR_35_collection_date %in% l8SR_30_collection_date) == F)] # which date are those imageries from the 35% LCC threshold

which((l8SR_40_collection_date %in% l8SR_35_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_40_collection_date[which((l8SR_40_collection_date %in% l8SR_35_collection_date) == F)] # which date are those imageries from the 40% LCC threshold

which((l8SR_45_collection_date %in% l8SR_40_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_45_collection_date[which((l8SR_45_collection_date %in% l8SR_40_collection_date) == F)] # which date are those imageries from the 45% LCC threshold

which((l8SR_50_collection_date %in% l8SR_45_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_50_collection_date[which((l8SR_50_collection_date %in% l8SR_45_collection_date) == F)] # which date are those imageries from the 50% LCC threshold

which((l8SR_55_collection_date %in% l8SR_50_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_55_collection_date[which((l8SR_55_collection_date %in% l8SR_50_collection_date) == F)] # which date are those imageries from the 55% LCC threshold

which((l8SR_60_collection_date %in% l8SR_55_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_60_collection_date[which((l8SR_60_collection_date %in% l8SR_55_collection_date) == F)] # which date are those imageries from the 60% LCC threshold

which((l8SR_65_collection_date %in% l8SR_60_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l8SR_65_collection_date[which((l8SR_65_collection_date %in% l8SR_60_collection_date) == F)] # which date are those imageries from the 65% LCC threshold


# Function to visualise the desired imagery from the available collection
L8_CollectionVis <- function(cloud_cover_land, image_index, tile_vis, roi_vis, roishp_vis){
  # Filter landsat image collection
  L8 <<- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
    filterDate('2013-01-01', '2021-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('WRS_ROW', 'equals', 84)$
    filterMetadata('WRS_PATH', 'equals', 175)$
    filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
  
  IDs <- L8$aggregate_array('system:index')$getInfo()
  # ee_print(L8)
  
  # Apply scaling factors
  L8applyScaleFactors <- function(image) {
    opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
    thermalBand <- image$select('ST_B.*')$multiply(0.00341802)$add(149.0)
    image$addBands(opticalBands, NULL, TRUE)$addBands(thermalBand, NULL, TRUE)
  }
  
  L8 <-  L8$map(L8applyScaleFactors)
  # Select specific image index
  L8_index <- image_index
  L8_selectedImage <- ee$Image(L8$toList(L8$size())$get(L8_index))
  #L8_selectedImage$getInfo()
  
  # Visualise selected image
  L8_visualisation <- list(bands = c('SR_B4', 'SR_B3', 'SR_B2'), min = 0.0, max = 0.3)
  
  # Add layers to the map
  Map$centerObject(roi_ee, zoom = 13) 
  Map$addLayer(L8_selectedImage, L8_visualisation, 'Landsat 8: whole designated area', shown = tile_vis) +
    Map$addLayer(L8_selectedImage$clip(roi_ee), L8_visualisation, paste('Landsat 8: TMNR: ', IDs[image_index+1]), 
                 shown = roi_vis) +
    Map$addLayer(roi_ee, name = 'ROI', shown = roishp_vis)
}

# Change the parameters below for visualise the desired aerial imagery with the selected % land cloud cover
{
  L8_ee_image_index <- 20 # image index in earth engine env. (note: starts from 0)
  LCC <- 65 # percentage cloud cover
  
  # Call L7 imagery from its collection using function defined above
  L8_CollectionVis(cloud_cover_land = LCC,
                   image_index = L8_ee_image_index,
                   tile_vis = F,
                   roi_vis = T,
                   roishp_vis = F)
}

L8_collectionID <- L8$aggregate_array('system:index')$getInfo() # all the imageries available

(L8_available_imageries <- length(L8_collectionID)) # the size of collection

(L8_image_ID <- L8_collectionID[L8_ee_image_index+1]) # ID of the currently visualised imagery

# EXPORT LANDSAT 8 SR IMAGERIES -------------------------------------------

{
  
  # Function to export Landsat 8 imagery
  L8_Collection_Export <- function(cloud_cover_land, export_folder = folder_name) {
    # Filter Landsat 8 image collection
    L8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
      filterDate('2013-01-01', '2021-12-31')$
      filterBounds(roi_ee)$
      filterMetadata('WRS_ROW', 'equals', 84)$
      filterMetadata('WRS_PATH', 'equals', 175)$
      filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
    
    # Get list of image IDs
    L8id <- L8$aggregate_array('system:index')
    
    # Apply scaling factors
    L8applyScaleFactors <- function(image) {
      opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
      thermalBand <- image$select('ST_B.*')$multiply(0.00341802)$add(149.0)
      image$addBands(opticalBands, NULL, TRUE)$addBands(thermalBand, NULL, TRUE)
    }
    
    # Apply scaling factors to the collection
    L8 <- L8$map(L8applyScaleFactors)
    
    # Function to export each image to Google Drive
    export_image <- function(image_id) {
      image <- L8$filter(ee$Filter$eq('system:index', image_id))$first()
      task <- ee_image_to_drive(
        image = image$toFloat(),
        description = as.character(image_id),
        folder = export_folder,
        scale = 30,
        region = roi_ee$geometry(),
        crs = 'EPSG:32734', # WGS 84 / UTM zone 34S
        maxPixels = 1e13,
        fileFormat = "GeoTIFF",
        formatOptions = list(cloudOptimized = TRUE)
      )
      task$start()
    }
    
    # Evaluate the image IDs and export each one
    L8id$getInfo() %>% lapply(export_image)
  }
  
  # Run the function to export Landsat 8 imagery
  L8_Collection_Export(cloud_cover_land = 65, export_folder = 'RGEE Landsat Collection')
  
}

# LANDSAT 9 SR IMAGE COLLECTION -------------------------------------------
# Filter landsat image collection
# Function that gives all the available aerial imagery at a specific % cloud cover
L9_collection <- function(cloud_cover_land){
  L9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$
    filterDate('2021-01-01', '2023-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('WRS_ROW', 'equals', 84)$
    filterMetadata('WRS_PATH', 'equals', 175)$
    filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
  
  return(L9$aggregate_array('system:index')$getInfo())
}

l9SR_5_collection_ID <-  L9_collection(5) # all the imageries available at 5% LCC
l9SR_10_collection_ID <-  L9_collection(10) # all the imageries available at 10% LCC
l9SR_15_collection_ID <-  L9_collection(15) # all the imageries available at 15% LCC
l9SR_20_collection_ID <-  L9_collection(20) # all the imageries available at 20% LCC
l9SR_25_collection_ID <-  L9_collection(25) # all the imageries available at 25% LCC
l9SR_30_collection_ID <-  L9_collection(30) # all the imageries available at 30% LCC
l9SR_35_collection_ID <-  L9_collection(35) # all the imageries available at 35% LCC
l9SR_40_collection_ID <-  L9_collection(40) # all the imageries available at 40% LCC
l9SR_45_collection_ID <-  L9_collection(45) # all the imageries available at 45% LCC
l9SR_50_collection_ID <-  L9_collection(50) # all the imageries available at 50% LCC
l9SR_55_collection_ID <-  L9_collection(55) # all the imageries available at 55% LCC
l9SR_60_collection_ID <-  L9_collection(60) # all the imageries available at 60% LCC
l9SR_65_collection_ID <-  L9_collection(65) # all the imageries available at 65% LCC

# Extract the date part and convert to date class
l9SR_5_collection_date <- as.Date(str_extract(l9SR_5_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_10_collection_date <- as.Date(str_extract(l9SR_10_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_15_collection_date <- as.Date(str_extract(l9SR_15_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_20_collection_date <- as.Date(str_extract(l9SR_20_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_25_collection_date <- as.Date(str_extract(l9SR_25_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_30_collection_date <- as.Date(str_extract(l9SR_30_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_35_collection_date <- as.Date(str_extract(l9SR_35_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_40_collection_date <- as.Date(str_extract(l9SR_40_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_45_collection_date <- as.Date(str_extract(l9SR_45_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_50_collection_date <- as.Date(str_extract(l9SR_50_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_55_collection_date <- as.Date(str_extract(l9SR_55_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_60_collection_date <- as.Date(str_extract(l9SR_60_collection_ID, "\\d{8}"), format = "%Y%m%d")
l9SR_65_collection_date <- as.Date(str_extract(l9SR_65_collection_ID, "\\d{8}"), format = "%Y%m%d")

# The -1 from the code below is to obtain the imageries with the correct earth engine index which starts with 0
which((l9SR_10_collection_date %in% l9SR_5_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_10_collection_date[which((l9SR_10_collection_date %in% l9SR_5_collection_date) == F)] # which date are those imageries from the 10% LCC threshold

which((l9SR_15_collection_date %in% l9SR_10_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_15_collection_date[which((l9SR_15_collection_date %in% l9SR_10_collection_date) == F)] # which date are those imageries from the 15% LCC threshold

which((l9SR_20_collection_date %in% l9SR_15_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_20_collection_date[which((l9SR_20_collection_date %in% l9SR_15_collection_date) == F)] # which date are those imageries from the 20% LCC threshold

which((l9SR_25_collection_date %in% l9SR_20_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_25_collection_date[which((l9SR_25_collection_date %in% l9SR_20_collection_date) == F)] # which date are those imageries from the 25% LCC threshold

which((l9SR_30_collection_date %in% l9SR_25_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_30_collection_date[which((l9SR_30_collection_date %in% l9SR_25_collection_date) == F)] # which date are those imageries from the 30% LCC threshold

which((l9SR_35_collection_date %in% l9SR_30_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_35_collection_date[which((l9SR_35_collection_date %in% l9SR_30_collection_date) == F)] # which date are those imageries from the 35% LCC threshold

which((l9SR_40_collection_date %in% l9SR_35_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_40_collection_date[which((l9SR_40_collection_date %in% l9SR_35_collection_date) == F)] # which date are those imageries from the 40% LCC threshold

which((l9SR_45_collection_date %in% l9SR_40_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_45_collection_date[which((l9SR_45_collection_date %in% l9SR_40_collection_date) == F)] # which date are those imageries from the 45% LCC threshold

which((l9SR_50_collection_date %in% l9SR_45_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_50_collection_date[which((l9SR_50_collection_date %in% l9SR_45_collection_date) == F)] # which date are those imageries from the 50% LCC threshold

which((l9SR_55_collection_date %in% l9SR_50_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_55_collection_date[which((l9SR_55_collection_date %in% l9SR_50_collection_date) == F)] # which date are those imageries from the 55% LCC threshold

which((l9SR_60_collection_date %in% l9SR_55_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_60_collection_date[which((l9SR_60_collection_date %in% l9SR_55_collection_date) == F)] # which date are those imageries from the 60% LCC threshold

which((l9SR_65_collection_date %in% l9SR_60_collection_date) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
l9SR_65_collection_date[which((l9SR_65_collection_date %in% l9SR_60_collection_date) == F)] # which date are those imageries from the 65% LCC threshold

# Function to visualise the desired imagery from the available collection
L9_CollectionVis <- function(cloud_cover_land, image_index, tile_vis, roi_vis, roishp_vis){
  # Filter landsat image collection
  L9 <<- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$
    filterDate('2021-01-01', '2023-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('WRS_ROW', 'equals', 84)$
    filterMetadata('WRS_PATH', 'equals', 175)$
    filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
  
  IDs <- L9$aggregate_array('system:index')$getInfo()
  # ee_print(L9)
  
  # Apply scaling factors
  L9applyScaleFactors <- function(image) {
    opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
    thermalBand <- image$select('ST_B.*')$multiply(0.00341802)$add(149.0)
    image$addBands(opticalBands, NULL, TRUE)$addBands(thermalBand, NULL, TRUE)
  }
  
  L9 <-  L9$map(L9applyScaleFactors)
  # Select specific image index
  L9_index <- image_index
  L9_selectedImage <- ee$Image(L9$toList(L9$size())$get(L9_index))
  #L9_selectedImage$getInfo()
  
  # Visualise selected image
  L9_visualisation <- list(bands = c('SR_B4', 'SR_B3', 'SR_B2'), min = 0.0, max = 0.3)
  
  # Add layers to the map
  Map$centerObject(roi_ee, zoom = 12) 
  Map$addLayer(L9_selectedImage, L9_visualisation, 'Landsat 9: whole designated area', shown = tile_vis) +
    Map$addLayer(L9_selectedImage$clip(roi_ee), L9_visualisation, paste('Landsat 9: TMNR: ', IDs[image_index+1]), 
                 shown = roi_vis) +
    Map$addLayer(roi_ee, name = 'ROI', shown = roishp_vis)
}

# Change the parameters below for visualise the desired aerial imagery with the selected % land cloud cover
{
  L9_ee_image_index <- 20 # image index in earth engine env. (note: starts from 0)
  LCC <- 65 # percentage cloud cover
  
  # Call L7 imagery from its collection using function defined above
  L9_CollectionVis(cloud_cover_land = LCC,
                   image_index = L9_ee_image_index,
                   tile_vis = F,
                   roi_vis = T,
                   roishp_vis = F)
}

L9_collectionID <- L9$aggregate_array('system:index')$getInfo() # all the imageries available

(L9_available_imageries <- length(L9_collectionID)) # the size of collection

(L9_image_ID <- L9_collectionID[L9_ee_image_index+1]) # ID of the currently visualised imagery

# EXPORT LANDSAT 9 SR IMAGERIES -------------------------------------------

{
  
  # Function to export Landsat 9 imagery
  L9_Collection_Export <- function(cloud_cover_land, export_folder = folder_name) {
    # Filter Landsat 9 image collection
    L9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$
      filterDate('2021-01-01', '2023-12-31')$
      filterBounds(roi_ee)$
      filterMetadata('WRS_ROW', 'equals', 84)$
      filterMetadata('WRS_PATH', 'equals', 175)$
      filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
    
    # Get list of image IDs
    L9id <- L9$aggregate_array('system:index')
    
    # Apply scaling factors
    L9applyScaleFactors <- function(image) {
      opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
      thermalBand <- image$select('ST_B.*')$multiply(0.00341802)$add(149.0)
      image$addBands(opticalBands, NULL, TRUE)$addBands(thermalBand, NULL, TRUE)
    }
    
    # Apply scaling factors to the collection
    L9 <- L9$map(L9applyScaleFactors)
    
    # Function to export each image to Google Drive
    export_image <- function(image_id) {
      image <- L9$filter(ee$Filter$eq('system:index', image_id))$first()
      task <- ee_image_to_drive(
        image = image$toFloat(),
        description = as.character(image_id),
        folder = export_folder,
        scale = 30,
        region = roi_ee$geometry(),
        crs = 'EPSG:32734', # WGS 84 / UTM zone 34S
        maxPixels = 1e13,
        fileFormat = "GeoTIFF",
        formatOptions = list(cloudOptimized = TRUE)
      )
      task$start()
    }
    
    # Evaluate the image IDs and export each one
    L9id$getInfo() %>% lapply(export_image)
  }
  
  # Run the function to export Landsat 9 imagery
  L9_Collection_Export(cloud_cover_land = 65, export_folder = 'RGEE Landsat Collection')
  
}



# SENTINEL 2 MSI TOA IMAGE COLLECTION -------------------------------------
# /**
#   * Function to mask clouds using the Sentinel-2 QA band
# * @param {ee.Image} image Sentinel-2 image
# * @return {ee.Image} cloud masked Sentinel-2 image
# */
  
# maskS2clouds <- function (image) {
#     qa <- image$select('QA60')
#     
#     # Bits 10 and 11 are clouds and cirrus, respectively
#     cloudBitMask <- bitwShiftL(1, 10)
#     cirrusBitMask <- bitwShiftL(1, 11)
#     
#   # Both flags should be set to zero, indicating clear conditions.
#     mask <-  qa$bitwiseAnd(cloudBitMask)$eq(0)$
#       And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
#     
#     return (image$updateMask(mask)$divide(10000))
#   }

S2_TOA_collection <- function(cloud_cover_land){
  
  S2_TOA <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED")$
    filterDate('2015-01-01', '2017-03-28')$
    filterBounds(roi_ee)$
    filterMetadata('MGRS_TILE', 'equals', '34HBH')$
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud_cover_land))
    return(S2_TOA$aggregate_array('system:index')$getInfo())
  
}

S2_TOA_5_collection_ID <- S2_TOA_collection(5)
S2_TOA_10_collection_ID <- S2_TOA_collection(10)
S2_TOA_15_collection_ID <- S2_TOA_collection(15)
S2_TOA_20_collection_ID <- S2_TOA_collection(20)
S2_TOA_25_collection_ID <- S2_TOA_collection(25)
S2_TOA_30_collection_ID <- S2_TOA_collection(30)

# Extract the date part and convert to date class
S2_TOA_5_collection_date <- as.Date(str_extract(S2_TOA_5_collection_ID, "\\d{8}"), format = "%Y%m%d")
S2_TOA_10_collection_date <- as.Date(str_extract(S2_TOA_10_collection_ID, "\\d{8}"), format = "%Y%m%d")
S2_TOA_15_collection_date <- as.Date(str_extract(S2_TOA_15_collection_ID, "\\d{8}"), format = "%Y%m%d")
S2_TOA_20_collection_date <- as.Date(str_extract(S2_TOA_20_collection_ID, "\\d{8}"), format = "%Y%m%d")
S2_TOA_25_collection_date <- as.Date(str_extract(S2_TOA_25_collection_ID, "\\d{8}"), format = "%Y%m%d")
S2_TOA_30_collection_date <- as.Date(str_extract(S2_TOA_30_collection_ID, "\\d{8}"), format = "%Y%m%d")

# The -1 from the code below is to obtain the imageries with the correct earth engine index which starts with 0
which((S2_TOA_10_collection_ID %in% S2_TOA_5_collection_ID) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
S2_TOA_10_collection_ID[which((S2_TOA_10_collection_ID %in% S2_TOA_5_collection_ID) == F)] # which date are those imageries from the 10% LCC threshold

which((S2_TOA_15_collection_ID %in% S2_TOA_10_collection_ID) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
S2_TOA_15_collection_ID[which((S2_TOA_15_collection_ID %in% S2_TOA_10_collection_ID) == F)] # which date are those imageries from the 15% LCC threshold

which((S2_TOA_20_collection_ID %in% S2_TOA_15_collection_ID) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
S2_TOA_20_collection_ID[which((S2_TOA_20_collection_ID %in% S2_TOA_15_collection_ID) == F)] # which date are those imageries from the 20% LCC threshold

which((S2_TOA_25_collection_ID %in% S2_TOA_20_collection_ID) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
S2_TOA_25_collection_ID[which((S2_TOA_25_collection_ID %in% S2_TOA_20_collection_ID) == F)] # which date are those imageries from the 20% LCC threshold

which((S2_TOA_30_collection_ID %in% S2_TOA_25_collection_ID) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
S2_TOA_30_collection_ID[which((S2_TOA_30_collection_ID %in% S2_TOA_25_collection_ID) == F)] # which date are those imageries from the 20% LCC threshold

# Function to visualise the desired imagery from the available collection
S2_TOA_CollectionVis <- function(cloud_cover_land, image_index, tile_vis, roi_vis, roishp_vis){
  # Filter Sentinel-2 image collection
  S2_TOA <<- ee$ImageCollection("COPERNICUS/S2_HARMONIZED")$
    filterDate('2015-01-01', '2017-03-28')$
    filterBounds(roi_ee)$
    filterMetadata('MGRS_TILE', 'equals', '34HBH')$
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud_cover_land))

  IDs <- S2_TOA$aggregate_array('system:index')$getInfo()
  # ee_print(S2_TOA)

  # Select specific image index
  S2_TOA_index <- image_index
  S2_TOA_selectedImage <- ee$Image(S2_TOA$toList(S2_TOA$size())$get(S2_TOA_index))
  #S2_TOA_selectedImage$getInfo()
  
  # Visualise selected image
  S2_TOA_visualisation <- list(bands = c('B4', 'B3', 'B2'), min = 0.0, max = 0.3)
  
  # Add layers to the map
  Map$centerObject(roi_ee, zoom = 13) 
  Map$addLayer(S2_TOA_selectedImage$divide(10000), S2_TOA_visualisation, 'Sentinel 2 TOA: whole designated area', shown = tile_vis) +
    Map$addLayer(S2_TOA_selectedImage$clip(roi_ee)$divide(10000), S2_TOA_visualisation, paste('Sentinel 2 TOA: TMNR: ', IDs[image_index+1]), 
                 shown = roi_vis) +
    Map$addLayer(roi_ee, name = 'ROI', shown = roishp_vis)
}


# Change the parameters below for visualise the desired aerial imagery with the selected % land cloud cover
{
  S2_TOA_ee_image_index <- 73 # image index in earth engine env. (note: starts from 0)
  LCC <- 5 # percentage cloud cover
  
  # Call L7 imagery from its collection using function defined above
  S2_TOA_CollectionVis(cloud_cover_land = LCC,
                   image_index = S2_TOA_ee_image_index,
                   tile_vis = F,
                   roi_vis = T,
                   roishp_vis = F)
}

S2_TOA_collectionID <- S2_TOA$aggregate_array('system:index')$getInfo() # all the imageries available

(S2_TOA_available_imageries <- length(S2_TOA_collectionID)) # the size of collection

(S2_TOA_image_ID <- S2_TOA_collectionID[S2_TOA_ee_image_index+1]) # ID of the currently visualised imagery



# EXPORT SENTINEL 2 TOA IMAGERIES -----------------------------------------

{
  
  # Function to export Sentinel 2 TOA imagery
  S2_TOA_Collection_Export <- function(cloud_cover_land, export_folder = folder_name) {
    # Filter Sentinel 2 TOA image collection
    S2_TOA <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED")$
      filterDate('2015-01-01', '2017-03-28')$
      filterBounds(roi_ee)$
      filterMetadata('MGRS_TILE', 'equals', '34HBH')$
      filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud_cover_land))
    
    # Get list of image IDs
    S2_TOA_id <- S2_TOA$aggregate_array('system:index')
    
    # Function to export each image to Google Drive
    export_image <- function(image_id) {
      image <- S2_TOA$filter(ee$Filter$eq('system:index', image_id))$first()
      task <- ee_image_to_drive(
        image = image$divide(10000),
        description = as.character(image_id),
        folder = export_folder,
        scale = 30, # upsample to 30m resolution
        region = roi_ee$geometry(),
        crs = 'EPSG:32734', # WGS 84 / UTM zone 34S
        maxPixels = 1e13,
        fileFormat = "GeoTIFF",
        formatOptions = list(cloudOptimized = TRUE)
      )
      task$start()
    }
    
    # Evaluate the image IDs and export each one
    S2_TOA_id$getInfo() %>% lapply(export_image)
  }
  
  # Run the function to export Sentinel 2 TOA imagery
  S2_TOA_Collection_Export(cloud_cover_land = 5, export_folder = 'RGEE Landsat Collection')
  
}

# SENTINEL 2 MSI SR IMAGE COLLECTION --------------------------------------

S2_SR_collection <- function(cloud_cover_land){
  
  S2_SR <<- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filterDate('2017-03-28', '2023-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('MGRS_TILE', 'equals', '34HBH')$
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud_cover_land))
  return(S2_SR$aggregate_array('system:index')$getInfo())
  
}
S2_SR_5_collection_ID <- S2_SR_collection(5)
S2_SR_10_collection_ID <- S2_SR_collection(10)
S2_SR_15_collection_ID <- S2_SR_collection(15)

# Extract the date part and convert to date class
S2_SR_5_collection_date <- as.Date(str_extract(S2_SR_5_collection_ID, "\\d{8}"), format = "%Y%m%d")
S2_SR_10_collection_date <- as.Date(str_extract(S2_SR_10_collection_ID, "\\d{8}"), format = "%Y%m%d")
S2_SR_15_collection_date <- as.Date(str_extract(S2_SR_15_collection_ID, "\\d{8}"), format = "%Y%m%d")

# The -1 from the code below is to obtain the imageries with the correct earth engine index which starts with 0
which((S2_SR_10_collection_ID %in% S2_SR_5_collection_ID) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
S2_SR_10_collection_ID[which((S2_SR_10_collection_ID %in% S2_SR_5_collection_ID) == F)] # which date are those imageries from the 10% LCC threshold

which((S2_SR_15_collection_ID %in% S2_SR_10_collection_ID) == F)-1 # index of imagery to verify from a higher % cloud cover excluding the ones from a lower % cloud cover
S2_SR_15_collection_ID[which((S2_SR_15_collection_ID %in% S2_SR_10_collection_ID) == F)] # which date are those imageries from the 15% LCC threshold

# Function to visualise the desired imagery from the available collection
S2_SR_CollectionVis <- function(cloud_cover_land, image_index, tile_vis, roi_vis, roishp_vis){
  # Filter Sentinel-2 image collection
  S2_SR <<- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filterDate('2017-03-28', '2023-12-31')$
    filterBounds(roi_ee)$
    filterMetadata('MGRS_TILE', 'equals', '34HBH')$
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud_cover_land))
  
  IDs <- S2_SR$aggregate_array('system:index')$getInfo()
  # ee_print(S2_SR)
  
  # Select specific image index
  S2_SR_index <- image_index
  S2_SR_selectedImage <- ee$Image(S2_SR$toList(S2_SR$size())$get(S2_SR_index))
  #S2_SR_selectedImage$getInfo()
  
  # Visualise selected image
  S2_SR_visualisation <- list(bands = c('B4', 'B3', 'B2'), min = 0.0, max = 0.3)
  
  # Add layers to the map
  Map$centerObject(roi_ee, zoom = 13) 
  Map$addLayer(S2_SR_selectedImage$divide(10000), S2_SR_visualisation, 'Sentinel 2 SR: whole designated area', shown = tile_vis) +
    Map$addLayer(S2_SR_selectedImage$clip(roi_ee)$divide(10000), S2_SR_visualisation, paste('Sentinel 2 SR: TMNR: ', IDs[image_index+1]), 
                 shown = roi_vis) +
    Map$addLayer(roi_ee, name = 'ROI', shown = roishp_vis)
}

# Change the parameters below for visualise the desired aerial imagery with the selected % land cloud cover
{
  S2_SR_ee_image_index <- 40 # image index in earth engine env. (note: starts from 0)
  LCC <- 15 # percentage cloud cover
  
  # Call L7 imagery from its collection using function defined above
  S2_SR_CollectionVis(cloud_cover_land = LCC,
                       image_index = S2_SR_ee_image_index,
                       tile_vis = F,
                       roi_vis = T,
                       roishp_vis = F)
}

S2_SR_collectionID <- S2_SR$aggregate_array('system:index')$getInfo() # all the imageries available

(S2_SR_available_imageries <- length(S2_SR_collectionID)) # the size of collection

(S2_SR_image_ID <- S2_SR_collectionID[S2_SR_ee_image_index+1]) # ID of the currently visualised imagery


# EXPORT SENTINEL 2 SR IMAGERIES -----------------------------------------
 
 {
   
   # Function to export Sentinel 2 SR imagery
   S2_SR_Collection_Export <- function(cloud_cover_land, export_folder = folder_name) {
     # Filter Sentinel 2 SR image collection
     S2_SR <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
       filterDate('2017-03-28', '2023-12-31')$
       filterBounds(roi_ee)$
       filterMetadata('MGRS_TILE', 'equals', '34HBH')$
       filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cloud_cover_land))
     
     # Get list of image IDs
     S2_SR_id <- S2_SR$aggregate_array('system:index')
     
     # Function to export each image to Google Drive
     export_image <- function(image_id) {
       image <- S2_SR$filter(ee$Filter$eq('system:index', image_id))$first()
       task <- ee_image_to_drive(
         image = image$divide(10000),
         description = as.character(image_id),
         folder = export_folder,
         scale = 30, # upsample to 30m resolution
         region = roi_ee$geometry(),
         crs = 'EPSG:32734', # WGS 84 / UTM zone 34S
         maxPixels = 1e13,
         fileFormat = "GeoTIFF",
         formatOptions = list(cloudOptimized = TRUE)
       )
       task$start()
     }
     
     # Evaluate the image IDs and export each one
     S2_SR_id$getInfo() %>% lapply(export_image)
   }
   
   # Run the function to export Sentinel 2 SR imagery
   S2_SR_Collection_Export(cloud_cover_land = 5, export_folder = 'RGEE Landsat Collection')
   
 }
 






















































