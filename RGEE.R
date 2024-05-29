# RGEE RESOURCES: https://csaybar.github.io/rgee-examples/

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
library(sf)
library(geojsonio)
library(tidyverse)
library(stars)
library(stringr)

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
  L7_ee_image_index <- 82 # image index in earth engine env. (note: starts from 0)
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

# Export trial
{
  # EXPORT TRIAL
  # # Function to export Landsat 7 imagery
  # L7_Collection_Export <- function(cloud_cover_land, export_folder = folder_name) {
  #   # Filter Landsat 7 image collection
  #   L7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$
  #     filterDate('2002-01-01', '2013-12-31')$
  #     filterBounds(roi_ee)$
  #     filterMetadata('WRS_ROW', 'equals', 84)$
  #     filterMetadata('WRS_PATH', 'equals', 175)$
  #     filterMetadata('CLOUD_COVER_LAND', 'less_than', cloud_cover_land)
  #   
  #   # Get list of image IDs
  #   L7id <- L7$aggregate_array('system:index')
  #   
  #   # Function to apply scaling factors
  #   L7applyScaleFactors <- function(image) {
  #     opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
  #     thermalBand <- image$select('ST_B6')$multiply(0.00341802)$add(149.0)
  #     image$addBands(opticalBands, NULL, TRUE)$addBands(thermalBand, NULL, TRUE)
  #   }
  #   
  #   # Apply scaling factors to the collection
  #   L7 <- L7$map(L7applyScaleFactors)
  #   
  #   # Function to export each image to Google Drive
  #   export_image <- function(image_id) {
  #     image <- L7$filter(ee$Filter$eq('system:index', image_id))$first()
  #     task <- ee_image_to_drive(
  #       image = image$toFloat(),
  #       description = as.character(image_id),
  #       folder = export_folder,
  #       scale = 30,
  #       region = roi_ee$geometry(),
  #       crs = 'EPSG:32734',
  #       maxPixels = 1e13,
  #       fileFormat = "GeoTIFF",
  #       formatOptions = list(cloudOptimized = TRUE)
  #     )
  #     task$start()
  #   }
  #   
  #   # Evaluate the image IDs and export each one
  #   L7id$getInfo() %>% lapply(export_image)
  # }
  # 
  # # Run the function to export Landsat 7 imagery
  # L7_Collection_Export(cloud_cover_land = 5, export_folder = 'RGEE Landsat Collection')
  
}
























































































