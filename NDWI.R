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

# Colour ramp for NDWI
NDWI_colour_ramp <- colorRampPalette(c("brown", "yellow", "green"))(100)

# Create a function to compute 
NDWI_function <- function(data, index, plot = NULL){
  data_name <- data[[index]]@file@name # extract name from raster
  data <- clamp(data[[index]], 0, 1) # clamp value from 0 to 1- THIS STEP IS VERY IMPORTANT FOR THE NDVI CALC TO BE CORRECT!!!
  NIR <- data[[4]] # select NIR band
  SWIR1 <- data[[5]] # select SWIR1 band
  
  NDWI <- (NIR-SWIR1) /(NIR+SWIR1) # NDWI computation
  NDWI_reproj <- projectRaster(NDWI, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb') # projecting the raster for the sake of axis labels to be shown as lon-lat
  NDWI@file@name <- paste('NDWI:', as.Date(data_name, format = "%Y%m%d")) # rename raster
  
  if(plot == T){
    # Visualisation of NDWI rasters
    plot(NDWI_reproj,
         col = NDWI_colour_ramp,
         main = NDWI@file@name)
  }
  
  return(NDWI)
} 


# Calculating NDWI for all aerial imagery BEFORE INTERPOLATION
NDWI_rasters_before_interpolation <- pblapply(seq_along(all_aerial_imagery_before_interpolation),
                                              function(x){NDWI_function(data = all_aerial_imagery_before_interpolation,
                                                                        index = x,
                                                                        plot = T)})

# Calculating NDWI for all aerial imagery AFTER INTERPOLATION
NDWI_rasters_after_interpolation <- pblapply(seq_along(trimmed_df_aerial_imagery_after_interpolation),
                                             function(x){NDWI_function(data = trimmed_df_aerial_imagery_after_interpolation,
                                                                       index = x,
                                                                       plot = T)})


# Save object
save(NDWI_rasters_before_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2002-2023 (without interpolation)/NDWI_rasters_before_interpolation.Rdata')
save(NDWI_rasters_after_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2014-2023 (with interpolation)/NDWI_rasters_after_interpolation.Rdata')

# Save rasters in one folder on local machine or hard drive
{
  Save_raster <- function(data, index, path){
    
    file_path <- paste0(path, data[[index]]@file@name)
    
    return(writeRaster(data[[index]],
                       filename = file_path, format = "GTiff", overwrite = TRUE))
  }
  
  # Bulk Save!!!!
  pblapply(seq_along(NDWI_rasters_before_interpolation),
           function(x) {Save_raster(data = NDWI_rasters_before_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2002-2023 (without interpolation)/')})
  
  # Bulk Save!!!!
  pblapply(seq_along(NDWI_rasters_after_interpolation),
           function(x) {Save_raster(data = NDWI_rasters_after_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NDWI 2014-2023 (with interpolation)/')})
  
}


