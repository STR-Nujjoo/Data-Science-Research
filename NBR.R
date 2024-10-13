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
  library(parallel)
}

# Colour ramp for NBR
NBR_colour_ramp <- colorRampPalette(c("brown", "yellow", "green"))(100)

# Create a function to compute 
NBR_function <- function(data, index, plot = NULL){
  data_name <- data[[index]]@file@name # extract name from raster
  data <- clamp(data[[index]], 0, 1) # clamp value from 0 to 1- THIS STEP IS VERY IMPORTANT FOR THE NDVI CALC TO BE CORRECT!!!
  NIR <- data[[4]] # select NIR band
  SWIR2 <- data[[6]] # select SWIR2 band
  
  NBR <- (NIR-SWIR2) /(NIR+SWIR2) # NBR computation
  NBR_reproj <- projectRaster(NBR, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb') # projecting the raster for the sake of axis labels to be shown as lon-lat
  NBR@file@name <- paste('NBR:', as.Date(data_name, format = "%Y%m%d")) # rename raster
  
  if(plot == T){
    # Visualisation of NBR rasters
    plot(NBR_reproj,
         col = NBR_colour_ramp,
         main = NBR@file@name)
  }
  
  return(NBR)
} 


# Calculating NBR for all aerial imagery BEFORE INTERPOLATION
NBR_rasters_before_interpolation <- pblapply(seq_along(all_aerial_imagery_before_interpolation),
                                              function(x){NBR_function(data = all_aerial_imagery_before_interpolation,
                                                                        index = x,
                                                                        plot = T)})

# Calculating NBR for all aerial imagery AFTER INTERPOLATION
NBR_rasters_after_interpolation <- pblapply(seq_along(trimmed_df_aerial_imagery_after_interpolation),
                                             function(x){NBR_function(data = trimmed_df_aerial_imagery_after_interpolation,
                                                                       index = x,
                                                                       plot = T)})


# Save object
save(NBR_rasters_before_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NBR 2002-2023 (without interpolation)/NBR_rasters_before_interpolation.Rdata')
save(NBR_rasters_after_interpolation, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NBR 2014-2023 (with interpolation)/NBR_rasters_after_interpolation.Rdata')

# Save rasters in one folder on local machine or hard drive
{
  Save_raster <- function(data, index, path){
    
    file_path <- paste0(path, data[[index]]@file@name)
    
    return(writeRaster(data[[index]],
                       filename = file_path, format = "GTiff", overwrite = TRUE))
  }
  
  # Bulk Save!!!!
  pblapply(seq_along(NBR_rasters_before_interpolation),
           function(x) {Save_raster(data = NBR_rasters_before_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NBR 2002-2023 (without interpolation)/')})
  
  # Bulk Save!!!!
  pblapply(seq_along(NBR_rasters_after_interpolation),
           function(x) {Save_raster(data = NBR_rasters_after_interpolation,
                                    index = x,
                                    path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/NBR 2014-2023 (with interpolation)/')})
  
}


