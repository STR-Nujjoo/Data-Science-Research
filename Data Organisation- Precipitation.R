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

# Creating color ramp for precipitation plot
blue_ramp <- colorRampPalette(c("#d0e1f9", "#4d8dd6", "#2b6ac2", "#0d4aa2", "#002b70"))

# PRECIPITATION DATA IMPORT -----------------------------------------------
# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import precipitation file names
total_precipitation_filenames_2010_2021 <- list.files('Raw Data/Climatological Data/Precipitation/WorldClim/wc2.1_cruts4.06_2.5m_prec_2010-2021', pattern = '.tif')
total_precipitation_filenames_2014_2021 <- total_precipitation_filenames_2010_2021[49:length(total_precipitation_filenames_2010_2021)] # filter concerning precipitation raster as from 2014

raster_to_point_prec <- function(index, plot = T){
  # Import precipitation rasters
  total_precipitation_raster <- raster(paste0('Raw Data/Climatological Data/Precipitation/WorldClim/wc2.1_cruts4.06_2.5m_prec_2010-2021/', total_precipitation_filenames_2014_2021[index]))
  raster_name <- str_extract(total_precipitation_filenames_2014_2021[index], "\\d{4}-\\d{2}") # extract date from tif file
  names(total_precipitation_raster) <- paste("prec", raster_name)
  # e <- drawExtent()
  large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
  total_precipitation_raster_crop <- crop(total_precipitation_raster, large_extent) # crop raster to extent
  total_precipitation_raster_proj <<- projectRaster(total_precipitation_raster_crop, crs = crs(roi_trans)) # project raster to EPSG:32734 (WGS 84 / UTM zone 34S)
  precipitation_df <- as.data.frame(total_precipitation_raster_proj, xy = T) # extract data into a data frame
  colnames(precipitation_df) <- c('x', 'y', 'total_precipitation') # rename column
  precipitation_df_NA_omit <- na.omit(precipitation_df) # omit NAs rows
  # Convert normal data frame to spatial point data frame
  precipitation_raster_to_point <- as_Spatial(st_as_sf(precipitation_df_NA_omit, 
                                                       coords = c("x", "y"), 
                                                       crs = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs"))
  
  if(plot == T){
    plot(total_precipitation_raster_proj, main = paste(raster_name, 'Total Precipitation'))
    plot(roi_trans, col = 'transparent', border = 'red', lwd = 1, add = T)
    plot(precipitation_raster_to_point, add = T, pch = 3, col = 'black')
  }
  
  return(precipitation_raster_to_point)
}

precipitation_raster_to_point_list <- pblapply(seq_along(total_precipitation_filenames_2014_2021), raster_to_point_prec)


# IDW ---------------------------------------------------------------------

# Creating 30x30 spatial resolution grid with defined large extent above
IDW_precipitation_grid <- expand.grid(
  x = seq(
    from = extent(total_precipitation_raster_proj)@xmin,
    to = extent(total_precipitation_raster_proj)@xmax,
    by = 30 # cell size in m 
  ),
  y = seq(
    from = extent(total_precipitation_raster_proj)@ymin,
    to = extent(total_precipitation_raster_proj)@ymax,
    by = 30 # cell size in m
  )
)

coordinates(IDW_precipitation_grid) <- ~ x + y # cast it into a SpatialPoints object
proj4string(IDW_precipitation_grid) <- proj4string(precipitation_raster_to_point_list[[1]]) # assign appropriate spatial reference system to grid
gridded(IDW_precipitation_grid) <- T # cast grid from SpatialPoints object into SpatialPixels object


# IDW function
IDW_prec <- function(index, plot = T){
  # IDW model
  beta <- 2 
  IDW_total_precipitation <- gstat::gstat(formula = total_precipitation ~ 1, # interpolate based on total precipitation
                                   data = precipitation_raster_to_point_list[[index]], 
                                   nmax = length(precipitation_raster_to_point_list[[index]]), 
                                   set = list(idp = beta))
  
  IDW_total_precipitation_pred <- predict(IDW_total_precipitation, IDW_precipitation_grid) # IDW interpolation using IDW model
  
  IDW_total_precipitation_raster <- raster(IDW_total_precipitation_pred) # convert IDW data into raster
  IDW_total_precipitation_raster_TMNR <- crop(IDW_total_precipitation_raster, roi_trans) # crop data to study area extent
  IDW_total_precipitation_raster_TMNR <- mask(IDW_total_precipitation_raster_TMNR, roi_trans) # mask data to study area extent
  names(IDW_total_precipitation_raster_TMNR) <- paste("IDW", str_extract(total_precipitation_filenames_2014_2021[index], "\\d{4}-\\d{2}")) # rename IDW output
  
  if(plot == T){
    plot(IDW_total_precipitation_raster_TMNR,
         col = blue_ramp(5),
         main = paste(str_extract(total_precipitation_filenames_2014_2021[index], "\\d{4}-\\d{2}"), 
                      "IDW Total Precipitation (mm)"))
  }
  
  return(IDW_total_precipitation_raster_TMNR)
}

precipitation_IDW_list <- pblapply(1:2, FUN = IDW_prec)

# example of the IDW precipitation plot outside the function
plot(precipitation_IDW_list[[1]],
     col = blue_ramp(5),
     main = paste(str_extract(total_precipitation_filenames_2014_2021[1], "\\d{4}-\\d{2}"), 
                  "IDW Total Precipitation (mm)"))

# Must perform some EDA for the precipitation data below...

mean(unlist(na.omit(as.data.frame(values(precipitation_IDW_list[[1]])))))

mean(unlist(na.omit(as.data.frame(values(precipitation_IDW_list[[2]])))))

precipitation_EDA_df <- data.frame(mean_total_prec = c(mean(unlist(na.omit(as.data.frame(values(precipitation_IDW_list[[1]]))))), 
                                 mean(unlist(na.omit(as.data.frame(values(precipitation_IDW_list[[2]])))))),
           date = c(str_extract(total_precipitation_filenames_2014_2021[1], "\\d{4}-\\d{2}"),
                    str_extract(total_precipitation_filenames_2014_2021[2], "\\d{4}-\\d{2}")))


plot(precipitation_EDA_df$mean_total_prec, type = 'b', col = 'steelblue')



format(as.Date(paste0(precipitation_EDA_df$date, "-01"), format = "%Y-%m-%d"), "%Y-%m")
str_extract(total_precipitation_filenames_2014_2021[1], "\\d{4}-\\d{2}")












