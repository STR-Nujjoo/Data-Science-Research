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

# Creating color ramp for precipitation plot
blue_ramp <- colorRampPalette(c("#E7FBFF", "#C6DBFF", "#6BAED6", "#2171B5", "#08306B"))

# PRECIPITATION DATA IMPORT -----------------------------------------------
# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import precipitation file names
precipitation_filenames_2014_2023 <- list.files('Raw Data/Climatological Data/Precipitation/CHIRPS', pattern = '.tif')


CHIRPS_raster_to_point_prec <- function(index, plot = T){
  # Import precipitation rasters
  index <- index
  precipitation_raster <- raster(paste0('Raw Data/Climatological Data/Precipitation/CHIRPS/', precipitation_filenames_2014_2023[index]))
  raster_name <- str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}") # extract date from tif file
  names(precipitation_raster) <- paste("prec", raster_name)
  # e <- drawExtent()
  large_extent <- extent(18.16773, 18.75474, -34.45613, -33.58649) # defining an extent larger than the study area for spatial interpolation
  precipitation_raster_crop <- crop(precipitation_raster, large_extent) # crop raster to extent
  precipitation_raster_proj <<- projectRaster(precipitation_raster_crop, crs = crs(roi_trans)) # project raster to EPSG:32734 (WGS 84 / UTM zone 34S)
  values(precipitation_raster_proj) <- ifelse(values(precipitation_raster_proj)<1, NA, values(precipitation_raster_proj)) # convert huge negative values (e.g, -9999 to NA)
  precipitation_df <- as.data.frame(precipitation_raster_proj, xy = T) # extract data into a data frame
  colnames(precipitation_df) <- c('x', 'y', 'precipitation') # rename column
  precipitation_df_NA_omit <- na.omit(precipitation_df) # omit NAs rows
  # Convert normal data frame to spatial point data frame
  precipitation_raster_to_point <- as_Spatial(st_as_sf(precipitation_df_NA_omit, 
                                                       coords = c("x", "y"), 
                                                       crs = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs"))

    if(plot == T){
    plot(precipitation_raster_proj, main = paste(raster_name, 'Precipitation'))
    plot(roi_trans, col = 'transparent', border = 'red', lwd = 1, add = T)
    plot(precipitation_raster_to_point, add = T, pch = 3, col = 'black')
  }

  return(precipitation_raster_to_point)
}

precipitation_raster_to_point_list <- pblapply(seq_along(precipitation_filenames_2014_2023), CHIRPS_raster_to_point_prec)

# Individual Visualisation example- simply run function againn with appropriate index
CHIRPS_raster_to_point_prec(index = 120)


# IDW ---------------------------------------------------------------------

# Creating 30x30 spatial resolution grid with defined large extent above
IDW_precipitation_grid <- expand.grid(
  x = seq(
    from = extent(precipitation_raster_proj)@xmin,
    to = extent(precipitation_raster_proj)@xmax,
    by = 30 # cell size in m 
  ),
  y = seq(
    from = extent(precipitation_raster_proj)@ymin,
    to = extent(precipitation_raster_proj)@ymax,
    by = 30 # cell size in m
  )
)

coordinates(IDW_precipitation_grid) <- ~ x + y # cast it into a SpatialPoints object
proj4string(IDW_precipitation_grid) <- proj4string(precipitation_raster_to_point_list[[1]]) # assign appropriate spatial reference system to grid
gridded(IDW_precipitation_grid) <- T # cast grid from SpatialPoints object into SpatialPixels object

# IDW functions

PRECIPITATION_IDW <-function(index, beta_vector){
  IDW_CHIRPS_optimal_beta <- function(index = index, beta){
    rast_data <- precipitation_raster_to_point_list[[index]]
    beta <- beta
    g <- gstat::gstat(formula = precipitation ~ 1, # interpolate based on total precipitation
                      data = rast_data, 
                      nmax = length(rast_data), 
                      set = list(idp = beta))
    set.seed(1)
    cv_list <- pblapply(seq(2,10, by = 1), function(x) {gstat.cv(g, nfold = x)}) # generate CV using different folds
    SSR <- sapply(1:length(cv_list), function (x) {sum((cv_list[[x]]@data$residual)^2)}) # calculate sum of square of the residuals for each nfold
    
    return(cbind(nfold = seq(2,10, by = 1), beta, SSR))
  } 
  
  beta_vector <- beta_vector 
  IDW_CHIRPS_optimal_beta_list <- pblapply(beta_vector, function(x){IDW_CHIRPS_optimal_beta(index, beta = x)})
  result <- do.call(rbind, IDW_CHIRPS_optimal_beta_list) |> as.data.frame()
  
  IDW_CHIRPS_prec <- function(index = index, plot = T){
    # IDW model
    rast_data <- precipitation_raster_to_point_list[[index]]
    opt_beta <- result$beta[which.min(result$SSR)]
    IDW_precipitation <- gstat::gstat(formula = precipitation ~ 1, # interpolate based on total precipitation
                                      data = rast_data, 
                                      nmax = length(rast_data), 
                                      set = list(idp = opt_beta))
    
    IDW_precipitation_pred <- predict(IDW_precipitation, IDW_precipitation_grid) # IDW interpolation using IDW model
    
    IDW_precipitation_raster <- raster(IDW_precipitation_pred) # convert IDW data into raster
    IDW_precipitation_raster_TMNR <- crop(IDW_precipitation_raster, roi_trans) # crop data to study area extent
    IDW_precipitation_raster_TMNR <- mask(IDW_precipitation_raster_TMNR, roi_trans) # mask data to study area extent
    names(IDW_precipitation_raster_TMNR) <- paste("IDW", str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}")) # rename IDW output
    
    if(plot == T){
      plot(IDW_precipitation_raster_TMNR,
           col = blue_ramp(5),
           main = paste(str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}"), 
                        "IDW Precipitation (mm)"))
    }
    
    return(IDW_precipitation_raster_TMNR)
  }
  
  precipitation_IDW_list <- pblapply(1:index, IDW_CHIRPS_prec)
  
  return(precipitation_IDW_list)
}

IDW_precipitation_rasters <- pblapply(1:length(precipitation_filenames_2014_2023), 
                                      function(x) {PRECIPITATION_IDW(index = x, beta_vector = 2:5)})


##############################################################################################################

# #CHECK#
# IDW_CHIRPS_optimal_beta <- function(index, beta){
#   rast_data <- precipitation_raster_to_point_list[[index]]
#   beta <- beta
#   g <- gstat::gstat(formula = precipitation ~ 1, # interpolate based on total precipitation
#                     data = rast_data, 
#                     nmax = length(rast_data), 
#                     set = list(idp = beta))
#   set.seed(1)
#   cv_list <- pblapply(seq(2,10, by = 1), function(x) {gstat.cv(g, nfold = x)}) # generate CV using different folds
#   SSR <- sapply(1:length(cv_list), function (x) {sum((cv_list[[x]]@data$residual)^2)}) # calculate sum of square of the residuals for each nfold
#   
#   return(cbind(nfold = seq(2,10, by = 1), beta, SSR))
# } 
# 
# beta_vector <- 2:5 
# IDW_CHIRPS_optimal_beta_list <- pblapply(beta_vector, function(x){IDW_CHIRPS_optimal_beta(index = 120, beta = x)})
# result <- do.call(rbind, IDW_CHIRPS_optimal_beta_list) |> as.data.frame()
# 
# IDW_CHIRPS_prec <- function(index, plot = T){
#   # IDW model
#   rast_data <- precipitation_raster_to_point_list[[index]]
#   opt_beta <- result$beta[which.min(result$SSR)]
#   IDW_precipitation <- gstat::gstat(formula = precipitation ~ 1, # interpolate based on total precipitation
#                                     data = rast_data, 
#                                     nmax = length(rast_data), 
#                                     set = list(idp = opt_beta))
#   
#   IDW_precipitation_pred <- predict(IDW_precipitation, IDW_precipitation_grid) # IDW interpolation using IDW model
#   
#   IDW_precipitation_raster <- raster(IDW_precipitation_pred) # convert IDW data into raster
#   IDW_precipitation_raster_TMNR <- crop(IDW_precipitation_raster, roi_trans) # crop data to study area extent
#   IDW_precipitation_raster_TMNR <- mask(IDW_precipitation_raster_TMNR, roi_trans) # mask data to study area extent
#   names(IDW_precipitation_raster_TMNR) <- paste("IDW", str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}")) # rename IDW output
#   
#   if(plot == T){
#     plot(IDW_precipitation_raster_TMNR,
#          col = blue_ramp(5),
#          main = paste(str_extract(precipitation_filenames_2014_2023[index], "\\d{4}.\\d{2}"), 
#                       "IDW Precipitation (mm)"))
#   }
#   
#   return(IDW_precipitation_raster_TMNR)
# }
# 
# 
# IDW_CHIRPS_prec(index = 120)
# 
# 
# 
