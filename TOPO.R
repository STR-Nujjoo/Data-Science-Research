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

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)


# ELEVATION ---------------------------------------------------------------

# Import elevation data
elevation_files <- list.files('Raw Data/Topographical Data/Elevation/', pattern = '.tif')
# Cast as raster
elevation_raster <- raster(paste0('Raw Data/Topographical Data/Elevation/', elevation_files[1]))
# project raster to  EPSG:32734 (WGS 84 / UTM zone 34S)
elevation_raster_trans <- projectRaster(elevation_raster,
                                        crs = crs(roi_trans),
                                        method = 'ngb') |>
  crop(roi_trans) |> # crop raster
  mask(roi_trans) # mask raster

# Visualise elevation data
plot(elevation_raster_trans, col = terrain.colors(200))
# plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, add = T)

# Save result
save(elevation_raster_trans, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Elevation/elevation_raster_trans.Rdata')
writeRaster(elevation_raster_trans, 
            filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Elevation/elevation_raster_trans', 
            format = "GTiff", 
            overwrite = TRUE)

# SLOPE -------------------------------------------------------------------

# Import slope data
slope_files <- list.files('Raw Data/Topographical Data/Slope/', pattern = '.tif')
# Cast as raster
slope_raster <- raster(paste0('Raw Data/Topographical Data/Slope/', slope_files[1]))
# project raster to  EPSG:32734 (WGS 84 / UTM zone 34S)
slope_raster_trans <- projectRaster(slope_raster,
                                        crs = crs(roi_trans),
                                        method = 'ngb') |>
  crop(roi_trans) |> # crop raster
  mask(roi_trans) # mask raster

# Visualise slope data
plot(slope_raster_trans, col = topo.colors(200))
# plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, add = T)

# Save result
save(slope_raster_trans, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Slope/slope_raster_trans.Rdata')
writeRaster(slope_raster_trans, 
            filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Slope/slope_raster_trans', 
            format = "GTiff", 
            overwrite = TRUE)

# ASPECT ------------------------------------------------------------------

# Import aspect data
aspect_files <- list.files('Raw Data/Topographical Data/Aspect/', pattern = '.tif')
# Cast as raster
aspect_raster <- raster(paste0('Raw Data/Topographical Data/Aspect/', aspect_files[1]))
# project raster to  EPSG:32734 (WGS 84 / UTM zone 34S)
aspect_raster_trans <- projectRaster(aspect_raster,
                                    crs = crs(roi_trans),
                                    method = 'ngb') |>
  crop(roi_trans) |> # crop raster
  mask(roi_trans) # mask raster

# Visualise aspect data
plot(aspect_raster_trans, col = rainbow(200))
# plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, add = T)

# Save result
save(aspect_raster_trans, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Aspect/aspect_raster_trans.Rdata')
writeRaster(aspect_raster_trans, 
            filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Aspect/aspect_raster_trans', 
            format = "GTiff", 
            overwrite = TRUE)


