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
  library(jcolors)
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

# FORMAL PLOT FOR THESIS

 # Maximum elevation value

max_elev <- as.data.frame(elevation_raster_trans, xy = T) %>%
  filter(Band_1==max(elevation_raster_trans|>values(), na.rm = TRUE)) %>%
  mutate(label = "1084m")%>%
  st_as_sf(coords = c("x", "y"), crs = crs(roi_trans))

elevation_plot <- tm_shape(elevation_raster_trans)+
  tm_raster(title = 'Elevation (m)',
            palette = terrain.colors(200), 
            style = 'cont', 
            breaks = seq(values(elevation_raster_trans)%>%na.omit()%>%min(), values(elevation_raster_trans)%>%na.omit()%>%max(), by = 100))+  
  tm_shape(max_elev)+
  tm_symbols(size = 0.1, col = "red", shape = 24, border.col = "black") +
  tm_text("label", size = 0.5, just = "bottom", ymod=-0.6) +
  tm_graticules(labels.size = 0.5, n.x = 3, n.y = 3, lines = F)+
tm_layout(legend.text.size = 0.37)


slope_plot <- tm_shape(slope_raster_trans)+
  tm_raster(title = 'Slope (º)',
            palette = jcolors::jcolors("pal12"), 
            style = 'cont', 
            breaks = seq(values(slope_raster_trans)%>%na.omit()%>%min(), values(slope_raster_trans)%>%na.omit()%>%max(), by = 10))+  
  tm_graticules(labels.size = 0.5, n.x = 3, n.y = 3, lines = F)+
tm_layout(legend.text.size = 0.37)


aspect_plot <- tm_shape(aspect_raster_trans)+
  tm_raster(title = 'Aspect',
            palette = c('gray',rainbow(200)),
            style = 'fixed',
            breaks = c(-1, 0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360),
            labels = c('Flat (-1)',
                       'N (0-22.5)',
                       'NE (22.5-67.5)', 
                       'E (67.5-112.5)', 
                       'SE (112.5-157.5)', 
                       'S (157.5-202.5)',
                       'SW (202.5-247.5)', 
                       'W (247.5-292.5)',
                       'NW (292.5-337.5)', 
                       'N (337.5-360)'))+  
  tm_graticules(labels.size = 0.5, n.x = 3, n.y = 3, lines = F)+
tm_layout(legend.text.size = 0.37)

combined_map <- tmap_arrange(elevation_plot,
             slope_plot,
             aspect_plot,
             ncol = 2,
             nrow=2)

# Save the combined map as a PDF
tmap_save(combined_map, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/combined_topo_map.pdf", width = 6.56, height = 6)























































































