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
  library(tmap)
}

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import vegetation type shapefile
veg_type <- readOGR('Raw Data/Vegetation Data/VegetationTypes_TMNR.shp')
veg_type_trans <- spTransform(veg_type, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

veg_colors <- c("#FFFF00", "#38A800", "#448970", "#89CD66", "#70A800", "#D1FF73", "#00734C")  # Define  vegetation colors

# Map each unique vegetation type to a color
veg_color_map <- setNames(veg_colors[1:length(veg_type_trans$NTNL_VGTN_)], 
                      veg_type_trans$NTNL_VGTN_)


# Visualising vegetation types data
plot(veg_type_trans, col = veg_color_map[veg_type_trans$NTNL_VGTN_], border = 'transparent')
legend("right", legend = st_as_sf(veg_type_trans)$NTNL_VGTN_, fill = colors[1:length(st_as_sf(veg_type_trans)$NTNL_VGTN_
)], cex = 0.5)


# Using tmap for visualisation
tm_shape(st_as_sf(veg_type_trans)) +
  tm_polygons("NTNL_VGTN_", 
              palette = veg_color_map,  # You can choose different color palettes like "Set3", "Dark2", etc.
              title = "Vegetation Types",
              border.col = "transparent") +
  tm_graticules(lines = TRUE,  # Adds graticule (grid) lines for lat/lon
                labels.size = 0.7,  # Adjusts label size for clarity
                col = "transparent",  # Color of the grid lines
                ticks= T) +  
  tm_layout(legend.outside = F, legend.title.size= 0.8, legend.text.size = 0.6)


