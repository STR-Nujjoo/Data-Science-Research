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

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import SANPARK fire data
sanpark_fire_data <- list.files('Raw Data/SANParks fire data/1962-2022', pattern = '.shp')
sanpark_fire_data <- sanpark_fire_data[seq(1, length(sanpark_fire_data), by = 2)]
sanpark_fire_data <- sanpark_fire_data[40:length(sanpark_fire_data)] # Trim data from 2002 to 2022 only


# FIRE HOTSPOT VISUALISATION FROM 2002 TO 2022 ----------------------------

sanpark_fire_shpfile_list <- pblapply(seq_along(sanpark_fire_data), function(x){
  readOGR(paste0('Raw Data/SANParks fire data/1962-2022/', sanpark_fire_data[x]))
}) # read in all shapefiles in a list

# Rearrange data columns for consistency and assign original coordinate system to shapefiles
sanpark_fire_shpfile_df_list <- pblapply(seq_along(sanpark_fire_data), function(x) {
  proj4string(sanpark_fire_shpfile_list[[x]]) <- '+proj=tmerc +lat_0=0 +lon_0=19 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # Lo19 Hartebbesthoek94
  st_as_sf(sanpark_fire_shpfile_list[[x]])  %>% # convert spatial features to data frame
  select(FIRETYPE, FIRECAUSE, YEAR, 
         STARTDATE,XHECTARES, geometry)})

# Merge all the shapefiles
sanpark_fire_shpfile_combind_list <- rbind(sanpark_fire_shpfile_df_list[[1]],
                                               sanpark_fire_shpfile_df_list[[2]],
                                               sanpark_fire_shpfile_df_list[[3]],
                                               sanpark_fire_shpfile_df_list[[4]],
                                               sanpark_fire_shpfile_df_list[[5]],
                                               sanpark_fire_shpfile_df_list[[6]],
                                               sanpark_fire_shpfile_df_list[[7]],
                                               sanpark_fire_shpfile_df_list[[8]],
                                               sanpark_fire_shpfile_df_list[[9]],
                                               sanpark_fire_shpfile_df_list[[10]],
                                               sanpark_fire_shpfile_df_list[[11]],
                                               sanpark_fire_shpfile_df_list[[12]],
                                               sanpark_fire_shpfile_df_list[[13]],
                                               sanpark_fire_shpfile_df_list[[14]],
                                               sanpark_fire_shpfile_df_list[[15]],
                                               sanpark_fire_shpfile_df_list[[16]],
                                               sanpark_fire_shpfile_df_list[[17]],
                                               sanpark_fire_shpfile_df_list[[18]],
                                               sanpark_fire_shpfile_df_list[[19]],
                                               sanpark_fire_shpfile_df_list[[20]],
                                               sanpark_fire_shpfile_df_list[[21]])

sanpark_fire_shpfile_combind_list_fixed <- st_buffer(sanpark_fire_shpfile_combind_list, dist = 0) # hack to fix geometry
sanpark_fire_shpfile_combind_list_fixed <- as(sanpark_fire_shpfile_combind_list_fixed, 'Spatial') # convert dataframe back to spatial features
sanpark_fire_shpfile_combind_list_trans <- spTransform(sanpark_fire_shpfile_combind_list_fixed, CRS(proj4string(roi_trans))) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)
sanpark_fire_shpfile_combind_list_trans_intersect <- intersect(sanpark_fire_shpfile_combind_list_trans, roi_trans) # crop polygon to ROI

# Visualisation of hotspots from 2002 to 2022
{
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = '2002-2022')
  plot(sanpark_fire_shpfile_combind_list_trans_intersect, col = alpha('red',.3), border = 'red', add = T)
  
}


# INDIVIDUAL YEAR FIRE HOTSPOTS VISUALISATION -----------------------------
yearly_sanpark_fire_shpfile <- function(data, index, plot = NULL){
  data <- data[[index]]
  # Check if SANPARK fire data CRS is missing or different, and reproject it
  if (is.na(proj4string(data))) {
    # Assign the correct projection
    proj4string(data) <- '+proj=tmerc +lat_0=0 +lon_0=19 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # Lo19 Hartebbesthoek94
    # Reproject to the same CRS as roi_trans
    sanpark_fire_shpfile_trans <- spTransform(data, CRS(proj4string(roi_trans)))
  } else {
    # Reproject to the same CRS as roi_trans
    sanpark_fire_shpfile_trans <- spTransform(data, CRS(proj4string(roi_trans)))
  }
  
  # check if geometry is valid
  sanpark_fire_shpfile_trans_df <- st_as_sf(sanpark_fire_shpfile_trans) # convert spatial data to data frame
  if(all(st_is_valid(sanpark_fire_shpfile_trans_df))==F){
    sanpark_fire_shpfile_trans_df_fixed <- st_buffer(sanpark_fire_shpfile_trans_df, dist = 0) # hack to fix geometry
    sanpark_fire_shpfile_trans_df_fixed <- as(sanpark_fire_shpfile_trans_df_fixed, 'Spatial') # convert back to spatial features
  } else{
    sanpark_fire_shpfile_trans_df_fixed <- as(sanpark_fire_shpfile_trans_df, 'Spatial') # convert back to spatial features
  }
  
  sanpark_fire_shpfile_intersect <- intersect(sanpark_fire_shpfile_trans_df_fixed, roi_trans) # crop polygon to ROI
  
  if(plot == T){
    # par(mar = c(5.1, 4.1, 4.1, 2.1)) # default margin
    # par(mar = c(bottom, left, top, right))
    par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
    plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
         main = sanpark_fire_shpfile_intersect@data$YEAR[1])
    plot(sanpark_fire_shpfile_intersect, col = alpha('red',.3), border = 'red', add = T)
    
    # plot(sanpark_fire_shpfile_trans, col = 'red', border = 'red')
    # plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, add = T)
  }
  
  return(sanpark_fire_shpfile_intersect)

}

# Visualise yearly SANPARKs fire data- Index range from 1 to 21
yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 1, plot = T)

# Visualisation in one layout
{
  par(mfrow = c(5,5))
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 1, plot = T) 
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 2, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 3, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 4, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 5, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 6, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 7, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 8, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 9, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 10, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 11, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 12, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 13, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 14, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 15, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 16, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 17, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 18, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 19, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 20, plot = T)
  yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 21, plot = T)
}








