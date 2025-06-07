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
  library(factoextra)
  library(caret)
  library(tmap)
  library(cowplot)
  library(gridExtra)
  library(performanceEstimation)
  library(leaflet)
  library(leafem)
  library(mapview)
  library(spatialRF)
  library(MASS)
  library(corrplot)
  library(randomForest)
  library(ranger)
  library(pingers)
  library(ROSE)
  library(smotefamily)
}

# Set fire color based on the condition
fire_color_condition_func <- function(data){
  xx <- data
  fire_color_condition <- if (all(values(xx) %>% na.omit() == 0)) {
    "lightgray"
  } else {
    c("lightgray", "red")
  }
  return(fire_color_condition)
}

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# import waterbodies shapefile
waterbodies <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/Storm_water_Waterbodies/Storm_water_Waterbodies.shp')
waterbodies_trans <- spTransform(waterbodies, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

target_waterbodies <- st_as_sf(waterbodies_trans) %>%
  filter(OBJECTID %in% c(1973, 1974, 1975, 1976, 1980)) %>% # select only the relevant waterbodies 
  st_as_sfc() # convert into sfc format

plot(roi_trans)
plot(target_waterbodies, add = T)

# Check data imbalance ----------------------------------------------------
prop.table(table(dfnorm_2014_2022$Fire_Value))*100 # proportion of data imbalance for 2014 to 2022 dataset
prop.table(table(fully_resampled_dfnorm_2014_2022$Fire_Value))*100 # proportion of data imbalance for 2014 to 2022 resampled dataset

prop.table(table(dfnorm_2002_2022$Fire_Value))*100 # proportion of data imbalance for 2002 to 2022 dataset

prop.table(table(dfnorm_2014_2022_training_set$Fire_Value))*100
prop.table(table(fully_resampled_dfnorm_2014_2022_training_set$Fire_Value))*100

table(RF_2014to2022_Val$Fire_Value)

# Application of Random Sampling on training and validation set -----------
# Trying resampling methods on training set-SMOTE
# RF_2014to2022_Train_over <- ovun.sample(Fire_Value ~., data = RF_2014to2022_Train, method = 'over', seed = 1) # over-sampling
str(RF_2014to2022_Train)
str(as_tibble(RF_2014to2022_Train_over$data))
table(RF_2014to2022_Train_over$data$Fire_Value) # check fire and no fire proportion
prop.table(table(RF_2014to2022_Train_over$data$Fire_Value))*100 # check fire and no fire proportion in terms of percentage

# visualise to see the effect the random sampling has on the data
x <- RF_2014to2022_Train %>%
  filter(Year==2019 & Month==1) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))

plot(xx, col = fire_color_condition, main = 'original 2019-01')
plot(roi_trans, col = 'transparent', border = 'black', add = T)

x <- RF_2014to2022_Train_over$data %>%
  filter(Year==2019 & Month==1) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = fire_color_condition, main = 'oversampled 2019-01')
plot(roi_trans, col = 'transparent', border = 'black', add = T)



RF_2014to2022_Train_under <- ovun.sample(Fire_Value ~., data = RF_2014to2022_Train, method = 'under', seed = 1) # under-sampling
str(RF_2014to2022_Train)
str(as_tibble(RF_2014to2022_Train_under$data))
table(RF_2014to2022_Train_under$data$Fire_Value) # check fire and no fire proportion
prop.table(table(RF_2014to2022_Train_under$data$Fire_Value))*100 # check fire and no fire proportion in terms of percentage

RF_2014to2022_Train_both <- ovun.sample(Fire_Value ~., data = RF_2014to2022_Train, method = 'both', seed = 1) # over & under-sampling
str(RF_2014to2022_Train)
str(as_tibble(RF_2014to2022_Train_both$data))
table(RF_2014to2022_Train_both$data$Fire_Value) # check fire and no fire proportion
prop.table(table(RF_2014to2022_Train_both$data$Fire_Value))*100 # check fire and no fire proportion in terms of percentage

# Trying resampling methods on validation set-SMOTE
# RF_2014to2022_Val_over <- ovun.sample(Fire_Value ~., data = RF_2014to2022_Val, method = 'over', seed = 1) # over-sampling: ERROR
str(RF_2014to2022_Val)
str(as_tibble(RF_2014to2022_Val_over$data))
table(RF_2014to2022_Val_over$data$Fire_Value) # check fire and no fire proportion
prop.table(table(RF_2014to2022_Val_over$data$Fire_Value))*100 # check fire and no fire proportion in terms of percentage

# visualise to see the effect the random sampling has on the data
x <- RF_2014to2022_Val %>%
  filter(Year==2021 & Month==4) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))

plot(xx, col = fire_color_condition, main = 'original 2021-04')
plot(roi_trans, col = 'transparent', border = 'black', add = T)

x <- RF_2014to2022_Val_over$data %>%
  filter(Year==2021 & Month==4) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = fire_color_condition, main = 'oversampled 2021-04')
plot(roi_trans, col = 'transparent', border = 'black', add = T)


RF_2014to2022_Val_under <- ovun.sample(Fire_Value ~., data = RF_2014to2022_Val, method = 'under', seed = 1) # under-sampling
str(RF_2014to2022_Val)
str(as_tibble(RF_2014to2022_Val_under$data))
table(RF_2014to2022_Val_under$data$Fire_Value) # check fire and no fire proportion
prop.table(table(RF_2014to2022_Val_under$data$Fire_Value))*100 # check fire and no fire proportion in terms of percentage

# visualise to see the effect the random sampling has on the data
x <- RF_2014to2022_Val %>%
  filter(Year==2021 & Month==4) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))

plot(xx, col = fire_color_condition, main = 'original 2021-04')
plot(roi_trans, col = 'transparent', border = 'black', add = T)

x <- RF_2014to2022_Val_under$data %>%
  filter(Year==2021 & Month==4) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = fire_color_condition, main = 'undersampled 2021-04')
plot(roi_trans, col = 'transparent', border = 'black', add = T)


RF_2014to2022_Val_both <- ovun.sample(Fire_Value ~., data = RF_2014to2022_Val, method = 'both', seed = 1) # # over & under-sampling
str(RF_2014to2022_Val)
str(as_tibble(RF_2014to2022_Val_both$data))
table(RF_2014to2022_Val_both$data$Fire_Value) # check fire and no fire proportion
prop.table(table(RF_2014to2022_Val_both$data$Fire_Value))*100 # check fire and no fire proportion in terms of percentage

# visualise to see the effect the random sampling has on the data
x <- RF_2014to2022_Val %>%
  filter(Year==2021 & Month==4) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))

plot(xx, col = fire_color_condition, main = 'original 2021-04')
plot(roi_trans, col = 'transparent', border = 'black', add = T)

x <- RF_2014to2022_Val_both$data %>%
  filter(Year==2021 & Month==4) %>%
  dplyr::select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = fire_color_condition, main = 'overundersampled 2021-04')
plot(roi_trans, col = 'transparent', border = 'black', add = T)


View(
  dfnorm_2002_2022 %>%
    group_by(Year, Month) %>%
    filter(Fire_Value==1) %>%
    tally()
)



dfnorm_2002_2022 %>%
  group_by(Year, Month) %>%
  filter(Year ==2022 & Fire_Value==1) %>%
  tally()


x <- dfnorm_2014_2022 %>%
  filter(Year==2022 & Month==3) %>%
  dplyr:: select(x,y,Fire_Value)
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = fire_color_condition)





# Buffering and undersampling fire rasters --------------------------------

# Function to translate a geometry manually
translate_geom <- function(geom, dx, dy) {
  geom_crs <- st_crs(geom)
  geom <- st_geometry(geom) + c(dx, dy)
  st_crs(geom) <- geom_crs
  return(geom)
}

# Function to generate a random translation and validate it
generate_valid_copy <- function(target_poly, existing, actual_study_area, buffered_study_area, max_attempts) {
  # Get bounding box of study area
  study_bbox <- st_bbox(actual_study_area)

  bbox <- st_bbox(target_poly)
  width <- bbox["xmax"] - bbox["xmin"]
  height <- bbox["ymax"] - bbox["ymin"]

  for (i in 1:max_attempts) {
    print(i)
    dx <- runif(1, study_bbox["xmin"], study_bbox["xmax"] - width) - bbox["xmin"]
    dy <- runif(1, study_bbox["ymin"], study_bbox["ymax"] - height) - bbox["ymin"]
    
    new_poly <- translate_geom(target_poly, dx, dy)

    # Check if it's fully inside and does not intersect with existing ones
    inside <- all(st_within(new_poly, buffered_study_area, sparse = FALSE))
    overlaps <- any(st_intersects(new_poly, existing, sparse = FALSE))

    if (inside && !overlaps) {
      message(paste0("Successful placement on attempt ", i))
      return(new_poly)
      
    }else{
      next
    }
      
  }
  warning("Could not place a non-overlapping polygon after multiple attempts. \nPolygon must be too large for further replication!")
  return(NULL)
}

RESAMPLED_FIRE_2014_2022_training_list <- pblapply(1:72, # training set index only
                                          function(x){
  r <- FIRE_2014_2022_stack@layers[[x]]
  fire_pixels <- r == 1 # select fire pixels only
  # plot(r)
  if(maxValue(fire_pixels)==1){ # this indicates that a fire actually took place
    
    # Group connected pixels into clumps (i.e., contiguous clusters)
    clumped <- clump(fire_pixels, directions = 8)  # 8 for diagonal connectivity too
    fire_polygons <- rasterToPolygons(clumped, dissolve=T) # Convert Fire Pixels to Polygons
    fire_polygons <- st_as_sf(fire_polygons) # convert the polygon into spatial dataframe
    # fire_polygons$geometry # check geometry
    
    # plot(roi_trans, col = 'transparent', border = 'black')
    # plot(fire_polygons$geometry[2], col = 'red', border = 'red', add = T)
    
    fire_polygons_buffered <- st_buffer(fire_polygons, dist = 90) # Buffering 90m (e.g., 3 cells if 30m resolution)
    buffered_clipped <- st_as_sfc(st_intersection(fire_polygons_buffered, st_as_sf(roi_trans))) # make sure polygons remain within ROI
    buffered_clipped_area <- st_area(buffered_clipped)|> as.numeric() /10000 # area in hectares (ha)
    chosen_polygon <- buffered_clipped[which.max(buffered_clipped_area)] # polygon to be used as the cookie cutter
    
    # To avoid repetitive rows when converting data to a dataframe at a later stage. Union intersected polygons if polygons overlap after buffering
    overlap_check_after_buffer <- st_intersects(buffered_clipped, sparse = F)
    if(any(overlap_check_after_buffer[row(overlap_check_after_buffer) != col(overlap_check_after_buffer)])){ # if any of the polygon intersects with each other unionise them
      buffered_clipped_union <- st_union(buffered_clipped)
    } else{
      buffered_clipped_union <- buffered_clipped
    }
    
    # This was considered because the ones which fall exactly on the edge of the study area was considered to be not within the study area.
    # Therefore, a quick fix is to buffer the study area by only a meter to force the correct interpretation.
    roi_trans_buffered <- st_buffer(st_as_sfc(roi_trans), dist = 1)
    # all(st_within(buffered_clipped, roi_trans_buffered, sparse = F)) # check
    
    # plot(roi_trans, col = 'transparent', border = 'black')
    # plot(buffered_clipped_union, add = T, border = 'red', col = alpha('red', .3))
    # plot(target_waterbodies, add = T)
    
    # Initialize list of polygons
    original_poly <- chosen_polygon
    existing_union <- c(buffered_clipped_union, target_waterbodies)  # start with the original and acknowledge that a fire won't take place within a waterbody
    n_cookie_cutter <- 4
    # Try to generate 4 non-overlapping copies
    for (i in 1:n_cookie_cutter) {
      new_copy <- generate_valid_copy(target_poly=original_poly, 
                                      existing = existing_union, 
                                      actual_study_area = roi_trans,
                                      buffered_study_area = roi_trans_buffered,
                                      max_attempts = 200)
      if (!is.null(new_copy)) {
        existing_union <- c(existing_union, new_copy)
        
      } else {
        break
      }
    }
    
    masked_polygon <- existing_union[-1:-length(c(buffered_clipped_union, target_waterbodies))] # from all the polygon generated separate the ones which will be used as masked polygon only
    
    # plot(roi_trans, col = 'transparent', border = 'green')
    # plot(buffered_clipped_union, add = T, col = 'red', border = 'black')
    # plot(masked_polygon, add = T, col = 'yellow', border = 'black')
    # plot(target_waterbodies, add = T)
    
    buffered_clipped_union <- st_as_sf(buffered_clipped_union) # convert buffered polygon to spatial dataframe
    buffered_clipped_union$value <- 1 # create a variable name value and assign the value to be 1 representing positives
    negative_polygon <- st_as_sf(masked_polygon) # convert negative polygon to spatial dataframe
    negative_polygon$value <- 0 # create a variable name value and assign the value to be 0 representing negatives
    all_poly <- rbind(buffered_clipped_union, negative_polygon) # merge the polygons
    
    # Convert sf back to Spatial for rasterize
    all_poly_sp <- as(all_poly,'Spatial')
    
    # Create an empty raster to match the original
    buffer_raster <- raster(r)
    
    # convert the polygons into a raster
    resampled_buffered_fire_raster <- rasterize(all_poly_sp, buffer_raster, field='value', background=NA)
    names(resampled_buffered_fire_raster) <- names(r)
    
    print(
      
      plot(resampled_buffered_fire_raster, 
           col = fire_color_condition_func(resampled_buffered_fire_raster),
           main = names(resampled_buffered_fire_raster))
  
    )
    
    
    return(resampled_buffered_fire_raster)
    
  } else{
    print(
      plot(r, col= fire_color_condition_func(r), main = names(r))
      )
    return(r)
  }
  
})

plot(roi_trans, add = T)
plot(target_waterbodies, add = T)

# Save rasters in one folder on local machine or hard drive
Save_SANparks_fire_raster <- function(data, index, path){
  
  file_path <- paste0(path, gsub("\\.", " ", names(data[[index]])))
  
  return(writeRaster(data[[index]],
                     filename = file_path, format = "GTiff", overwrite = TRUE))
}


pblapply(seq_along(RESAMPLED_FIRE_2014_2022_training_list), function(x){
  Save_SANparks_fire_raster(data = RESAMPLED_FIRE_2014_2022_training_list,
              index = x,
              path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (buffered)/')
  
  })


# # Save object
# save(RESAMPLED_FIRE_2014_2022_training_list,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (buffered)/RESAMPLED_FIRE_2014_2022_list.Rdata')
# load object
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (buffered)/RESAMPLED_FIRE_2014_2022_list.Rdata')

# Reimporting  and visualising the resampled fire data for the training set only
x <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (buffered)/', pattern = '.tif')
RESAMPLED_FIRE_2014_2022_training_list <- pblapply(seq_along(x), function(i){
  xx <- raster(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (buffered)/', x[i]))
  # plot(xx,
  #      col = fire_color_condition_func(xx),
  #      main= names(xx))
  # plot(roi_trans, add = T)
  # plot(target_waterbodies, add = T)
  return(xx)
})

# Reimporting  and visualising the resampled fire data for the training set only (modified timeframe)
x <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (buffered)/', pattern = '.tif')
x <- x[1:60] # readjust training set from 2014 to 2018
RESAMPLED_FIRE_2014_2022_buffered_training_list <- pblapply(seq_along(x), function(i){
  xx <- raster(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (buffered)/', x[i]))
  # plot(xx,
  #      col = fire_color_condition_func(xx),
  #      main= names(xx))
  # plot(roi_trans, add = T)
  # plot(target_waterbodies, add = T)
  return(xx)
})


# Resampling without buffering
RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_list <- pblapply(1:60, # training set index only
                                                   function(x){
                                                     r <- FIRE_2014_2022_stack@layers[[x]]
                                                     fire_pixels <- r == 1 # select fire pixels only
                                                     # plot(r)
                                                     if(maxValue(fire_pixels)==1){ # this indicates that a fire actually took place
                                                       
                                                       # Group connected pixels into clumps (i.e., contiguous clusters)
                                                       clumped <- clump(fire_pixels, directions = 8)  # 8 for diagonal connectivity too
                                                       fire_polygons <- rasterToPolygons(clumped, dissolve=T) # Convert Fire Pixels to Polygons
                                                       fire_polygons <- st_as_sfc(fire_polygons) # convert the polygon into spatial dataframe
                                                       fire_polygons_area <- st_area(fire_polygons)|> as.numeric() / 10000 # area in hectares (ha)
                                                       # fire_polygons$geometry # check geometry
                                                       
                                                       # plot(roi_trans, col = 'transparent', border = 'black')
                                                       # plot(chosen_polygon, col = 'red', border = 'red', add = T)
                                                       
                                                      chosen_polygon <- fire_polygons[which.max(fire_polygons_area)] # polygon to be used as the cookie cutter
                                                       
                                                       # To avoid repetitive rows when converting data to a dataframe at a later stage. Union intersected polygons if polygons overlap
                                                       overlap_check <- st_intersects(fire_polygons, sparse = F)
                                                       if(any(overlap_check[row(overlap_check) != col(overlap_check)])){ # if any of the polygon intersects with each other unionise them
                                                         fire_polygons_union <- st_union(fire_polygons)
                                                       } else{
                                                         fire_polygons_union <- fire_polygons
                                                       }
                                                       
                                                       # This was considered because the ones which fall exactly on the edge of the study area was considered to be not within the study area.
                                                       # Therefore, a quick fix is to buffer the study area by only a meter to force the correct interpretation.
                                                       roi_trans_buffered <- st_buffer(st_as_sfc(roi_trans), dist = 1)
                                                       # all(st_within(buffered_clipped, roi_trans_buffered, sparse = F)) # check
                                                       
                                                       # plot(roi_trans, col = 'transparent', border = 'black')
                                                       # plot(buffered_clipped_union, add = T, border = 'red', col = alpha('red', .3))
                                                       # plot(target_waterbodies, add = T)
                                                       
                                                       # Initialize list of polygons
                                                       original_poly <- chosen_polygon
                                                       existing_union <- c(fire_polygons_union, target_waterbodies)  # start with the original and acknowledge that a fire won't take place within a waterbody
                                                       n_cookie_cutter <- 4
                                                       # Try to generate 4 non-overlapping copies
                                                       for (i in 1:n_cookie_cutter) {
                                                         new_copy <- generate_valid_copy(target_poly=original_poly, 
                                                                                         existing = existing_union, 
                                                                                         actual_study_area = roi_trans,
                                                                                         buffered_study_area = roi_trans_buffered,
                                                                                         max_attempts = 200)
                                                         if (!is.null(new_copy)) {
                                                           existing_union <- c(existing_union, new_copy)
                                                           
                                                         } else {
                                                           break
                                                         }
                                                       }
                                                       
                                                       masked_polygon <- existing_union[-1:-length(c(fire_polygons_union, target_waterbodies))] # from all the polygon generated separate the ones which will be used as masked polygon only
                                                       
                                                       # plot(roi_trans, col = 'transparent', border = 'green')
                                                       # plot(fire_polygons_union, add = T, col = 'red', border = 'black')
                                                       # plot(masked_polygon, add = T, col = 'yellow', border = 'black')
                                                       # plot(target_waterbodies, add = T)
                                                       
                                                       fire_polygons_union <- st_as_sf(fire_polygons_union) # convert polygons to spatial dataframe
                                                       fire_polygons_union$value <- 1 # create a variable name value and assign the value to be 1 representing positives
                                                       negative_polygon <- st_as_sf(masked_polygon) # convert negative polygon to spatial dataframe
                                                       negative_polygon$value <- 0 # create a variable name value and assign the value to be 0 representing negatives
                                                       all_poly <- rbind(fire_polygons_union, negative_polygon) # merge the polygons
                                                       
                                                       # Convert sf back to Spatial for rasterize
                                                       all_poly_sp <- as(all_poly,'Spatial')
                                                       
                                                       # Create an empty raster to match the original
                                                       new_raster <- raster(r)
                                                       
                                                       # convert the polygons into a raster
                                                       resampled_non_buffered_fire_raster <- rasterize(all_poly_sp, new_raster, field='value', background=NA)
                                                       names(resampled_non_buffered_fire_raster) <- names(r)
                                                       
                                                       print(
                                                         {
                                                           plot(resampled_non_buffered_fire_raster, 
                                                                col = fire_color_condition_func(resampled_non_buffered_fire_raster),
                                                                main = names(resampled_non_buffered_fire_raster))
                                                           plot(roi_trans, add = T)
                                                           plot(target_waterbodies, add = T)
                                                         }
                                                         
                                                         
                                                       )
                                                       
                                                       
                                                       return(resampled_non_buffered_fire_raster)
                                                       
                                                     } else{
                                                       print(
                                                         plot(r, col= fire_color_condition_func(r), main = names(r))
                                                       )
                                                       return(r)
                                                     }
                                                     
})

# saving individual rasters
pblapply(seq_along(RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_list), function(x){
  Save_SANparks_fire_raster(data = RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_list,
                            index = x,
                            path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (non-buffered)/')
  
})

# # Save object
# save(RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_list,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (non-buffered)/RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_list.Rdata')
# # load object
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/Resampled SANParks fire data/Individual rasters (non-buffered)/RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_list.Rdata')































