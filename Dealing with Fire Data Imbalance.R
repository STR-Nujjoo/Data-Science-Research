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
fire_color_condition <- if (all(values(xx) %>% na.omit() == 0)) {
  "lightgray"
} else {
  c("lightgray", "red")
}

# import waterbodies shapefile
waterbodies <- st_read('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/Storm_water_Waterbodies.shp')

# Check data imbalance ----------------------------------------------------

prop.table(table(dfnorm_2014_2022$Fire_Value))*100 # proportion of data imbalance for 2014 to 2022 dataset
prop.table(table(dfnorm_2002_2022$Fire_Value))*100 # proportion of data imbalance for 2002 to 2022 dataset

table(RF_2014to2022_Train$Fire_Value)
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
generate_valid_copy <- function(target_poly, existing, study_area, max_attempts = 100) {
  # Get bounding box of study area
  study_bbox <- st_bbox(study_area)
  
  study_area <- st_as_sf(study_area)
  # study_area <- st_as_sf(roi_trans)
  
  bbox <- st_bbox(target_poly)
  # bbox <- st_bbox(chosen_polygon)
  width <- bbox["xmax"] - bbox["xmin"]
  height <- bbox["ymax"] - bbox["ymin"]

  for (i in 1:max_attempts) {
    dx <- runif(1, study_bbox["xmin"], study_bbox["xmax"] - width) - bbox["xmin"]
    dy <- runif(1, study_bbox["ymin"], study_bbox["ymax"] - height) - bbox["ymin"]
    
    new_poly <- translate_geom(target_poly, dx, dy)
    # new_poly <- translate_geom(chosen_polygon, dx, dy)
    
    # Check if it's fully inside and does not intersect with existing ones
    inside <- all(st_within(new_poly, study_area, sparse = FALSE))
    overlaps <- any(st_within(new_poly, existing, sparse = FALSE))
    # overlaps <- any(st_intersects(new_poly, chosen_polygon, sparse = FALSE))
    
    
    if (inside && !overlaps) return(new_poly)
  }
  return(c(NULL,
         warning("Polygon is too large for further replication!")))
}

FIRE_2002_2022_stack
#46
r <- FIRE_2014_2022_stack@layers[[99]]
plot(r)
fire_pixels <- r == 1 # select fire pixels only

# Group connected pixels into clumps (i.e., contiguous clusters)
clumped <- clump(fire_pixels, directions = 8)  # 8 for diagonal connectivity too
fire_polygons <- rasterToPolygons(clumped, dissolve=T) # Convert Fire Pixels to Polygons
fire_polygons <- st_as_sf(fire_polygons) # convert the polygon into spatial dataframe
fire_polygons$geometry # check geometry

plot(roi_trans, col = 'transparent', border = 'black')
plot(fire_polygons$geometry[2], col = 'red', border = 'red', add = T)

fire_polygons_buffered <- st_buffer(fire_polygons, dist = 90) # Buffering 90m (e.g., 3 cells if 30m resolution)
buffered_clipped <- st_as_sfc(st_intersection(fire_polygons_buffered, st_as_sf(roi_trans))) # make sure polygons remain within ROI
# To avoid repetitive rows when converting data to a dataframe at a later stage. Union intersected polygons
if(all(st_intersects(buffered_clipped, sparse = F))){
  buffered_clipped_union <- st_union(buffered_clipped)
} else{
  buffered_clipped_union <- buffered_clipped
}


plot(roi_trans, col = 'transparent', border = 'black')
plot(buffered_clipped_union, add = T, col = 'red', border = 'red')

buffered_clipped_area <- st_area(buffered_clipped)|> as.numeric() /10000 # area in hectares (ha)
chosen_polygon <- buffered_clipped[which.max(buffered_clipped_area)]
# chosen_polygon_extent <- chosen_polygon |> st_bbox() # extent of the largest fire polygon in that group of fire polygons

# replicated_polygon <- generate_valid_copy(target_poly = chosen_polygon, existing = buffered_clipped, roi_trans)
# 
# existing_union <- c(buffered_clipped, replicated_polygon)
# 
# replicated_polygon1 <- generate_valid_copy(poly = chosen_polygon, existing = existing_union, roi_trans)
# 
# existing_union[-1] # including only masked polygon
# 
# chosen_polygon|>class()
# plot(roi_trans, col = 'transparent', border = 'black')
# plot(buffered_clipped, add = T, col = 'red', border = 'red')
# plot(replicated_polygon, add = T, col = 'yellow', border = 'yellow')

# Initialize list of polygons
# replicated <- c()
original_poly <- chosen_polygon
existing_union <- buffered_clipped_union  # start with the original

# Try to generate 4 non-overlapping copies
for (i in 1:4) {
  new_copy <- generate_valid_copy(original_poly, existing_union, study_area)
  if (!is.null(new_copy)) {
    # replicated[1] <- new_copy
    existing_union <- c(existing_union, new_copy)
  } else {
    break
    warning("Could not place a non-overlapping polygon after multiple attempts.")
  }
}

masked_polygon <- existing_union[-1:-length(buffered_clipped_union)] # from all the polygon generated separate the ones which will be used as masked polygon only

plot(roi_trans, col = 'transparent', border = 'black')
plot(buffered_clipped_union, add = T, col = 'red', border = 'red')
plot(masked_polygon, add = T, col = 'yellow', border = 'yellow')


# Convert sf back to Spatial for rasterize
buffered_sp <- as(buffered_clipped_union, "Spatial")

# Create an empty raster to match the original
buffer_raster <- raster(r)
buffer_raster <- rasterize(buffered_sp, buffer_raster, field=1, background=0) |> mask(roi_trans)
resampled_buffered_fire_raster <- mask(buffer_raster, as(masked_polygon, "Spatial"), inverse=T)
plot(buffer_raster, col = fire_color_condition)
plot(resampled_buffered_fire_raster, col = fire_color_condition)




