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
options(scipen = 999, digits = 10) # avoid scientific notation
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
# View(st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect))
# Cleaning data
sanpark_fire_shpfile_combind_list_trans_intersect <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect) %>% # convert spatial feature to spatial dataframe
  mutate(FIRECAUSE = case_when(FIRECAUSE ==  "Accident"~"Accident",
                               FIRECAUSE ==  "Arson"~"Arson",
                               FIRECAUSE == "Arson - Vagrants" ~ "Vagrant",
                               FIRECAUSE == "Lighting Strike" ~ "Lightning Strike",
                               FIRECAUSE == "Lightning Strike" ~ "Lightning Strike",
                               FIRECAUSE ==  "Prescribed"~"Prescribed",
                               FIRECAUSE == "unknown"~"Unknown",
                               FIRECAUSE == "Unknown"~"Unknown",
                               FIRECAUSE == "Vagrant"~"Vagrant",
                               FIRECAUSE ==  "Wildfire"~"Wildfire"),
         
         FIRETYPE = case_when(FIRETYPE=="cigarette"~"Cigarette",
                    FIRETYPE=="Prescribed"~"Prescribed",
                    FIRETYPE=="unknown"~"Unknown",
                    FIRETYPE=="Wild Fire"~"Wildfire",
                    FIRETYPE=="Wildfire"~"Wildfire",
                    FIRETYPE=="WildFire"~"Wildfire"))  %>%
  
  mutate(STARTDATE = as.Date(STARTDATE, format = '%Y%m%d'), # reformat date
         YEARMONTH = format(as.Date(STARTDATE), '%Y-%m') |> as.factor(), # extract year and month
         YEAR_extract = year(STARTDATE) |> as.factor(), # extract year only 
         Area_calc_in_ha = as.numeric(st_area(geometry)/10000)) %>% # calculate missing areas in ha
  arrange(STARTDATE) %>% # rearrange date in correct order
  select(-YEAR, -XHECTARES) %>% # remove supplied year as it creates confusion as in the year for2007-11-30 will be 2008 (we want to keep the year!)
  as('Spatial') # convert dataframe to spatial feature again

# Removing prescribed burning from burnt area
sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect) %>%
  filter(FIRECAUSE!="Prescribed") %>%
  as('Spatial')
# st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed) |> View()
# Visualisation of hotspots from 2002 to 2022 (excluding prescribed burning)
{
  plot(veg_type_trans, col = veg_color_map[veg_type_trans$NTNL_VGTN_], border = 'transparent', main = '2002-2022')
  plot(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed, col = alpha('red',.2), lwd = .5,border = 'red', add = T)
  # plot(as(firms_fire_shpfile_trans_df_2002_2023, 'Spatial'), pch = 16, cex = .5, col = 'red', add = T) # add firms data to the visualisation
  }

# PRESCRIBED BURNING VISUALISATION ----------------------------------------

# filter out prescribed burnt region
sanpark_fire_shpfile_combind_list_trans_intersect_prescribed <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect) %>%
  filter(FIRECAUSE=="Prescribed") %>%
  as('Spatial')

{
  plot(veg_type_trans, col = veg_color_map[veg_type_trans$NTNL_VGTN_], border = 'transparent', main = "2012 Prescribed Burning")
  # plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, main = "2012 Prescribed Burning")
  plot(sanpark_fire_shpfile_combind_list_trans_intersect_prescribed, col = alpha('red',.3), border = 'red', add = T)
  }

# INDIVIDUAL YEAR FIRE HOTSPOTS VISUALISATION -----------------------------
# Extract unique years of fire from SANParks
SANPark_fire_year <- c(st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed)$YEAR_extract %>% unique())

# Create function to plot yearly fire occurrences from SANPark
yearly_sanpark_fire_plot <- function(data, index){
  
    plot(roi_trans, col = 'transparent', border='black', lwd=1, main = SANPark_fire_year[index])
    plot(st_as_sf(data) %>%
           filter(YEAR_extract==SANPark_fire_year[index]) %>%
           as('Spatial'), col = alpha('red',.3), border = 'red', add=T)

}

# Visualise yearly SANPARKs fire data- Index range from 1 to 21
yearly_sanpark_fire_plot(data = sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed, index = 2)

# Visualisation in one layout
{
  par(mfrow = c(5,4))
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  pblapply(seq_along(SANPark_fire_year), 
           function(x){yearly_sanpark_fire_plot(data = sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed, index = x)})
}

# INDIVIDUAL MONTH FIRE HOTSPOTS VISUALISATION ----------------------------

sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_MONTHLY <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed)

# create a list for the unique yearmonth fire
Sanpark_unique_yearmonth_list <- unique(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_MONTHLY$YEARMONTH)

# Remove 2007-01 and 2010-01from the monthly sanpark fire raster list (polygon too small to detect fire, hence rasterised)
Sanpark_unique_yearmonth_list <- Sanpark_unique_yearmonth_list[c(-16,-24)]

monthly_sanpark_fire_shpfile <- function(data, index, plot=NULL){
  data <- data %>%
    filter(YEARMONTH == Sanpark_unique_yearmonth_list[index]) %>%
    as('Spatial') # convert df to spatial data

  if(plot==T){
    par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
    plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
         main = data$YEARMONTH[1])
    plot(data, col = alpha('red',.3), border = 'red', add = T)
  }
  return(data)
}

# Visualise monthly SANPARKs fire data- Missing month means that no fire detected
monthly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_MONTHLY, 
                             index = 24, 
                             plot = T)

monthly_sanpark_fire_shpfile_list <- pblapply(seq_along(Sanpark_unique_yearmonth_list), 
         function(x){monthly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_MONTHLY, 
                                                  index = x, 
                                                  plot = T)})
# View(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_MONTHLY)

# RASTERISE SHAPE FILE ----------------------------------------------------

rasterise_polygons <- function(study_area, polygon_shapefile, index, resolution, plot = NULL){
  empty_raster <- raster(extent(study_area), res = resolution) # creating empty raster with 30x30 spatial resolution
  crs(empty_raster) <- crs(study_area) # assigning crs to empty raster
  # rasterise polygon shape file with 1s and 0s
  rasterised_polygon <- rasterize(polygon_shapefile[[index]], 
                 empty_raster,
                 field = 1,
                 background = 0) |>
    crop(study_area) |>
    mask(study_area) |>
    ratify() # make raster as a factor
  
  levels(rasterised_polygon) <- data.frame(ID = c(0, 1), fire_status = c("No Fire", "Fire")) # redefine levels
  
  if(plot ==T){
    plot(rasterised_polygon, col= c('lightgray','red'), main = polygon_shapefile[[index]]@data$YEARMONTH[1], legend = F)
    plot(study_area, col = 'transparent', border = 'black', lwd = 1,
         add = T)
  }
  return(rasterised_polygon)
}

rasterise_polygons(study_area = roi_trans,
                   polygon_shapefile = monthly_sanpark_fire_shpfile_list, 
                   index = 32, 
                   resolution = 30, 
                   plot = T)

monthly_sanpark_fire_raster_list <- pblapply(seq_along(monthly_sanpark_fire_shpfile_list),
         function(x){rasterise_polygons(study_area = roi_trans,
                                        polygon_shapefile = monthly_sanpark_fire_shpfile_list, 
                                        index = x, 
                                        resolution = 30, 
                                        plot = T)})

# rename SANparks fire rasters 
pblapply(seq_along(Sanpark_unique_yearmonth_list), function(x){names(monthly_sanpark_fire_raster_list[[x]]) <<- paste0("Fire ", Sanpark_unique_yearmonth_list[[x]])})

# Check if rasterisation is correct
{
  monthly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_MONTHLY, 
                               index = 34, 
                               plot = T)
  
  rasterise_polygons(study_area = roi_trans,
                     polygon_shapefile = monthly_sanpark_fire_shpfile_list, 
                     index = 26, 
                     resolution = 30, 
                     plot = T)
}

# GENERATING NO FIRE RASTER/0 RASTER --------------------------------------

empty_raster <- raster(extent(roi_trans), res = 30) # creating empty raster with 30x30 spatial resolution
crs(empty_raster) <- crs(roi_trans) # assigning crs to empty raster
values(empty_raster) <- 0 # Populating the raster with 0

# Create fire raster with levels
No_fire_raster <- empty_raster |> 
  crop(roi_trans) |> 
  mask(roi_trans) |> 
  ratify()

levels(No_fire_raster) <- data.frame(ID = 0, fire_status = "No Fire") # redefine levels

# Generate date for full envisaged timeframe of study
full_timeframe <- seq(as.Date('2002-01-01'), as.Date('2023-12-01'), by = 'month')

# Extract dates when fire was detected on a monthly basis
Fire_dates <-sapply(seq_along(monthly_sanpark_fire_raster_list), function(x) monthly_sanpark_fire_shpfile_list[[x]]$YEARMONTH[1] |>paste0('-01'))

# Exclude the fire dates to find out which dates had NO fire
No_fire_dates <- full_timeframe[!full_timeframe %in% as.Date(Fire_dates)]

# Replicate no fire rasters to fill in the gaps in the data
No_fire_RASTERS <- replicate(length(No_fire_dates), No_fire_raster)

# Name NO fire rasters 
pblapply(seq_along(No_fire_dates), function(x){names(No_fire_RASTERS[[x]]) <<- paste0("Fire ", format(No_fire_dates, "%Y-%m")[x])})

# # Saving results
# {
#   # Save object
#   save(monthly_sanpark_fire_raster_list,
#        file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/SANparks/monthly_sanpark_fire_raster_list.Rdata')
#   save(No_fire_RASTERS,
#        file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/SANparks/No_fire_RASTERS.Rdata')
#   
# 
#   # Save rasters in one folder on local machine or hard drive
#   Save_SANparks_fire_raster <- function(index, path){
# 
#     file_path <- paste0(path, gsub("\\.", " ", names(monthly_sanpark_fire_raster_list[[index]])))
# 
#     return(writeRaster(monthly_sanpark_fire_raster_list[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
# 
#   pblapply(seq_along(monthly_sanpark_fire_raster_list), function(x){
#     Save_SANparks_fire_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/SANparks/')
# 
#   })
#   
#   # Save rasters in one folder on local machine or hard drive
#   Save_NO_fire_raster <- function(index, path){
#     
#     file_path <- paste0(path, gsub("\\.", " ", names(No_fire_RASTERS[[index]])))
#     
#     return(writeRaster(No_fire_RASTERS[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
#   
#   pblapply(seq_along(No_fire_RASTERS), function(x){
#     Save_NO_fire_raster(index = x, path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/SANparks/')
#     
#   })
# }

# reload filenames for fire
fire_filenames <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/SANparks/', pattern = '.tif')
FIRE_DATA <- pblapply(seq_along(fire_filenames), 
       function(x){raster(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Fire hotspots/SANparks/',
                                                          fire_filenames[x]))})

# plot all the compiled fire data for visual inspection!
lapply(seq_along(FIRE_DATA), function(x){
  # Set color based on the condition
  fire_color_condition <- if (all(values(FIRE_DATA[[x]]) %>% na.omit() == 0)) {
    "lightgray"
  } else {
    c("lightgray", "red")
  }
  
  plot(FIRE_DATA[[x]], 
       col = fire_color_condition,
       main = gsub("Fire\\.(\\d{4})\\.(\\d{2})", "\\1-\\2", names(FIRE_DATA[[x]])), 
       legend = F)
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1,
       add = T)
})

# EDA
# Extract and plot 2014 to 2022 fire data (add to thesis)
sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_2014_2022 <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed)%>%
  filter(YEAR_extract %in% 2014:2022)%>%
  as('Spatial')

# Finding all the causes of fire
firecause_summary <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_2014_2022) %>% 
  data.frame() %>% 
  select('FIRECAUSE') %>% 
  table() %>%
  as_tibble()

burnt_area_2014_2022 <- tm_shape(veg_type_trans)+
  tm_polygons("NTNL_VGTN_", 
              palette = veg_color_map,  # You can choose different color palettes like "Set3", "Dark2", etc.
              title = "Vegetation Types",
              border.col = "transparent",
              legend.show = T) +
  tm_shape(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_2014_2022)+
  tm_polygons('Area_calc_in_ha',
              palette = 'red',
              alpha = .2,
              border.col = 'red',
              legend.show = F)+
  tm_graticules(lines = F)+
  tm_layout(legend.position = c("left", "top"), legend.text.size = 0.37)

# Save the burnt area map as a PDF
tmap_save(burnt_area_2014_2022, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/burnt_area_2014_2022.pdf", width = 4, height = 4)


# remember to change this when 2023 fire data is obtained
FIRE_DATA_2014_2022 <- sapply(145:252, function(x) FIRE_DATA[[x]]|>values()|>na.omit()|>max())
fire_dates_2014_2022 <- seq(as.Date("2014-01-01"), as.Date("2022-12-01"), by = "month")

fire_or_no_fire_df <- tibble(date = fire_dates_2014_2022, fire_status = factor(FIRE_DATA_2014_2022))

nrow(fire_or_no_fire_df) 
sum(fire_or_no_fire_df$fire_status==0) # no fire events from 2014-2022
sum(fire_or_no_fire_df$fire_status==1) # fire events from 2014-2022
