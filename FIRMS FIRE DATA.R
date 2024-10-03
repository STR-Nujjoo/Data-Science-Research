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

# Import FIRMS fire data- Data requested from 2002-01-01 to 2024-10-02
firms_fire_data <- list.files('Raw Data/FIRMS fire data/DL_FIRE_M-C61_523508', pattern = '.shp')
firms_fire_shpfile <- readOGR(paste0('Raw Data/FIRMS fire data/DL_FIRE_M-C61_523508/', firms_fire_data))
firms_fire_shpfile_trans <- spTransform(firms_fire_shpfile, CRS(proj4string(roi_trans))) # Reproject to  EPSG:32734 (WGS 84 / UTM zone 34S)
firms_fire_shpfile_trans_intersect <- intersect(firms_fire_shpfile_trans, roi_trans) # crop fire data within ROI only

# Convert shapefile to a spatial dataframe
firms_fire_shpfile_trans_df <- st_as_sf(firms_fire_shpfile_trans_intersect)

# Split date into columns for further analyses
firms_fire_shpfile_trans_df <- firms_fire_shpfile_trans_df %>%
  mutate(ACQ_YEAR = format(as.Date(ACQ_DATE), '%Y') |> as.factor(),
                                      ACQ_MONTH = format(as.Date(ACQ_DATE), '%m') |> as.factor(),
                                      ACQ_YEARMONTH = format(as.Date(ACQ_DATE), '%Y/%m')|> as.factor())

firms_fire_shpfile_trans_df_2002_2023 <- firms_fire_shpfile_trans_df %>%
  filter(!ACQ_YEAR %in% '2024') # excluding year 2024


# VISUALISING FIRMS FIRE DATA FROM 2002-2023 ------------------------------

# Plot fire hotspots for the whole duration (i.e, 2002 to 2023)
{
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, main = '2002-2023')
  plot(as(firms_fire_shpfile_trans_df_2002_2023, 'Spatial'), pch = 16, cex = .5, col = 'red', add = T)
}


# VISUALISING FIRMS FIRE DATA ON A MONTHLY BASIS FROM 2002-2023 -----------

# Function to extract monthly fire detection since 2002 to 2023
firms_fire_shpfile_trans_df_2002_2023_monthly <- function(data, index){
   data %>%
    filter(ACQ_YEARMONTH == levels(data$ACQ_YEARMONTH)[index])
}

# Monthly fire detection since 2002 to 2023
firms_fire_shpfile_trans_df_2002_2023_monthly_list <- lapply(1:(length(levels(firms_fire_shpfile_trans_df_2002_2023$ACQ_YEARMONTH))-1), 
      function(x) {firms_fire_shpfile_trans_df_2002_2023_monthly(data =  firms_fire_shpfile_trans_df_2002_2023, index = x)})

# Function to plot monthly fire detection
FIRMS_monthly_fire_plots <- function(data, index){
   Spatialdata <- as(data[[index]], 'Spatial') # convert dataframe back to spatial feature
                          
   # Plot fire hotspots
   plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
        main = data[[index]]$ACQ_YEARMONTH[1])
   plot(Spatialdata, pch = 16, cex = .5, col = 'red', add = T)
   
}

# NOTE THAT THE MISSING MONTHS MEANS THAT NASA-FIRMS DID NOT DETECT ANY FIRES DURING THAT PERIOD!
# monthly plot
{
  par(mfrow = c(6,5))
  lapply(1:length(firms_fire_shpfile_trans_df_2002_2023_monthly_list),
         function(x){FIRMS_monthly_fire_plots(data = firms_fire_shpfile_trans_df_2002_2023_monthly_list,
                                              index = x)})
}


# VISUALISING FIRMS DATA YEARLY FROM 2002-2023 ----------------------------

# Function to extract yearly fire detection since 2002 to 2023
firms_fire_shpfile_trans_df_2002_2023_yearly <- function(data, index){
  data %>%
    filter(ACQ_YEAR == levels(data$ACQ_YEAR)[index])
}

# Yearly fire detection since 2002 to 2023
firms_fire_shpfile_trans_df_2002_2023_yearly_list <- pblapply(1:(length(levels(firms_fire_shpfile_trans_df_2002_2023$ACQ_YEAR))-1), 
         function(x) {
           firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = x)
         })

# Function to plot yearly fire detection
FIRMS_yearly_fire_plots <- function(data, index){
  Spatialdata <- as(data[[index]], 'Spatial') # convert dataframe back to spatial feature
  
  # Plot fire hotspots
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = data[[index]]$ACQ_YEAR[1])
  plot(Spatialdata, pch = 16, cex = .5, col = 'red', add = T)
  
}

# NOTE THAT THE MISSING YEARS MEANS THAT NASA-FIRMS DID NOT DETECT ANY FIRES DURING THAT PERIOD!
# yearly plot
{
  par(mfrow = c(4,4))
  lapply(1:length(firms_fire_shpfile_trans_df_2002_2023_yearly_list),
         function(x){FIRMS_yearly_fire_plots(data = firms_fire_shpfile_trans_df_2002_2023_yearly_list,
                                              index = x)})
}

