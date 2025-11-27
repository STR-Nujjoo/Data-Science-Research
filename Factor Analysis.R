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
  library(parallelDist)
}


# Define min-max normalisation function -----------------------------------
minmax_norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


# Converting stacked rasters into dataframe [2014-2022] -------------------
lulc_2014_2022_df <- as.data.frame(LULC_2014_2022_stack, xy = T) # converting stacked LULC into dataframe
lulc_2014_2022_df_xy <- lulc_2014_2022_df[,1:2] # extracting xy coordinates from LULC dataframe
lulc_2014_2022_dfX <- lulc_2014_2022_df[,-c(1:2)] # LULC dataframe with only predictor variables
lulc_2014_2022_dfX <- apply(lulc_2014_2022_dfX, 2, function(x){as.numeric(as.factor(x))}) |> as.data.frame() # converting the classes into numeric
lulc_2014_2022_dfXxy <- cbind(lulc_2014_2022_df_xy, lulc_2014_2022_dfX) # reattach coordinates to LULC dataframe consisting of the predictor variables
lulc_2014_2022_dfXxy_long <- lulc_2014_2022_dfXxy %>%
  # convert dataframe into long format where there is only one LULC column
  pivot_longer(
    cols = starts_with("LULC"),
    names_to = "LULC",
    values_to = "LULC_Class"
  ) %>% 
  na.omit() %>%
  mutate(Date = as.Date(str_extract(LULC, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'LULC_Class')) # select relevant columns only

lulc_2014_2022_dfXxy_long$x <- round(lulc_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
lulc_2014_2022_dfXxy_long$y <- round(lulc_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
lulc_2014_2022_dfXxy_long$LULC_Class <- minmax_norm(lulc_2014_2022_dfXxy_long$LULC_Class) # apply min-max normalisation
# head(lulc_2014_2022_dfXxy_long); str(lulc_2014_2022_dfXxy_long)


ndvi_2014_2022_df <- as.data.frame(NDVI_2014_2022_stack, xy = T) # converting stacked NDVI into dataframe
ndvi_2014_2022_dfXxy_long <-  ndvi_2014_2022_df %>%
  # convert dataframe into long format where there is only one NDVI column
  pivot_longer(
    cols = starts_with("NDVI"),
    names_to = "NDVI",
    values_to = "NDVI_Value"
  ) %>% 
  na.omit() %>%
  mutate(Date = as.Date(str_extract(NDVI, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDVI_Value')) # select relevant columns only

ndvi_2014_2022_dfXxy_long$x <- round(ndvi_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long$y <- round(ndvi_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long$NDVI_Value <- minmax_norm(ndvi_2014_2022_dfXxy_long$NDVI_Value) # apply min-max normalisation
# head(ndvi_2014_2022_dfXxy_long); str(ndvi_2014_2022_dfXxy_long)


ndmi_2014_2022_df <- as.data.frame(NDMI_2014_2022_stack, xy = T) # converting stacked NDMI into dataframe
ndmi_2014_2022_dfXxy_long <-  ndmi_2014_2022_df %>%
  # convert dataframe into long format where there is only one NDMI column
  pivot_longer(
    cols = starts_with("NDMI"),
    names_to = "NDMI",
    values_to = "NDMI_Value"
  ) %>% 
  na.omit() %>%
  mutate(Date = as.Date(str_extract(NDMI, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDMI_Value')) # select relevant columns only

ndmi_2014_2022_dfXxy_long$x <- round(ndmi_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long$y <- round(ndmi_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long$NDMI_Value <- minmax_norm(ndmi_2014_2022_dfXxy_long$NDMI_Value) # apply min-max normalisation
# head(ndmi_2014_2022_dfXxy_long); str(ndmi_2014_2022_dfXxy_long)

nbr_2014_2022_df <- as.data.frame(NBR_2014_2022_stack, xy = T) # converting stacked NBR into dataframe
nbr_2014_2022_dfXxy_long <-  nbr_2014_2022_df %>%
  # convert dataframe into long format where there is only one NBR column
  pivot_longer(
    cols = starts_with("NBR"),
    names_to = "NBR",
    values_to = "NBR_Value"
  ) %>% 
  na.omit() %>%
  mutate(Date = as.Date(str_extract(NBR, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NBR_Value')) # select relevant columns only

nbr_2014_2022_dfXxy_long$x <- round(nbr_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
nbr_2014_2022_dfXxy_long$y <- round(nbr_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
nbr_2014_2022_dfXxy_long$NBR_Value <- minmax_norm(nbr_2014_2022_dfXxy_long$NBR_Value) # apply min-max normalisation
# head(nbr_2014_2022_dfXxy_long); str(nbr_2014_2022_dfXxy_long)


atp_2014_2022_df <- as.data.frame(ATP_2014_2022_stack, xy = T) # converting stacked ATP into dataframe
atp_2014_2022_dfXxy_long <-  atp_2014_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value')) # select relevant columns only

atp_2014_2022_dfXxy_long$x <- round(atp_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
atp_2014_2022_dfXxy_long$y <- round(atp_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
atp_2014_2022_dfXxy_long$TP_Value <- minmax_norm(atp_2014_2022_dfXxy_long$TP_Value) # apply min-max normalisation
# head(atp_2014_2022_dfXxy_long); str(atp_2014_2022_dfXxy_long)

amt_2014_2022_df <- as.data.frame(AMT_2014_2022_stack, xy = T) # converting stacked AMT into dataframe
amt_2014_2022_dfXxy_long <-  amt_2014_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) # select relevant columns only

amt_2014_2022_dfXxy_long$x <- round(amt_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
amt_2014_2022_dfXxy_long$y <- round(amt_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
amt_2014_2022_dfXxy_long$AMT_Value <- minmax_norm(amt_2014_2022_dfXxy_long$AMT_Value) # apply min-max normalisation
# head(amt_2014_2022_dfXxy_long); str(amt_2014_2022_dfXxy_long)

answs_2014_2022_df <- as.data.frame(ANSWS_2014_2022_stack, xy = T) # converting stacked ANSWS into dataframe
answs_2014_2022_dfXxy_long <-  answs_2014_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) # select relevant columns only

answs_2014_2022_dfXxy_long$x <- round(answs_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
answs_2014_2022_dfXxy_long$y <- round(answs_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
answs_2014_2022_dfXxy_long$ANSWS_Value <- minmax_norm(answs_2014_2022_dfXxy_long$ANSWS_Value) # apply min-max normalisation
# head(answs_2014_2022_dfXxy_long); str(answs_2014_2022_dfXxy_long)

arh_2014_2022_df <- as.data.frame(ARH_2014_2022_stack, xy = T) # converting stacked ARH into dataframe
arh_2014_2022_dfXxy_long <-  arh_2014_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) # select relevant columns only

arh_2014_2022_dfXxy_long$x <- round(arh_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
arh_2014_2022_dfXxy_long$y <- round(arh_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
arh_2014_2022_dfXxy_long$ARH_Value <- minmax_norm(arh_2014_2022_dfXxy_long$ARH_Value) # apply min-max normalisation
# head(arh_2014_2022_dfXxy_long); str(arh_2014_2022_dfXxy_long)


elev_2014_2022_df <- as.data.frame(elevation_replicated_for_2014_to_2022_stack, xy = T) # converting stacked elevation into dataframe
elev_2014_2022_dfXxy_long <-  elev_2014_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) # select relevant columns only

elev_2014_2022_dfXxy_long$x <- round(elev_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
elev_2014_2022_dfXxy_long$y <- round(elev_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
elev_2014_2022_dfXxy_long$Elev_Value <- minmax_norm(elev_2014_2022_dfXxy_long$Elev_Value) # apply min-max normalisation
# head(elev_2014_2022_dfXxy_long); str(elev_2014_2022_dfXxy_long)

slope_2014_2022_df <- as.data.frame(slope_replicated_for_2014_to_2022_stack, xy = T) # converting stacked slope into dataframe
slope_2014_2022_dfXxy_long <-  slope_2014_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) # select relevant columns only

slope_2014_2022_dfXxy_long$x <- round(slope_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
slope_2014_2022_dfXxy_long$y <- round(slope_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
slope_2014_2022_dfXxy_long$Slope_Value <- minmax_norm(slope_2014_2022_dfXxy_long$Slope_Value) # apply min-max normalisation
# head(slope_2014_2022_dfXxy_long); str(slope_2014_2022_dfXxy_long)


aspect_2014_2022_df <- as.data.frame(aspect_replicated_for_2014_to_2022_stack, xy = T) # converting stacked aspect into dataframe
aspect_2014_2022_dfXxy_long <-  aspect_2014_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) # select relevant columns only

aspect_2014_2022_dfXxy_long$x <- round(aspect_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
aspect_2014_2022_dfXxy_long$y <- round(aspect_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
aspect_2014_2022_dfXxy_long$Aspect_Value <- minmax_norm(aspect_2014_2022_dfXxy_long$Aspect_Value) # apply min-max normalisation
# head(aspect_2014_2022_dfXxy_long); str(aspect_2014_2022_dfXxy_long)

fire_2014_2022_df <- as.data.frame(FIRE_2014_2022_stack, xy = T) # converting stacked fire into dataframe
fire_2014_2022_dfXxy_long <-  fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) # select relevant columns only

fire_2014_2022_dfXxy_long$x <- round(fire_2014_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
fire_2014_2022_dfXxy_long$y <- round(fire_2014_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p

# head(fire_2014_2022_dfXxy_long); str(fire_2014_2022_dfXxy_long)

# Add all the normalised dataframe in one list ----------------------------
dfnorm_2014_2022_list <- list(lulc_2014_2022_dfXxy_long, 
                              ndvi_2014_2022_dfXxy_long,
                              ndmi_2014_2022_dfXxy_long,
                              nbr_2014_2022_dfXxy_long,
                              atp_2014_2022_dfXxy_long,
                              amt_2014_2022_dfXxy_long,
                              answs_2014_2022_dfXxy_long,
                              arh_2014_2022_dfXxy_long,
                              elev_2014_2022_dfXxy_long,
                              slope_2014_2022_dfXxy_long,
                              aspect_2014_2022_dfXxy_long,
                              fire_2014_2022_dfXxy_long)

# Combine all the normalised dataframe into one [2014-2022] ---------------
dfnorm_2014_2022 <- reduce(dfnorm_2014_2022_list, inner_join, by = c("x", "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
# remove waterbodies
dfnorm_2014_2022 <- dfnorm_2014_2022 %>%
  filter(LULC_Class!=1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.
# str(dfnorm_2014_2022)

# save dataframe
# save(dfnorm_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022.Rdata')

Fire_periods <- resampled_dfnorm_2014_2022 %>%
  group_by(Year, Month, Fire_Value) %>%
  tally() %>% # compute the number of pixels with and without fire 
  filter(Fire_Value==1) %>% # extract the ones with fire only
  dplyr::select(Year, Month)

fully_resampled_dfnorm_2014_2022 <- resampled_dfnorm_2014_2022 %>%
  inner_join(Fire_periods, by = c('Year', 'Month')) %>% # only retain period with fire events
  filter(LULC_Class!=1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.

# save dataframe
# save(fully_resampled_dfnorm_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_dfnorm_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_dfnorm_2014_2022.Rdata')

  
# xx <- rasterFromXYZ(fully_resampled_dfnorm_2014_2022, res = c(30,30), crs = crs(roi_trans))
# plot(xx, col = fire_color_condition_func(xx), cex.main = .9, main = '2014-01')


# # multicollinearity test on 2014 to 2022 dataset
# multicollinearity_reduction_via_pearson_correlation_2014_2022 <- spatialRF::auto_cor(
#   x = dfnorm_2014_2022[colnames(dfnorm_2014_2022)[-c(1,2,3,4,16)]],
#   cor.threshold = 0.5,
# ) 

# VIF test on 2014 to 2022 dataset
VIF_2014_2022 <- spatialRF::auto_vif(x = fully_resampled_dfnorm_2014_2022[colnames(fully_resampled_dfnorm_2014_2022)[-c(1,2,3,4,16)]], 
                           vif.threshold = 5)

VIF_2014_2022$vif

# Converting stacked rasters into dataframe [2002-2022] -------------------

atp_2002_2022_df <- as.data.frame(ATP_2002_2022_stack, xy = T) # converting stacked ATP into dataframe
atp_2002_2022_dfXxy_long <-  atp_2002_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value')) # select relevant columns only

atp_2002_2022_dfXxy_long$x <- round(atp_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
atp_2002_2022_dfXxy_long$y <- round(atp_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
atp_2002_2022_dfXxy_long$TP_Value <- minmax_norm(atp_2002_2022_dfXxy_long$TP_Value) # apply min-max normalisation
# head(atp_2002_2022_dfXxy_long); str(atp_2002_2022_dfXxy_long)


amt_2002_2022_df <- as.data.frame(AMT_2002_2022_stack, xy = T) # converting stacked AMT into dataframe
amt_2002_2022_dfXxy_long <-  amt_2002_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) # select relevant columns only

amt_2002_2022_dfXxy_long$x <- round(amt_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
amt_2002_2022_dfXxy_long$y <- round(amt_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
amt_2002_2022_dfXxy_long$AMT_Value <- minmax_norm(amt_2002_2022_dfXxy_long$AMT_Value) # apply min-max normalisation
# head(amt_2002_2022_dfXxy_long); str(amt_2002_2022_dfXxy_long)

answs_2002_2022_df <- as.data.frame(ANSWS_2002_2022_stack, xy = T) # converting stacked ANSWS into dataframe
answs_2002_2022_dfXxy_long <-  answs_2002_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) # select relevant columns only

answs_2002_2022_dfXxy_long$x <- round(answs_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
answs_2002_2022_dfXxy_long$y <- round(answs_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
answs_2002_2022_dfXxy_long$ANSWS_Value <- minmax_norm(answs_2002_2022_dfXxy_long$ANSWS_Value) # apply min-max normalisation
# head(answs_2002_2022_dfXxy_long); str(answs_2002_2022_dfXxy_long)

arh_2002_2022_df <- as.data.frame(ARH_2002_2022_stack, xy = T) # converting stacked ARH into dataframe
arh_2002_2022_dfXxy_long <-  arh_2002_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) # select relevant columns only

arh_2002_2022_dfXxy_long$x <- round(arh_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
arh_2002_2022_dfXxy_long$y <- round(arh_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
arh_2002_2022_dfXxy_long$ARH_Value <- minmax_norm(arh_2002_2022_dfXxy_long$ARH_Value) # apply min-max normalisation
# head(arh_2002_2022_dfXxy_long); str(arh_2002_2022_dfXxy_long)

elev_2002_2022_df <- as.data.frame(elevation_replicated_for_2002_to_2022_stack, xy = T) # converting stacked elevation into dataframe
elev_2002_2022_dfXxy_long <-  elev_2002_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) # select relevant columns only

elev_2002_2022_dfXxy_long$x <- round(elev_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
elev_2002_2022_dfXxy_long$y <- round(elev_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
elev_2002_2022_dfXxy_long$Elev_Value <- minmax_norm(elev_2002_2022_dfXxy_long$Elev_Value) # apply min-max normalisation
# head(elev_2002_2022_dfXxy_long); str(elev_2002_2022_dfXxy_long)

slope_2002_2022_df <- as.data.frame(slope_replicated_for_2002_to_2022_stack, xy = T) # converting stacked slope into dataframe
slope_2002_2022_dfXxy_long <-  slope_2002_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) # select relevant columns only

slope_2002_2022_dfXxy_long$x <- round(slope_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
slope_2002_2022_dfXxy_long$y <- round(slope_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
slope_2002_2022_dfXxy_long$Slope_Value <- minmax_norm(slope_2002_2022_dfXxy_long$Slope_Value) # apply min-max normalisation
# head(slope_2002_2022_dfXxy_long); str(slope_2002_2022_dfXxy_long)


aspect_2002_2022_df <- as.data.frame(aspect_replicated_for_2002_to_2022_stack, xy = T) # converting stacked aspect into dataframe
aspect_2002_2022_dfXxy_long <-  aspect_2002_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) # select relevant columns only

aspect_2002_2022_dfXxy_long$x <- round(aspect_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
aspect_2002_2022_dfXxy_long$y <- round(aspect_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p
aspect_2002_2022_dfXxy_long$Aspect_Value <- minmax_norm(aspect_2002_2022_dfXxy_long$Aspect_Value) # apply min-max normalisation
# head(aspect_2002_2022_dfXxy_long); str(aspect_2002_2022_dfXxy_long)

fire_2002_2022_df <- as.data.frame(FIRE_2002_2022_stack, xy = T) # converting stacked fire into dataframe
fire_2002_2022_dfXxy_long <-  fire_2002_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) # select relevant columns only

fire_2002_2022_dfXxy_long$x <- round(fire_2002_2022_dfXxy_long$x, 5) # round x coordinates to 5 d.p
fire_2002_2022_dfXxy_long$y <- round(fire_2002_2022_dfXxy_long$y, 5) # round y coordinates to 5 d.p

# head(fire_2002_2022_dfXxy_long); str(fire_2002_2022_dfXxy_long)

# Add all the normalised dataframe in one list ----------------------------
dfnorm_2002_2022_list <- list(atp_2002_2022_dfXxy_long,
                              amt_2002_2022_dfXxy_long,
                              answs_2002_2022_dfXxy_long,
                              arh_2002_2022_dfXxy_long,
                              elev_2002_2022_dfXxy_long,
                              slope_2002_2022_dfXxy_long,
                              aspect_2002_2022_dfXxy_long,
                              fire_2002_2022_dfXxy_long)


# Combine all the normalised dataframe into one [2002-2022] ---------------
dfnorm_2002_2022 <- reduce(dfnorm_2002_2022_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)

# str(dfnorm_2002_2022)

# save dataframe
# save(dfnorm_2002_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022.Rdata')

# # multicollinearity test on 2002 to 2022 dataset
# multicollinearity_reduction_via_pearson_correlation_2002_2022 <- spatialRF::auto_cor(
#   x = dfnorm_2002_2022[colnames(dfnorm_2002_2022)[-c(1,2,3,4,12)]],
#   cor.threshold = 0.5
# ) 

# VIF test on 2002 to 2022 dataset
VIF_2002_2022 <- spatialRF::auto_vif(x = dfnorm_2002_2022[colnames(dfnorm_2002_2022)[-c(1,2,3,4,12)]], 
                                     vif.threshold = 5)

VIF_2002_2022$vif



# resampled_fire_2014_2022_df <- as.data.frame(RESAMPLED_FIRE_2014_2022_training_stack, xy = T)# converting stacked fire into dataframe
# resampled_fire_2014_2022_dfXxy_long_train <-  resampled_fire_2014_2022_df %>%
#   # convert dataframe into long format where there is only one fire column
#   pivot_longer(
#     cols = starts_with("Fire"),
#     names_to = "Fire",
#     values_to = "Fire_Value"
#   ) %>%
#   na.omit() %>% # very important to omit rows after pivot longer
#   mutate(Year = str_extract(Fire, "\\d{4}") |> as.integer(), # extract year from date
#          Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}") |> as.integer()) %>% # extract month from date
#   dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) # select relevant columns only
# 
# resampled_fire_2014_2022_dfXxy_long_train$x <- round(resampled_fire_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
# resampled_fire_2014_2022_dfXxy_long_train$y <- round(resampled_fire_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
# # head(resampled_fire_2014_2022_dfXxy_long_train); str(resampled_fire_2014_2022_dfXxy_long_train)



# Add all the normalised training set from the 2014 to 2022 dataset in one list
# dfnorm_2014_2022_training_list <-  list(lulc_2014_2022_dfXxy_long_train,
#                                         ndvi_2014_2022_dfXxy_long_train,
#                                         ndmi_2014_2022_dfXxy_long_train,
#                                         nbr_2014_2022_dfXxy_long_train,
#                                         atp_2014_2022_dfXxy_long_train,
#                                         amt_2014_2022_dfXxy_long_train,
#                                         answs_2014_2022_dfXxy_long_train,
#                                         arh_2014_2022_dfXxy_long_train,
#                                         elev_2014_2022_dfXxy_long_train,
#                                         slope_2014_2022_dfXxy_long_train,
#                                         aspect_2014_2022_dfXxy_long_train,
#                                         fire_2014_2022_dfXxy_long_train)

# resampled_dfnorm_2014_2022_training_list <-  list(lulc_2014_2022_dfXxy_long_train,
#                                                   ndvi_2014_2022_dfXxy_long_train,
#                                                   ndmi_2014_2022_dfXxy_long_train,
#                                                   nbr_2014_2022_dfXxy_long_train,
#                                                   atp_2014_2022_dfXxy_long_train,
#                                                   amt_2014_2022_dfXxy_long_train,
#                                                   answs_2014_2022_dfXxy_long_train,
#                                                   arh_2014_2022_dfXxy_long_train,
#                                                   elev_2014_2022_dfXxy_long_train,
#                                                   slope_2014_2022_dfXxy_long_train,
#                                                   aspect_2014_2022_dfXxy_long_train,
#                                                   resampled_fire_2014_2022_dfXxy_long_train)


# # Combine all the normalised training set from the 2014 to 2022 dataset in one dataframe 
# dfnorm_2014_2022_training_set <- reduce(dfnorm_2014_2022_training_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
# # remove waterbodies
# dfnorm_2014_2022_training_set <- dfnorm_2014_2022_training_set %>%
#   filter(LULC_Class!=1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.
# # str(dfnorm_2014_2022_training_set)
# 
# # save dataframe
# save(dfnorm_2014_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_training_set.Rdata')
# # load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_training_set.Rdata')
# 
# resampled_dfnorm_2014_2022_training_set <- reduce(resampled_dfnorm_2014_2022_training_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
# # str(resampled_dfnorm_2014_2022_training_set)
# 
# # save dataframe
# # save(resampled_dfnorm_2014_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/resampled_dfnorm_2014_2022_training_set.Rdata')
# # load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/resampled_dfnorm_2014_2022_training_set.Rdata')
# 
# training_Fire_periods <- resampled_dfnorm_2014_2022_training_set %>%
#   group_by(Year, Month, Fire_Value) %>%
#   tally() %>% # compute the number of pixels with and without fire 
#   filter(Fire_Value==1) %>% # extract the ones with fire only
#   dplyr::select(Year, Month)
# 
# fully_resampled_dfnorm_2014_2022_training_set <- resampled_dfnorm_2014_2022_training_set %>%
#   inner_join(training_Fire_periods, by = c('Year', 'Month')) %>% # only retain period with fire events
#   filter(LULC_Class!=1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.
# 
# # save dataframe
# # save(fully_resampled_dfnorm_2014_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_dfnorm_2014_2022_training_set.Rdata')
# # load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_dfnorm_2014_2022_training_set.Rdata')


################################################################################
# Convert training set raster stack [from 2002-2022 dataset] into dataframe
atp_2002_2022_df <- as.data.frame(ATP_2002_2022_stack, xy = T, na.rm = T) # converting stacked ATP into dataframe
atp_2002_2022_dfXxy_long_train <-  atp_2002_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

atp_2002_2022_dfXxy_long_train$x <- round(atp_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
atp_2002_2022_dfXxy_long_train$y <- round(atp_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
atp_2002_2022_dfXxy_long_train$TP_Value <- minmax_norm(atp_2002_2022_dfXxy_long_train$TP_Value) # apply min-max normalisation
# head(atp_2002_2022_dfXxy_long_train); str(atp_2002_2022_dfXxy_long_train)


amt_2002_2022_df <- as.data.frame(AMT_2002_2022_stack, xy = T, na.rm = T) # converting stacked AMT into dataframe
amt_2002_2022_dfXxy_long_train <-  amt_2002_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

amt_2002_2022_dfXxy_long_train$x <- round(amt_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
amt_2002_2022_dfXxy_long_train$y <- round(amt_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
amt_2002_2022_dfXxy_long_train$AMT_Value <- minmax_norm(amt_2002_2022_dfXxy_long_train$AMT_Value) # apply min-max normalisation
# head(amt_2002_2022_dfXxy_long_train); str(amt_2002_2022_dfXxy_long_train)

answs_2002_2022_df <- as.data.frame(ANSWS_2002_2022_stack, xy = T, na.rm = T) # converting stacked ANSWS into dataframe
answs_2002_2022_dfXxy_long_train <-  answs_2002_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

answs_2002_2022_dfXxy_long_train$x <- round(answs_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
answs_2002_2022_dfXxy_long_train$y <- round(answs_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
answs_2002_2022_dfXxy_long_train$ANSWS_Value <- minmax_norm(answs_2002_2022_dfXxy_long_train$ANSWS_Value) # apply min-max normalisation
# head(answs_2002_2022_dfXxy_long_train); str(answs_2002_2022_dfXxy_long_train)

arh_2002_2022_df <- as.data.frame(ARH_2002_2022_stack, xy = T, na.rm = T) # converting stacked ARH into dataframe
arh_2002_2022_dfXxy_long_train <-  arh_2002_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

arh_2002_2022_dfXxy_long_train$x <- round(arh_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
arh_2002_2022_dfXxy_long_train$y <- round(arh_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
arh_2002_2022_dfXxy_long_train$ARH_Value <- minmax_norm(arh_2002_2022_dfXxy_long_train$ARH_Value) # apply min-max normalisation
# head(arh_2002_2022_dfXxy_long_train); str(arh_2002_2022_dfXxy_long_train)

elev_2002_2022_df <- as.data.frame(elevation_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked elevation into dataframe
elev_2002_2022_dfXxy_long_train <-  elev_2002_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

elev_2002_2022_dfXxy_long_train$x <- round(elev_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
elev_2002_2022_dfXxy_long_train$y <- round(elev_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
elev_2002_2022_dfXxy_long_train$Elev_Value <- minmax_norm(elev_2002_2022_dfXxy_long_train$Elev_Value) # apply min-max normalisation
# head(elev_2002_2022_dfXxy_long_train); str(elev_2002_2022_dfXxy_long_train)

slope_2002_2022_df <- as.data.frame(slope_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked slope into dataframe
slope_2002_2022_dfXxy_long_train <-  slope_2002_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

slope_2002_2022_dfXxy_long_train$x <- round(slope_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
slope_2002_2022_dfXxy_long_train$y <- round(slope_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
slope_2002_2022_dfXxy_long_train$Slope_Value <- minmax_norm(slope_2002_2022_dfXxy_long_train$Slope_Value) # apply min-max normalisation
# head(slope_2002_2022_dfXxy_long_train); str(slope_2002_2022_dfXxy_long_train)


aspect_2002_2022_df <- as.data.frame(aspect_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked aspect into dataframe
aspect_2002_2022_dfXxy_long_train <-  aspect_2002_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

aspect_2002_2022_dfXxy_long_train$x <- round(aspect_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
aspect_2002_2022_dfXxy_long_train$y <- round(aspect_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
aspect_2002_2022_dfXxy_long_train$Aspect_Value <- minmax_norm(aspect_2002_2022_dfXxy_long_train$Aspect_Value) # apply min-max normalisation
# head(aspect_2002_2022_dfXxy_long_train); str(aspect_2002_2022_dfXxy_long_train)

fire_2002_2022_df <- as.data.frame(FIRE_2002_2022_stack, xy = T, na.rm = T) # converting stacked fire into dataframe
fire_2002_2022_dfXxy_long_train <-  fire_2002_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% 2002:2018)  # filter years to be used as training set

fire_2002_2022_dfXxy_long_train$x <- round(fire_2002_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
fire_2002_2022_dfXxy_long_train$y <- round(fire_2002_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
# head(fire_2002_2022_dfXxy_long_train); str(fire_2002_2022_dfXxy_long_train)

# Add all the normalised training set from the 2002 to 2022 dataset in one list 
dfnorm_2002_2022_training_list <-  list(atp_2002_2022_dfXxy_long_train,
                                        amt_2002_2022_dfXxy_long_train,
                                        answs_2002_2022_dfXxy_long_train,
                                        arh_2002_2022_dfXxy_long_train,
                                        elev_2002_2022_dfXxy_long_train,
                                        slope_2002_2022_dfXxy_long_train,
                                        aspect_2002_2022_dfXxy_long_train,
                                        fire_2002_2022_dfXxy_long_train)

# Combine all the normalised training set from the 2002 to 2022 dataset in one dataframe 
dfnorm_2002_2022_training_set <- reduce(dfnorm_2002_2022_training_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
# str(dfnorm_2002_2022_training_set)

# save dataframe
# save(dfnorm_2002_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022_training_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022_training_set.Rdata')

# Convert validation set raster stack [from 2002-2022 dataset] into dataframe
atp_2002_2022_df <- as.data.frame(ATP_2002_2022_stack, xy = T, na.rm = T) # converting stacked ATP into dataframe
atp_2002_2022_dfXxy_long_val <-  atp_2002_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

atp_2002_2022_dfXxy_long_val$x <- round(atp_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
atp_2002_2022_dfXxy_long_val$y <- round(atp_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
atp_2002_2022_dfXxy_long_val$TP_Value <- minmax_norm(atp_2002_2022_dfXxy_long_val$TP_Value) # apply min-max normalisation
# head(atp_2002_2022_dfXxy_long_val); str(atp_2002_2022_dfXxy_long_val)


# amt_2002_2022_df <- as.data.frame(AMT_2002_2022_stack, xy = T, na.rm = T) # converting stacked AMT into dataframe
amt_2002_2022_dfXxy_long_val <-  amt_2002_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

amt_2002_2022_dfXxy_long_val$x <- round(amt_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
amt_2002_2022_dfXxy_long_val$y <- round(amt_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
amt_2002_2022_dfXxy_long_val$AMT_Value <- minmax_norm(amt_2002_2022_dfXxy_long_val$AMT_Value) # apply min-max normalisation
# head(amt_2002_2022_dfXxy_long_val); str(amt_2002_2022_dfXxy_long_val)

# answs_2002_2022_df <- as.data.frame(ANSWS_2002_2022_stack, xy = T, na.rm = T) # converting stacked ANSWS into dataframe
answs_2002_2022_dfXxy_long_val <-  answs_2002_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

answs_2002_2022_dfXxy_long_val$x <- round(answs_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
answs_2002_2022_dfXxy_long_val$y <- round(answs_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
answs_2002_2022_dfXxy_long_val$ANSWS_Value <- minmax_norm(answs_2002_2022_dfXxy_long_val$ANSWS_Value) # apply min-max normalisation
# head(answs_2002_2022_dfXxy_long_val); str(answs_2002_2022_dfXxy_long_val)

# arh_2002_2022_df <- as.data.frame(ARH_2002_2022_stack, xy = T, na.rm = T) # converting stacked ARH into dataframe
arh_2002_2022_dfXxy_long_val <-  arh_2002_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

arh_2002_2022_dfXxy_long_val$x <- round(arh_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
arh_2002_2022_dfXxy_long_val$y <- round(arh_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
arh_2002_2022_dfXxy_long_val$ARH_Value <- minmax_norm(arh_2002_2022_dfXxy_long_val$ARH_Value) # apply min-max normalisation
# head(arh_2002_2022_dfXxy_long_val); str(arh_2002_2022_dfXxy_long_val)

# elev_2002_2022_df <- as.data.frame(elevation_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked elevation into dataframe
elev_2002_2022_dfXxy_long_val <-  elev_2002_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

elev_2002_2022_dfXxy_long_val$x <- round(elev_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
elev_2002_2022_dfXxy_long_val$y <- round(elev_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
elev_2002_2022_dfXxy_long_val$Elev_Value <- minmax_norm(elev_2002_2022_dfXxy_long_val$Elev_Value) # apply min-max normalisation
# head(elev_2002_2022_dfXxy_long_val); str(elev_2002_2022_dfXxy_long_val)

# slope_2002_2022_df <- as.data.frame(slope_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked slope into dataframe
slope_2002_2022_dfXxy_long_val <-  slope_2002_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

slope_2002_2022_dfXxy_long_val$x <- round(slope_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
slope_2002_2022_dfXxy_long_val$y <- round(slope_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
slope_2002_2022_dfXxy_long_val$Slope_Value <- minmax_norm(slope_2002_2022_dfXxy_long_val$Slope_Value) # apply min-max normalisation
# head(slope_2002_2022_dfXxy_long_val); str(slope_2002_2022_dfXxy_long_val)

# aspect_2002_2022_df <- as.data.frame(aspect_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked aspect into dataframe
aspect_2002_2022_dfXxy_long_val <-  aspect_2002_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

aspect_2002_2022_dfXxy_long_val$x <- round(aspect_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
aspect_2002_2022_dfXxy_long_val$y <- round(aspect_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
aspect_2002_2022_dfXxy_long_val$Aspect_Value <- minmax_norm(aspect_2002_2022_dfXxy_long_val$Aspect_Value) # apply min-max normalisation
# head(aspect_2002_2022_dfXxy_long_val); str(aspect_2002_2022_dfXxy_long_val)

# fire_2002_2022_df <- as.data.frame(FIRE_2002_2022_stack, xy = T, na.rm = T) # converting stacked fire into dataframe
fire_2002_2022_dfXxy_long_val <-  fire_2002_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

fire_2002_2022_dfXxy_long_val$x <- round(fire_2002_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
fire_2002_2022_dfXxy_long_val$y <- round(fire_2002_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p

# head(fire_2002_2022_dfXxy_long_val); str(fire_2002_2022_dfXxy_long_val)

# Add all the normalised validation set from the 2002 to 2022 dataset in one list 
dfnorm_2002_2022_validation_list <-  list(atp_2002_2022_dfXxy_long_val,
                                        amt_2002_2022_dfXxy_long_val,
                                        answs_2002_2022_dfXxy_long_val,
                                        arh_2002_2022_dfXxy_long_val,
                                        elev_2002_2022_dfXxy_long_val,
                                        slope_2002_2022_dfXxy_long_val,
                                        aspect_2002_2022_dfXxy_long_val,
                                        fire_2002_2022_dfXxy_long_val)

# Combine all the normalised validation set from the 2002 to 2022 dataset in one dataframe 
dfnorm_2002_2022_validation_set <- reduce(dfnorm_2002_2022_validation_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
# str(dfnorm_2002_2022_validation_set)

# save dataframe
# save(dfnorm_2002_2022_validation_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022_validation_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022_validation_set.Rdata')

# Convert test set raster stack [from 2002-2022 dataset] into dataframe
atp_2002_2022_df <- as.data.frame(ATP_2002_2022_stack, xy = T, na.rm = T) # converting stacked ATP into dataframe
atp_2002_2022_dfXxy_long_test <-  atp_2002_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

atp_2002_2022_dfXxy_long_test$x <- round(atp_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
atp_2002_2022_dfXxy_long_test$y <- round(atp_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
atp_2002_2022_dfXxy_long_test$TP_Value <- minmax_norm(atp_2002_2022_dfXxy_long_test$TP_Value) # apply min-max normalisation
head(atp_2002_2022_dfXxy_long_test); str(atp_2002_2022_dfXxy_long_test)


amt_2002_2022_df <- as.data.frame(AMT_2002_2022_stack, xy = T, na.rm = T) # converting stacked AMT into dataframe
amt_2002_2022_dfXxy_long_test <-  amt_2002_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

amt_2002_2022_dfXxy_long_test$x <- round(amt_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
amt_2002_2022_dfXxy_long_test$y <- round(amt_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
amt_2002_2022_dfXxy_long_test$AMT_Value <- minmax_norm(amt_2002_2022_dfXxy_long_test$AMT_Value) # apply min-max normalisation
head(amt_2002_2022_dfXxy_long_test); str(amt_2002_2022_dfXxy_long_test)

answs_2002_2022_df <- as.data.frame(ANSWS_2002_2022_stack, xy = T, na.rm = T) # converting stacked ANSWS into dataframe
answs_2002_2022_dfXxy_long_test <-  answs_2002_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

answs_2002_2022_dfXxy_long_test$x <- round(answs_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
answs_2002_2022_dfXxy_long_test$y <- round(answs_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
answs_2002_2022_dfXxy_long_test$ANSWS_Value <- minmax_norm(answs_2002_2022_dfXxy_long_test$ANSWS_Value) # apply min-max normalisation
head(answs_2002_2022_dfXxy_long_test); str(answs_2002_2022_dfXxy_long_test)

arh_2002_2022_df <- as.data.frame(ARH_2002_2022_stack, xy = T, na.rm = T) # converting stacked ARH into dataframe
arh_2002_2022_dfXxy_long_test <-  arh_2002_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

arh_2002_2022_dfXxy_long_test$x <- round(arh_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
arh_2002_2022_dfXxy_long_test$y <- round(arh_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
arh_2002_2022_dfXxy_long_test$ARH_Value <- minmax_norm(arh_2002_2022_dfXxy_long_test$ARH_Value) # apply min-max normalisation
head(arh_2002_2022_dfXxy_long_test); str(arh_2002_2022_dfXxy_long_test)

elev_2002_2022_df <- as.data.frame(elevation_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked elevation into dataframe
elev_2002_2022_dfXxy_long_test <-  elev_2002_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

elev_2002_2022_dfXxy_long_test$x <- round(elev_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
elev_2002_2022_dfXxy_long_test$y <- round(elev_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
elev_2002_2022_dfXxy_long_test$Elev_Value <- minmax_norm(elev_2002_2022_dfXxy_long_test$Elev_Value) # apply min-max normalisation
head(elev_2002_2022_dfXxy_long_test); str(elev_2002_2022_dfXxy_long_test)

slope_2002_2022_df <- as.data.frame(slope_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked slope into dataframe
slope_2002_2022_dfXxy_long_test <-  slope_2002_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

slope_2002_2022_dfXxy_long_test$x <- round(slope_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
slope_2002_2022_dfXxy_long_test$y <- round(slope_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
slope_2002_2022_dfXxy_long_test$Slope_Value <- minmax_norm(slope_2002_2022_dfXxy_long_test$Slope_Value) # apply min-max normalisation
head(slope_2002_2022_dfXxy_long_test); str(slope_2002_2022_dfXxy_long_test)


aspect_2002_2022_df <- as.data.frame(aspect_replicated_for_2002_to_2022_stack, xy = T, na.rm = T) # converting stacked aspect into dataframe
aspect_2002_2022_dfXxy_long_test <-  aspect_2002_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

aspect_2002_2022_dfXxy_long_test$x <- round(aspect_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
aspect_2002_2022_dfXxy_long_test$y <- round(aspect_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
aspect_2002_2022_dfXxy_long_test$Aspect_Value <- minmax_norm(aspect_2002_2022_dfXxy_long_test$Aspect_Value) # apply min-max normalisation
head(aspect_2002_2022_dfXxy_long_test); str(aspect_2002_2022_dfXxy_long_test)

fire_2002_2022_df <- as.data.frame(FIRE_2002_2022_stack, xy = T, na.rm = T) # converting stacked fire into dataframe
fire_2002_2022_dfXxy_long_test <-  fire_2002_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as validation set

fire_2002_2022_dfXxy_long_test$x <- round(fire_2002_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
fire_2002_2022_dfXxy_long_test$y <- round(fire_2002_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
head(fire_2002_2022_dfXxy_long_test); str(fire_2002_2022_dfXxy_long_test)

# Add all the normalised test set from the 2002 to 2022 dataset in one list 
dfnorm_2002_2022_test_list <-  list(atp_2002_2022_dfXxy_long_test,
                                          amt_2002_2022_dfXxy_long_test,
                                          answs_2002_2022_dfXxy_long_test,
                                          arh_2002_2022_dfXxy_long_test,
                                          elev_2002_2022_dfXxy_long_test,
                                          slope_2002_2022_dfXxy_long_test,
                                          aspect_2002_2022_dfXxy_long_test,
                                          fire_2002_2022_dfXxy_long_test)

# Combine all the normalised test set from the 2002 to 2022 dataset in one dataframe 
dfnorm_2002_2022_test_set <- reduce(dfnorm_2002_2022_test_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
# str(dfnorm_2002_2022_test_set)

# save dataframe
# save(dfnorm_2002_2022_test_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022_test_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Dataframe format (normalised)/dfnorm_2002_2022_test_set.Rdata')



#--------------------------------------------------------------------------------

# Example on how to convert the tabular data into raster format again after normalisation
x <- dfnorm_2014_2022 %>%
  filter(Year == 2021 & Month==04) %>%
  select(x,y,NDVI_Value)
  
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = NDVI_colour_ramp)
plot(roi_trans, col = 'transparent', border = 'black', add = T)


# Finding relative feature importance -------------------------------------
dfnorm_2014_2022$Fire_Value <- as.factor(dfnorm_2014_2022$Fire_Value) # convert response variable into factor

# # Create a subset of the dataframe
# set.seed(1)
# dfnorm_2014_2022_subset <- dfnorm_2014_2022 %>%
#   filter(Year %in% c(2018)) %>%
#   shuffle()
# 
# set.seed(1)
# rf_dfnorm_2014_2022 <- randomForest(Fire_Value ~., data = dfnorm_2014_2022_subset[,-c(1:4)],
#                               ntree = 100,
#                               importance = T, 
#                               do.trace = 10)
# 
# which.min(sqrt(rf_dfnorm_2014_2022$err.rate[,1]))
# 
# plot(rf_dfnorm_2014_2022$err.rate[,1], type = 'l')
# 
newdataX <- dfnorm_2014_2022 %>%
  filter(Year %in% c(2019)) %>%
  dplyr::select(-c(1:4,16))
newdataY <- dfnorm_2014_2022 %>%
  filter(Year %in% c(2019)) %>%
  dplyr::select(16) %>%
  as.vector()
# 
# y <- predict(rf_dfnorm_2014_2022, newdataX) |> as.vector()|>as.numeric()
# 
# (sum(newdataY$Fire_Value==y)/length(y))*100
# 
# create combinations of hyperparameters
rf_gridsearch <- expand.grid(mtry = 2:(ncol(dfnorm_2014_2022) - 1),
                       splitrule = c('gini', 'hellinger'), # gini for classification
                       min.node.size=seq(1, 16, 5))

# use ranger to run all these models
set.seed(1)
rf_gridsearch_Model_dfnorm_2014_2022_subset <- train(Fire_Value ~.,
                       data =  dfnorm_2014_2022_subset,
                       method = 'ranger',
                       num.trees = 100,
                       verbose = T,
                       trControl = trainControl(method = 'oob', verboseIter = T, allowParallel = T),
                       tuneGrid = rf_gridsearch,
                       importance = 'permutation') # Variable importance according to Mean Decrease in Accuracy (MDA)
# rerun above using ranger
# keep.probs = T
# save model
# save(rf_gridsearch_Model_dfnorm_2014_2022_subset, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_gridsearch_Model_dfnorm_2014_2022_subset.Rdata')

varImp(rf_gridsearch_Model_dfnorm_2014_2022_subset)
y <- predict(rf_gridsearch_Model_dfnorm_2014_2022_subset, newdataX) |> as.vector()|>as.numeric()

(sum(newdataY$Fire_Value==y)/length(y))*100



varImp(rf_dfnorm_2014_2022)

varImp(rfModel,scale = TRUE)[["importance"]][1,]


# Trying RF spatial model on data
dfnorm_2014_2022%>%
  group_by(Year)%>%
  filter(Fire_Value==1)%>%
  tally()

dfnorm_2014_2022_subset <- dfnorm_2014_2022 %>%
    filter(Year %in% c(2021), # picked a random year where a fire event took place
           Month %in% c(4)) # filter only the months where a fire took place

dfnorm_2014_2022_subset$x_norm <- minmax_norm(dfnorm_2014_2022_subset$x)
dfnorm_2014_2022_subset$y_norm <- minmax_norm(dfnorm_2014_2022_subset$y)
# x <- dfnorm_2014_2022_subset[,17:18]
# save(x, file = '/Users/tanweernujjoo/Desktop/dfsubset_xynorm.Rdata')

# 
# distance_matrix <- parDist(as.matrix(dfnorm_2014_2022_subset[,c('x_norm','y_norm')]),
#                            method = "euclidean", 
#                            threads = 4)

?dist

head(distance_matrix)
distance_matrix[1]
class(distance_matrix)

str(distance_matrix)
distance_matrix[2]
diag(distance_matrix)

model.non.spatial <- spatialRF::rf(
  data = dfnorm_2014_2022_subset,
  dependent.variable.name = dfnorm_2014_2022_subset$Fire_Value,
  predictor.variable.names = colnames(dfnorm_2014_2022_subset)[-c(1,2,3,4,16)],
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  seed = random.seed,
  verbose = FALSE
)



# load('/Users/tanweernujjoo/Desktop/plant_richness_df.rda')
# load('/Users/tanweernujjoo/Desktop/distance_matrix.rda')
# distance.matrix <- distance_matrix
# #distance thresholds (same units as distance_matrix)
# distance.thresholds <- c(0, 1000, 2000, 4000, 8000)
# range(my_distance_matrix)
# dim(distance_matrix)
# dim(plant_richness_df)
# head(plant_richness_df)
# str(plant_richness_df)
# summary(plant_richness_df[,c('x','y')])
# xy_plant_richness <- st_as_sf(plant_richness_df[,c('x','y')], coords = c("x", "y"), crs = 4326)
# my_distance_matrix <- (st_distance(xy_plant_richness)/1000) |> round() # distance matrix in km
# quantile(distance_matrix)
# #names of the response variable and the predictors
# dependent.variable.name <- "richness_species_vascular"
# predictor.variable.names <- colnames(plant_richness_df)[5:21]
# 
# preference.order <- c(
#   "climate_bio1_average_X_bias_area_km2",
#   "climate_aridity_index_average",
#   "climate_hypervolume",
#   "climate_bio1_average",
#   "climate_bio15_minimum",
#   "bias_area_km2"
# )
# 
# predictor.variable.names <- spatialRF::auto_cor(
#   x = plant_richness_df[, predictor.variable.names],
#   cor.threshold = 0.6,
#   preference.order = preference.order
# ) %>%
#   spatialRF::auto_vif(
#     vif.threshold = 2.5,
#     preference.order = preference.order
#   )
# names(predictor.variable.names)
# 
# spatialRF::plot_training_df_moran(
#   data = plant_richness_df,
#   dependent.variable.name = dependent.variable.name,
#   predictor.variable.names = predictor.variable.names,
#   distance.matrix = distance.matrix,
#   distance.thresholds = distance.thresholds,
#   fill.color = viridis::viridis(
#     100,
#     option = "F",
#     direction = -1
#   ),
#   point.color = "gray40"
# )
# 
# ?rf_spatial
