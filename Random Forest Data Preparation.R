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
  library(corrplot)
  library(randomForest)
  library(ranger)
}

# Define min-max normalisation function -----------------------------------
minmax_norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Preparing data for Random Forest modelling ------------------------------

# TRAINING ----------------------------------------------------------------

# Convert training set raster stack [from 2014-2022 dataset] into dataframe

lulc_2014_2022_df <- as.data.frame(LULC_2014_2022_stack, xy = T) # converting stacked LULC into dataframe
lulc_2014_2022_df_xy <- lulc_2014_2022_df[,1:2] # extracting xy coordinates from LULC dataframe
lulc_2014_2022_dfX <- lulc_2014_2022_df[,-c(1:2)] # LULC dataframe with only predictor variables
lulc_2014_2022_dfX <- apply(lulc_2014_2022_dfX, 2, function(x){as.numeric(as.factor(x))}) |> as.data.frame() # converting the classes into numeric
lulc_2014_2022_dfXxy <- cbind(lulc_2014_2022_df_xy, lulc_2014_2022_dfX) # reattach coordinates to LULC dataframe consisting of the predictor variables
lulc_2014_2022_dfXxy_long_train <- lulc_2014_2022_dfXxy %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'LULC_Class')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

lulc_2014_2022_dfXxy_long_train$x <- round(lulc_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
lulc_2014_2022_dfXxy_long_train$y <- round(lulc_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
lulc_2014_2022_dfXxy_long_train$LULC_Class <- minmax_norm(lulc_2014_2022_dfXxy_long_train$LULC_Class) # apply min-max normalisation
# head(lulc_2014_2022_dfXxy_long_train); str(lulc_2014_2022_dfXxy_long_train)

ndvi_2014_2022_df <- as.data.frame(NDVI_2014_2022_stack, xy = T) # converting stacked NDVI into dataframe
ndvi_2014_2022_dfXxy_long_train <-  ndvi_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDVI_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

ndvi_2014_2022_dfXxy_long_train$x <- round(ndvi_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long_train$y <- round(ndvi_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long_train$NDVI_Value <- minmax_norm(ndvi_2014_2022_dfXxy_long_train$NDVI_Value) # apply min-max normalisation
# head(ndvi_2014_2022_dfXxy_long_train); str(ndvi_2014_2022_dfXxy_long_train)

ndmi_2014_2022_df <- as.data.frame(NDMI_2014_2022_stack, xy = T) # converting stacked NDMI into dataframe
ndmi_2014_2022_dfXxy_long_train <-  ndmi_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDMI_Value'))  %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

ndmi_2014_2022_dfXxy_long_train$x <- round(ndmi_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long_train$y <- round(ndmi_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long_train$NDMI_Value <- minmax_norm(ndmi_2014_2022_dfXxy_long_train$NDMI_Value) # apply min-max normalisation
# head(ndmi_2014_2022_dfXxy_long_train); str(ndmi_2014_2022_dfXxy_long_train)

nbr_2014_2022_df <- as.data.frame(NBR_2014_2022_stack, xy = T) # converting stacked NBR into dataframe
nbr_2014_2022_dfXxy_long_train <-  nbr_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NBR_Value'))  %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

nbr_2014_2022_dfXxy_long_train$x <- round(nbr_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
nbr_2014_2022_dfXxy_long_train$y <- round(nbr_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
nbr_2014_2022_dfXxy_long_train$NBR_Value <- minmax_norm(nbr_2014_2022_dfXxy_long_train$NBR_Value) # apply min-max normalisation
# head(nbr_2014_2022_dfXxy_long_train); str(nbr_2014_2022_dfXxy_long_train)

atp_2014_2022_df <- as.data.frame(ATP_2014_2022_stack, xy = T) # converting stacked ATP into dataframe
atp_2014_2022_dfXxy_long_train <-  atp_2014_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value'))  %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

atp_2014_2022_dfXxy_long_train$x <- round(atp_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
atp_2014_2022_dfXxy_long_train$y <- round(atp_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
atp_2014_2022_dfXxy_long_train$TP_Value <- minmax_norm(atp_2014_2022_dfXxy_long_train$TP_Value) # apply min-max normalisation
# head(atp_2014_2022_dfXxy_long_train); str(atp_2014_2022_dfXxy_long_train)

amt_2014_2022_df <- as.data.frame(AMT_2014_2022_stack, xy = T) # converting stacked AMT into dataframe
amt_2014_2022_dfXxy_long_train <-  amt_2014_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

amt_2014_2022_dfXxy_long_train$x <- round(amt_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
amt_2014_2022_dfXxy_long_train$y <- round(amt_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
amt_2014_2022_dfXxy_long_train$AMT_Value <- minmax_norm(amt_2014_2022_dfXxy_long_train$AMT_Value) # apply min-max normalisation
# head(amt_2014_2022_dfXxy_long_train); str(amt_2014_2022_dfXxy_long_train)

answs_2014_2022_df <- as.data.frame(ANSWS_2014_2022_stack, xy = T) # converting stacked ANSWS into dataframe
answs_2014_2022_dfXxy_long_train <-  answs_2014_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

answs_2014_2022_dfXxy_long_train$x <- round(answs_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
answs_2014_2022_dfXxy_long_train$y <- round(answs_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
answs_2014_2022_dfXxy_long_train$ANSWS_Value <- minmax_norm(answs_2014_2022_dfXxy_long_train$ANSWS_Value) # apply min-max normalisation
# head(answs_2014_2022_dfXxy_long_train); str(answs_2014_2022_dfXxy_long_train)

arh_2014_2022_df <- as.data.frame(ARH_2014_2022_stack, xy = T) # converting stacked ARH into dataframe
arh_2014_2022_dfXxy_long_train <-  arh_2014_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

arh_2014_2022_dfXxy_long_train$x <- round(arh_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
arh_2014_2022_dfXxy_long_train$y <- round(arh_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
arh_2014_2022_dfXxy_long_train$ARH_Value <- minmax_norm(arh_2014_2022_dfXxy_long_train$ARH_Value) # apply min-max normalisation
# head(arh_2014_2022_dfXxy_long_train); str(arh_2014_2022_dfXxy_long_train)


elev_2014_2022_df <- as.data.frame(elevation_replicated_for_2014_to_2022_stack, xy = T) # converting stacked elevation into dataframe
elev_2014_2022_dfXxy_long_train <-  elev_2014_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

elev_2014_2022_dfXxy_long_train$x <- round(elev_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
elev_2014_2022_dfXxy_long_train$y <- round(elev_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
elev_2014_2022_dfXxy_long_train$Elev_Value <- minmax_norm(elev_2014_2022_dfXxy_long_train$Elev_Value) # apply min-max normalisation
# head(elev_2014_2022_dfXxy_long_train); str(elev_2014_2022_dfXxy_long_train)

slope_2014_2022_df <- as.data.frame(slope_replicated_for_2014_to_2022_stack, xy = T) # converting stacked slope into dataframe
slope_2014_2022_dfXxy_long_train <-  slope_2014_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

slope_2014_2022_dfXxy_long_train$x <- round(slope_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
slope_2014_2022_dfXxy_long_train$y <- round(slope_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
slope_2014_2022_dfXxy_long_train$Slope_Value <- minmax_norm(slope_2014_2022_dfXxy_long_train$Slope_Value) # apply min-max normalisation
# head(slope_2014_2022_dfXxy_long_train); str(slope_2014_2022_dfXxy_long_train)


aspect_2014_2022_df <- as.data.frame(aspect_replicated_for_2014_to_2022_stack, xy = T) # converting stacked aspect into dataframe
aspect_2014_2022_dfXxy_long_train <-  aspect_2014_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

aspect_2014_2022_dfXxy_long_train$x <- round(aspect_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
aspect_2014_2022_dfXxy_long_train$y <- round(aspect_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
aspect_2014_2022_dfXxy_long_train$Aspect_Value <- minmax_norm(aspect_2014_2022_dfXxy_long_train$Aspect_Value) # apply min-max normalisation
# head(aspect_2014_2022_dfXxy_long_train); str(aspect_2014_2022_dfXxy_long_train)

fire_2014_2022_df <- as.data.frame(FIRE_2014_2022_stack, xy = T) # converting stacked fire into dataframe
fire_2014_2022_dfXxy_long_train <-  fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% 2014:2018) # filter years to be used as training set

fire_2014_2022_dfXxy_long_train$x <- round(fire_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
fire_2014_2022_dfXxy_long_train$y <- round(fire_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
# head(fire_2014_2022_dfXxy_long_train); str(fire_2014_2022_dfXxy_long_train)


lagged_fire_2014_2022_df <- as.data.frame(pblapply(1:60, # 2013-12-2018-11: lagged FIRE training set 
                                                   function(x) {LAGGED_FIRE_2014_2022_stack[[x]]}) |> stack(), xy = T) # converting stacked fire into dataframe
lagged_fire_2014_2022_dfXxy_long_train <-  lagged_fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% 2013:2018) %>% # filter years to be used as training set
  rename("Lagged_Fire_Value"="Fire_Value",
         "lagged_year" = 'Year',
         "lagged_month" = 'Month')

lagged_fire_2014_2022_dfXxy_long_train$x <- round(lagged_fire_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
lagged_fire_2014_2022_dfXxy_long_train$y <- round(lagged_fire_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
# head(lagged_fire_2014_2022_dfXxy_long_train); str(lagged_fire_2014_2022_dfXxy_long_train)

resampled_buffered_fire_2014_2022_df <- as.data.frame(RESAMPLED_FIRE_2014_2022_buffered_training_stack, xy = T)# converting stacked fire into dataframe
resampled_buffered_fire_2014_2022_dfXxy_long_train <-  resampled_buffered_fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>%
  na.omit() %>% # very important to omit rows after pivot longer
  mutate(Year = str_extract(Fire, "\\d{4}") |> as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}") |> as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) # select relevant columns only

resampled_buffered_fire_2014_2022_dfXxy_long_train$x <- round(resampled_buffered_fire_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
resampled_buffered_fire_2014_2022_dfXxy_long_train$y <- round(resampled_buffered_fire_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
# head(resampled_buffered_fire_2014_2022_dfXxy_long_train); str(resampled_buffered_fire_2014_2022_dfXxy_long_train)


resampled_non_buffered_fire_2014_2022_df <- as.data.frame(RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_stack, xy = T)# converting stacked fire into dataframe
resampled_non_buffered_fire_2014_2022_dfXxy_long_train <-  resampled_non_buffered_fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>%
  na.omit() %>% # very important to omit rows after pivot longer
  mutate(Year = str_extract(Fire, "\\d{4}") |> as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}") |> as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) # select relevant columns only

resampled_non_buffered_fire_2014_2022_dfXxy_long_train$x <- round(resampled_non_buffered_fire_2014_2022_dfXxy_long_train$x, 5) # round x coordinates to 5 d.p
resampled_non_buffered_fire_2014_2022_dfXxy_long_train$y <- round(resampled_non_buffered_fire_2014_2022_dfXxy_long_train$y, 5) # round y coordinates to 5 d.p
# head(resampled_non_buffered_fire_2014_2022_dfXxy_long_train); str(resampled_non_buffered_fire_2014_2022_dfXxy_long_train)

resampled_buffered_dfnorm_2014_2022_training_list <-  list(lulc_2014_2022_dfXxy_long_train,
                                                           ndvi_2014_2022_dfXxy_long_train,
                                                           ndmi_2014_2022_dfXxy_long_train,
                                                           # nbr_2014_2022_dfXxy_long_train, # removed due to VIF score
                                                           atp_2014_2022_dfXxy_long_train,
                                                           amt_2014_2022_dfXxy_long_train,
                                                           answs_2014_2022_dfXxy_long_train,
                                                           arh_2014_2022_dfXxy_long_train,
                                                           elev_2014_2022_dfXxy_long_train,
                                                           slope_2014_2022_dfXxy_long_train,
                                                           aspect_2014_2022_dfXxy_long_train,
                                                           resampled_buffered_fire_2014_2022_dfXxy_long_train)


resampled_non_buffered_dfnorm_2014_2022_training_list <-  list(lulc_2014_2022_dfXxy_long_train,
                                                               ndvi_2014_2022_dfXxy_long_train,
                                                               ndmi_2014_2022_dfXxy_long_train,
                                                               # nbr_2014_2022_dfXxy_long_train, # removed due to VIF score
                                                               atp_2014_2022_dfXxy_long_train,
                                                               amt_2014_2022_dfXxy_long_train,
                                                               answs_2014_2022_dfXxy_long_train,
                                                               arh_2014_2022_dfXxy_long_train,
                                                               elev_2014_2022_dfXxy_long_train,
                                                               slope_2014_2022_dfXxy_long_train,
                                                               aspect_2014_2022_dfXxy_long_train,
                                                               resampled_non_buffered_fire_2014_2022_dfXxy_long_train)


resampled_buffered_dfnorm_2014_2022_training_set <- reduce(resampled_buffered_dfnorm_2014_2022_training_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)

# creating lagged year and month column from the training set above
resampled_buffered_dfnorm_2014_2022_training_set <- resampled_buffered_dfnorm_2014_2022_training_set %>%
  mutate(lagged_month = Month-1,
         lagged_year = ifelse(lagged_month==0, Year-1, Year),
         lagged_month = case_when(lagged_month==0 ~ 12,
                                  TRUE ~ lagged_month))

# add lagged response to predictor variables
resampled_buffered_dfnorm_2014_2022_training_set <- resampled_buffered_dfnorm_2014_2022_training_set %>% inner_join(lagged_fire_2014_2022_dfXxy_long_train, by = c('x','y','lagged_year','lagged_month'))
str(resampled_buffered_dfnorm_2014_2022_training_set)

# remove unecessary columns
resampled_buffered_dfnorm_2014_2022_training_set <- resampled_buffered_dfnorm_2014_2022_training_set[,-c(16,17)]

# relocate the response variable to be the last column
resampled_buffered_dfnorm_2014_2022_training_set <- resampled_buffered_dfnorm_2014_2022_training_set %>%
  relocate(Fire_Value, .after = last_col())

View(resampled_buffered_dfnorm_2014_2022_training_set)
# save dataframe
# save(resampled_buffered_dfnorm_2014_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/resampled_buffered_dfnorm_2014_2022_training_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/resampled_buffered_dfnorm_2014_2022_training_set.Rdata')

training_Fire_periods <- resampled_buffered_dfnorm_2014_2022_training_set %>%
  group_by(Year, Month, Fire_Value) %>%
  tally() %>% # compute the number of pixels with and without fire
  filter(Fire_Value==1) %>% # extract the period having at least a fire event
  dplyr::select(Year, Month)

fully_resampled_buffered_dfnorm_2014_2022_training_set <- resampled_buffered_dfnorm_2014_2022_training_set %>%
  inner_join(training_Fire_periods, by = c('Year', 'Month')) %>% # only retain period with fire events
  filter(LULC_Class!=1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.

# save dataframe
# save(fully_resampled_buffered_dfnorm_2014_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_buffered_dfnorm_2014_2022_training_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_buffered_dfnorm_2014_2022_training_set.Rdata')

resampled_non_buffered_dfnorm_2014_2022_training_set <- reduce(resampled_non_buffered_dfnorm_2014_2022_training_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
str(resampled_non_buffered_dfnorm_2014_2022_training_set)

# creating lagged year and month column from the training set above
resampled_non_buffered_dfnorm_2014_2022_training_set <- resampled_non_buffered_dfnorm_2014_2022_training_set %>%
  mutate(lagged_month = Month-1,
         lagged_year = ifelse(lagged_month==0, Year-1, Year),
         lagged_month = case_when(lagged_month==0 ~ 12,
                                  TRUE ~ lagged_month))

# add lagged response to predictor variables
resampled_non_buffered_dfnorm_2014_2022_training_set <- resampled_non_buffered_dfnorm_2014_2022_training_set %>% inner_join(lagged_fire_2014_2022_dfXxy_long_train, by = c('x','y','lagged_year','lagged_month'))
str(resampled_non_buffered_dfnorm_2014_2022_training_set)

# remove unnecessary columns
resampled_non_buffered_dfnorm_2014_2022_training_set <- resampled_non_buffered_dfnorm_2014_2022_training_set[,-c(16,17)]

# relocate the response variable to be the last column
resampled_non_buffered_dfnorm_2014_2022_training_set <- resampled_non_buffered_dfnorm_2014_2022_training_set %>%
  relocate(Fire_Value, .after = last_col())

View(resampled_non_buffered_dfnorm_2014_2022_training_set)

# save dataframe
# save(resampled_non_buffered_dfnorm_2014_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/resampled_non_buffered_dfnorm_2014_2022_training_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/resampled_non_buffered_dfnorm_2014_2022_training_set.Rdata')

training_Fire_periods <- resampled_non_buffered_dfnorm_2014_2022_training_set %>%
  group_by(Year, Month, Fire_Value) %>%
  tally() %>% # compute the number of pixels with and without fire
  filter(Fire_Value==1) %>% # extract the period having at least a fire event
  dplyr::select(Year, Month)

fully_resampled_non_buffered_dfnorm_2014_2022_training_set <- resampled_non_buffered_dfnorm_2014_2022_training_set %>%
  inner_join(training_Fire_periods, by = c('Year', 'Month')) %>% # only retain period with fire events
  filter(LULC_Class!=1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.

# save dataframe
# save(fully_resampled_non_buffered_dfnorm_2014_2022_training_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_non_buffered_dfnorm_2014_2022_training_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_non_buffered_dfnorm_2014_2022_training_set.Rdata')

x <- fully_resampled_non_buffered_dfnorm_2014_2022_training_set %>%
  filter(Year==2018, Month ==11) %>%
  dplyr::select(x,y,Lagged_Fire_Value)

xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(roi_trans)
# plot(target_waterbodies, add = T)
plot(xx, col = fire_color_condition_func(xx), cex.main = .9, main = '2018-11', add = T)

# VALIDATION --------------------------------------------------------------

# Convert validation set raster stack [from 2014-2022 dataset] into dataframe
lulc_2014_2022_df <- as.data.frame(LULC_2014_2022_stack, xy = T) # converting stacked LULC into dataframe
lulc_2014_2022_df_xy <- lulc_2014_2022_df[,1:2] # extracting xy coordinates from LULC dataframe
lulc_2014_2022_dfX <- lulc_2014_2022_df[,-c(1:2)] # LULC dataframe with only predictor variables
lulc_2014_2022_dfX <- apply(lulc_2014_2022_dfX, 2, function(x){as.numeric(as.factor(x))}) |> as.data.frame() # converting the classes into numeric
lulc_2014_2022_dfXxy <- cbind(lulc_2014_2022_df_xy, lulc_2014_2022_dfX) # reattach coordinates to LULC dataframe consisting of the predictor variables
lulc_2014_2022_dfXxy_long_val <- lulc_2014_2022_dfXxy %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'LULC_Class')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

lulc_2014_2022_dfXxy_long_val$x <- round(lulc_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
lulc_2014_2022_dfXxy_long_val$y <- round(lulc_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
lulc_2014_2022_dfXxy_long_val$LULC_Class <- minmax_norm(lulc_2014_2022_dfXxy_long_val$LULC_Class) # apply min-max normalisation
# head(lulc_2014_2022_dfXxy_long_val); str(lulc_2014_2022_dfXxy_long_val)

ndvi_2014_2022_df <- as.data.frame(NDVI_2014_2022_stack, xy = T) # converting stacked NDVI into dataframe
ndvi_2014_2022_dfXxy_long_val <-  ndvi_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDVI_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

ndvi_2014_2022_dfXxy_long_val$x <- round(ndvi_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long_val$y <- round(ndvi_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long_val$NDVI_Value <- minmax_norm(ndvi_2014_2022_dfXxy_long_val$NDVI_Value) # apply min-max normalisation
# head(ndvi_2014_2022_dfXxy_long_val); str(ndvi_2014_2022_dfXxy_long_val)

ndmi_2014_2022_df <- as.data.frame(NDMI_2014_2022_stack, xy = T) # converting stacked NDMI into dataframe
ndmi_2014_2022_dfXxy_long_val <-  ndmi_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDMI_Value'))  %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

ndmi_2014_2022_dfXxy_long_val$x <- round(ndmi_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long_val$y <- round(ndmi_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long_val$NDMI_Value <- minmax_norm(ndmi_2014_2022_dfXxy_long_val$NDMI_Value) # apply min-max normalisation
# head(ndmi_2014_2022_dfXxy_long_val); str(ndmi_2014_2022_dfXxy_long_val)

nbr_2014_2022_df <- as.data.frame(NBR_2014_2022_stack, xy = T) # converting stacked NBR into dataframe
nbr_2014_2022_dfXxy_long_val <-  nbr_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NBR_Value'))  %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

nbr_2014_2022_dfXxy_long_val$x <- round(nbr_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
nbr_2014_2022_dfXxy_long_val$y <- round(nbr_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
nbr_2014_2022_dfXxy_long_val$NBR_Value <- minmax_norm(nbr_2014_2022_dfXxy_long_val$NBR_Value) # apply min-max normalisation
# head(nbr_2014_2022_dfXxy_long_val); str(nbr_2014_2022_dfXxy_long_val)

atp_2014_2022_df <- as.data.frame(ATP_2014_2022_stack, xy = T) # converting stacked ATP into dataframe
atp_2014_2022_dfXxy_long_val <-  atp_2014_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value'))  %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

atp_2014_2022_dfXxy_long_val$x <- round(atp_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
atp_2014_2022_dfXxy_long_val$y <- round(atp_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
atp_2014_2022_dfXxy_long_val$TP_Value <- minmax_norm(atp_2014_2022_dfXxy_long_val$TP_Value) # apply min-max normalisation
# head(atp_2014_2022_dfXxy_long_val); str(atp_2014_2022_dfXxy_long_val)

amt_2014_2022_df <- as.data.frame(AMT_2014_2022_stack, xy = T) # converting stacked AMT into dataframe
amt_2014_2022_dfXxy_long_val <-  amt_2014_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

amt_2014_2022_dfXxy_long_val$x <- round(amt_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
amt_2014_2022_dfXxy_long_val$y <- round(amt_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
amt_2014_2022_dfXxy_long_val$AMT_Value <- minmax_norm(amt_2014_2022_dfXxy_long_val$AMT_Value) # apply min-max normalisation
# head(amt_2014_2022_dfXxy_long_val); str(amt_2014_2022_dfXxy_long_val)

answs_2014_2022_df <- as.data.frame(ANSWS_2014_2022_stack, xy = T) # converting stacked ANSWS into dataframe
answs_2014_2022_dfXxy_long_val <-  answs_2014_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

answs_2014_2022_dfXxy_long_val$x <- round(answs_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
answs_2014_2022_dfXxy_long_val$y <- round(answs_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
answs_2014_2022_dfXxy_long_val$ANSWS_Value <- minmax_norm(answs_2014_2022_dfXxy_long_val$ANSWS_Value) # apply min-max normalisation
# head(answs_2014_2022_dfXxy_long_val); str(answs_2014_2022_dfXxy_long_val)

arh_2014_2022_df <- as.data.frame(ARH_2014_2022_stack, xy = T) # converting stacked ARH into dataframe
arh_2014_2022_dfXxy_long_val <-  arh_2014_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value'))  %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

arh_2014_2022_dfXxy_long_val$x <- round(arh_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
arh_2014_2022_dfXxy_long_val$y <- round(arh_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
arh_2014_2022_dfXxy_long_val$ARH_Value <- minmax_norm(arh_2014_2022_dfXxy_long_val$ARH_Value) # apply min-max normalisation
# head(arh_2014_2022_dfXxy_long_val); str(arh_2014_2022_dfXxy_long_val)


elev_2014_2022_df <- as.data.frame(elevation_replicated_for_2014_to_2022_stack, xy = T) # converting stacked elevation into dataframe
elev_2014_2022_dfXxy_long_val <-  elev_2014_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value'))  %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

elev_2014_2022_dfXxy_long_val$x <- round(elev_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
elev_2014_2022_dfXxy_long_val$y <- round(elev_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
elev_2014_2022_dfXxy_long_val$Elev_Value <- minmax_norm(elev_2014_2022_dfXxy_long_val$Elev_Value) # apply min-max normalisation
# head(elev_2014_2022_dfXxy_long_val); str(elev_2014_2022_dfXxy_long_val)

slope_2014_2022_df <- as.data.frame(slope_replicated_for_2014_to_2022_stack, xy = T) # converting stacked slope into dataframe
slope_2014_2022_dfXxy_long_val <-  slope_2014_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value'))  %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

slope_2014_2022_dfXxy_long_val$x <- round(slope_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
slope_2014_2022_dfXxy_long_val$y <- round(slope_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
slope_2014_2022_dfXxy_long_val$Slope_Value <- minmax_norm(slope_2014_2022_dfXxy_long_val$Slope_Value) # apply min-max normalisation
# head(slope_2014_2022_dfXxy_long_val); str(slope_2014_2022_dfXxy_long_val)


aspect_2014_2022_df <- as.data.frame(aspect_replicated_for_2014_to_2022_stack, xy = T) # converting stacked aspect into dataframe
aspect_2014_2022_dfXxy_long_val <-  aspect_2014_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

aspect_2014_2022_dfXxy_long_val$x <- round(aspect_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
aspect_2014_2022_dfXxy_long_val$y <- round(aspect_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
aspect_2014_2022_dfXxy_long_val$Aspect_Value <- minmax_norm(aspect_2014_2022_dfXxy_long_val$Aspect_Value) # apply min-max normalisation
# head(aspect_2014_2022_dfXxy_long_val); str(aspect_2014_2022_dfXxy_long_val)

lagged_fire_2014_2022_df_val <- as.data.frame(pblapply(61:84, # 2018-12-2020-11: lagged FIRE validation set 
                                                   function(x) {LAGGED_FIRE_2014_2022_stack[[x]]}) |> stack(), xy = T) # converting stacked fire into dataframe
lagged_fire_2014_2022_dfXxy_long_val <-  lagged_fire_2014_2022_df_val %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  # filter(Year %in% 2018:2020) %>% # filter years to be used as training set
  rename("Lagged_Fire_Value"="Fire_Value",
         "lagged_year" = 'Year',
         "lagged_month" = 'Month')

lagged_fire_2014_2022_dfXxy_long_val$x <- round(lagged_fire_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
lagged_fire_2014_2022_dfXxy_long_val$y <- round(lagged_fire_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
# head(lagged_fire_2014_2022_dfXxy_long_val); str(lagged_fire_2014_2022_dfXxy_long_val)

fire_2014_2022_df <- as.data.frame(FIRE_2014_2022_stack, xy = T) # converting stacked fire into dataframe
fire_2014_2022_dfXxy_long_val <-  fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% 2019:2020) # filter years to be used as validation set

fire_2014_2022_dfXxy_long_val$x <- round(fire_2014_2022_dfXxy_long_val$x, 5) # round x coordinates to 5 d.p
fire_2014_2022_dfXxy_long_val$y <- round(fire_2014_2022_dfXxy_long_val$y, 5) # round y coordinates to 5 d.p
# head(fire_2014_2022_dfXxy_long_val); str(fire_2014_2022_dfXxy_long_val)

# Add all the normalised validation set from the 2014 to 2022 dataset in one list 
dfnorm_2014_2022_validation_list <-  list(lulc_2014_2022_dfXxy_long_val, 
                                          ndvi_2014_2022_dfXxy_long_val,
                                          ndmi_2014_2022_dfXxy_long_val,
                                          # nbr_2014_2022_dfXxy_long_val,
                                          atp_2014_2022_dfXxy_long_val,
                                          amt_2014_2022_dfXxy_long_val,
                                          answs_2014_2022_dfXxy_long_val,
                                          arh_2014_2022_dfXxy_long_val,
                                          elev_2014_2022_dfXxy_long_val,
                                          slope_2014_2022_dfXxy_long_val,
                                          aspect_2014_2022_dfXxy_long_val,
                                          fire_2014_2022_dfXxy_long_val)

# Combine all the normalised validation set from the 2014 to 2022 dataset in one dataframe 
dfnorm_2014_2022_validation_set <- reduce(dfnorm_2014_2022_validation_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)

# creating lagged year and month column from the training set above
dfnorm_2014_2022_validation_set <- dfnorm_2014_2022_validation_set %>%
  mutate(lagged_month = Month-1,
         lagged_year = ifelse(lagged_month==0, Year-1, Year),
         lagged_month = case_when(lagged_month==0 ~ 12,
                                  TRUE ~ lagged_month))

# add lagged response to predictor variables
dfnorm_2014_2022_validation_set <- dfnorm_2014_2022_validation_set %>% inner_join(lagged_fire_2014_2022_dfXxy_long_val, by = c('x','y','lagged_year','lagged_month'))
str(dfnorm_2014_2022_validation_set)

# remove unnecessary columns
dfnorm_2014_2022_validation_set <- dfnorm_2014_2022_validation_set[,-c(16,17)]

# relocate the response variable to be the last column
dfnorm_2014_2022_validation_set <- dfnorm_2014_2022_validation_set %>%
  relocate(Fire_Value, .after = last_col())

View(dfnorm_2014_2022_validation_set)

# Remove waterbodies from dataset
dfnorm_2014_2022_validation_set <- dfnorm_2014_2022_validation_set %>%
  filter(LULC_Class!= 1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.
# str(dfnorm_2014_2022_validation_set)

# save dataframe
# save(dfnorm_2014_2022_validation_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_validation_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_validation_set.Rdata')

# TESTING -----------------------------------------------------------------

# Convert test set raster stack [from 2014-2022 dataset] into dataframe
lulc_2014_2022_df <- as.data.frame(LULC_2014_2022_stack, xy = T) # converting stacked LULC into dataframe
lulc_2014_2022_df_xy <- lulc_2014_2022_df[,1:2] # extracting xy coordinates from LULC dataframe
lulc_2014_2022_dfX <- lulc_2014_2022_df[,-c(1:2)] # LULC dataframe with only predictor variables
lulc_2014_2022_dfX <- apply(lulc_2014_2022_dfX, 2, function(x){as.numeric(as.factor(x))}) |> as.data.frame() # converting the classes into numeric
lulc_2014_2022_dfXxy <- cbind(lulc_2014_2022_df_xy, lulc_2014_2022_dfX) # reattach coordinates to LULC dataframe consisting of the predictor variables
lulc_2014_2022_dfXxy_long_test <- lulc_2014_2022_dfXxy %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'LULC_Class')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

lulc_2014_2022_dfXxy_long_test$x <- round(lulc_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
lulc_2014_2022_dfXxy_long_test$y <- round(lulc_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
lulc_2014_2022_dfXxy_long_test$LULC_Class <- minmax_norm(lulc_2014_2022_dfXxy_long_test$LULC_Class) # apply min-max normalisation
# head(lulc_2014_2022_dfXxy_long_test); str(lulc_2014_2022_dfXxy_long_test)


ndvi_2014_2022_df <- as.data.frame(NDVI_2014_2022_stack, xy = T) # converting stacked NDVI into dataframe
ndvi_2014_2022_dfXxy_long_test <-  ndvi_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDVI_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

ndvi_2014_2022_dfXxy_long_test$x <- round(ndvi_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long_test$y <- round(ndvi_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
ndvi_2014_2022_dfXxy_long_test$NDVI_Value <- minmax_norm(ndvi_2014_2022_dfXxy_long_test$NDVI_Value) # apply min-max normalisation
# head(ndvi_2014_2022_dfXxy_long_test); str(ndvi_2014_2022_dfXxy_long_test)

ndmi_2014_2022_df <- as.data.frame(NDMI_2014_2022_stack, xy = T) # converting stacked NDMI into dataframe
ndmi_2014_2022_dfXxy_long_test <-  ndmi_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NDMI_Value'))  %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

ndmi_2014_2022_dfXxy_long_test$x <- round(ndmi_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long_test$y <- round(ndmi_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
ndmi_2014_2022_dfXxy_long_test$NDMI_Value <- minmax_norm(ndmi_2014_2022_dfXxy_long_test$NDMI_Value) # apply min-max normalisation
# head(ndmi_2014_2022_dfXxy_long_test); str(ndmi_2014_2022_dfXxy_long_test)

nbr_2014_2022_df <- as.data.frame(NBR_2014_2022_stack, xy = T) # converting stacked NBR into dataframe
nbr_2014_2022_dfXxy_long_test <-  nbr_2014_2022_df %>%
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
  dplyr::select(c('x', 'y', 'Year', 'Month', 'NBR_Value'))  %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

nbr_2014_2022_dfXxy_long_test$x <- round(nbr_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
nbr_2014_2022_dfXxy_long_test$y <- round(nbr_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
nbr_2014_2022_dfXxy_long_test$NBR_Value <- minmax_norm(nbr_2014_2022_dfXxy_long_test$NBR_Value) # apply min-max normalisation
# head(nbr_2014_2022_dfXxy_long_test); str(nbr_2014_2022_dfXxy_long_test)

atp_2014_2022_df <- as.data.frame(ATP_2014_2022_stack, xy = T) # converting stacked ATP into dataframe
atp_2014_2022_dfXxy_long_test <-  atp_2014_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'TP_Value'))  %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

atp_2014_2022_dfXxy_long_test$x <- round(atp_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
atp_2014_2022_dfXxy_long_test$y <- round(atp_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
atp_2014_2022_dfXxy_long_test$TP_Value <- minmax_norm(atp_2014_2022_dfXxy_long_test$TP_Value) # apply min-max normalisation
# head(atp_2014_2022_dfXxy_long_test); str(atp_2014_2022_dfXxy_long_test)

amt_2014_2022_df <- as.data.frame(AMT_2014_2022_stack, xy = T) # converting stacked AMT into dataframe
amt_2014_2022_dfXxy_long_test <-  amt_2014_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

amt_2014_2022_dfXxy_long_test$x <- round(amt_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
amt_2014_2022_dfXxy_long_test$y <- round(amt_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
amt_2014_2022_dfXxy_long_test$AMT_Value <- minmax_norm(amt_2014_2022_dfXxy_long_test$AMT_Value) # apply min-max normalisation
# head(amt_2014_2022_dfXxy_long_test); str(amt_2014_2022_dfXxy_long_test)

answs_2014_2022_df <- as.data.frame(ANSWS_2014_2022_stack, xy = T) # converting stacked ANSWS into dataframe
answs_2014_2022_dfXxy_long_test <-  answs_2014_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

answs_2014_2022_dfXxy_long_test$x <- round(answs_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
answs_2014_2022_dfXxy_long_test$y <- round(answs_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
answs_2014_2022_dfXxy_long_test$ANSWS_Value <- minmax_norm(answs_2014_2022_dfXxy_long_test$ANSWS_Value) # apply min-max normalisation
# head(answs_2014_2022_dfXxy_long_test); str(answs_2014_2022_dfXxy_long_test)

arh_2014_2022_df <- as.data.frame(ARH_2014_2022_stack, xy = T) # converting stacked ARH into dataframe
arh_2014_2022_dfXxy_long_test <-  arh_2014_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

arh_2014_2022_dfXxy_long_test$x <- round(arh_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
arh_2014_2022_dfXxy_long_test$y <- round(arh_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
arh_2014_2022_dfXxy_long_test$ARH_Value <- minmax_norm(arh_2014_2022_dfXxy_long_test$ARH_Value) # apply min-max normalisation
# head(arh_2014_2022_dfXxy_long_test); str(arh_2014_2022_dfXxy_long_test)


elev_2014_2022_df <- as.data.frame(elevation_replicated_for_2014_to_2022_stack, xy = T) # converting stacked elevation into dataframe
elev_2014_2022_dfXxy_long_test <-  elev_2014_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

elev_2014_2022_dfXxy_long_test$x <- round(elev_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
elev_2014_2022_dfXxy_long_test$y <- round(elev_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
elev_2014_2022_dfXxy_long_test$Elev_Value <- minmax_norm(elev_2014_2022_dfXxy_long_test$Elev_Value) # apply min-max normalisation
# head(elev_2014_2022_dfXxy_long_test); str(elev_2014_2022_dfXxy_long_test)

slope_2014_2022_df <- as.data.frame(slope_replicated_for_2014_to_2022_stack, xy = T) # converting stacked slope into dataframe
slope_2014_2022_dfXxy_long_test <-  slope_2014_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

slope_2014_2022_dfXxy_long_test$x <- round(slope_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
slope_2014_2022_dfXxy_long_test$y <- round(slope_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
slope_2014_2022_dfXxy_long_test$Slope_Value <- minmax_norm(slope_2014_2022_dfXxy_long_test$Slope_Value) # apply min-max normalisation
# head(slope_2014_2022_dfXxy_long_test); str(slope_2014_2022_dfXxy_long_test)


aspect_2014_2022_df <- as.data.frame(aspect_replicated_for_2014_to_2022_stack, xy = T) # converting stacked aspect into dataframe
aspect_2014_2022_dfXxy_long_test <-  aspect_2014_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

aspect_2014_2022_dfXxy_long_test$x <- round(aspect_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
aspect_2014_2022_dfXxy_long_test$y <- round(aspect_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
aspect_2014_2022_dfXxy_long_test$Aspect_Value <- minmax_norm(aspect_2014_2022_dfXxy_long_test$Aspect_Value) # apply min-max normalisation
# head(aspect_2014_2022_dfXxy_long_test); str(aspect_2014_2022_dfXxy_long_test)

lagged_fire_2014_2022_df_test <- as.data.frame(pblapply(85:108, # 2018-12-2020-11: lagged FIRE validation set 
                                                       function(x) {LAGGED_FIRE_2014_2022_stack[[x]]}) |> stack(), xy = T) # converting stacked fire into dataframe
lagged_fire_2014_2022_dfXxy_long_test <-  lagged_fire_2014_2022_df_test %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>%
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  # filter(Year %in% 2018:2020) %>% # filter years to be used as training set
  rename("Lagged_Fire_Value"="Fire_Value",
         "lagged_year" = 'Year',
         "lagged_month" = 'Month')

lagged_fire_2014_2022_dfXxy_long_test$x <- round(lagged_fire_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
lagged_fire_2014_2022_dfXxy_long_test$y <- round(lagged_fire_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
# head(lagged_fire_2014_2022_dfXxy_long_test); str(lagged_fire_2014_2022_dfXxy_long_test)


fire_2014_2022_df <- as.data.frame(FIRE_2014_2022_stack, xy = T) # converting stacked fire into dataframe
fire_2014_2022_dfXxy_long_test <-  fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% 2021:2022) # filter years to be used as test set

fire_2014_2022_dfXxy_long_test$x <- round(fire_2014_2022_dfXxy_long_test$x, 5) # round x coordinates to 5 d.p
fire_2014_2022_dfXxy_long_test$y <- round(fire_2014_2022_dfXxy_long_test$y, 5) # round y coordinates to 5 d.p
# head(fire_2014_2022_dfXxy_long_test); str(fire_2014_2022_dfXxy_long_test)

# Add all the normalised test set from the 2014 to 2022 dataset in one list 
dfnorm_2014_2022_test_list <-  list(lulc_2014_2022_dfXxy_long_test, 
                                    ndvi_2014_2022_dfXxy_long_test,
                                    ndmi_2014_2022_dfXxy_long_test,
                                    # nbr_2014_2022_dfXxy_long_test,
                                    atp_2014_2022_dfXxy_long_test,
                                    amt_2014_2022_dfXxy_long_test,
                                    answs_2014_2022_dfXxy_long_test,
                                    arh_2014_2022_dfXxy_long_test,
                                    elev_2014_2022_dfXxy_long_test,
                                    slope_2014_2022_dfXxy_long_test,
                                    aspect_2014_2022_dfXxy_long_test,
                                    fire_2014_2022_dfXxy_long_test)

# Combine all the normalised test set from the 2014 to 2022 dataset in one dataframe 
dfnorm_2014_2022_test_set <- reduce(dfnorm_2014_2022_test_list, inner_join, by = c('x', "y", "Year", "Month")) # merge all the table on the common columns (to preserve both spatial-temporal consistency!)
# str(dfnorm_2014_2022_test_set)

# creating lagged year and month column from the training set above
dfnorm_2014_2022_test_set <- dfnorm_2014_2022_test_set %>%
  mutate(lagged_month = Month-1,
         lagged_year = ifelse(lagged_month==0, Year-1, Year),
         lagged_month = case_when(lagged_month==0 ~ 12,
                                  TRUE ~ lagged_month))

# add lagged response to predictor variables
dfnorm_2014_2022_test_set <- dfnorm_2014_2022_test_set %>% inner_join(lagged_fire_2014_2022_dfXxy_long_test, by = c('x','y','lagged_year','lagged_month'))
str(dfnorm_2014_2022_test_set)

# remove unnecessary columns
dfnorm_2014_2022_test_set <- dfnorm_2014_2022_test_set[,-c(16,17)]

# relocate the response variable to be the last column
dfnorm_2014_2022_test_set <- dfnorm_2014_2022_test_set %>%
  relocate(Fire_Value, .after = last_col())

View(dfnorm_2014_2022_test_set)


# Remove waterbodies from dataset
dfnorm_2014_2022_test_set <- dfnorm_2014_2022_test_set %>%
  filter(LULC_Class!= 1) # filter out water bodies as they won't contain fire events- water bodies = 1 after normalisation. Before normalisation it was 5.
#
# save dataframe
# save(dfnorm_2014_2022_test_set, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_test_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_test_set.Rdata')

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Load RF data here if necessary: Copied above!
# training set-buffered
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_buffered_dfnorm_2014_2022_training_set.Rdata')

# training set- non buffered
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_non_buffered_dfnorm_2014_2022_training_set.Rdata')

# validation set
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_validation_set.Rdata')

# test set
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_test_set.Rdata')

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



