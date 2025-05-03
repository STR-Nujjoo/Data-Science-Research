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
lulc_2014_2022_df <- as.data.frame(LULC_2014_2022_stack, xy = T, na.rm = T) # converting stacked LULC into dataframe
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
  mutate(Date = as.Date(str_extract(LULC, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'LULC_Class')) # select relevant columns only
  
lulc_2014_2022_dfXxy_long$LULC_Class <- minmax_norm(lulc_2014_2022_dfXxy_long$LULC_Class) # apply min-max normalisation
# head(lulc_2014_2022_dfXxy_long); str(lulc_2014_2022_dfXxy_long)


ndvi_2014_2022_df <- as.data.frame(NDVI_2014_2022_stack, xy = T, na.rm = T) # converting stacked NDVI into dataframe
ndvi_2014_2022_dfXxy_long <-  ndvi_2014_2022_df %>%
  # convert dataframe into long format where there is only one NDVI column
  pivot_longer(
    cols = starts_with("NDVI"),
    names_to = "NDVI",
    values_to = "NDVI_Value"
  ) %>% 
  mutate(Date = as.Date(str_extract(NDVI, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'NDVI_Value')) # select relevant columns only

ndvi_2014_2022_dfXxy_long$NDVI_Value <- minmax_norm(ndvi_2014_2022_dfXxy_long$NDVI_Value) # apply min-max normalisation
# head(ndvi_2014_2022_dfXxy_long); str(ndvi_2014_2022_dfXxy_long)


ndmi_2014_2022_df <- as.data.frame(NDMI_2014_2022_stack, xy = T, na.rm = T) # converting stacked NDMI into dataframe
ndmi_2014_2022_dfXxy_long <-  ndmi_2014_2022_df %>%
  # convert dataframe into long format where there is only one NDMI column
  pivot_longer(
    cols = starts_with("NDMI"),
    names_to = "NDMI",
    values_to = "NDMI_Value"
  ) %>% 
  mutate(Date = as.Date(str_extract(NDMI, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'NDMI_Value')) # select relevant columns only

ndmi_2014_2022_dfXxy_long$NDMI_Value <- minmax_norm(ndmi_2014_2022_dfXxy_long$NDMI_Value) # apply min-max normalisation
# head(ndmi_2014_2022_dfXxy_long); str(ndmi_2014_2022_dfXxy_long)

nbr_2014_2022_df <- as.data.frame(NBR_2014_2022_stack, xy = T, na.rm = T) # converting stacked NBR into dataframe
nbr_2014_2022_dfXxy_long <-  nbr_2014_2022_df %>%
  # convert dataframe into long format where there is only one NBR column
  pivot_longer(
    cols = starts_with("NBR"),
    names_to = "NBR",
    values_to = "NBR_Value"
  ) %>% 
  mutate(Date = as.Date(str_extract(NBR, "\\d{8}"), '%Y%m%d'), # extract date from columns name
         Year = year(Date)|>as.integer(), # extract year from date
         Month = month(Date)|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'NBR_Value')) # select relevant columns only

nbr_2014_2022_dfXxy_long$NBR_Value <- minmax_norm(nbr_2014_2022_dfXxy_long$NBR_Value) # apply min-max normalisation
# head(nbr_2014_2022_dfXxy_long); str(nbr_2014_2022_dfXxy_long)


atp_2014_2022_df <- as.data.frame(ATP_2014_2022_stack, xy = T, na.rm = T) # converting stacked ATP into dataframe
atp_2014_2022_dfXxy_long <-  atp_2014_2022_df %>%
  # convert dataframe into long format where there is only one ATP column
  pivot_longer(
    cols = starts_with("TP"),
    names_to = "TP",
    values_to = "TP_Value"
  ) %>% 
  mutate(Year = str_extract(TP, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(TP, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'TP_Value')) # select relevant columns only

atp_2014_2022_dfXxy_long$TP_Value <- minmax_norm(atp_2014_2022_dfXxy_long$TP_Value) # apply min-max normalisation
# head(atp_2014_2022_dfXxy_long); str(atp_2014_2022_dfXxy_long)

amt_2014_2022_df <- as.data.frame(AMT_2014_2022_stack, xy = T, na.rm = T) # converting stacked AMT into dataframe
amt_2014_2022_dfXxy_long <-  amt_2014_2022_df %>%
  # convert dataframe into long format where there is only one AMT column
  pivot_longer(
    cols = starts_with("AMT"),
    names_to = "AMT",
    values_to = "AMT_Value"
  ) %>% 
  mutate(Year = str_extract(AMT, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(AMT, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'AMT_Value')) # select relevant columns only

amt_2014_2022_dfXxy_long$AMT_Value <- minmax_norm(amt_2014_2022_dfXxy_long$AMT_Value) # apply min-max normalisation
# head(amt_2014_2022_dfXxy_long); str(amt_2014_2022_dfXxy_long)

answs_2014_2022_df <- as.data.frame(ANSWS_2014_2022_stack, xy = T, na.rm = T) # converting stacked ANSWS into dataframe
answs_2014_2022_dfXxy_long <-  answs_2014_2022_df %>%
  # convert dataframe into long format where there is only one ANSWS column
  pivot_longer(
    cols = starts_with("ANSWS"),
    names_to = "ANSWS",
    values_to = "ANSWS_Value"
  ) %>% 
  mutate(Year = str_extract(ANSWS, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ANSWS, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'ANSWS_Value')) # select relevant columns only

answs_2014_2022_dfXxy_long$ANSWS_Value <- minmax_norm(answs_2014_2022_dfXxy_long$ANSWS_Value) # apply min-max normalisation
# head(answs_2014_2022_dfXxy_long); str(answs_2014_2022_dfXxy_long)

arh_2014_2022_df <- as.data.frame(ARH_2014_2022_stack, xy = T, na.rm = T) # converting stacked ARH into dataframe
arh_2014_2022_dfXxy_long <-  arh_2014_2022_df %>%
  # convert dataframe into long format where there is only one ARH column
  pivot_longer(
    cols = starts_with("ARH"),
    names_to = "ARH",
    values_to = "ARH_Value"
  ) %>% 
  mutate(Year = str_extract(ARH, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(ARH, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'ARH_Value')) # select relevant columns only

arh_2014_2022_dfXxy_long$ARH_Value <- minmax_norm(arh_2014_2022_dfXxy_long$ARH_Value) # apply min-max normalisation
# head(arh_2014_2022_dfXxy_long); str(arh_2014_2022_dfXxy_long)


elev_2014_2022_df <- as.data.frame(elevation_replicated_for_2014_to_2022_stack, xy = T, na.rm = T) # converting stacked elevation into dataframe
elev_2014_2022_dfXxy_long <-  elev_2014_2022_df %>%
  # convert dataframe into long format where there is only one elevation column
  pivot_longer(
    cols = starts_with("Elev"),
    names_to = "Elev",
    values_to = "Elev_Value"
  ) %>% 
  mutate(Year = str_extract(Elev, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Elev, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'Elev_Value')) # select relevant columns only

elev_2014_2022_dfXxy_long$Elev_Value <- minmax_norm(elev_2014_2022_dfXxy_long$Elev_Value) # apply min-max normalisation
# head(elev_2014_2022_dfXxy_long); str(elev_2014_2022_dfXxy_long)

slope_2014_2022_df <- as.data.frame(slope_replicated_for_2014_to_2022_stack, xy = T, na.rm = T) # converting stacked slope into dataframe
slope_2014_2022_dfXxy_long <-  slope_2014_2022_df %>%
  # convert dataframe into long format where there is only one slope column
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope",
    values_to = "Slope_Value"
  ) %>% 
  mutate(Year = str_extract(Slope, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Slope, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'Slope_Value')) # select relevant columns only

slope_2014_2022_dfXxy_long$Slope_Value <- minmax_norm(slope_2014_2022_dfXxy_long$Slope_Value) # apply min-max normalisation
# head(slope_2014_2022_dfXxy_long); str(slope_2014_2022_dfXxy_long)


aspect_2014_2022_df <- as.data.frame(aspect_replicated_for_2014_to_2022_stack, xy = T, na.rm = T) # converting stacked aspect into dataframe
aspect_2014_2022_dfXxy_long <-  aspect_2014_2022_df %>%
  # convert dataframe into long format where there is only one aspect column
  pivot_longer(
    cols = starts_with("Aspect"),
    names_to = "Aspect",
    values_to = "Aspect_Value"
  ) %>% 
  mutate(Year = str_extract(Aspect, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Aspect, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'Aspect_Value')) # select relevant columns only

aspect_2014_2022_dfXxy_long$Aspect_Value <- minmax_norm(aspect_2014_2022_dfXxy_long$Aspect_Value) # apply min-max normalisation
# head(aspect_2014_2022_dfXxy_long); str(aspect_2014_2022_dfXxy_long)

fire_2014_2022_df <- as.data.frame(FIRE_2014_2022_stack, xy = T, na.rm = T) # converting stacked fire into dataframe
fire_2014_2022_dfXxy_long <-  fire_2014_2022_df %>%
  # convert dataframe into long format where there is only one fire column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) # select relevant columns only
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
# str(dfnorm_2014_2022)

# save dataframe
# save(dfnorm_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/dfnorm_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/dfnorm_2014_2022.Rdata')

# Example on how to convert the tabular data into raster format again after normalisation
x <- dfnorm_2014_2022 %>%
  filter(Year == 2021 & Month==04) %>%
  select(x,y,NDVI_Value)
  
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = NDVI_colour_ramp)
plot(roi_trans, col = 'transparent', border = 'black', add = T)


fa <- spatialRF::auto_cor(
  x = dfnorm_2014_2022[colnames(dfnorm_2014_2022)[-c(1,2,3,4,16)]],
  cor.threshold = 2,
) 

fa$cor

fa1 <- spatialRF::auto_vif(x = dfnorm_2014_2022[colnames(dfnorm_2014_2022)[-c(1,2,3,4,16)]], 
                           vif.threshold = 1000)

names(fa1)
fa1$vif


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
# newdataX <- dfnorm_2014_2022 %>%
#   filter(Year %in% c(2019)) %>%
#   dplyr::select(-c(1:4,16))
# newdataY <- dfnorm_2014_2022 %>%
#   filter(Year %in% c(2019)) %>%
#   dplyr::select(16) %>%
#   as.vector()
# 
# y <- predict(rf_dfnorm_2014_2022, newdataX) |> as.vector()|>as.numeric()
# 
# (sum(newdataY$Fire_Value==y)/length(y))*100
# 
# # create combinations of hyperparameters
# rf_gridsearch <- expand.grid(mtry = 2:11,
#                        splitrule = 'gini', # gini for classification
#                        min.node.size=2) 
# 
# # use ranger to run all these models
# set.seed(1)
# rf_gridsearch_Model_dfnorm_2014_2022_subset <- train(Fire_Value ~., 
#                        data =  dfnorm_2014_2022_subset[,-c(1:4)],
#                        method = 'ranger',
#                        num.trees = 100,
#                        verbose = T,
#                        trControl = trainControl(method = 'oob', verboseIter = T, allowParallel = T),
#                        tuneGrid = rf_gridsearch,
#                        importance = 'permutation') # Variable importance according to Mean Decrease in Accuracy (MDA)
# 
# # save model
# # save(rf_gridsearch_Model_dfnorm_2014_2022_subset, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_gridsearch_Model_dfnorm_2014_2022_subset.Rdata')
# 
# varImp(rf_gridsearch_Model_dfnorm_2014_2022_subset)
# y <- predict(rf_gridsearch_Model_dfnorm_2014_2022_subset, newdataX) |> as.vector()|>as.numeric()
# 
# (sum(newdataY$Fire_Value==y)/length(y))*100



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

distance_matrix <- parDist(as.matrix(dfnorm_2014_2022_subset[,c('x_norm','y_norm')]),
                           method = "euclidean", 
                           threads = 4)

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
