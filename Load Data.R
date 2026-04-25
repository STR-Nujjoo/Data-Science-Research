
# rm(list = ls()) # clear global environment



library(pbapply)
Raw_processed_data <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/RAW', pattern = '.Rdata')
pbsapply(seq_along(Raw_processed_data), function(x){load(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/RAW/', Raw_processed_data[x]), envir = .GlobalEnv)})

not_normalised_stack_2014_2022 <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised', pattern = '.Rdata')
pbsapply(seq_along(not_normalised_stack_2014_2022), function(x){load(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/', not_normalised_stack_2014_2022[x]), envir = .GlobalEnv)})                        

# loading full normalised table from 2014 to 2022
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022.Rdata')

not_normalised_stack_2002_2022 <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised', pattern = '.Rdata')
pbsapply(seq_along(not_normalised_stack_2002_2022), function(x){load(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/', not_normalised_stack_2002_2022[x]), envir = .GlobalEnv)})


# loading non normalised fire data
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/FIRE_2014_2022_stack.Rdata')

# Load data for Random forest Model 0- 2014-2022 resampled
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/fully_resampled_dfnorm_2014_2022_training_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_validation_set.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Dataframe format (normalised)/dfnorm_2014_2022_test_set.Rdata')

# load outputs from random forest model 0
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_2014_2022_resampled_dataset.Rdata')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_probabilities_2014_2022_resampled_dataset.Rdata')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_thresholds_2014_2022_resampled_dataset.Rdata')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_F1_scores_2014_2022_resampled_dataset.Rdata')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_metrics_2014_2022_resampled_dataset.Rdata')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_results_2014_2022_resampled_dataset.Rdata')


# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)
