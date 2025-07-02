# Loading relevant libraries
{
  library(keras)
  library(tensorflow)
  library(abind)
  library(raster)
  library(sp)
  library(tidyverse)
  library(rgdal)
  library(sf)
  library(naniar)
  library(terra)
  library(colorRamps)
  library(pbapply)
  library(caret)
  library(tmap)
  library(cowplot)
  library(gridExtra)
  library(mltools)
  library(ROCR)
  library(parallel)
  library(tfaddons)
  library(rgeoda)
  
}

reticulate::py_install("tensorflow-addons", pip = TRUE)

# PRELIMINARY FUNCTIONS ---------------------------------------------------

# Function to be applied on the raster values; return: rasterLayer object
raster_stack_minmax_norm <- function(stack_raster, index) {
  
  data <- stack_raster # raster stack
  min_val <-  min(minValue(data)) # global minimum of raster stack
  max_val <- max(maxValue(data)) # global maximum of raster stack
  index <- index # raster index
  val <- data[[index]] # relevant raster only
  
  x <- (val - min_val) / (max_val - min_val) # normalisation calculation
  x[is.na(values(x))] <- 0 # convert all NA values after normalisation to 0
  x[x < 0] <- 0     # correct tiny negative values due to floating point error to 0
  x[x > 1] <- 1     # similarly just in case of overshoots restrict value to 1
  
  return(x)
}

# Specificity metric created for the Keras interface
specificity_metric <- function(threshold){
  function(y_true, y_pred) {
    y_pred_binary <- k_cast(k_greater(y_pred, threshold), k_floatx())
    y_true_binary <- k_cast(y_true, k_floatx())
    
    # True Negatives: predicted 0 and actual 0
    tn <- k_sum(k_cast(k_equal(y_pred_binary + y_true_binary, 0), k_floatx()))
    
    # False Positives: predicted 1 but actual 0
    fp <- k_sum(k_cast(k_equal(y_pred_binary - y_true_binary, 1), k_floatx()))
    
    specificity <- tn / (tn + fp + k_epsilon())  # Avoid division by zero
    return(specificity)
  }
}

# F1 score metric created for the Keras interface
f1_score_metric <- function(threshold){
  function(y_true, y_pred) {
    y_pred_binary <- k_cast(k_greater(y_pred, threshold), k_floatx())
    
    tp <- k_sum(y_true * y_pred_binary)
    fp <- k_sum((1 - y_true) * y_pred_binary)
    fn <- k_sum(y_true * (1 - y_pred_binary))
    
    precision <- tp / (tp + fp + k_epsilon())
    recall <- tp / (tp + fn + k_epsilon())
    
    f1 <- 2 * (precision * recall) / (precision + recall + k_epsilon())
    return(f1)
  } # checked! It is doing the right calculation
  
}

# MCC metric created for the Keras interface
mcc_metric <- function(threshold){
  function(y_true, y_pred) {
    y_pred_binary <- k_cast(k_greater(y_pred, threshold), k_floatx())
    
    tp <- k_sum(y_true * y_pred_binary)
    tn <- k_sum((1 - y_true) * (1 - y_pred_binary))
    fp <- k_sum((1 - y_true) * y_pred_binary)
    fn <- k_sum(y_true * (1 - y_pred_binary))
    
    numerator <- (tp * tn) - (fp * fn)
    denominator <- k_sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
    
    return(numerator / (denominator + k_epsilon()))
  }
}


# Define color code for fire rasters
fire_color_condition_func <- function(data){
  xx <- data
  fire_color_condition <- if (all(values(xx) %>% na.omit() == 0)) { # if raster contains only negatives
    "lightgray"
  } else { # if raster is binary
    c("lightgray", "red")
  }
  return(fire_color_condition)
}


tfa <- reticulate::import("tensorflow_addons", delay_load = TRUE)
focal_loss <- tfa$losses$SigmoidFocalCrossEntropy
focal_loss_fn <- function(alpha = NULL, gamma = NULL) {
  loss_fn <- tfa$losses$SigmoidFocalCrossEntropy(alpha = alpha, gamma = gamma)
  function(y_true, y_pred) {
    loss_fn(y_true, y_pred)
  }
}



# READING & LOADING RELEVANT OBJECTS --------------------------------------

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# identifying dupicates aerial imageries from 2014 to 2022
duplicate_aerial_imageries_to_remove <- c('20140425', '20140612', '20140714', '20141002', '20150122', '20150223', '20150903',
                                          '20161226', '20180319', '20181130', '20200425', '20210106', '20211224', '20220610', '20231003',
                                          '20231206')

# LULC for resampling to obtain correct dimensions 
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/RAW/FINAL_LULC.Rdata', envir = .GlobalEnv)

# reading all the file names
final_lulc_names <- sapply(seq_along(FINAL_LULC), function (x){sub('LULC ', '', FINAL_LULC[[x]]@file@name)})

# removing the duplicate LULC
LULC <- lapply(seq_along(which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)), 
               function (x) {FINAL_LULC[[which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from LULC- we're only dealing with 108 periods now from 2014 to 2022
LULC_2014_2022 <- lapply(1:108, function (x) {LULC[[x]]})

lapply(1:108, function (x) {names(LULC_2014_2022[[x]]) <- LULC_2014_2022[[x]]@file@name
names(LULC_2014_2022[[x]]) <<- gsub('[.]','', names(LULC_2014_2022[[x]]))}) # rename layers

# Import fire data alone for weight computations
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/RAW/FIRE_DATA.Rdata', envir = .GlobalEnv)

FIRE_2002_2022 <- lapply(1:252, function(x) {FIRE_DATA[[x]]})
FIRE_2014_2022 <- lapply(145:252, function(x) {FIRE_2002_2022[[x]]})

# response variable 2014 to 2022
FIRE_2014_2022_stack <- stack(FIRE_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()

FIRE_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: FIRE training set 
                                       function(x) {FIRE_2014_2022_stack[[x]]}) |> stack()
FIRE_2014_2018_stack_norm_train <- pblapply(seq_along(FIRE_2014_2018_stack_train@layers), # 2014-2018: FIRE training set normalised
                                            function(x) {raster_stack_minmax_norm(FIRE_2014_2018_stack_train, x)}) |> stack()

# Loading full 2014 to 2022 dataset in convLSTM format
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2014_2018_train.RData')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2014_2018_train.RData')

load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2019_2020_val.RData')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2019_2020_val.RData')

load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2021_2022_test.RData')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2021_2022_test.RData')


# subset predictor variables data to test convLSTM
# subset training set
# extracting only 2017 and 2018 rasters timesteps and selecting only NDVI, NDMI, ATP, AMT and ANSWS 
predictor_variables_2014_2018_train_subset <- predictor_variables_2014_2018_train[,37:60,,,c(2,3,5,6,7), drop = F]
dim(predictor_variables_2014_2018_train_subset)

# subset response variables data to test convLSTM
response_variable_2014_2018_train_subset <- response_variable_2014_2018_train[,37:60,,,, drop = F]
dim(response_variable_2014_2018_train_subset)

# subset validation set for predictor variable only as the timesteps were not disturbed
predictor_variables_2019_2020_val_subset <- predictor_variables_2019_2020_val[,,,,c(2,3,5,6,7), drop = F]
dim(predictor_variables_2019_2020_val_subset)

# subset test set for predictor variable only as the timesteps were not disturbed
predictor_variables_2021_2022_test_subset <- predictor_variables_2021_2022_test[,,,,c(2,3,5,6,7), drop = F]
dim(predictor_variables_2021_2022_test_subset)

# Convert training fire stack into a dataframe to find out the ratio of class imbalance for fire to non-fire events
fire_df <- as.data.frame(FIRE_2014_2018_stack_norm_train, xy = T) %>% # converting stacked fire into dataframe
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  na.omit() %>%
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value')) %>% # select relevant columns only
  filter(Year %in% c(2017, 2018)) # filter out 2017 and 2018 from the data to match the subset

# CONVOLUTION LSTM FULL FRAMEWORK -----------------------------------------

# reading data for modelling process
trainX <- predictor_variables_2014_2018_train_subset
dim(trainX) # (samples, time_steps, height, width, variables)- channels_last format
trainY <- response_variable_2014_2018_train_subset
dim(trainY) # (samples, time_steps, height, width, variables)- channels_last format

# fire_class_imbalance_subset <- table(response_variable_2014_2018_train_subset) # fire class imbalance (global imbalance)
# fire_class_imbalance_prop_subset <- prop.table(table(response_variable_2014_2018_train_subset)) # fire class imbalance proportion
# calculated_class_weights_subset <- max(fire_class_imbalance_subset)/fire_class_imbalance_subset # global class weights if we are using class_weight in the algorithm

# Sample weight preparation
spatial_temporal_weight <- function(year, temporal_weight){sapply(1:12, function(x){ # spatial weight is systematically calculated already in this function
  
  fire_count_df <- fire_df %>%
    group_by(Year,Month,Fire_Value) %>%
    tally() # count the number of fire or no fire pixels per month per year
  
  fire_seasons <- c(1,2,3,4,10,11,12) # adding more weights temporally for months which had fire conisstently over the years
  
  weight_values_calc <- fire_count_df %>%
    filter(Year == year, Month == x)
  
  if(x %in% fire_seasons){ # if month are in the fire seasons define above...

    if(length(weight_values_calc$Fire_Value) > 1){ # if both classed are present
      spatial_temporal_weight <- ((max(weight_values_calc$n)/weight_values_calc$n)[2])*temporal_weight #... increase weight by a factor of n
    }else{
      spatial_temporal_weight <- 1
    }
  } else{ # if month are not in the fire seasons define above...
    
    if(length(weight_values_calc$Fire_Value) > 1){ # if both classed are present
      spatial_temporal_weight <- (max(weight_values_calc$n)/weight_values_calc$n)[2]
    }else{
      spatial_temporal_weight <- 1
    }
  }
  
  return(spatial_temporal_weight)
})}

# These are the weights that will be assigned to fire pixels otherwise 1 to no fire pixels
concatenated_spatial_temporal_weight <- pbsapply(seq_along(unique(fire_df$Year)), function(x){
  spatial_temporal_weight(year = unique(fire_df$Year)[x], temporal_weight = 1.5)
})|>c()

# spatial temporal sample weight array
spatial_temporal_weight_array <- array(0, dim = dim(trainY)) # defining empty array for weighted fire rasters
dim(spatial_temporal_weight_array)
for(i in 1:dim(trainY)[2]){
  n <- i
  x <- trainY[1,n,,,1]*concatenated_spatial_temporal_weight[n]
  w <- ifelse(x==0, 1, x)
  spatial_temporal_weight_array[1, n, , , 1] <- w
}


valX <- predictor_variables_2019_2020_val_subset
dim(valX) # (samples, time_steps, height, width, variables)- channels_last format
valY <- response_variable_2019_2020_val
dim(valY) # (samples, time_steps, height, width, variables)- channels_last format

testX <- predictor_variables_2021_2022_test_subset
dim(testX) # (samples, time_steps, height, width, variables)- channels_last format
testY <- response_variable_2021_2022_test
dim(testY) # (samples, time_steps, height, width, variables)- channels_last format


ConvLSTM_framework <- function(t){
  # Building a convolution lstm for wildfire susceptibility
  tensorflow::set_random_seed(1)
  ConvLSTM_model <- keras_model_sequential() %>%
    # 1st ConvLSTM layer
    layer_conv_lstm_2d(
      input_shape = list(NULL, dim(trainX)[3], dim(trainX)[4], dim(trainX)[5]), # samples = 1, time_steps=NULL to allow for varying timesteps months, channels = 2 predictor variables, rows = 32, cols = 32
      filters = 64, 
      kernel_size = c(3, 3), 
      data_format = 'channels_last',
      kernel_regularizer = regularizer_l2(0.001), # applies L2 regularisation to the kernel weights
      recurrent_regularizer = regularizer_l2(0.001), # applies it to recurrent weights (inside the LSTM)
      bias_regularizer = regularizer_l2(0.001), # applies it to biases
      activation = "tanh",
      padding = "same", 
      return_sequences = T, # It is important for this to be TRUE so that the time steps are also returned
    ) %>%
    
    # Normalize the activations of the previous layer (commonly used!)- 1st batch normalisation
    layer_batch_normalization() %>%
    
    # dropout
    layer_dropout(rate = 0.2) %>%
    
    # 2nd ConvLSTM layer
    layer_conv_lstm_2d(
      filters = 64, 
      kernel_size = c(3, 3), 
      data_format = 'channels_last',
      kernel_regularizer = regularizer_l2(0.001), # applies L2 regularisation to the kernel weights
      recurrent_regularizer = regularizer_l2(0.001), # applies it to recurrent weights (inside the LSTM)
      bias_regularizer = regularizer_l2(0.001), # applies it to biases
      activation = "tanh",
      padding = "same", 
      return_sequences = T, # It is important for this to be TRUE so that the time steps are also returned
    ) %>%
    
    # Normalize the activations of the previous layer (commonly used!)- 2nd batch normalisation
    layer_batch_normalization() %>%
    
    # dropout
    layer_dropout(rate = 0.2) %>%
    
    # # Dense layers
    time_distributed(layer_dense(units = 50, activation = "tanh")) %>%
    
    # dropout
    layer_dropout(rate = 0.5) %>%
    
    
    # # Output layer
    time_distributed(layer_dense(units = 1, activation = "sigmoid"))
  
  focal_loss_fn_alpha_0_9_gamma_2 <- focal_loss_fn(alpha = 0.9, gamma = 2)
  # Compile the ConvLSTM_model
  tensorflow::set_random_seed(1)
  ConvLSTM_model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.0001, weight_decay = 0.03),
    # focal loss sigmoid crossentropy
    loss = focal_loss_fn_alpha_0_9_gamma_2,
    metrics = list(
      metric_binary_accuracy(name = 'binary_accuracy', threshold = t),
      metric_recall(name = 'recall', thresholds = t),
      metric_precision(name = 'precision', thresholds = t),
      custom_metric("specificity", metric_fn = specificity_metric(threshold = t)),
      custom_metric(name = 'f1_score', metric_fn = f1_score_metric(threshold = t)),
      custom_metric(name = 'MCC', metric_fn = mcc_metric(threshold = t)),
      metric_false_negatives(name = 'fn'),
      metric_false_positives(name = 'fp'),
      metric_true_negatives(name = 'tn'),
      metric_true_positives(name = 'tp')
    )
    
  )
}
# ?compile.keras.engine.training.Model

main_training_results <- list()
# thresholds <- seq(0.4,0.7, by = .01) # threshold list 
thresholds <- c(0.5,0.51, 0.52)
for(t in thresholds){
  cat("Training for threshold: ", t, "\n")
  
  tensorflow::set_random_seed(1)
  ConvLSTM_model <- ConvLSTM_framework(t = t)
  # ConvLSTM_model%>%summary()
  
model_path <- paste0("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/ConvLSTM best model per threshold/model_threshold_", sprintf("%.2f", t), ".h5")

  callback_list <- list(
    callback_early_stopping(
      monitor = "val_MCC",
      min_delta = 0.0005,
      patience = 50,           # number of epochs to wait for improvement
      mode = "max",            # because higher MCC is better
      restore_best_weights = TRUE
    ),
    callback_model_checkpoint(
      filepath = model_path,
      save_best_only = TRUE,
      monitor = "val_MCC",
      mode = "max"
    )
  )
  
  tensorflow::set_random_seed(1)
  history <- ConvLSTM_model %>% fit(
    trainX, trainY,
    validation_data = list(valX, valY),
    use_multiprocessing = T,
    epochs = 4, # 300 looks reasonable
    batch_size = 100,
    sample_weight = spatial_temporal_weight_array, # this assign weights to rasters on a more individual level (spatial-temporal) as in a raster with fire with more weight than a raster with no fire
    callbacks = callback_list,
    shuffle = F # very important to ensure temporal continuity/consistency
  )
  
  # save all training metrics in a list
  main_training_results[[as.character(t)]] <- list(threshold = t,
                                                   history = history,
                                                   best_epoch = which.max(history$metrics$val_MCC),
                                                   model_file = model_path,
                                                   best_val_MCC = max(history$metrics$val_MCC))
  
}

save(main_training_results, file = '..directory.../main_training_results.Rdata')

# Extract all the best validation MCCs from the different thresholds
best_val_MCCs <- sapply(seq_along(main_training_results), function(x){main_training_results[[x]]$best_val_MCC})

# Visualise the best validation MCCs of the best model for each threshold
#....
main_training_results[[1]]$history$metrics
optimal_ConvLSTM_model_index <- which.max(best_val_MCCs)

validation_metrics <- c(val_loss = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_loss[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_binary_accuracy = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_binary_accuracy[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_recall = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_recall[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_precision = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_precision[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_specificity = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_specificity[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_f1_score = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_f1_score[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_MCC = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_fn = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_fn[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_fp = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_fp[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_tn = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_tn[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
  val_tp = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_tp[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)])



# Creating a function to load each best model for each threshold
load_model_by_threshold <- function(file_path, t){
  tensorflow::set_random_seed(1)
  path <- file_path
  x <- load_model_hdf5(path, 
                       custom_objects = list(specificity = specificity_metric(threshold = t),
                                             f1_score = f1_score_metric(threshold = t),
                                             MCC = mcc_metric(threshold = t),
                                             focal_loss_fn_alpha_0_9_gamma_2 = focal_loss_fn(alpha = 0.9, gamma = 2)),
                       compile = T)
}


# reading file names from folder if needed
# MODELS_PATH <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/ConvLSTM best model per threshold/')

optimal_ConvLSTM_threshold <- main_training_results[[optimal_ConvLSTM_model_index]]$threshold
# Loading the BEST model from the optimal threshold
optimal_ConvLSTM_model <- load_model_by_threshold(file_path = main_training_results[[optimal_ConvLSTM_model_index]]$model_file,
                         t = optimal_ConvLSTM_threshold) # extract the threshold as part of the name to ensure consistency

# Creating a function to calculate AUC_ROC and AUC_PR separately
AUC_metrics <- function(best_model, true_dataX, true_dataY, threshold){
  tensorflow::set_random_seed(1)
  predicted_dataX <- best_model %>% predict(true_dataX)
  x <- ifelse(as.vector(predicted_dataX) > threshold, 1, 0)
  p <- prediction(x, as.vector(true_dataY))
  AUC_ROC <- performance(p, measure = 'auc')@y.values[[1]] # AUC_ROC
  AUC_PR <- performance(p, measure = 'aucpr')@y.values[[1]] # AUC_PR
  
  return(c(AUC_ROC = AUC_ROC, AUC_PR = AUC_PR))
}

val_AUCs <- AUC_metrics(best_model = optimal_ConvLSTM_model, 
            true_dataX = valX, 
            true_dataY = valY, 
            threshold = optimal_ConvLSTM_threshold)



tensorflow::set_random_seed(1)
val_acc_check <- optimal_ConvLSTM_model %>% evaluate(valX, valY)

# Check if the optimal model is correctly extracted to match the optimal outcome of the validation accuracy of the best model prior to loading the best model
if(all(val_acc_check == validation_metrics)){
  print('Verification Successful!')
}else{
  print('Verification Unsuccessful!')
}


# ?fit.keras.engine.training.Model
# plot(history)
tensorflow::set_random_seed(1)
test_metrics <- optimal_ConvLSTM_model %>% evaluate(testX, testY);test_metrics

test_AUCs <- AUC_metrics(best_model = optimal_ConvLSTM_model, 
                        true_dataX = testX, 
                        true_dataY = testY, 
                        threshold = optimal_ConvLSTM_threshold)


# fire predicted for 2021 and 2022 - This is where all the probabilities are stored
tensorflow::set_random_seed(1)
predicted <- optimal_ConvLSTM_model %>% predict(testX)
dim(predicted)
summary(predicted)
# as.vector(predicted[1,15,,,1])[which(as.vector(testY[1,15,,,1])==1)]|>summary()


# creating time label
timesteps_labels <- c('Fire 2021-01', 'Fire 2021-02', 'Fire 2021-03', 'Fire 2021-04', 'Fire 2021-05', 'Fire 2021-06', 'Fire 2021-07','Fire 2021-08', 'Fire 2021-09', 'Fire 2021-10', 'Fire 2021-11', 'Fire 2021-12',
                      'Fire 2022-01', 'Fire 2022-02', 'Fire 2022-03', 'Fire 2022-04', 'Fire 2022-05', 'Fire 2022-06', 'Fire 2022-07','Fire 2022-08', 'Fire 2022-09', 'Fire 2022-10', 'Fire 2022-11', 'Fire 2022-12')


# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)
# To allow parallel processing in pbapply functions export items used in the function to the cluster
clusterExport(cl, varlist = c("predicted", 'roi_trans', 'LULC_2014_2022', 'timesteps_labels', 'testY')) 
# Loading relevant packages on cluster
clusterEvalQ(cl, {
  library(caret)
  library(raster)
  library(tidyverse)
  })

# Converting the predicted probabilities into their respective raster while cropping each raster to the study area
predicted_raster_list <- pblapply(1:dim(predicted)[2],
         function(x){
           index <- x
           
           predicted_normal_Raster_format <- predicted[1, index, , , 1] # not rasterised yet!
           dim(predicted_normal_Raster_format)
           
           # Rasterise predicted probabilities
           predicted_raster <- raster(predicted_normal_Raster_format, 
                                      crs = crs(roi_trans), 
                                      xmn = extent(LULC_2014_2022[[1]])[1], #xmin 
                                      xmx = extent(LULC_2014_2022[[1]])[2], #xmax
                                      ymn = extent(LULC_2014_2022[[1]])[3], #ymin
                                      ymx = extent(LULC_2014_2022[[1]])[4] #ymax
           ) |> mask(roi_trans) # mask to study area to remove unecessary probabiliities
           res(predicted_raster) <- 30 # update spatial resolution to 30m
           names(predicted_raster) <- timesteps_labels[index] # rename layer
           return(predicted_raster)
         },
         cl = cl) # parallelise computation

# Converting the original fire test array for the test set into rasters again while cropping each raster to the study area
true_test_raster_list <- pblapply(1:dim(testY)[2], function(x){
  index <- x
  y_true <- testY[1, index, , , 1] 
  # Rasterise fire test data restricted to roi
  y_true_raster <- raster(y_true, 
                          crs = crs(roi_trans), 
                          xmn = extent(LULC_2014_2022[[1]])[1], #xmin 
                          xmx = extent(LULC_2014_2022[[1]])[2], #xmax
                          ymn = extent(LULC_2014_2022[[1]])[3], #ymin
                          ymx = extent(LULC_2014_2022[[1]])[4] #ymax
  ) |> mask(roi_trans) # mask to study area to remove unecessary probabiliities
  res(y_true_raster) <- 30 # update spatial resolution to 30m
  names(y_true_raster) <- timesteps_labels[index]  # rename layer
  return(y_true_raster)
  
},
cl = cl)

# create empty list to save threshold list and relevant metrics for each predicted rasters
ConvLSTM_threshold_list <- list()
ConvLSTM_specificity_list <- list()
ConvLSTM_f1_score_list <- list()
for (i in 1:length(predicted_raster_list)){
  cat('Iteration ', i, ' out of ', length(predicted_raster_list), '\n')
  i <- i
  index <- i
  # threshold <- seq(minValue(predicted_raster_list[[index]]), maxValue(predicted_raster_list[[index]]), by = 0.00001) # generate a sequence of threshold to classify response variable based on probability class
  threshold <- optimal_ConvLSTM_threshold
  # To allow parallel processing in pbapply functions export items used in the function to the cluster
  clusterExport(cl, varlist = c('predicted_raster_list', 'true_test_raster_list', 'threshold', 'index')) 
  
  # This function output the f1 score (if both classes exist) or specificity (if only the negative class exists) for each threshold generated for each test raster
  F1_SCORES_SPECIFICITY <- pbsapply(seq_along(threshold), function (x){
    pred_class <- ifelse(as.vector(predicted_raster_list[[index]]) > threshold, 1, 0)
    confusion_matrix <- confusionMatrix(factor(as.vector(pred_class), levels = c('0','1')), 
                                        factor(as.vector(true_test_raster_list[[index]]), 
                                               levels = c('0','1')), 
                                        positive = '1', 
                                        mode = 'everything') # apply threshold on positive class; 1 in this case
    
    if(sum(confusion_matrix$table[,2])==0){ # if there's only negatives (i.e, no fire events)- F1 score is invalid
      c(Specificity=confusion_matrix$byClass['Specificity'][[1]])
      
    }else{ # if both classes are available, then use f1 score instead
      c(F1_score=confusion_matrix$byClass['F1'][[1]])
    }
    
  }, cl = cl)
  
  ConvLSTM_threshold_list[[i]] <- threshold
  
  if(names(F1_SCORES_SPECIFICITY)[1]=='Specificity'){ # if function outputs specificity
    ConvLSTM_specificity_list[[i]] <- F1_SCORES_SPECIFICITY|>unname()
    ConvLSTM_f1_score_list[[i]] <- NA
  }else{ # if function outputs f1 score
    ConvLSTM_specificity_list[[i]] <- NA
    ConvLSTM_f1_score_list[[i]] <- F1_SCORES_SPECIFICITY|>unname()
  }
  
}

stopCluster(cl)

# creating empty list to save relevant output
y_pred_raster_list <- list()
ConvLSTM_metrics_list <- list()
ConvLSTM_test_results <- NULL
for(i in 1:length(ConvLSTM_f1_score_list)){
  
  cat('Iteration ', i, ' out of ', length(ConvLSTM_f1_score_list), '\n')
  
  index <- i # index for each raster prediction
  if(all(is.na(ConvLSTM_f1_score_list[[index]]))){ # if f1 score is irrelevant/NA
    optimal_threshold <- ConvLSTM_threshold_list[[index]][which.max(ConvLSTM_specificity_list[[index]])] # optimal threshold will be based on maximised specificity
    optimal_specificity <- ConvLSTM_specificity_list[[index]][which.max(ConvLSTM_specificity_list[[index]])]
    optimal_F1_score <- NA
  }else{ # if specificity is irrelevant/NA
    optimal_threshold <- ConvLSTM_threshold_list[[index]][which.max(ConvLSTM_f1_score_list[[index]])] # optimal threshold will be based on maximised f1 score
    optimal_F1_score <- ConvLSTM_f1_score_list[[index]][which.max(ConvLSTM_f1_score_list[[index]])] 
    optimal_specificity <- NA
  }
  
  # predicted classes after optimal threshold is applied
  y_pred <- ifelse(as.array(predicted_raster_list[[index]]) > optimal_threshold, 1, 0)
  
  # Rasterise predicted classes after optimal threshold has been applied
  y_pred_raster <- raster(y_pred[,,1], 
                          crs = crs(roi_trans), 
                          xmn = extent(LULC_2014_2022[[1]])[1], #xmin 
                          xmx = extent(LULC_2014_2022[[1]])[2], #xmax
                          ymn = extent(LULC_2014_2022[[1]])[3], #ymin
                          ymx = extent(LULC_2014_2022[[1]])[4] #ymax
  )|>mask(roi_trans)
  res(y_pred_raster) <- 30 # update spatial resolution to 30m
  names(y_pred_raster) <- paste0(timesteps_labels[index]," (predicted)")
  
  # generate confusion matrix with all metrics
  CM <- confusionMatrix(factor(as.vector(y_pred_raster), levels = c('0','1')),
                        factor(as.vector(true_test_raster_list[[index]]), levels = c('0','1')), 
                        positive = '1', mode = 'everything')
  
  if(sum(CM$table[,2])==0){ # if no fire events exist at all (i.e, raster has only negatives/0) 
    #...the below metrics are then irrelevant
    test_AUC_ROC <- NA
    test_AUC_PR <- NA
    test_MCC <- NA
  }else{ # both positives and negatives exist
    ROCR_test_prediction <- prediction(as.vector(y_pred_raster)|>na.omit()|>as.vector(), as.vector(true_test_raster_list[[index]])|>na.omit()|>as.vector())
    test_AUC_ROC <- performance(ROCR_test_prediction, measure = 'auc')@y.values[[1]] # AUC_ROC
    test_AUC_PR <- performance(ROCR_test_prediction, measure = 'aucpr')@y.values[[1]] # AUC_PR
    # An MCC value of +1 indicates perfect agreement between the model's predictions and the actual labels, 
    # while -1 indicates total disagreement.
    # A value of 0 suggests the model performs no better than random guessing
    test_MCC <- mcc(as.factor(as.vector(y_pred_raster)),as.factor(as.vector(true_test_raster_list[[index]]))) # test accuracy using matthew's correlation coefficient
    
  }
  y_pred_raster_list[[i]] <- y_pred_raster
  ConvLSTM_metrics_list[[i]] <- CM
  ConvLSTM_test_results <- rbind(ConvLSTM_test_results, 
                                 tibble(optimal_threshold = optimal_threshold,
                                       overall_accuracy = CM$overall['Accuracy'][[1]],
                                       precision = CM$byClass['Precision'][[1]],
                                       recall = CM$byClass['Recall'][[1]],
                                       specificity = CM$byClass['Specificity'][[1]],
                                       F1_score = optimal_F1_score, 
                                       AUC_ROC = test_AUC_ROC,
                                       AUC_PR = test_AUC_PR,
                                       MCC = test_MCC,
                                       fire_period = timesteps_labels[index],
                                       true_fire_status = ifelse(maxValue(true_test_raster_list[[index]])==1, 
                                                                 'positive',
                                                                 'negative')))
  
}

# View(ConvLSTM_test_results)

# creating a function for visualisation
WS_visualisation <- function(true_raster, raster_with_probabilities, raster_factor, classes_breaks_method = c('natural_breaks', 'quantile')){
  
  # Subdivision types
  quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
  natural_breaks_subdivisions <- natural_breaks(k = 5, df=as.data.frame(raster_with_probabilities, na.rm = T))
  
  # Susceptibility quantile classes- makes more sense
  wildfire_susceptibility_quantile_classes <- matrix(c(
    -0.1, quantile_subdivisions[2], 1, # very low
    quantile_subdivisions[2], quantile_subdivisions[3], 2, # low
    quantile_subdivisions[3], quantile_subdivisions[4], 3, # moderate
    quantile_subdivisions[4], quantile_subdivisions[5], 4, # high
    quantile_subdivisions[5], 1, 5 # very high
  ), ncol = 3, byrow = TRUE)
  
  # Susceptibility natural breaks classes
  wildfire_susceptibility_natural_breaks_classes <- matrix(c(
    -0.1, natural_breaks_subdivisions[1], 1, # very low
    natural_breaks_subdivisions[1], natural_breaks_subdivisions[2], 2, # low
    natural_breaks_subdivisions[2], natural_breaks_subdivisions[3], 3, # moderate
    natural_breaks_subdivisions[3], natural_breaks_subdivisions[4], 4, # high
    natural_breaks_subdivisions[4], 1, 5 # very high
  ), ncol = 3, byrow = TRUE)
  
  if(classes_breaks_method=='natural_breaks'){
    # Reclassify raster accordingly
    classified_raster <- classify(raster_with_probabilities|>rast(), wildfire_susceptibility_natural_breaks_classes)
    levels(classified_raster) <- data.frame(
      ID = 1:5,
      Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
    )
    
    # Update levels
    classified_raster <- droplevels(classified_raster)
    
  } else if(classes_breaks_method == 'quantile'){
    
    # Reclassify raster accordingly
    classified_raster <- classify(raster_with_probabilities|>rast(), wildfire_susceptibility_quantile_classes)
    levels(classified_raster) <- data.frame(
      ID = 1:5,
      Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
    )
    
    # Update levels
    classified_raster <- droplevels(classified_raster)
  }
  
  # Update levels of other rasters
  levels(true_raster) <- data.frame(
    ID = 0:1,
    fire_status = c('No Fire', 'Fire')
  )
  true_raster <- droplevels(true_raster|>rast())
  
  levels(raster_factor) <- data.frame(
    ID = 0:1,
    fire_status = c('No Fire', 'Fire')
  )
  raster_factor <- droplevels(raster_factor|>rast())
  
  
  # define a color palette for the wildfire susceptibility class
  WS_palette <- c('#007206', '#7DB810', '#F2FE1E', '#FFAC12','#FC3B09')
  
  # print(
  # Visualising the fire data used as testY
  p1 <- tm_shape(true_raster)+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(true_raster))+
    tm_layout(main.title= 'True Fire map',
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5,
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(lines = F)
  # )
  
  p2 <- tm_shape(raster_factor)+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(raster_factor))+
    tm_layout(main.title= 'Predicted Fire Map',
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5,
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(lines = F)
  
  # print(
  # Visualise the sd of wildfire probabilities raster
  p3 <- tm_shape(raster_with_probabilities)+
    tm_raster(style = "sd", title = "", palette = '-RdBu')+
    tm_layout(main.title= 'SD Map',
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5,
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(lines = F)
  # )
  
  # print(
  # Visualise the classified raster
  p4 <- tm_shape(classified_raster)+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(classified_raster)[[1]]$ID)])+
    tm_layout(main.title= 'Wildfire Susceptibility Map',
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5,
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(lines = F)
  # )
  
  return(tmap_arrange(p1,p2,p3,p4, nrow = 2, ncol = 2)) 
}


# VISUALISATION OF WSM ----------------------------------------------------

call_fire_period <- 'Fire 2022-03'

# optmised_threshold_plot(fire_period = call_fire_period)

WS_visualisation(true_raster = true_test_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_with_probabilities = predicted_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_factor = y_pred_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 classes_breaks_method = 'natural_breaks')

WS_visualisation(true_raster = true_test_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_with_probabilities = predicted_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_factor = y_pred_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 classes_breaks_method = 'quantile')




# # save all results
# save(evaluation,
#      predicted,
#      predicted_raster_list,
#      true_test_raster_list,
#      ConvLSTM_threshold_list,
#      ConvLSTM_specificity_list,
#      ConvLSTM_f1_score_list,
#      y_pred_raster_list,
#      ConvLSTM_metrics_list,
#      ConvLSTM_test_results,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/ConvLSTM main subset results/ConvLSTM_main_outputs_on_subset.Rdata')






