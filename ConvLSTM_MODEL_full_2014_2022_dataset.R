
# create new virtual environment in R using python 3 (Note: the path was obtained by checking "which python3.10" in my terminal!)
# reticulate::virtualenv_create("r-reticulate", python = "/usr/local/bin/python3.10") 

# install Tensorflow into the new environment
# reticulate::virtualenv_install("r-reticulate", packages = "tensorflow==2.15.0") # why this specific version of tensorflow?-because of tfaddons which uses older version of tensorflow

# install Tensorflow extension into the new environment
# reticulate::virtualenv_install("r-reticulate", packages = "tensorflow-addons==0.23.0")
# reticulate::py_install("tensorflow-addons", pip = TRUE)

# activate and use the new environment
# reticulate::use_virtualenv("r-reticulate", required = TRUE)

# install_tensorflow()
# tensorflow::tf_config()

# Loading relevant libraries
{
  library(reticulate)
  library(tensorflow)
  library(keras)
  library(raster)
  library(tidyverse)
  library(pbapply)
  library(parallel)
}


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


# # Define color code for fire rasters
# fire_color_condition_func <- function(data){
#   xx <- data
#   fire_color_condition <- if (all(values(xx) %>% na.omit() == 0)) { # if raster contains only negatives
#     "lightgray"
#   } else { # if raster is binary
#     c("lightgray", "red")
#   }
#   return(fire_color_condition)
# }


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
# roi <- readOGR('Wildfire_Data_Stefan/TMNR shapefile/tmnr_boundary.shp')
# roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# identifying dupicates aerial imageries from 2014 to 2022
duplicate_aerial_imageries_to_remove <- c('20140425', '20140612', '20140714', '20141002', '20150122', '20150223', '20150903',
                                          '20161226', '20180319', '20181130', '20200425', '20210106', '20211224', '20220610', '20231003',
                                          '20231206')


# LULC for resampling to obtain correct dimensions 
final_lulc_filenames <- list.files('/home/njjsye001/LULC_2014-2023_post-processing', pattern = '.tif')
FINAL_LULC <- pblapply(seq_along(final_lulc_filenames), 
                       function(x) {raster(paste0('/home/njjsye001/LULC_2014-2023_post-processing/',final_lulc_filenames[x]))})

# reading all the file names
final_lulc_names <- sapply(seq_along(FINAL_LULC), function (x){sub('LULC.', '', names(FINAL_LULC[[x]]))})

# removing the duplicate LULC
LULC <- lapply(seq_along(which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)), 
               function (x) {FINAL_LULC[[which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from LULC- we're only dealing with 108 periods now from 2014 to 2022
LULC_2014_2022 <- lapply(1:108, function (x) {LULC[[x]]})

# lapply(1:108, function (x) {names(LULC_2014_2022[[x]]) <- LULC_2014_2022[[x]]@file@name
# names(LULC_2014_2022[[x]]) <<- gsub('[.]','', names(LULC_2014_2022[[x]]))}) # rename layers

# Import fire data alone for weight computations
fire_data_filenames <- list.files('/home/njjsye001/SANParks', pattern = '.tif')
FIRE_DATA <- pblapply(seq_along(fire_data_filenames), 
                       function(x) {raster(paste0('/home/njjsye001/SANParks/',fire_data_filenames[x]))})

FIRE_2002_2022 <- lapply(1:252, function(x) {FIRE_DATA[[x]]})
FIRE_2014_2022 <- lapply(145:252, function(x) {FIRE_2002_2022[[x]]})

# response variable 2014 to 2022
FIRE_2014_2022_stack <- stack(FIRE_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()

FIRE_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: FIRE training set 
                                       function(x) {FIRE_2014_2022_stack[[x]]}) |> stack()
FIRE_2014_2018_stack_norm_train <- pblapply(seq_along(FIRE_2014_2018_stack_train@layers), # 2014-2018: FIRE training set normalised
                                            function(x) {raster_stack_minmax_norm(FIRE_2014_2018_stack_train, x)}) |> stack()

# Loading full 2014 to 2022 dataset in convLSTM format
load('/home/njjsye001/ConvLSTM_data_format/predictor_variables_2014_2018_train.RData')
load('/home/njjsye001/ConvLSTM_data_format/response_variable_2014_2018_train.RData')

load('/home/njjsye001/ConvLSTM_data_format/predictor_variables_2019_2020_val.RData')
load('/home/njjsye001/ConvLSTM_data_format/response_variable_2019_2020_val.RData')

load('/home/njjsye001/ConvLSTM_data_format/predictor_variables_2021_2022_test.RData')
load('/home/njjsye001/ConvLSTM_data_format/response_variable_2021_2022_test.RData')


# training set
# removing NBR after applying VIF
predictor_variables_2014_2018_train_modified <- predictor_variables_2014_2018_train[,,,,-4, drop = F] # index 4 imply removal of NBR as a predictor variable
# predictor_variables_2014_2018_train_modified <- predictor_variables_2014_2018_train[,37:60,,,-4, drop = F] # index 4 imply removal of NBR as a predictor variable
dim(predictor_variables_2014_2018_train_modified)

# validation set for predictor variable only as the timesteps were not disturbed
predictor_variables_2019_2020_val_modified <- predictor_variables_2019_2020_val[,,,,-4, drop = F]
dim(predictor_variables_2019_2020_val_modified)

# subset test set for predictor variable only as the timesteps were not disturbed
predictor_variables_2021_2022_test_modified <- predictor_variables_2021_2022_test[,,,,-4, drop = F]
dim(predictor_variables_2021_2022_test_modified)

#...Note: All response variable data remain unchanged

# Convert training fire stack into a dataframe to find out the ratio of class imbalance for fire to non-fire events
fire_df <- as.data.frame(FIRE_2014_2018_stack_norm_train, xy = T) %>% # converting stacked fire into dataframe
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) %>% 
  na.omit() %>% # I don't think this is necessary in this case as all NAs were replaced by 0- but you never know!
  mutate(Year = str_extract(Fire, "\\d{4}")|>as.integer(), # extract year from date
         Month = str_extract(Fire, "(?<=\\d{4}\\.)\\d{2}")|>as.integer()) %>% # extract month from date
  dplyr::select(c('x', 'y', 'Year', 'Month', 'Fire_Value'))  # select relevant columns only

# CONVOLUTION LSTM FULL FRAMEWORK -----------------------------------------

# reading data for modelling process
trainX <- predictor_variables_2014_2018_train_modified
dim(trainX) # (samples, time_steps, height, width, variables)- channels_last format
trainY <- response_variable_2014_2018_train
# trainY <- response_variable_2014_2018_train[,37:60,,,, drop = F]
dim(trainY) # (samples, time_steps, height, width, variables)- channels_last format

# fire_class_imbalance <- table(response_variable_2014_2018_train) # fire class imbalance (global imbalance)
# fire_class_imbalance_prop <- prop.table(table(response_variable_2014_2018_train)) # fire class imbalance proportion
# calculated_class_weights <- max(fire_class_imbalance)/fire_class_imbalance # global class weights if we are using class_weight in the algorithm

# Sample weight preparation
spatial_temporal_weight <- function(year, temporal_weight){sapply(1:12, function(x){ # spatial weight is systematically calculated already in this function
  
  fire_count_df <- fire_df %>%
    group_by(Year,Month,Fire_Value) %>%
    tally() # count the number of fire or no fire pixels per month per year
  
  fire_seasons <- c(1,2,3,4,10,11,12) # adding more weights temporally for months which had fire consistently over the years
  
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

valX <- predictor_variables_2019_2020_val_modified
dim(valX) # (samples, time_steps, height, width, variables)- channels_last format
valY <- response_variable_2019_2020_val
dim(valY) # (samples, time_steps, height, width, variables)- channels_last format

testX <- predictor_variables_2021_2022_test_modified
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
# thresholds <- 0.5
thresholds <- seq(0.4,0.7, by = .01) # threshold list
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 1))
for(t in thresholds){
  cat("Training for threshold: ", t, "\n")
  
  tensorflow::set_random_seed(1)
  ConvLSTM_model <- ConvLSTM_framework(t = t)
  # ConvLSTM_model%>%summary()
  
  model_path <- paste0("/home/njjsye001/Models/ConvLSTM_best_model_per_threshold/model_threshold_", sprintf("%.2f", t), ".h5")
  
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
  # py_help(tf$keras$Model$fit) # this helps to find out the arguments in the fit function
  tensorflow::set_random_seed(1)
  history <- ConvLSTM_model %>% fit(
    trainX, trainY,
    validation_data = list(valX, valY),
    use_multiprocessing = T,
    workers = n_cores,
    epochs = 300, # 300
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

save(main_training_results, file = '/home/njjsye001/main_training_results_2014_2022.Rdata')
