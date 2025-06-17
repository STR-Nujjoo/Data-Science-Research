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
  library(rgeoda)
  library(mltools)
  library(ROCR)
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
# NBR_rasters_after_interpolation[[1]]
# ?raster::values
# 
# stack(NBR_rasters_after_interpolation[[1]], 
#       x, 
#       NBR_rasters_after_interpolation[[100]])
# 
# x <- resample(WorldClim_temperature_raster_list[[1]], 
#               NBR_rasters_after_interpolation[[1]], method = 'ngb')
# 
# names(WorldClim_temperature_raster_list[[1]])
# 
# names(NBR_rasters_after_interpolation[[1]])


# CONVOLUTION LSTM FRAMEWORK ----------------------------------------------

# Define spatial extent and resolution
extent <- c(0, 32, 0, 32)  # xmin, xmax, ymin, ymax
resolution <- 1

# Create a template raster for the region
template_raster <- rast(extent = extent, resolution = resolution, crs = "EPSG:4326")

# Define dates
dates <- seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "month")

# Define cells to mask (e.g., cell indices 50 and 100 will be NA)
na_cells <- c(1:32, seq(1,32*32, by = 32), seq(32,32*32, by = 32), 50:55, 95:100)  # choose any valid cell numbers here



# Generate precipitation rasters with NA in the same locations
set.seed(123)
precipitation_list <- pblapply(dates, function(date) {
  # Generate values
  vals <- runif(ncell(template_raster), 0, 300)
  
  # Apply NA mask
  vals[na_cells] <- NA
  
  # Create raster with values and convert to raster package format
  rast(template_raster, vals = vals, names = paste0("Precipitation_", date)) |> raster()
})
# plot(precipitation_list[[1]], main = 'Precipitation data example', col = rainbow(100))


# Generate temperature rasters with NA in the same locations
set.seed(123)
temperature_list <- pblapply(dates, function(date) {
  # Generate values
  vals <- runif(ncell(template_raster), -10, 35)
  
  # Apply NA mask
  vals[na_cells] <- NA
  
  # Create raster with values and convert to raster package format
  rast(template_raster, vals = vals, names = paste0("Temperature_", date)) |> raster()
})

# plot(temperature_list[[1]], main = 'Temperature data example', col = rainbow(100))

# Generate fire rasters with NA in the same locations
set.seed(123)
fire_list <- pblapply(dates, function(date) {
  # Generate values
  vals <- sample(c(0, 1), ncell(template_raster), replace = TRUE, prob = c(0.95, 0.05))
  
  # Apply NA mask
  vals[na_cells] <- NA
  
  # Create raster with values and convert to raster package format
  rast(template_raster, vals = vals, names = paste0("Fire_", date)) |> raster()
})

# plot(fire_list[[1]], main = 'Fire data example', col = fire_color_condition_func(fire_list[[1]]))

# Min-Max noralisation function to be applied on the raster values; return: rasterLayer object 
raster_stack_minmax_norm <- function(stack_raster, index) {
  
  data <- stack_raster # raster stack
  min_val <-  min(minValue(data)) # global minimum of raster stack
  max_val <- max(maxValue(data)) # global maximum of raster stack
  index <- index # raster index
  val <- data[[index]] # relevant raster only
  
  x <- (val - min_val) / (max_val - min_val) # normalisation calculation
  x[is.na(values(x))] <- 0 # convert all NA values after normalisation to 0
  return(x)
}

# Stack rasters for each predictor variable
precipitation_stack <- stack(precipitation_list)
temperature_stack <- stack(temperature_list)

# Stack rasters for response variable
fire_stack <- stack(fire_list)

# normalising training set
precipitation_stack_train <- pblapply(1:4, function(x){precipitation_stack@layers[[x]]})|> stack()
precipitation_stack_train_norm <- pblapply(1:4, function(x){raster_stack_minmax_norm(precipitation_stack_train,x)})|>stack()

temperature_stack_train <-  pblapply(1:4, function(x){temperature_stack@layers[[x]]})|> stack()
temperature_stack_train_norm <- pblapply(1:4, function(x){raster_stack_minmax_norm(temperature_stack_train,x)})|>stack()

fire_stack_train <- pblapply(1:4, function(x){fire_stack@layers[[x]]})|> stack()
fire_stack_train_norm <- pblapply(1:4, function(x){raster_stack_minmax_norm(fire_stack_train,x)})|> stack() # normalisation is unnecessary because values ranges from 0 to 1 already!

# Convert fire stack into a dataframe to find out the ratio of class imbalance for fire to non-fire events
fire_df <- as.data.frame(fire_stack_train_norm, xy = T) %>% 
  # convert dataframe into long format where there is only one NDVI column
  pivot_longer(
    cols = starts_with("Fire"),
    names_to = "Fire",
    values_to = "Fire_Value"
  ) 
prop.table(table(fire_df$Fire_Value))
max(table(fire_df$Fire_Value))/table(fire_df$Fire_Value) # weight class

# normalising validation set
precipitation_stack_val <- precipitation_stack@layers[[5]]
precipitation_stack_val_norm <- raster_stack_minmax_norm(precipitation_stack_val, 1)

temperature_stack_val <- temperature_stack@layers[[5]]
temperature_stack_val_norm <- raster_stack_minmax_norm(temperature_stack_val, 1)

fire_stack_val <- fire_stack@layers[[5]]
fire_stack_val_norm <-  raster_stack_minmax_norm(fire_stack_val,1) # normalisation is unnecessary because values ranges from 0 to 1 already!

# normalising test set
precipitation_stack_test <- precipitation_stack@layers[[6]]
precipitation_stack_test_norm <- raster_stack_minmax_norm(precipitation_stack_test, 1)

temperature_stack_test <- temperature_stack@layers[[6]]
temperature_stack_test_norm <- raster_stack_minmax_norm(temperature_stack_test, 1)

fire_stack_test <- fire_stack@layers[[6]]
fire_stack_test_norm <-  raster_stack_minmax_norm(fire_stack_test, 1) # normalisation is unnecessary because values ranges from 0 to 1 already!


# Training, validation and test set
# 1st 4 months training and then 5th month validation and then 6th month testing
# Reshape to fit for convolution lstm model
trainX <- abind(precipitation_stack_train_norm|>as.array(),
                temperature_stack_train_norm|>as.array(),
                along = 4) |> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(trainX)
trainX <- array(trainX, dim = c(1, dim(trainX))) # Adjust dimension to include sample dimension to be 1
str(trainX)

trainY <- abind(fire_stack_train_norm|>as.array(), 
                along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(trainY)
trainY <- array(trainY, dim = c(1, dim(trainY))) # Adjust dimension to include sample dimension to be 1
dim(trainY)
# trainY <- to_categorical(trainY)

valX <- abind(precipitation_stack_val_norm|>as.array(),
              temperature_stack_val_norm|>as.array(),
              along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(valX)
valX <- array(valX, dim = c(1, dim(valX))) # Adjust dimension to include sample dimension to be 1
dim(valX)

valY <- abind(fire_stack_val_norm|>as.array(), 
              along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(valY)
valY <- array(valY, dim = c(1, dim(valY))) # Adjust dimension to include sample dimension to be 1
dim(valY)
# valY <- to_categorical(valY)

testX <- abind(precipitation_stack_test_norm|>as.array(),
               temperature_stack_test_norm|>as.array(),
               along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(testX)
testX <- array(testX, dim = c(1, dim(testX))) # Adjust dimension to include sample dimension to be 1
dim(testX)

testY <- abind(fire_stack_test_norm|>as.array(), 
               along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(testY)
testY <- array(testY, dim = c(1, dim(testY))) # Adjust dimension to include sample dimension to be 1
dim(testY)
# testY <- to_categorical(testY)


# # Reshape for ConvLSTM format 
# combined_array <- abind(precipitation_stack|> as.array(),
#                         temperature_stack|> as.array(),
#                         along = 4) # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
# 
# dim(combined_array)
# # CHECK
# # when combining the array the 1st array is the precipitation at the 1st time step
# all(combined_array[,,1,1]|>raster()|>values() == precipitation_list[[1]]|>values()) 
# # when combining the array the 2nd array is the precipitation at the 2nd time step
# all(combined_array[,,2,1]|>raster()|>values() == precipitation_list[[2]]|>values()) 
# #....etc....
# # when combining the array the 6th array is the precipitation at the 6th time step
# all(combined_array[,,6,1]|>raster()|>values() == precipitation_list[[6]]|>values()) 
# 
# 
# # when combining the array the 1st array is the temperature at the 1st time step
# all(combined_array[,,1,2]|>raster()|>values() == temperature_list[[1]]|>values()) 
# # when combining the array the 2nd array is the temperature at the 2nd time step
# all(combined_array[,,2,2]|>raster()|>values() == temperature_list[[2]]|>values()) 
# #....etc....
# # when combining the array the 6th array is the temperature at the 6th time step
# all(combined_array[,,6,2]|>raster()|>values() == temperature_list[[6]]|>values()) 
# #######

# # Function that apply min/max normalisation
# minmax_normalisation_function <- function(predictor_variables) {
#   # variable shape: [height, width, time_steps, channels]
#   for (ch in 1:dim(predictor_variables)[4]) {
#     channel_data <- predictor_variables[,,,ch]
#     min_val <- min(channel_data)
#     max_val <- max(channel_data)
#     predictor_variables[,,,ch] <- (channel_data - min_val) / (max_val - min_val)
#   }
#   return(predictor_variables)
# }

# # Normalised dataset
# combined_array_norm <- minmax_normalisation_function(predictor_variables = combined_array)
# combined_array_norm[,,,, drop = F] |> dim()

# fire_array <- abind(as.array(fire_stack), along = 4) # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)

# Training, validation and test set
# 1st 4 months training and then 5th month validation and then 6th month testing

# trainX <- combined_array_norm[,,1:4,, drop = F] |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# dim(trainX)
# trainX <- array(trainX, dim = c(1, dim(trainX))) # Adjust dimension to include sample dimension to be 1
# dim(trainX)
# 
# trainY <- abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# # trainY <- to_categorical(abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,4,1,2)), num_classes = 2)
# 
# dim(trainY)
# trainY <- array(trainY, dim = c(1, dim(trainY))) # Adjust dimension to include sample dimension to be 1
# dim(trainY)
# 
# valX <- combined_array_norm[,,5,, drop = F] |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# dim(valX)
# valX <- array(valX, dim = c(1, dim(valX))) # Adjust dimension to include sample dimension to be 1
# dim(valX)
# 
# valY <- abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# # valY <- to_categorical(abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
# dim(valY)
# valY <- array(valY, dim = c(1, dim(valY))) # Adjust dimension to include sample dimension to be 1
# dim(valY)
# 
# testX <- combined_array_norm[,,6,, drop = F] |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# dim(testX)
# testX <- array(testX, dim = c(1, dim(testX))) # Adjust dimension to include sample dimension to be 1
# dim(testX)
# 
# testY <- abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# # testY <- to_categorical(abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
# dim(testY)
# testY <- array(testY, dim = c(1, dim(testY))) # Adjust dimension to include sample dimension to be 1
# dim(testY)

# Defining a custom loss function that ignores -999
# This custom loss should mask out the pixels with the value -999 in y_true during training.
# masked_binary_crossentropy <- function(mask_value) {
#   function(y_true, y_pred) {
#     mask <- k_cast(k_not_equal(y_true, mask_value), k_floatx())  # mask is 0 where value == -999
#     loss <- k_binary_crossentropy(y_true, y_pred)  # compute standard BCE
#     masked_loss <- loss * mask  # zero out masked values
#     return(k_sum(masked_loss) / (k_sum(mask) + k_epsilon()))  # normalize by unmasked count
#   }
# }
masked_binary_crossentropy_with_class_weights <- function(mask_value, class_weights = c('0' = NULL, '1' = NULL)) {
  function(y_true, y_pred) {
    # Create mask to exclude 9999
    mask <- k_cast(k_not_equal(y_true, mask_value), k_floatx())
    
    # Apply class weights: if y_true == 1 → weight = class_weights["1"], else → weight = class_weights["0"]
    weight_1 <- class_weights[["1"]]
    weight_0 <- class_weights[["0"]]
    weights <- k_cast(k_equal(y_true, 1), k_floatx()) * weight_1 + k_cast(k_equal(y_true, 0), k_floatx()) * weight_0
    
    # Compute binary crossentropy
    loss <- k_binary_crossentropy(y_true, y_pred)
    
    # Apply both mask and weights
    weighted_loss <- loss * weights * mask
    
    # Return mean loss over valid pixels
    return(k_sum(weighted_loss) / (k_sum(weights * mask) + k_epsilon()))
  }
}


# masked_accuracy <- function(mask_value) {
#   function(y_true, y_pred) {
#     mask <- k_cast(k_not_equal(y_true, mask_value), k_floatx())
#     y_pred_binary <- k_cast(k_greater(y_pred, 0.5), k_floatx())  # binarize predictions
#     correct_preds <- k_cast(k_equal(y_true, y_pred_binary), k_floatx())
#     masked_acc <- correct_preds * mask
#     return(k_sum(masked_acc) / (k_sum(mask) + k_epsilon()))
#   }
# }

masked_weighted_accuracy <- function(mask_value, class_weights = c('0' = NULL, '1' = NULL)) {
  function(y_true, y_pred) {
    # Create mask for valid (non-masked) entries
    mask <- k_cast(k_not_equal(y_true, mask_value), k_floatx())
    
    # Binarize predictions at 0.5 threshold
    y_pred_binary <- k_cast(k_greater(y_pred, 0.5), k_floatx())
    
    # Compute whether predictions are correct
    correct_preds <- k_cast(k_equal(y_true, y_pred_binary), k_floatx())
    
    # Assign class weights to each prediction based on y_true
    weights <- (k_cast(k_equal(y_true, 0), k_floatx()) * class_weights["0"] +
                  k_cast(k_equal(y_true, 1), k_floatx()) * class_weights["1"])
    
    # Apply mask to correct predictions and weights
    weighted_correct <- correct_preds * weights * mask
    masked_weights <- weights * mask
    
    # Compute weighted accuracy: sum(weighted correct preds) / sum(weights)
    return(k_sum(weighted_correct) / (k_sum(masked_weights) + k_epsilon()))
  }
}




# NOTE: Avoid max pooling and layer flattening for our purpose

# Building a convolution lstm following this literature: Deep Learning Methods for Daily Wildfire Danger Forecasting
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  # 1st ConvLSTM layer
  layer_conv_lstm_2d(
    input_shape = list(NULL, 32, 32, 2), # samples = 1, time_steps=NULL to allow for varying timesteps months, channels = 2 predictor variables, rows = 32, cols = 32
                     filters = 64, 
                     kernel_size = c(3, 3), 
                     data_format = 'channels_last',
                     kernel_regularizer = regularizer_l2(0.001), # applies L2 regularisation to the kernel weights
                     recurrent_regularizer = regularizer_l2(0.001), # applies it to recurrent weights (inside the LSTM)
                     bias_regularizer = regularizer_l2(0.001), # applies it to biases
                     # recurrent_activation='hard_sigmoid',
                     activation = "relu",
                     padding = "same", 
                     return_sequences = T, # It is important for this to be TRUE so that the time steps are also returned
                     ) %>%
  
  # Normalize the activations of the previous layer (commonly used!)- 1st batch normalisation
  layer_batch_normalization() %>%
  
  # dropout
  layer_dropout(rate = 0.2) %>%
  
  # 1st ConvLSTM layer
  layer_conv_lstm_2d(
    input_shape = list(NULL, 32, 32, 2), # samples = 1, time_steps=NULL to allow for varying timesteps months, channels = 2 predictor variables, rows = 32, cols = 32
    filters = 64, 
    kernel_size = c(3, 3), 
    data_format = 'channels_last',
    # kernel_regularizer = regularizer_l2(0.001), # applies L2 regularisation to the kernel weights
    # recurrent_regularizer = regularizer_l2(0.001), # applies it to recurrent weights (inside the LSTM)
    # bias_regularizer = regularizer_l2(0.001), # applies it to biases
    # recurrent_activation='hard_sigmoid',
    activation = "relu",
    padding = "same", 
    return_sequences = T, # It is important for this to be TRUE so that the time steps are also returned
  ) %>%
  
  # dropout
  layer_dropout(rate = 0.2) %>%
  
  # flattening
  # time_distributed(layer_flatten()) %>%

  # # Dense layers
  time_distributed(layer_dense(units = 50, activation = "relu")) %>%
  
  # dropout
  layer_dropout(rate = 0.5) %>%
  
  # time_distributed(layer_dense(units = 8, activation = "relu")) %>%
 
  #  # dropout
  # layer_dropout(rate = 0.5) %>%
  # 
  # # Output layer
  time_distributed(layer_dense(units = 1, activation = "sigmoid"))

# Compile the model
tensorflow::set_random_seed(1)
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001, weight_decay = 0.03),
  loss = "binary_crossentropy",
  # loss = masked_binary_crossentropy_with_class_weights(mask_value = 9999, class_weights = c('0' = 1, '1' = 3.9)),
  metrics = c("accuracy")
  # metrics = masked_weighted_accuracy(mask_value = 9999, class_weights = c("0" = 1, "1" = 3.9))
)

model%>%summary()
tensorflow::set_random_seed(1)
history <- model %>% fit(
  trainX, trainY,
  validation_data = list(valX, valY),
  use_multiprocessing = T,
  callbacks = callback_tensorboard(),
  epochs = 200,
  batch_size = 1,
  class_weight = list('0' = 1, '1' = 20.8), # this takes care of class imbalance
  shuffle = F # very important to ensure temporal continuity/consistency
)

# ?fit.keras.engine.training.Model
plot(history)
evaluation <- model %>% evaluate(testX, testY)
cat("Test Loss:", evaluation[['loss']], "\nTest Accuracy:", evaluation[['accuracy']], "\n")
# cat("Test Loss:", evaluation[['loss']], "\nTest Accuracy:", evaluation[['python_function']], "\n")


# fire predicted for month 6
predicted <- model %>% predict(testX)
dim(predicted)
summary(predicted)
predicted_normal_Raster_format <- predicted[1, 1, , , 1]
# predicted_normal_Raster_format[which(testY[1,1,,,1] == 9999)] <- NA # convert the masked values back to NA


predicted_raster <- rast(predicted_normal_Raster_format, crs = "EPSG:4326", ext = ext(extent))
dim(predicted_normal_Raster_format)
hist(predicted_normal_Raster_format|>as.numeric()|>na.omit())

# An MCC value of +1 indicates perfect agreement between the model's predictions and the actual labels, 
# while -1 indicates total disagreement.
# A value of 0 suggests the model performs no better than random guessing
threshold <- seq(minValue(raster(predicted_raster)), maxValue(raster(predicted_raster)), by = 0.00001) # generate a sequence of threshold to classify response variable based on probability class
y_true <- testY[1, 1, , , 1] 
# y_true[which(testY[1, 1, , , 1]==9999)] <- NA
F1_SCORES <- pbsapply(seq_along(threshold), function (x){
  pred_class <- ifelse(predicted_normal_Raster_format > threshold[x], 1, 0)
  return(confusionMatrix(as.factor(as.vector(pred_class)), as.factor(as.vector(y_true)), positive = '1', mode = 'everything')$byClass['F1'][[1]])})# apply threshold on positive class; 1 in this case

optimal_threshold <- threshold[which.max(F1_SCORES)]
optimal_F1_score <- F1_SCORES[which.max(F1_SCORES)]
{
  par(mar = c(4.1, 4, .2, .8)) # customised margin
  plot(threshold, F1_SCORES,
       type = 'l',
       # pch = 19,
       # main = 'Chosen Threshold from Optimal RF Model',
       # cex.main = .9,
       cex.lab = .8,
       cex.axis = .8,
       # cex = .3,
       col = 'seagreen',
       xlab = paste0('Threshold'),
       ylab = 'F1 Score',
       # xlim = c(min(threshold), 0.01)
  )
  points(optimal_threshold, 
         optimal_F1_score, 
         pch = 19, cex = .1, col = 'seagreen')
  points(optimal_threshold, 
         optimal_F1_score, 
         pch = 19, cex = .5, col = 'greenyellow')
  abline(v=optimal_threshold,
         h=optimal_F1_score,
         lty = "dashed",
         col= 'greenyellow')
  text(optimal_threshold+.0002, 
       optimal_F1_score-.3, 
       labels=paste("Threshold = ", optimal_threshold|>round(3)),
       cex=.6,
       col="seagreen",
       srt=270)
  text(optimal_threshold-.002, 
       optimal_F1_score-.02, 
       labels=paste("F1 Score = ", optimal_F1_score|>round(3)),
       cex=.6,
       col="seagreen")
}


y_pred <- ifelse(predicted_normal_Raster_format>optimal_threshold, 1,0)
y_pred_raster <- rast(y_pred, crs = "EPSG:4326", ext = ext(extent))

CM <- confusionMatrix(as.factor(as.vector(y_pred)),as.factor(as.vector(y_true)), positive = '1', mode = 'everything')
ROCR_test_prediction <- prediction(as.vector(y_pred), as.vector(y_true))
test_AUC_ROC <- performance(ROCR_test_prediction, measure = 'auc')@y.values[[1]] # AUC_ROC
test_AUC_PR <- performance(ROCR_test_prediction, measure = 'aucpr')@y.values[[1]] # AUC_PR
test_MCC <- mcc(as.factor(as.vector(y_pred)),as.factor(as.vector(y_true))) # test accuracy using matthew's correlation coefficient

test_accuracy <- cbind(optimal_threshold = optimal_threshold,
                       precision = CM$byClass['Precision'][[1]]|>round(3),
                       recall = CM$byClass['Recall'][[1]]|>round(3),
                       F1_score = optimal_F1_score|>round(3), 
                       AUC_ROC = test_AUC_ROC|>round(3),
                       AUC_PR = test_AUC_PR|>round(3),
                       MCC = test_MCC|>round(3)) |> as_tibble()



# creating a function for visualisation
WS_visualisation <- function(raster_with_probabilities, raster_factor, classes_breaks_method = c('natural_breaks', 'quantile')){
  
  # Subdivision types
  quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
  natural_breaks_subdivisions <- natural_breaks(k = 5, df=as.data.frame(raster_with_probabilities))
  
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
    classified_raster <- classify(raster_with_probabilities, wildfire_susceptibility_natural_breaks_classes)
    levels(classified_raster) <- data.frame(
      ID = 1:5,
      Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
    )
    
    # Update levels
    classified_raster <- droplevels(classified_raster)
    
  } else if(classes_breaks_method == 'quantile'){
    
    # Reclassify raster accordingly
    classified_raster <- classify(raster_with_probabilities, wildfire_susceptibility_quantile_classes)
    levels(classified_raster) <- data.frame(
      ID = 1:5,
      Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
    )
    
    # Update levels
    classified_raster <- droplevels(classified_raster)
  }
  
  
  # define a color palette for the wildfire susceptibility class
  WS_palette <- c('#007206', '#7DB810', '#F2FE1E', '#FFAC12','#FC3B09')
  
  # print(
    # Visualising the fire data used as testY
    p1 <- tm_shape(fire_stack[[6]]|> rast())+
      tm_raster(style = "cat", title = "", palette = c('white','#FC3B09'))+
      tm_layout(main.title= 'True Fire map',
                main.title.size =.9,
                main.title.position = c("center", "top"),
                legend.outside = T,
                legend.text.size = .5,
                legend.outside.position = 'bottom')+
      tm_graticules(lines = F)
  # )
    p2 <- tm_shape(raster_factor)+
      tm_raster(style = "cat", title = "", palette = c('white','#FC3B09'))+
      tm_layout(main.title= 'Predicted Fire Map',
                main.title.size =.9,
                main.title.position = c("center", "top"),
                legend.outside = T,
                legend.text.size = .5,
                legend.outside.position = 'bottom')+
      tm_graticules(lines = F)
  
  # print(
    # Visualise the classified raster
    p3 <- tm_shape(classified_raster)+
      tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(classified_raster)[[1]]$ID)])+
      tm_layout(main.title= 'Wildfire Susceptibility Map',
                main.title.size =.9,
                main.title.position = c("center", "top"),
                legend.outside = T,
                legend.text.size = .5,
                legend.outside.position = 'bottom')+
      tm_graticules(lines = F)
  # )
   return(tmap_arrange(p1,p2,p3, nrow = 2, ncol = 2)) 
}

WS_visualisation(raster_with_probabilities = predicted_raster, raster_factor = y_pred_raster, classes_breaks_method = 'natural_breaks')
WS_visualisation(raster_with_probabilities = predicted_raster, raster_factor = y_pred_raster, classes_breaks_method = 'quantile')


###############################CHATGPT ConvolutionLSTM architecture##########################

# Define spatial dimensions and channel/timestep counts
img_rows <- 32
img_cols <- 32
predictor_channels <- 2  # precipitation and temperature
fire_channels <- 1       # binary fire maps

# Time dimensions:
# For training, we use the first 4 months.
# For validation, month 5.
# For testing, month 6.
time_train <- NULL # allowing for variable time steps
time_val <- 1
time_test <- 1

# Define input shape for training (without samples dimension)
# For training, time dimension is 4, channels are 2.
input_shape <- list(time_train, img_rows, img_cols, predictor_channels)

# Build a convolutional LSTM model.
# Here, the model is set up to process the sequence of predictors and output a sequence.
# If you prefer to predict only the last timestep, you can adjust by setting return_sequences = FALSE
# and adapting y_train to have a single timestep.
model <- keras_model_sequential() %>%
  layer_conv_lstm_2d(
    filters = 40,
    kernel_size = c(3,3),
    activation = 'relu',
    input_shape = input_shape,
    padding = "same",
    return_sequences = TRUE,  # outputs a sequence (4 timesteps) for training
    data_format = "channels_last"
  ) %>%
  layer_batch_normalization() %>%
  # Use a Conv2D layer applied at each timestep via time_distributed wrapper.
  time_distributed(layer_conv_2d(
    filters = fire_channels,
    kernel_size = c(3,3),
    activation = "sigmoid",
    padding = "same",
    data_format = "channels_last"
  ))

# Compile the model. We use binary_crossentropy since fire is binary.
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.001, weight_decay = 0.03),
  metrics = c("accuracy")
)

# View the model summary
summary(model)

# Train the model.
# Here, the model is trained on a sequence-to-sequence task (months 1-4 predictors to months 1-4 fire maps).
# If your goal is to predict only a future timestep (e.g. month 6) based on a historical sequence,
# you could modify the architecture to output only the last timestep by setting return_sequences = FALSE.
tensorflow::set_random_seed(1)
history <- model %>% fit(
  x = trainX,
  y = trainY,
  batch_size = 1,
  epochs = 200,
  shuffle = F,
  validation_data = list(valX, valY)
) # very bad model

# After training, you can evaluate the model on the test set.
model %>% evaluate(testX, testY)

# fire predicted for month 6
predicted <- model %>% predict(testX)
dim(predicted)
range(predicted)
predicted_normal_Raster_format <- predicted[1, 1, , , 1]
predicted_raster <- rast(predicted_normal_Raster_format, crs = "EPSG:4326", ext = ext(extent))

# An MCC value of +1 indicates perfect agreement between the model's predictions and the actual labels, 
# while -1 indicates total disagreement.
# A value of 0 suggests the model performs no better than random guessing
y_true <- testY[1, 1, , , 1]
y_pred <- ifelse(predicted[1, 1, , , 1]>0.5, 1,0)
mcc(preds = y_pred, actuals = y_true) # test accuracy using matthew's correlation coefficient

# Subdivision types
quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
natural_breaks_subdivisions <- natural_breaks(k = 5, df=as.data.frame(predicted_raster))

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

WS_visualisation(raster = predicted_raster, 'natural_breaks')
WS_visualisation(raster = predicted_raster, 'quantile')

############################https://medium.com/neuronio/an-introduction-to-convlstm-55c9025563a7###################

# This ConvLSTM is similarly adopted from an online source by Alexandre Xavier
time_train <- NULL
pixels_x <- 32
pixels_y <- 32
channels <- 2

tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  
  # First ConvLSTM block
  layer_conv_lstm_2d(filters = 20, 
                     kernel_size = c(3, 3),
                     input_shape = list(time_train, pixels_x, pixels_y, channels),
                     data_format = 'channels_last',
                     recurrent_activation = 'hard_sigmoid',
                     activation = 'tanh',
                     padding = 'same',
                     return_sequences = TRUE) %>%
  layer_batch_normalization() %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_max_pooling_3d(pool_size = c(1, 2, 2),
  #                      padding = "same",
  #                      data_format = 'channels_last') %>%
  
  # Second ConvLSTM block
  layer_conv_lstm_2d(filters = 10,
                     kernel_size = c(3, 3),
                     data_format = 'channels_last',
                     activation = "tanh",
                     padding = 'same',
                     return_sequences = TRUE) %>%
  layer_batch_normalization() %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_max_pooling_3d(pool_size = c(1, 3, 3),
  #                      padding = "same",
  #                      data_format = 'channels_last') %>%
  
  # Branch (flatten + dense)
  layer_conv_lstm_2d(filters = 5,
                     kernel_size = c(3, 3),
                     activation = "relu",
                     kernel_initializer = "random_uniform",
                     data_format = 'channels_last',
                     padding = 'same',
                     return_sequences = TRUE) %>%
  # layer_max_pooling_3d(pool_size = c(1, 2, 2),
  #                      padding = "same",
  #                      data_format = 'channels_last') %>%
  # time_distributed(layer_flatten()) %>%
  time_distributed(layer_dense(units = 512, activation = 'relu')) %>%
  time_distributed(layer_dense(units = 32, activation = 'relu')) %>%
  time_distributed(layer_dense(units = 1, activation = 'sigmoid')) 

# time_distributed(
#   layer_conv_2d(filters = 1, kernel_size = c(1,1), activation = 'sigmoid', padding = 'same')
# ) # binary output

# # Define custom MCC metric
# mcc_metric <- custom_metric("matthews_correlation", function(y_true, y_pred) {
#   y_pred_pos <- k_round(k_clip(y_pred, 0, 1))
#   y_pred_neg <- 1 - y_pred_pos
#   
#   y_pos <- k_round(k_clip(y_true, 0, 1))
#   y_neg <- 1 - y_pos
#   
#   tp <- k_sum(y_pos * y_pred_pos)
#   tn <- k_sum(y_neg * y_pred_neg)
#   fp <- k_sum(y_neg * y_pred_pos)
#   fn <- k_sum(y_pos * y_pred_neg)
#   
#   numerator <- tp * tn - fp * fn
#   denominator <- k_sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
#   
#   return(k_switch(k_equal(denominator, 0), 0, numerator / (denominator + k_epsilon())))
# })

# # Compile with Matthew's Correlation Coefficient
# model %>% compile(
#   loss = 'binary_crossentropy',
#   optimizer = optimizer_adam(learning_rate = 0.001, weight_decay = 0.03),
#   metrics = c('accuracy', mcc_metric)
# )

# Compile
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.001, weight_decay = 0.03),
  metrics = c('accuracy')
)

model %>% summary()

tensorflow::set_random_seed(1)
history <- model %>% fit(
  x = trainX,
  y = trainY,
  validation_data = list(valX, valY),
  epochs = 100,
  batch_size = 16,
  shuffle = FALSE
)

# After training, you can evaluate the model on the test set.
model %>% evaluate(testX, testY)

# fire predicted for month 6
predicted <- model %>% predict(testX)
dim(predicted)
range(predicted)
predicted_normal_Raster_format <- predicted[1, 1, , , 1]
predicted_raster <- rast(predicted_normal_Raster_format, crs = "EPSG:4326", ext = ext(extent))

# An MCC value of +1 indicates perfect agreement between the model's predictions and the actual labels, 
# while -1 indicates total disagreement.
# A value of 0 suggests the model performs no better than random guessing
y_true <- testY[1, 1, , , 1]
y_pred <- ifelse(predicted[1, 1, , , 1]>0.5, 1,0)
mcc(preds = y_pred, actuals = y_true) # test accuracy using matthew's correlation coefficient

# Subdivision types
quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
natural_breaks_subdivisions <- natural_breaks(k = 5, df=as.data.frame(predicted_raster))

# Susceptibility quantile classes- makes more sense
wildfire_susceptibility_quantile_classes <- matrix(c(
  0, quantile_subdivisions[2], 1, # very low
  quantile_subdivisions[2], quantile_subdivisions[3], 2, # low
  quantile_subdivisions[3], quantile_subdivisions[4], 3, # moderate
  quantile_subdivisions[4], quantile_subdivisions[5], 4, # high
  quantile_subdivisions[5], 1, 5 # very high
), ncol = 3, byrow = TRUE)

# Susceptibility natural breaks classes
wildfire_susceptibility_natural_breaks_classes <- matrix(c(
  0, natural_breaks_subdivisions[1], 1, # very low
  natural_breaks_subdivisions[1], natural_breaks_subdivisions[2], 2, # low
  natural_breaks_subdivisions[2], natural_breaks_subdivisions[3], 3, # moderate
  natural_breaks_subdivisions[3], natural_breaks_subdivisions[4], 4, # high
  natural_breaks_subdivisions[4], 1, 5 # very high
), ncol = 3, byrow = TRUE)

WS_visualisation(raster = predicted_raster, 'natural_breaks')
WS_visualisation(raster = predicted_raster, 'quantile')


# Trying different models -------------------------------------------------
# Define spatial dimensions and channel/timestep counts
img_rows <- 32
img_cols <- 32
predictor_channels <- 2  # precipitation and temperature
fire_channels <- 1       # binary fire maps

# Time dimensions:
# For training, we use the first 4 months.
# For validation, month 5.
# For testing, month 6.
time_train <- NULL # allowing for variable time steps
time_val <- 1
time_test <- 1

# Define input shape for training (without samples dimension)
# For training, time dimension is 4, channels are 2.
input_shape <- list(time_train, img_rows, img_cols, predictor_channels)

tensorflow::set_random_seed(1)
# Trying 1 layer model
model <- keras_model_sequential() %>%
  layer_conv_lstm_2d(
    filters = 256,
    kernel_size = c(5, 5),
    input_shape = input_shape,
    padding = "same",
    return_sequences = TRUE,
    activation = "tanh"
  ) %>%
  layer_conv_3d(
    filters = 1,
    kernel_size = c(1, 1, 1),
    activation = "sigmoid",
    padding = "same"
  )

# Compile
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.001, weight_decay = 0.03),
  metrics = c('accuracy')
)

model %>% summary()

tensorflow::set_random_seed(1)
history <- model %>% fit(
  x = trainX,
  y = trainY,
  validation_data = list(valX, valY),
  epochs = 50,
  batch_size = 16,
  shuffle = FALSE
)

# After training, you can evaluate the model on the test set.
model %>% evaluate(testX, testY)

# fire predicted for month 6
predicted <- model %>% predict(testX)
dim(predicted)
range(predicted)
predicted_normal_Raster_format <- predicted[1, 1, , , 1]
predicted_raster <- rast(predicted_normal_Raster_format, crs = "EPSG:4326", ext = ext(extent))

# An MCC value of +1 indicates perfect agreement between the model's predictions and the actual labels, 
# while -1 indicates total disagreement.
# A value of 0 suggests the model performs no better than random guessing
y_true <- testY[1, 1, , , 1]
y_pred <- ifelse(predicted[1, 1, , , 1]>0.5, 1,0)
mcc(preds = y_pred, actuals = y_true) # test accuracy using matthew's correlation coefficient

# Subdivision types
quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
natural_breaks_subdivisions <- natural_breaks(k = 5, df=as.data.frame(predicted_raster))

# Susceptibility quantile classes- makes more sense
wildfire_susceptibility_quantile_classes <- matrix(c(
  0, quantile_subdivisions[2], 1, # very low
  quantile_subdivisions[2], quantile_subdivisions[3], 2, # low
  quantile_subdivisions[3], quantile_subdivisions[4], 3, # moderate
  quantile_subdivisions[4], quantile_subdivisions[5], 4, # high
  quantile_subdivisions[5], 1, 5 # very high
), ncol = 3, byrow = TRUE)

# Susceptibility natural breaks classes
wildfire_susceptibility_natural_breaks_classes <- matrix(c(
  0, natural_breaks_subdivisions[1], 1, # very low
  natural_breaks_subdivisions[1], natural_breaks_subdivisions[2], 2, # low
  natural_breaks_subdivisions[2], natural_breaks_subdivisions[3], 3, # moderate
  natural_breaks_subdivisions[3], natural_breaks_subdivisions[4], 4, # high
  natural_breaks_subdivisions[4], 1, 5 # very high
), ncol = 3, byrow = TRUE)

WS_visualisation(raster = predicted_raster, 'natural_breaks')
WS_visualisation(raster = predicted_raster, 'quantile')

##
# Trying 2 layers model
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  layer_conv_lstm_2d(
    filters = 128,
    kernel_size = c(5, 5),
    input_shape = input_shape,
    padding = "same",
    return_sequences = TRUE,
    activation = "tanh"
  ) %>%
  layer_conv_lstm_2d(
    filters = 128,
    kernel_size = c(5, 5),
    padding = "same",
    return_sequences = TRUE,
    activation = "tanh"
  ) %>%
  layer_conv_3d(
    filters = 1,
    kernel_size = c(1, 1, 1),
    activation = "sigmoid",
    padding = "same"
  )

# Compile
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.001, weight_decay = 0.03),
  metrics = c('accuracy')
)

model %>% summary()

tensorflow::set_random_seed(1)
history <- model %>% fit(
  x = trainX,
  y = trainY,
  validation_data = list(valX, valY),
  epochs = 50,
  batch_size = 16,
  shuffle = FALSE
)

# After training, you can evaluate the model on the test set.
model %>% evaluate(testX, testY)

# fire predicted for month 6
predicted <- model %>% predict(testX)
dim(predicted)
range(predicted)
predicted_normal_Raster_format <- predicted[1, 1, , , 1]
predicted_raster <- rast(predicted_normal_Raster_format, crs = "EPSG:4326", ext = ext(extent))

# An MCC value of +1 indicates perfect agreement between the model's predictions and the actual labels, 
# while -1 indicates total disagreement.
# A value of 0 suggests the model performs no better than random guessing
y_true <- testY[1, 1, , , 1]
y_pred <- ifelse(predicted[1, 1, , , 1]>0.5, 1,0)
mcc(preds = y_pred, actuals = y_true) # test accuracy using matthew's correlation coefficient

# Subdivision types
quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
natural_breaks_subdivisions <- natural_breaks(k = 5, df=as.data.frame(predicted_raster))

# Susceptibility quantile classes- makes more sense
wildfire_susceptibility_quantile_classes <- matrix(c(
  0, quantile_subdivisions[2], 1, # very low
  quantile_subdivisions[2], quantile_subdivisions[3], 2, # low
  quantile_subdivisions[3], quantile_subdivisions[4], 3, # moderate
  quantile_subdivisions[4], quantile_subdivisions[5], 4, # high
  quantile_subdivisions[5], 1, 5 # very high
), ncol = 3, byrow = TRUE)

# Susceptibility natural breaks classes
wildfire_susceptibility_natural_breaks_classes <- matrix(c(
  0, natural_breaks_subdivisions[1], 1, # very low
  natural_breaks_subdivisions[1], natural_breaks_subdivisions[2], 2, # low
  natural_breaks_subdivisions[2], natural_breaks_subdivisions[3], 3, # moderate
  natural_breaks_subdivisions[3], natural_breaks_subdivisions[4], 4, # high
  natural_breaks_subdivisions[4], 1, 5 # very high
), ncol = 3, byrow = TRUE)

WS_visualisation(raster = predicted_raster, 'natural_breaks')
WS_visualisation(raster = predicted_raster, 'quantile')

###
# Trying 3 layers model
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  layer_conv_lstm_2d(
    filters = 128,
    kernel_size = c(5, 5),
    input_shape = input_shape,
    padding = "same",
    return_sequences = TRUE,
    activation = "tanh"
  ) %>%
  layer_conv_lstm_2d(
    filters = 64,
    kernel_size = c(5, 5),
    padding = "same",
    return_sequences = TRUE,
    activation = "tanh"
  ) %>%
  layer_conv_lstm_2d(
    filters = 64,
    kernel_size = c(5, 5),
    padding = "same",
    return_sequences = TRUE,
    activation = "tanh"
  ) %>%

  # check the difference between the 2 last layers
  # layer_conv_2d(filters = 1, 
  #               kernel_size = c(1,1),
  #               activation = "sigmoid")

  layer_conv_3d(
    filters = 1,
    kernel_size = c(1, 1, 1),
    activation = "sigmoid",
    padding = "same"
  )

# Compile
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.001, weight_decay = 0.03),
  metrics = c('accuracy')
)

model %>% summary()

tensorflow::set_random_seed(1)
history <- model %>% fit(
  x = trainX,
  y = trainY,
  validation_data = list(valX, valY),
  epochs = 50,
  batch_size = 16,
  shuffle = FALSE
)

# After training, you can evaluate the model on the test set.
model %>% evaluate(testX, testY)

# fire predicted for month 6
predicted <- model %>% predict(testX)
dim(predicted)
range(predicted)
predicted_normal_Raster_format <- predicted[1, 1, , , 1]
predicted_raster <- rast(predicted_normal_Raster_format, crs = "EPSG:4326", ext = ext(extent))

# An MCC value of +1 indicates perfect agreement between the model's predictions and the actual labels, 
# while -1 indicates total disagreement.
# A value of 0 suggests the model performs no better than random guessing
y_true <- testY[1, 1, , , 1]
y_pred <- ifelse(predicted[1, 1, , , 1]>0.5, 1,0)
mcc(preds = y_pred, actuals = y_true) # test accuracy using matthew's correlation coefficient

# Subdivision types
quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
natural_breaks_subdivisions <- natural_breaks(k = 5, df=as.data.frame(predicted_raster))

# Susceptibility quantile classes- makes more sense
wildfire_susceptibility_quantile_classes <- matrix(c(
  0, quantile_subdivisions[2], 1, # very low
  quantile_subdivisions[2], quantile_subdivisions[3], 2, # low
  quantile_subdivisions[3], quantile_subdivisions[4], 3, # moderate
  quantile_subdivisions[4], quantile_subdivisions[5], 4, # high
  quantile_subdivisions[5], 1, 5 # very high
), ncol = 3, byrow = TRUE)

# Susceptibility natural breaks classes
wildfire_susceptibility_natural_breaks_classes <- matrix(c(
  0, natural_breaks_subdivisions[1], 1, # very low
  natural_breaks_subdivisions[1], natural_breaks_subdivisions[2], 2, # low
  natural_breaks_subdivisions[2], natural_breaks_subdivisions[3], 3, # moderate
  natural_breaks_subdivisions[3], natural_breaks_subdivisions[4], 4, # high
  natural_breaks_subdivisions[4], 1, 5 # very high
), ncol = 3, byrow = TRUE)

WS_visualisation(raster = predicted_raster, 'natural_breaks')
WS_visualisation(raster = predicted_raster, 'quantile')




# Must loop model to predict on final input in the model to be able to perform an autoregressive forecasting

last_known_data <- testX

# # Assuming `model` is your trained Convolutional LSTM model
# # `last_known_data` is the last known input array, shape: (1, height, width, channels)
forecast_steps <- 4  # Example: forecast for 'n' months
predicted_array <- array(NA, dim = c(1, forecast_steps, 32, 32, 1))
dim(predicted_array)

# Start with the last known data
current_input <- last_known_data
dim(current_input)
tensorflow::set_random_seed(1)
for (t in 1:forecast_steps) {
  # Predict the next step
  next_output <- predict(model, current_input)
  dim(next_output)
  crange(next_output)

  # Store the output
  predicted_array[1,t,,,] <- next_output

  # Use the output as input for the next step
  current_input <- next_output
}


