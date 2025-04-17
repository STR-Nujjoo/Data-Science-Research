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

# Generate precipitation and temperature rasters for each month
set.seed(123)
precipitation_list <- lapply(dates, function(date) {
  rast(template_raster, vals = runif(ncell(template_raster), 0, 300), names = paste0("Precipitation_", date)) |> raster()
})
plot(precipitation_list[[1]], main = 'Precipitation data example')

temperature_list <- lapply(dates, function(date) {
  rast(template_raster, vals = runif(ncell(template_raster), -10, 35), names = paste0("Temperature_", date)) |> raster()
})

plot(temperature_list[[1]], main = 'Temperature data example')

fire_list <- lapply(dates, function(date) {
  rast(template_raster, vals = sample(c(0, 1), ncell(template_raster), replace = TRUE, prob = c(0.8, 0.2)), names = paste0("Fire_", date))|> raster()
})

plot(fire_list[[1]], main = 'Fire data example')

# Stack rasters for each variable
precipitation_stack <- stack(precipitation_list)
temperature_stack <- stack(temperature_list)

# Reshape for ConvLSTM format 
combined_array <- abind(precipitation_stack|> as.array(),
                        temperature_stack|> as.array(),
                        along = 4) # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)

dim(combined_array)
# CHECK
# when combining the array the 1st array is the precipitation at the 1st time step
all(combined_array[,,1,1]|>raster()|>values() == precipitation_list[[1]]|>values()) 
# when combining the array the 2nd array is the precipitation at the 2nd time step
all(combined_array[,,2,1]|>raster()|>values() == precipitation_list[[2]]|>values()) 
#....etc....
# when combining the array the 6th array is the precipitation at the 6th time step
all(combined_array[,,6,1]|>raster()|>values() == precipitation_list[[6]]|>values()) 


# when combining the array the 1st array is the temperature at the 1st time step
all(combined_array[,,1,2]|>raster()|>values() == temperature_list[[1]]|>values()) 
# when combining the array the 2nd array is the temperature at the 2nd time step
all(combined_array[,,2,2]|>raster()|>values() == temperature_list[[2]]|>values()) 
#....etc....
# when combining the array the 6th array is the temperature at the 6th time step
all(combined_array[,,6,2]|>raster()|>values() == temperature_list[[6]]|>values()) 
#######

# Function that apply min/max normalisation
minmax_normalisation_function <- function(predictor_variables) {
  # variable shape: [height, width, time_steps, channels]
  for (ch in 1:dim(predictor_variables)[4]) {
    channel_data <- predictor_variables[,,,ch]
    min_val <- min(channel_data)
    max_val <- max(channel_data)
    predictor_variables[,,,ch] <- (channel_data - min_val) / (max_val - min_val)
  }
  return(predictor_variables)
}

# Normalised dataset
combined_array_norm <- minmax_normalisation_function(predictor_variables = combined_array)
combined_array_norm[,,,, drop = F] |> dim()

# response variable
fire_stack <- stack(fire_list)
fire_array <- abind(as.array(fire_stack), along = 4) # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)

# Training, validation and test set
# 1st 4 months training and then 5th month validation and then 6th month testing

trainX <- combined_array_norm[,,1:4,, drop = F] |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(trainX)
trainX <- array(trainX, dim = c(1, dim(trainX))) # Adjust dimension to include sample dimension to be 1
dim(trainX)

trainY <- abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# trainY <- to_categorical(abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,4,1,2)), num_classes = 2)

dim(trainY)
trainY <- array(trainY, dim = c(1, dim(trainY))) # Adjust dimension to include sample dimension to be 1
dim(trainY)

valX <- combined_array_norm[,,5,, drop = F] |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(valX)
valX <- array(valX, dim = c(1, dim(valX))) # Adjust dimension to include sample dimension to be 1
dim(valX)

valY <- abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# valY <- to_categorical(abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
dim(valY)
valY <- array(valY, dim = c(1, dim(valY))) # Adjust dimension to include sample dimension to be 1
dim(valY)

testX <- combined_array_norm[,,6,, drop = F] |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(testX)
testX <- array(testX, dim = c(1, dim(testX))) # Adjust dimension to include sample dimension to be 1
dim(testX)

testY <- abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# testY <- to_categorical(abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
dim(testY)
testY <- array(testY, dim = c(1, dim(testY))) # Adjust dimension to include sample dimension to be 1
dim(testY)

# NOTE: Avoid max pooling and layer flattening for our purpose

# Building a convolution lstm following this literature: Deep Learning Methods for Daily Wildfire Danger Forecasting
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  
  # 1st ConvLSTM layer
  layer_conv_lstm_2d(input_shape = list(NULL, 32, 32, 2), # samples = 1, time_steps=NULL to allow for varying timesteps months, channels = 2 predictor variables, rows = 32, cols = 32
                     filters = 16, 
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

  # # Dense layers
  time_distributed(layer_dense(units = 16, activation = "relu")) %>%
  
  # dropout
  layer_dropout(rate = 0.5) %>%
  
  time_distributed(layer_dense(units = 8, activation = "relu")) %>%
 
   # dropout
  layer_dropout(rate = 0.5) %>%
  
  # # Output layer
  time_distributed(layer_dense(units = 1, activation = "sigmoid"))

# Compile the model
tensorflow::set_random_seed(1)
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001, weight_decay = 0.03),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model%>%summary()
tensorflow::set_random_seed(1)
history <- model %>% fit(
  trainX, trainY,
  validation_data = list(valX, valY),
  epochs = 200,
  batch_size = 128,
  shuffle = F # very important to ensure temporal continuity/consistency
)

# ?fit.keras.engine.training.Model

evaluation <- model %>% evaluate(testX, testY)
cat("Test Loss:", evaluation[['loss']], "\nTest Accuracy:", evaluation[['accuracy']], "\n")

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

# creating a function for visualisation
WS_visualisation <- function(raster, classes_breaks_method = c('natural_breaks', 'quantile')){
  if(classes_breaks_method=='natural_breaks'){
    # Reclassify raster accordingly
    classified_raster <- classify(raster, wildfire_susceptibility_natural_breaks_classes)
    levels(classified_raster) <- data.frame(
      ID = 1:5,
      Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
    )
    
    # Update levels
    classified_raster <- droplevels(classified_raster)
    
  } else if(classes_breaks_method == 'quantile'){
    
    # Reclassify raster accordingly
    classified_raster <- classify(raster, wildfire_susceptibility_quantile_classes)
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
      tm_layout(main.title= 'Fire map',
                main.title.size =.9,
                main.title.position = c("center", "top"),
                legend.outside = T,
                legend.text.size = .5,
                legend.outside.position = 'bottom')+
      tm_graticules(lines = F)
  # )
  
  
  # print(
    # Visualise the classified raster
    p2 <- tm_shape(classified_raster)+
      tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(classified_raster)[[1]]$ID)])+
      tm_layout(main.title= 'Wildfire Susceptibility Map',
                main.title.size =.9,
                main.title.position = c("center", "top"),
                legend.outside = T,
                legend.text.size = .5,
                legend.outside.position = 'bottom')+
      tm_graticules(lines = F)
  # )
   return(tmap_arrange(p1,p2, nrow = 1, ncol = 2)) 
}

WS_visualisation(raster = predicted_raster, classes_breaks_method = 'natural_breaks')
WS_visualisation(raster = predicted_raster, classes_breaks_method = 'quantile')


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
  batch_size = 16,
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


# # Assuming `model` is your trained Convolutional LSTM model
# # `last_known_data` is the last known input array, shape: (1, height, width, channels)
# forecast_steps <- 12  # Example: forecast for 12 months
# predicted_array <- array(NA, dim = c(forecast_steps, nrow(last_known_data), ncol(last_known_data), nlayers(last_known_data)))
# 
# # Start with the last known data
# current_input <- array(last_known_data, dim = c(1, nrow(last_known_data), ncol(last_known_data), nlayers(last_known_data)))
# 
# for (t in 1:forecast_steps) {
#   # Predict the next step
#   next_output <- predict(model, current_input)
#   
#   # Store the output
#   predicted_array[t,,,] <- next_output
#   
#   # Use the output as input for the next step
#   current_input <- array(next_output, dim = c(1, nrow(last_known_data), ncol(last_known_data), nlayers(last_known_data)))
# }
