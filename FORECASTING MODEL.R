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

# response variable
fire_stack <- stack(fire_list)
fire_array <- abind(as.array(fire_stack), along = 4) # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)

# Training, validation and test set
# 1st 4 months training and then 5th month validation and then 6th month testing

trainX <- abind(precipitation_stack[[1:4]] |> as.array(),
                temperature_stack[[1:4]] |> as.array(),
                along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(trainX)
trainX <- array(trainX, dim = c(1, dim(trainX)))
dim(trainX)

trainY <- abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# trainY <- to_categorical(abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,4,1,2)), num_classes = 2)

dim(trainY)
trainY <- array(trainY, dim = c(1, dim(trainY)))
dim(trainY)

valX <- abind(precipitation_stack[[5]] |> as.array(),
              temperature_stack[[5]] |> as.array(),
              along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(valX)
valX <- array(valX, dim = c(1, dim(valX)))
dim(valX)

valY <- abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# valY <- to_categorical(abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
dim(valY)
valY <- array(valY, dim = c(1, dim(valY)))
dim(valY)

testX <- abind(precipitation_stack[[6]] |> as.array(),
               temperature_stack[[6]] |> as.array(),
               along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
dim(testX)
testX <- array(testX, dim = c(1, dim(testX)))
dim(testX)

testY <- abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format
# testY <- to_categorical(abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
dim(testY)
testY <- array(testY, dim = c(1, dim(testY)))
dim(testY)

# ?compile.keras.engine.training.Model 

# time_steps <- 3
# variables <- 2
# Building a convolution lstm following this literature: Deep Learning Methods for Daily Wildfire Danger Forecasting
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  
  # 1st ConvLSTM layer
  layer_conv_lstm_2d(input_shape = list(NULL, 32, 32, 2), # samples = everything?, time_steps=NULL to allow for varying timesteps months, channels = 2 predictor variables, rows = 32, cols = 32
                     filters = 16, 
                     kernel_size = c(3, 3), 
                     data_format = 'channels_last',
                     recurrent_activation='hard_sigmoid',
                     activation = "tanh",
                     padding = "same", 
                     return_sequences = T, # It is important for this to be TRUE so that the time steps are also returned
                     ) %>%
  
  # Normalize the activations of the previous layer (commonly used!)- 1st batch normalisation
  layer_batch_normalization() %>%
  
  # 2nd ConvLSTM layer
  layer_conv_lstm_2d(
    filters = 8,
    kernel_size = c(3, 3),
    data_format = 'channels_last',
    padding = "same",
    return_sequences = T
  ) %>%
  
  # Normalize the activations of the previous layer (commonly used!)- 2nd batch normalisation
  layer_batch_normalization() %>%
  # Dropout
  time_distributed(layer_dropout(rate = 0.2)) %>%
  
  layer_conv_lstm_2d(
    filters = 4,
    kernel_size = c(3, 3),
    data_format = 'channels_last',
    kernel_initializer = "random_uniform",
    padding = "same",
    return_sequences = T) %>% # Output directly as (time_steps, height, width, classes)
  
  # flattenning into 2D
  # time_distributed(layer_flatten()) %>%

  # # Dense layers
  time_distributed(layer_dense(units = 16, activation = "relu")) %>%
  time_distributed(layer_dense(units = 8, activation = "relu")) %>%
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
  epochs = 100,
  batch_size = 16,
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
predicted_thresholding <- ifelse(predicted_normal_Raster_format>0.5, 1, 0)
mean(testY[1,1,,,1] == predicted_thresholding)

predicted_raster <- rast(predicted_thresholding, crs = "EPSG:4326", ext = ext(extent)) |>raster()

plot(fire_stack[[6]], main = 'Fire 2023-06 simulated', col = c('white', 'red'))
plot(predicted_raster, main = 'Fire 2023-06 predicted', col = c('white', 'red'))




###############################CHATGPT PROMPTED EXPLANATION!##########################
library(keras)

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
  epochs = 100,
  shuffle = F,
  validation_data = list(valX, valY)
)

# After training, you can evaluate the model on the test set.
model %>% evaluate(testX, testY)

# fire predicted for month 6
predicted <- model %>% predict(testX)
dim(predicted)
range(predicted)
predicted_normal_Raster_format <- predicted[1, 1, , , 1]
predicted_thresholding <- ifelse(predicted_normal_Raster_format>0.6, 1, 0)
mean(testY[1,1,,,1] == predicted_thresholding)

predicted_raster <- rast(predicted_thresholding, crs = "EPSG:4326", ext = ext(extent)) |>raster()

plot(fire_stack[[6]], main = 'Fire 2023-06 simulated', col = c('white', 'red'))
plot(predicted_raster, main = 'Fire 2023-06 predicted', col = c('white', 'red'))




############################CHATGPT CONVLSTM ADOPTED VERSION 2###################
library(keras)
tensorflow::set_random_seed(1)

frames <- NULL
pixels_x <- 32
pixels_y <- 32
channels <- 2

model <- keras_model_sequential() %>%
  
  # First ConvLSTM block
  layer_conv_lstm_2d(filters = 64, 
                     kernel_size = c(3, 3),
                     input_shape = list(frames, pixels_x, pixels_y, channels),
                     data_format = 'channels_last',
                     recurrent_activation = 'hard_sigmoid',
                     activation = 'tanh',
                     padding = 'same',
                     return_sequences = TRUE) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>%
  # layer_max_pooling_3d(pool_size = c(1, 2, 2),
  #                      padding = "same",
  #                      data_format = 'channels_last') %>%
  
  # Second ConvLSTM block
  layer_conv_lstm_2d(filters = 32,
                     kernel_size = c(3, 3),
                     data_format = 'channels_last',
                     activation = "tanh",
                     padding = 'same',
                     return_sequences = TRUE) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>%
  # layer_max_pooling_3d(pool_size = c(1, 3, 3),
  #                      padding = "same",
  #                      data_format = 'channels_last') %>%
  
  # Branch (flatten + dense)
  layer_conv_lstm_2d(filters = 20,
                     kernel_size = c(3, 3),
                     activation = "relu",
                     # kernel_initializer = "random_uniform",
                     data_format = 'channels_last',
                     padding = 'same',
                     return_sequences = TRUE) %>%
  # layer_max_pooling_3d(pool_size = c(1, 2, 2),
  #                      padding = "same",
  #                      data_format = 'channels_last') %>%
  # time_distributed(layer_flatten()) %>%
  time_distributed(layer_dense(units = 50, activation = 'relu')) %>%
  time_distributed(layer_dense(units = 1, activation = 'sigmoid')) 
# time_distributed(
#   layer_conv_2d(filters = 1, kernel_size = c(1,1), activation = 'sigmoid', padding = 'same')
# ) # binary output

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
predicted_thresholding <- ifelse(predicted_normal_Raster_format>0.48, 1, 0)
mean(testY[1,1,,,1] == predicted_thresholding)

predicted_raster <- rast(predicted_thresholding, crs = "EPSG:4326", ext = ext(extent)) |>raster()

plot(fire_stack[[6]], main = 'Fire 2023-06 simulated', col = c('white', 'red'))
plot(predicted_raster, main = 'Fire 2023-06 predicted', col = c('white', 'red'))


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
