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

NBR_rasters_after_interpolation[[1]]
?raster::values

stack(NBR_rasters_after_interpolation[[1]], 
      x, 
      NBR_rasters_after_interpolation[[100]])

x <- resample(WorldClim_temperature_raster_list[[1]], 
              NBR_rasters_after_interpolation[[1]], method = 'ngb')

names(WorldClim_temperature_raster_list[[1]])

names(NBR_rasters_after_interpolation[[1]])




# CONVOLUTION LSTM FRAMEWORK ----------------------------------------------

# # Generate dummy data (100 samples, 10 time steps, 32x32 image size, 3 channels)
# set.seed(123)
# n_samples <- 100
# time_steps <- 10
# height <- 32
# width <- 32
# channels <- 3
# 
# # Predictor variables
# x_data <- array(runif(n_samples * time_steps * height * width * channels), 
#                 dim = c(n_samples, time_steps, height, width, channels))
# 
# # Binary response variable (0 or 1)
# set.seed(123)
# y_data <- to_categorical(sample(0:1, n_samples, replace = TRUE), 2)
# 
# # Split into training and testing sets
# set.seed(123)
# train_indices <- sample(1:n_samples, size = 0.8 * n_samples)
# x_train <- x_data[train_indices, , , , ]
# y_train <- y_data[train_indices, ]
# 
# x_test <- x_data[-train_indices, , , , ]
# y_test <- y_data[-train_indices, ]

library(terra)

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
  rast(template_raster, vals = runif(ncell(template_raster), 0, 300), names = paste0("Precipitation_", date))
})

temperature_list <- lapply(dates, function(date) {
  rast(template_raster, vals = runif(ncell(template_raster), -10, 35), names = paste0("Temperature_", date))
})

fire_list <- lapply(dates, function(date) {
  rast(template_raster, vals = sample(c(0, 1), ncell(template_raster), replace = TRUE, prob = c(0.8, 0.2)), names = paste0("Fire_", date))
})

# Stack rasters for each variable
precipitation_stack <- rast(precipitation_list)
temperature_stack <- rast(temperature_list)
fire_stack <- rast(fire_list)

# # Combine into a single SpatRaster
# combined_stack <- c(precipitation_stack, temperature_stack)
# 
# # Convert raster to array
# raster_array <- as.array(combined_stack)

fire_array <- abind(as.array(fire_stack), along = 4)|>aperm(c(3,1,2,4))

# Reshape for CNN-LSTM format (This is correct!)

combined_array <- abind(precipitation_stack|> as.array(),
                        temperature_stack|> as.array(),
                        along = 4) # Shape: (height, width, time_steps, variables)

# aperm(combined_array, c(3,1,2,4))

# Training, validation and test set
# 1st 4 months training and then 5th month validation and then 6th month testing

trainX <- abind(precipitation_stack[[1:4]] |> as.array(),
                temperature_stack[[1:4]] |> as.array(),
                along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)

trainX <- array(trainX, dim = c(32, dim(trainX)))
dim(trainX)

# trainY <- to_categorical(abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
trainY <- abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,1,2,4))
trainY <- array(trainY, dim = c(32, dim(trainY)))
dim(trainY)

valX <- abind(precipitation_stack[[5]] |> as.array(),
              temperature_stack[[5]] |> as.array(),
              along = 4) |> aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)
valX <- array(valX, dim = c(32, dim(valX)))
dim(valX)

# valY <- to_categorical(abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
valY <- abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,1,2,4))
valY <- array(valY, dim = c(32, dim(valY)))
dim(valY)

testX <- abind(precipitation_stack[[6]] |> as.array(),
               temperature_stack[[6]] |> as.array(),
               along = 4) |> aperm(c(3,1,2,4)) # Reorder shape: (time_steps, height, width, variables)
testX <- array(testX, dim = c(32, dim(testX)))
dim(testX)

# testY <- to_categorical(abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
testY <- abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,1,2,4))
testY <- array(testY, dim = c(32, dim(testY)))
dim(testY)

# ?compile.keras.engine.training.Model 

# time_steps <- 3
# variables <- 2
# Building a convolution lstm following this literature: Deep Learning Methods for Daily Wildfire Danger Forecasting
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  # ConvLSTM layer
  layer_conv_lstm_2d(input_shape = c(4, 32, 32, 2),
                     filters = 32, 
                     kernel_size = c(3, 3), 
                     padding = "same", 
                     return_sequences = T, 
                    # return_state = TRUE,
                     ) %>%
  layer_conv_lstm_2d(
    filters = 16,
    kernel_size = c(3, 3),
    padding = "same",
    return_sequences = T
  ) %>%
  # # Max pooling
  time_distributed(layer_max_pooling_2d(pool_size = c(2, 2)))%>%
  # Dropout
  time_distributed(layer_dropout(rate = 0.3)) %>%
  
  time_distributed(layer_conv_2d(
    filters = 2,
    kernel_size = c(1, 1),
    activation = "sigmoid",
    padding = "same"
  ))  # Output directly as (time_steps, height, width, classes)
  # # Dense layers
  # time_distributed(layer_dense(units = 16, activation = "linear")) %>%
  # time_distributed(layer_dense(units = 8, activation = "linear")) %>%
  # # Output layer
  # time_distributed(layer_dense(units = 2, activation = "softmax"))

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
  epochs = 10,
  batch_size = 32,
  shuffle = F # very important to ensure temporal continuity/consistency
)

evaluation <- model %>% evaluate(testX, testY)
cat("Test Loss:", evaluation[['loss']], "\nTest Accuracy:", evaluation[['accuracy']], "\n")

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

tensorflow::set_random_seed(1)
# Define the model
model <- keras_model_sequential() %>%
  # First ConvLSTM layer
  layer_conv_lstm_2d(input_shape = c(4, 32, 32, 2),   # 4 time steps, 32x32 spatial, 2 variables (precipitation, temperature)
                     filters = 64, 
                     kernel_size = c(3, 3), 
                     padding = "same", 
                     return_sequences = TRUE) %>%   # Retain sequence to process temporal data
  # Second ConvLSTM layer
  layer_conv_lstm_2d(filters = 32, 
                     kernel_size = c(3, 3), 
                     padding = "same", 
                     return_sequences = TRUE) %>%
  # TimeDistributed layer for pooling, dropout, flattening, and dense layers
  time_distributed(layer_max_pooling_2d(pool_size = c(2, 2))) %>%
  time_distributed(layer_dropout(rate = 0.3)) %>%
  time_distributed(layer_flatten()) %>%
  time_distributed(layer_dense(units = 128, activation = "relu")) %>%
  time_distributed(layer_dense(units = 64, activation = "relu")) %>%
  # Output layer (binary classification for each pixel: fire/no fire)
  time_distributed(layer_dense(units = 1, activation = "sigmoid"))  # Sigmoid for binary classification

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# Model Summary to check architecture
model %>% summary()


# Train the model
history <- model %>% fit(
  trainX, trainY, 
  validation_data = list(valX, valY),
  epochs = 10,  # Adjust epochs as needed
  batch_size = 16,
  shuffle = FALSE   # Important to maintain temporal consistency
)


