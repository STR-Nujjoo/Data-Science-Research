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
                along = 4) |> aperm(c(3,4,1,2))  # Reorder shape: (time_steps, variables, height, width)

# trainX <- array(trainX, dim = c(32, dim(trainX)))
# dim(trainX)

# trainY <- to_categorical(abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,1,2,4)), num_classes = 2)
trainY <- abind(as.array(fire_stack[[1:4]]), along = 4) |> aperm(c(3,4,1,2))  # Reorder shape: (time_steps, variables, height, width)
# trainY <- array(trainY, dim = c(32, dim(trainY)))
# dim(trainY)

valX <- abind(precipitation_stack[[5]] |> as.array(),
              temperature_stack[[5]] |> as.array(),
              along = 4) |> aperm(c(3,4,1,2))  # Reorder shape: (time_steps, variables, height, width)
# valX <- array(valX, dim = c(32, dim(valX)))
# dim(valX)

valY <- abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,4,1,2))  # Reorder shape: (time_steps, variables, height, width)
valY <- to_categorical(abind(as.array(fire_stack[[5]]), along = 4) |> aperm(c(3,4,1,2)), num_classes = 2)

# valY <- array(valY, dim = c(32, dim(valY)))
# dim(valY)

testX <- abind(precipitation_stack[[6]] |> as.array(),
               temperature_stack[[6]] |> as.array(),
               along = 4) |> aperm(c(3,4,1,2))  # Reorder shape: (time_steps, variables, height, width)
# testX <- array(testX, dim = c(32, dim(testX)))
# dim(testX)

testY <- abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,4,1,2))  # Reorder shape: (time_steps, variables, height, width)
testY <- to_categorical(abind(as.array(fire_stack[[6]]), along = 4) |> aperm(c(3,4,1,2)), num_classes = 2)
dim(testY)
# testY <- array(testY, dim = c(32, dim(testY)))
# dim(testY)

# ?compile.keras.engine.training.Model 

# time_steps <- 3
# variables <- 2
# Building a convolution lstm following this literature: Deep Learning Methods for Daily Wildfire Danger Forecasting
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  
  # 1st ConvLSTM layer
  layer_conv_lstm_2d(input_shape = c(4, 2, 32, 32), # samples = everything?, time_steps=4 months, channels = 2 predictor variables, rows = 32, cols = 32
                     filters = 16, 
                     kernel_size = c(3, 3), 
                     data_format = 'channels_first',
                     recurrent_activation='hard_sigmoid',
                     activation = "tanh",
                     padding = "same", 
                     return_sequences = T, # It is important for this to be TRUE so that the time steps are also returned
                     ) %>%
  
  # Normalize the activations of the previous layer (commonly used!)- 1st batch normalisation
  layer_batch_normalization() %>%
  
  # 1st max pooling
    layer_max_pooling_3d(pool_size = c(1, 2, 2),
                         padding = "same",
                         data_format = 'channels_first') %>%
  
  # 2nd ConvLSTM layer
  layer_conv_lstm_2d(
    filters = 8,
    kernel_size = c(3, 3),
    data_format = 'channels_first',
    padding = "same",
    return_sequences = T
  ) %>%
  
  # Normalize the activations of the previous layer (commonly used!)- 2nd batch normalisation
  layer_batch_normalization() %>%
  
  # 2nd max pooling
  layer_max_pooling_3d(pool_size = c(1, 3, 3),
                       padding = "same",
                       data_format = 'channels_first') %>%
  # 
  # Dropout
  # time_distributed(layer_dropout(rate = 0.3)) %>%
  
  layer_conv_lstm_2d(
    filters = 5,
    kernel_size = c(3, 3),
    data_format = 'channels_first',
    kernel_initializer = "random_uniform",
    padding = "same",
    return_sequences = T) %>% # Output directly as (time_steps, height, width, classes)

  # 3rd max pooling
  layer_max_pooling_3d(pool_size = c(1, 2, 2),
                       padding = "same",
                       data_format = 'channels_first') %>%
  
  # flattenning into 2D
  time_distributed(layer_flatten()) %>%
    
  # # Dense layers
  time_distributed(layer_dense(units = 16, activation = "relu")) %>%
  time_distributed(layer_dense(units = 8, activation = "relu")) %>%
  # # Output layer
  time_distributed(layer_dense(units = 1, activation = "softmax"))

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
  batch_size = 16,
  shuffle = F # very important to ensure temporal continuity/consistency
)

# ?fit.keras.engine.training.Model

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
