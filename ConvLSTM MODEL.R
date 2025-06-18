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
  library(parallel)
}

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)


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



# subset predictor variables data to test convLSTM
# subset training set
# extracting only 2017 and 2018 rasters timesteps and selecting only NDVI, NDMI, ATP, AMT and ANSWS 
predictor_variables_2014_2018_train_subset <- predictor_variables_2014_2018_train[,37:60,,,c(2,3,5,6,7), drop = F]

dim(predictor_variables_2014_2018_train_subset)

# subset response variables data to test convLSTM
response_variable_2014_2018_train_subset <- response_variable_2014_2018_train[,37:60,,,, drop = F]
dim(response_variable_2014_2018_train)

# subset validation set for predictor variable only as the timesteps were not disturbed
predictor_variables_2019_2020_val_subset <- predictor_variables_2019_2020_val[,,,,c(2,3,5,6,7), drop = F]

# subset test set for predictor variable only as the timesteps were not disturbed
predictor_variables_2021_2022_test_subset <- predictor_variables_2021_2022_test[,,,,c(2,3,5,6,7), drop = F]
dim(predictor_variables_2021_2022_test_subset)


# reading data for modelling
trainX <- predictor_variables_2014_2018_train_subset
dim(trainX) # (samples, time_steps, height, width, variables)- channels_last format
trainY <- response_variable_2014_2018_train_subset
dim(trainY) # (samples, time_steps, height, width, variables)- channels_last format

fire_class_imbalance_subset <- table(response_variable_2014_2018_train_subset) # fire class imbalance
fire_class_imbalance_prop_subset <- prop.table(table(response_variable_2014_2018_train_subset)) # fire class imbalance proportion
calculated_class_weights_subset <- max(fire_class_imbalance_subset)/fire_class_imbalance_subset # class weights to be applied to convLSTM

valX <- predictor_variables_2019_2020_val_subset
dim(valX) # (samples, time_steps, height, width, variables)- channels_last format
valY <- response_variable_2019_2020_val
dim(valY) # (samples, time_steps, height, width, variables)- channels_last format

testX <- predictor_variables_2021_2022_test_subset
dim(testX) # (samples, time_steps, height, width, variables)- channels_last format
testY <- response_variable_2021_2022_test
dim(testY) # (samples, time_steps, height, width, variables)- channels_last format

# NOTE: Avoid max pooling and layer flattening for our purpose
# Building a convolution lstm for wildfire susceptibility
tensorflow::set_random_seed(1)
model <- keras_model_sequential() %>%
  # 1st ConvLSTM layer
  layer_conv_lstm_2d(
    input_shape = list(NULL, 372, 382, 5), # samples = 1, time_steps=NULL to allow for varying timesteps months, channels = 2 predictor variables, rows = 32, cols = 32
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
  metrics = c("accuracy")
)

model%>%summary()

tensorflow::set_random_seed(1)
history <- model %>% fit(
  trainX, trainY,
  validation_data = list(valX, valY),
  use_multiprocessing = T,
  # callbacks = callback_tensorboard(),
  epochs = 10,
  batch_size = 10,
  class_weight = list('0' = 1, '1' = round(calculated_class_weights_subset[2][[1]],1)), # this takes care of class imbalance
  shuffle = F # very important to ensure temporal continuity/consistency
)

# ?fit.keras.engine.training.Model
plot(history)
evaluation <- model %>% evaluate(testX, testY)
cat("Test Loss:", evaluation[['loss']], "\nTest Accuracy:", evaluation[['accuracy']], "\n")
# cat("Test Loss:", evaluation[['loss']], "\nTest Accuracy:", evaluation[['python_function']], "\n")


# fire predicted for 2021 and 2022 - This is where all the probabilities are stored
predicted <- model %>% predict(testX)
dim(predicted)
summary(predicted)

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

length(predicted_raster_list)
for (i in 1:4){
  cat('Iteration ', i, ' out of ', length(predicted_raster_list), '\n')
  i <- i
  index <- i
  threshold <- seq(minValue(predicted_raster_list[[index]]), maxValue(predicted_raster_list[[index]]), by = 0.00001) # generate a sequence of threshold to classify response variable based on probability class
  
  # To allow parallel processing in pbapply functions export items used in the function to the cluster
  clusterExport(cl, varlist = c('predicted_raster_list', 'true_test_raster_list', 'threshold', 'index')) 
  
  # This function output the f1 score (if both classes exist) or specificity (if only the negative class exists) for each threshold generated for each test raster
  F1_SCORES_SPECIFICITY <- pbsapply(seq_along(threshold), function (x){
    pred_class <- ifelse(as.vector(predicted_raster_list[[index]]) > threshold[x], 1, 0)
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
  
  CM <- confusionMatrix(factor(as.vector(y_pred_raster), levels = c('0','1')),
                        factor(as.vector(true_test_raster_list[[index]]), levels = c('0','1')), 
                        positive = '1', mode = 'everything')
  
  if(sum(CM$table[,2])==0){
    test_AUC_ROC <- NA
    test_AUC_PR <- NA
    test_MCC <- NA
  }else{
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
                                       overall_accuracy = CM$overall['Accuracy'][[1]]|>round(3),
                                       precision = CM$byClass['Precision'][[1]]|>round(3),
                                       recall = CM$byClass['Recall'][[1]]|>round(3),
                                       specificity = CM$byClass['Specificity'][[1]]|>round(3),
                                       F1_score = optimal_F1_score|>round(3), 
                                       AUC_ROC = test_AUC_ROC|>round(3),
                                       AUC_PR = test_AUC_PR|>round(3),
                                       MCC = test_MCC|>round(3),
                                       fire_period = timesteps_labels[i],
                                       true_fire_status = ifelse(maxValue(true_test_raster_list[[index]])==1, 
                                                                 'positive',
                                                                 'negative')))
  
}


# Creating a function to plot the optimal threshold chosen while maximising either f1 score or specificity where appropriate
optmised_threshold_plot <- function(fire_period){
  index <- which(timesteps_labels==as.character(fire_period))
  if(ConvLSTM_test_results$true_fire_status[index]=='positive'){ # if there's indeed a fire outbreak, then the threshold was optimised based on the f1 score...
    {
      par(mar = c(4.1, 4, .2, .8)) # customised margin
      plot(ConvLSTM_threshold_list[[index]], ConvLSTM_f1_score_list[[index]],
           type = 'l',
           # pch = 19,
           # main = 'Chosen Threshold from Optimal RF Model',
           # cex.main = .9,
           cex.lab = .8,
           cex.axis = .8,
           # cex = .3,
           col = 'seagreen',
           xlab = 'Threshold',
           ylab = 'F1 Score',
           # xlim = c(min(threshold), 0.01)
      )
      points(ConvLSTM_test_results$optimal_threshold[index],
             ConvLSTM_test_results$F1_score[index],
             pch = 19, cex = .1, col = 'seagreen')
      points(ConvLSTM_test_results$optimal_threshold[index],
             ConvLSTM_test_results$F1_score[index],
             pch = 19, cex = .5, col = 'greenyellow')
      abline(v=ConvLSTM_test_results$optimal_threshold[index],
             h=ConvLSTM_test_results$F1_score[index],
             lty = "dashed",
             col= 'greenyellow')
      text(ConvLSTM_test_results$optimal_threshold[index]+.0004,
           ConvLSTM_test_results$F1_score[index]-.09,
           labels=paste("Threshold = ", ConvLSTM_test_results$optimal_threshold[index]|>round(3)),
           cex=.6,
           col="seagreen",
           srt=270)
      text(ConvLSTM_test_results$optimal_threshold[index]-.002,
           ConvLSTM_test_results$F1_score[index]-.01,
           labels=paste("F1 Score = ", ConvLSTM_test_results$F1_score[index]|>round(3)),
           cex=.6,
           col="seagreen")
    }
    
  }else{ #...otherwise threshold was optimised on specificity
    {
      par(mar = c(4.1, 4, .2, .8)) # customised margin
      plot(ConvLSTM_threshold_list[[index]], ConvLSTM_specificity_list[[index]],
           type = 'l',
           # pch = 19,
           # main = 'Chosen Threshold from Optimal RF Model',
           # cex.main = .9,
           cex.lab = .8,
           cex.axis = .8,
           # cex = .3,
           col = 'seagreen',
           xlab = 'Threshold',
           ylab = 'Specificity',
           # xlim = c(min(threshold), 0.01)
      )
      points(ConvLSTM_test_results$optimal_threshold[index],
             ConvLSTM_test_results$specificity[index],
             pch = 19, cex = .1, col = 'seagreen')
      points(ConvLSTM_test_results$optimal_threshold[index],
             ConvLSTM_test_results$specificity[index],
             pch = 19, cex = .5, col = 'greenyellow')
      abline(v=ConvLSTM_test_results$optimal_threshold[index],
             h=ConvLSTM_test_results$specificity[index],
             lty = "dashed",
             col= 'greenyellow')
      text(ConvLSTM_test_results$optimal_threshold[index]+.0004,
           ConvLSTM_test_results$specificity[index]-.09,
           labels=paste("Threshold = ", ConvLSTM_test_results$optimal_threshold[index]|>round(3)),
           cex=.6,
           col="seagreen",
           srt=270)
      text(ConvLSTM_test_results$optimal_threshold[index]-.002,
           ConvLSTM_test_results$specificity[index]-.01,
           labels=paste("Specificity = ", ConvLSTM_test_results$specificity[index]|>round(3)),
           cex=.6,
           col="seagreen")
    }
  }
}


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
  
  # Update levels of rasters
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
  # Visualise the classified raster
  p3 <- tm_shape(classified_raster)+
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
  return(tmap_arrange(p1,p2,p3, nrow = 2, ncol = 2)) 
}


call_fire_period <- 'Fire 2021-01'

optmised_threshold_plot(fire_period = call_fire_period)

WS_visualisation(true_raster = true_test_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_with_probabilities = predicted_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_factor = y_pred_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 classes_breaks_method = 'natural_breaks')

WS_visualisation(true_raster = true_test_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_with_probabilities = predicted_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_factor = y_pred_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 classes_breaks_method = 'quantile')










