# Import relevant libraries
{
  library(reticulate)
  library(keras)
  library(tensorflow)
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
  library(rgeoda)
  library(parallel)
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


# tfa <- reticulate::import("tensorflow_addons", delay_load = TRUE)
# focal_loss <- tfa$losses$SigmoidFocalCrossEntropy
# focal_loss_fn <- function(alpha = NULL, gamma = NULL) {
#   loss_fn <- tfa$losses$SigmoidFocalCrossEntropy(alpha = alpha, gamma = gamma)
#   function(y_true, y_pred) {
#     loss_fn(y_true, y_pred)
#   }
# }

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

# Import fire data 
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/RAW/FIRE_DATA.Rdata', envir = .GlobalEnv)

FIRE_2002_2022 <- lapply(1:252, function(x) {FIRE_DATA[[x]]})
FIRE_2014_2022 <- lapply(145:252, function(x) {FIRE_2002_2022[[x]]})


load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2019_2020_val.RData')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2019_2020_val.RData')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2021_2022_test.RData')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2021_2022_test.RData')

predictor_variables_2019_2020_val_modified <- predictor_variables_2019_2020_val[,,,,-4, drop = F]
valX <- predictor_variables_2019_2020_val_modified
dim(valX) # (samples, time_steps, height, width, variables)- channels_last format
valY <- response_variable_2019_2020_val
dim(valY) # (samples, time_steps, height, width, variables)- channels_last format

predictor_variables_2021_2022_test_modified <- predictor_variables_2021_2022_test[,,,,-4, drop = F]
dim(predictor_variables_2021_2022_test_modified)
testX <- predictor_variables_2021_2022_test_modified
dim(testX) # (samples, time_steps, height, width, variables)- channels_last format
testY <- response_variable_2021_2022_test
dim(testY) # (samples, time_steps, height, width, variables)- channels_last format

load('Wildfire_Data_Stefan/main_training_results_2014_2022_2.Rdata')
# str(main_training_results)

# creating temporal slice
make_temporal_samples <- function(X, Y, seq_len, target = c("next", "all")) {
  target <- match.arg(target)
  n_time <- dim(X)[2]
  X_seq <- list(); Y_seq <- list()
  
  # dynamic end index
  t_end <- if (target == "next") n_time - 1 else n_time
  
  for (t in seq_len:t_end) {
    X_seq[[length(X_seq) + 1]] <- X[, (t - seq_len + 1):t, , , , drop = FALSE]
    
    if (target == "next") {
      Y_seq[[length(Y_seq) + 1]] <- Y[, t + 1, , , , drop = FALSE]
    } else {
      Y_seq[[length(Y_seq) + 1]] <- Y[, (t - seq_len + 1):t, , , , drop = FALSE]
    }
  }
  
  list(
    X = abind::abind(X_seq, along = 1),
    Y = abind::abind(Y_seq, along = 1)
  )
}

seq_len <- 2  # no. of consecutive months 

val_samples <- make_temporal_samples(valX, valY, seq_len, target = 'all')
updated_valX <- val_samples$X
dim(updated_valX)
updated_valY <- val_samples$Y
dim(updated_valY)

test_samples  <- make_temporal_samples(testX, testY, seq_len, target = 'all')
updated_testX <- test_samples$X
dim(updated_testX)
updated_testY <- test_samples$Y
dim(updated_testY)

# Extract all the best validation MCCs from the different thresholds
best_val_MCCs <- sapply(seq_along(main_training_results), function(x){main_training_results[[x]]$best_val_MCC})
# best_val_MCCs <- sapply(seq_along(main_training_results), function(x){main_training_results[[x]]$best_val_f1_score})


# Visualise the best validation MCCs of the best model for each threshold
thresholds <- seq(0.5,0.7, by = .05) # threshold list
# thresholds <- seq(0.55,0.65, by = .01) # threshold list

optimal_ConvLSTM_model_index <- which.max(best_val_MCCs)
optimal_ConvLSTM_threshold <- main_training_results[[optimal_ConvLSTM_model_index]]$threshold;optimal_ConvLSTM_threshold
# optimal_ConvLSTM_threshold <- 0.6
# main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics

{
  par(mar = c(4.1, 4, .2, 0.2)) # customised margin
  plot(x = thresholds, 
       y = best_val_MCCs, 
       type = 'b', 
       xlab = 'Threshold', ylab = 'Validation MCC Score', 
       cex.lab = .8,
       cex.axis = .8,
       col = 'seagreen')
  points(optimal_ConvLSTM_threshold,
         max(best_val_MCCs),
         pch = 19, cex = .75, col = 'seagreen')
  
}

options(scipen=999)

# visualise history plot of best model
main_training_results[[optimal_ConvLSTM_model_index]]$history |> plot()

# validation_metrics <- c(val_loss = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_loss[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_binary_accuracy = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_binary_accuracy[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_recall = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_recall[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_precision = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_precision[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_specificity = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_specificity[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_f1_score = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_f1_score[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_MCC = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_fn = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_fn[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_fp = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_fp[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_tn = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_tn[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)],
#                         val_tp = main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_tp[which.max(main_training_results[[optimal_ConvLSTM_model_index]]$history$metrics$val_MCC)])

# Creating a function to load each best model for each threshold
load_model_by_threshold <- function(file_path, t){
  tensorflow::set_random_seed(1)
  path <- file_path
  x <- load_model_hdf5(path, 
                       custom_objects = list(specificity = specificity_metric(threshold = t),
                                             f1_score = f1_score_metric(threshold = t),
                                             MCC = mcc_metric(threshold = t)
                                             # focal_loss_fn_alpha_0_9_gamma_2 = focal_loss_fn(alpha = 0.54, gamma = 2)
                       ),
                       compile = T)
}


# reading file names from folder if needed
MODELS_PATH <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Wildfire_Data_Stefan/Models2/')

# Loading the BEST model from the optimal threshold
optimal_ConvLSTM_model <- load_model_by_threshold(file_path = paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Wildfire_Data_Stefan/Models2/', MODELS_PATH[optimal_ConvLSTM_model_index]),
                                                  t = optimal_ConvLSTM_threshold) # extract the threshold as part of the name to ensure consistency


# function to deconstruct the 5D tensor with overlapping rasters
reconstruct_sequence_weighted <- function(predY, 
                                          seq_len, # number of timesteps per window
                                          total_time, # total months in full sequence
                                          weights = c() # for weighted average
) {
  stopifnot(length(weights) == seq_len) # weight assigned must strictly equal to sample sequence 
  weights <- weights / sum(weights)  # normalise weight to sum = 1
  
  n_samples <- dim(predY)[1]
  H <- dim(predY)[3]
  W <- dim(predY)[4]
  C <- dim(predY)[5]
  
  summed <- array(0, dim = c(total_time, H, W, C))
  weights_sum <- array(0, dim = c(total_time, H, W, C))
  
  for (i in 1:n_samples) {
    for (t in 1:seq_len) {
      time_index <- i + t - 1
      if (time_index <= total_time) {
        w <- weights[t]
        summed[time_index, , , ] <- summed[time_index, , , ] + w * predY[i, t, , , ]
        weights_sum[time_index, , , ] <- weights_sum[time_index, , , ] + w
      }
    }
  }
  
  avg <- summed / weights_sum
  avg[is.na(avg)] <- 0
  
  array(avg, dim = c(1, total_time, H, W, C))
}

# weighting scheme for aggregation based on the different sample length
if (seq_len==2){
  w = c(1,2)
}else if (seq_len == 3){
  w = c(1,2,3)
}else if (seq_len==4){
  w = c(1,2,3,4)
} else if (seq_len==1){
  w = 1
}

# Creating a function to calculate AUC_ROC and AUC_PR separately
AUC_metrics <- function(best_model, true_dataX, true_dataY, threshold, plt_aucroc = T, plt_aucpr = T){
  tensorflow::set_random_seed(1)
  predicted_dataX <- best_model %>% predict(true_dataX)
  predicted_dataX <- reconstruct_sequence_weighted(predicted_dataX, seq_len = dim(true_dataX)[2], total_time = dim(true_dataY)[2], weights = w)
  x <- ifelse(as.vector(predicted_dataX) > threshold, 1, 0)
  p <- prediction(x, as.vector(true_dataY))
  AUC_ROC <- performance(p, measure = 'auc')@y.values[[1]] # AUC_ROC
  AUC_PR <- performance(p, measure = 'aucpr')@y.values[[1]] # AUC_PR
  
  if (plt_aucroc==T){
    # Visualising AUC ROC curve
    plot(performance(p, 'tpr', 'fpr'), colorize = T, xlab = '1-Specificity', ylab = 'Recall')
    lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
  }
  
  if (plt_aucpr==T){
    # Visualising AUC PR curve
    plot(performance(p, 'prec', 'rec'), colorize = T)
    lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
  }
  
  return(c(AUC_ROC = AUC_ROC, AUC_PR = AUC_PR))
}

val_AUCs <- AUC_metrics(best_model = optimal_ConvLSTM_model, 
                        true_dataX = updated_valX, 
                        true_dataY = valY, 
                        threshold = optimal_ConvLSTM_threshold);val_AUCs


tensorflow::set_random_seed(1)
val_acc_check <- optimal_ConvLSTM_model %>% evaluate(updated_valX, updated_valY)

tensorflow::set_random_seed(1)
pred_prob_val <- optimal_ConvLSTM_model %>% predict(updated_valX)
summary(pred_prob_val)
hist(pred_prob_val)

val_pred_weighted <- reconstruct_sequence_weighted(pred_prob_val,
                                                     seq_len,
                                                     total_time = 24, # 2021 to 2022
                                                     weights = w)
dim(val_pred_weighted)
summary(val_pred_weighted)

# Evaluation metric for all the valY without repetition
val_pred_class <- ifelse(as.vector(val_pred_weighted) > optimal_ConvLSTM_threshold, 1, 0)
val_CM <- confusionMatrix(factor(as.vector(val_pred_class), levels = c('0','1')), 
                              factor(as.vector(valY), 
                                     levels = c('0','1')), 
                              positive = '1', 
                              mode = 'everything');val_CM

# save(val_acc_check, val_AUCs,val_CM, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/ConvLSTM/2014-2022/Training_validation metrics/Val_metrics_AUCs_CM_stats_2014_2022.Rdata')

# # Check if the optimal model is correctly extracted to match the optimal outcome of the validation accuracy of the best model prior to loading the best model
# if(all(round(val_acc_check,5) == round(validation_metrics,5))){
#   print('Verification Successful!')
# }else{
#   print('Verification Unsuccessful!')
# }

# ?fit.keras.engine.training.Model
# plot(history)
tensorflow::set_random_seed(1)
test_metrics <- optimal_ConvLSTM_model %>% evaluate(updated_testX, updated_testY);test_metrics

tensorflow::set_random_seed(1)
test_AUCs <- AUC_metrics(best_model = optimal_ConvLSTM_model, 
                         true_dataX = updated_testX, 
                         true_dataY = testY, 
                         threshold = optimal_ConvLSTM_threshold);test_AUCs

# fire predicted for 2021 and 2022 - This is where all the probabilities are stored
tensorflow::set_random_seed(1)
predicted <- optimal_ConvLSTM_model %>% predict(updated_testX)
dim(predicted)

summary(predicted)
hist(predicted)
# as.vector(predicted[1,15,,,1])[which(as.vector(testY[1,15,,,1])==1)]|>summary()

final_pred_weighted <- reconstruct_sequence_weighted(predicted,
                                                     seq_len,
                                                     total_time = 24, # 2021 to 2022
                                                     weights = w)
dim(final_pred_weighted)
summary(final_pred_weighted)

# Evaluation metric for all the testY without repetition
overall_pred_class <- ifelse(as.vector(final_pred_weighted) > optimal_ConvLSTM_threshold, 1, 0)
overall_CM <- confusionMatrix(factor(as.vector(overall_pred_class), levels = c('0','1')), 
                factor(as.vector(testY), 
                       levels = c('0','1')), 
                positive = '1', 
                mode = 'everything');overall_CM

# save(test_AUCs,predicted,final_pred_weighted,overall_CM, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/ConvLSTM/2014-2022/Test metrics/convlstm_test_results_2014_2022.Rdata')

# creating time label
timesteps_labels <- c('Fire 2021-01', 'Fire 2021-02', 'Fire 2021-03', 'Fire 2021-04', 'Fire 2021-05', 'Fire 2021-06', 'Fire 2021-07','Fire 2021-08', 'Fire 2021-09', 'Fire 2021-10', 'Fire 2021-11', 'Fire 2021-12',
                      'Fire 2022-01', 'Fire 2022-02', 'Fire 2022-03', 'Fire 2022-04', 'Fire 2022-05', 'Fire 2022-06', 'Fire 2022-07','Fire 2022-08', 'Fire 2022-09', 'Fire 2022-10', 'Fire 2022-11', 'Fire 2022-12')

# timesteps_labels <- c('Fire 2021-01', 'Fire 2021-02', 'Fire 2021-03', 'Fire 2021-04')
                      # 'Fire 2021-05', 'Fire 2021-06', 'Fire 2021-07','Fire 2021-08', 'Fire 2021-09', 'Fire 2021-10', 'Fire 2021-11', 'Fire 2021-12')


# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)
# To allow parallel processing in pbapply functions export items used in the function to the cluster
clusterExport(cl, varlist = c("final_pred_weighted", 'roi_trans', 'LULC_2014_2022', 'timesteps_labels', 'testY')) 
# Loading relevant packages on cluster
clusterEvalQ(cl, {
  library(caret)
  library(raster)
  library(tidyverse)
})

# Converting the predicted probabilities into their respective raster while cropping each raster to the study area
predicted_raster_list <- pblapply(1:dim(final_pred_weighted)[2],
                                  function(x){
                                    index <- x
                                    
                                    predicted_normal_Raster_format <- final_pred_weighted[1, index, , , 1] # not rasterised yet!
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
    optimal_threshold <-  optimal_ConvLSTM_threshold # optimal threshold 
    optimal_specificity <- ConvLSTM_specificity_list[[index]][which.max(ConvLSTM_specificity_list[[index]])]
    optimal_F1_score <- NA
  }else{ # if specificity is irrelevant/NA
    optimal_threshold <- optimal_ConvLSTM_threshold # optimal threshold  
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
WS_visualisation <- function(index, true_raster, raster_with_probabilities, raster_factor, classes_breaks_method = c('natural_breaks', 'quantile')){
  period_name <- sub("^Fire\\s*", "", timesteps_labels[index])
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
    tm_layout(main.title= paste0(period_name,': True Fire Status'),
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
    tm_layout(main.title= paste0(period_name,': Predicted Fire Status'),
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
    tm_layout(main.title= paste0(period_name,': Standard Deviation Map'),
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
    tm_layout(main.title= paste0(period_name,': WSM'),
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

call_fire_period <- 'Fire 2021-10'

# optmised_threshold_plot(fire_period = call_fire_period)

# WS_visualisation(index = which(timesteps_labels==call_fire_period),
#                  true_raster = true_test_raster_list[[which(timesteps_labels==call_fire_period)]],
#                  raster_with_probabilities = predicted_raster_list[[which(timesteps_labels==call_fire_period)]],
#                  raster_factor = y_pred_raster_list[[which(timesteps_labels==call_fire_period)]],
#                  classes_breaks_method = 'natural_breaks')

WS_visualisation(index = which(timesteps_labels==call_fire_period),
                 true_raster = true_test_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_with_probabilities = predicted_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 raster_factor = y_pred_raster_list[[which(timesteps_labels==call_fire_period)]], 
                 classes_breaks_method = 'quantile')

# save all results
# save(
#      final_pred_weighted,
#      predicted_raster_list,
#      true_test_raster_list,
#      ConvLSTM_threshold_list,
#      ConvLSTM_specificity_list,
#      ConvLSTM_f1_score_list,
#      y_pred_raster_list,
#      ConvLSTM_metrics_list,
#      ConvLSTM_test_results,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/ConvLSTM/2014-2022/results_for_visualisation/ConvLSTM_main_results_2014_2022.Rdata')

# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/ConvLSTM/2014-2022/results_for_visualisation/ConvLSTM_main_results_2014_2022.Rdata')
