{
  library(ranger)
  library(pbapply)
  library(tidyverse)
  library(raster)
  library(terra)
  library(rgdal)
  library(ROCR)
  library(mltools)
  library(caret)
  library(parallel)
  library(rgeoda)
  library(tmap)
  library(cowplot)
}

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)


# creating a function for visualisation
WS_visualisation_from_RF <- function(df, year, month, test_probs, test_pred_class, classes_breaks_method = c('natural_breaks', 'quantile')){
  
  # Test dataframe with relevant content only!
  x <- cbind(df[,c('x','y','Year','Month','Fire_Value')], 
             test_probs = test_probs[,'1'], 
             test_pred_class = test_pred_class)|>
    as_tibble() %>%
    filter(Year == year, Month==month) %>%
    dplyr::select(x, y, Year, Month, Fire_Value, test_probs, test_pred_class)
  
  # Extract TRUE fire event we want to visualise
  xx_true <- x %>%
    dplyr::select(x,y,Fire_Value) %>%
    rasterFromXYZ(res = c(30,30), crs = crs(roi_trans))
  # names(xx_true) <- paste0('True Fire Events: ', unique(x$Year), '-',unique(x$Month))
  
  # Extract PREDICTED fire event we want to visualise
  xx_pred <- x %>%
    dplyr::select(x,y,test_pred_class) %>%
    rasterFromXYZ(res = c(30,30), crs = crs(roi_trans))
  # names(xx_pred) <- paste0('Predicted Fire Events: ', unique(x$Year), '-',unique(x$Month))
  
  xx_pred_prob <- x %>%
    dplyr::select(x,y,test_probs) %>%
    rasterFromXYZ(res = c(30,30), crs = crs(roi_trans))
  # names(xx_pred_prob) <- paste0('WSM: ', unique(x$Year), '-',unique(x$Month))
  
  # Subdivision types
  quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
  natural_breaks_subdivisions <- natural_breaks(k = 5, df=x[,'test_probs'])
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
    classified_raster <- classify(rast(xx_pred_prob), wildfire_susceptibility_natural_breaks_classes)
    levels(classified_raster) <- data.frame(
      ID = 1:5,
      Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
    )
    
    # Update levels
    classified_raster <- droplevels(classified_raster)
    
  } else if(classes_breaks_method == 'quantile'){
    
    # Reclassify raster accordingly
    classified_raster <- classify(rast(xx_pred_prob), wildfire_susceptibility_quantile_classes)
    
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
  # Visualising the true test fire data 
  p1 <- tm_shape(xx_true|> rast())+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(xx_true))+
    tm_layout(main.title= paste0(sprintf("%d-%02d", year, month),': True Fire Status'),
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5,
              legend.outside.position = 'bottom')+
    tm_graticules(lines = F)
  # )
  
  # print(
  # Visualising the predicted fire data
  p2 <- tm_shape(xx_pred|> rast())+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(xx_pred))+
    tm_layout(main.title= paste0(sprintf("%d-%02d", year, month),': Predicted Fire Status'),
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5,
              legend.outside.position = 'bottom')+
    tm_graticules(lines = F)
  # )
  
  # Visualise the sd of wildfire probabilities raster
  p3 <- tm_shape(xx_pred_prob)+
    tm_raster(style = "sd", title = "", palette = '-RdBu')+
    tm_layout(main.title= paste0(sprintf("%d-%02d", year, month),': Standard Deviation Map'),
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
    tm_layout(main.title= paste0(sprintf("%d-%02d", year, month),': WSM'),
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5,
              legend.outside.position = 'bottom')+
    tm_graticules(lines = F)
  # )
  return(
    tmap_arrange(p1,p2,p3,p4, nrow = 2, ncol = 2)
  ) 
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


# RF MODEL 0 --------------------------------------------------------------
# Importing structured and normalised data for modelling using RF --------

# RF_2014to2022_Train <- fully_resampled_dfnorm_2014_2022_training_set # random forest training set from 2014 to 2022 dataframe
# RF_2014to2022_Train$Month <- as.factor(RF_2014to2022_Train$Month) # converting month to factor
# RF_2014to2022_Train$Fire_Value <- as.factor(RF_2014to2022_Train$Fire_Value) # converting fire value to factor
# 
# RF_2014to2022_Val <- dfnorm_2014_2022_validation_set  # random forest validation set from 2014 to 2022 dataframe
# RF_2014to2022_Val$Month <- as.factor(RF_2014to2022_Val$Month) # converting month to factor
# RF_2014to2022_Val$Fire_Value <- as.factor(RF_2014to2022_Val$Fire_Value) # converting fire value to factor
# 
# RF_2014to2022_Test <- dfnorm_2014_2022_test_set # random forest test set from 2014 to 2022 dataframe
# RF_2014to2022_Test$Month <- as.factor(RF_2014to2022_Test$Month) # converting month to factor
# RF_2014to2022_Test$Fire_Value <- as.factor(RF_2014to2022_Test$Fire_Value) # converting fire value to factor
# 
# 

# RF_2002to2022_Train <- dfnorm_2002_2022_training_set # random forest training set from 2002 to 2022 dataframe
# RF_2002to2022_Train$Month <- as.factor(RF_2002to2022_Train$Month) # converting month to factor
# RF_2002to2022_Train$Fire_Value <- as.factor(RF_2002to2022_Train$Fire_Value) # converting fire value to factor
# 
# RF_2002to2022_Val<- dfnorm_2002_2022_validation_set # random forest validation set from 2002 to 2022 dataframe
# RF_2002to2022_Val$Month <- as.factor(RF_2002to2022_Val$Month) # converting month to factor
# RF_2002to2022_Val$Fire_Value <- as.factor(RF_2002to2022_Val$Fire_Value) # converting fire value to factor
# 
# RF_2002to2022_Test <- dfnorm_2002_2022_test_set # random forest test set from 2002 to 2022 dataframe
# RF_2002to2022_Test$Month <- as.factor(RF_2002to2022_Test$Month) # converting month to factor
# RF_2002to2022_Test$Fire_Value <- as.factor(RF_2002to2022_Test$Fire_Value) # converting fire value to factor


# subsetTrain <- RF_2014to2022_Train %>%
#   filter(Year==2018 & Month==2) %>%
#   dplyr::select(-Month)
# 
# subsetVal <- RF_2014to2022_Train %>%
#   filter(Year==2019 & Month==2) %>%
#   dplyr::select(-Month)
# 
# subsetTest <- RF_2014to2022_Val %>%
#   filter(Year==2020 & Month==2) %>%
#   dplyr::select(-Month)


# Applying random forest on resampled dataset

# # Split the data
# train_set <- subsetTrain
# val_set   <- subsetVal
# test_set  <- subsetTest

# # # full dataset from 2014 to 2022 timeframe
# train_set <- RF_2014to2022_Train # 2014-2019
# prop.table(table(train_set$Fire_Value))*100 # calculate proportion of imbalance after resampling
# val_set   <- RF_2014to2022_Val # 2020-2021
# prop.table(table(val_set$Fire_Value))*100 # calculate proportion of imbalance
# test_set  <- RF_2014to2022_Test # 2022
# prop.table(table(test_set$Fire_Value))*100 # calculate proportion of imbalance
# 
# # save(RF_2014to2022_Train,
# #      RF_2014to2022_Val,
# #      RF_2014to2022_Test,
# #      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/RF Model 0 input data/bufferedTrain20142019_Val20202021_Test2022.Rdata')
# 
# 
# # create combinations of hyperparameters
# rf_gridsearch <-  expand.grid(mtry = 2:(ncol(train_set) - 1),
#                               splitrule = c('gini', 'hellinger'), # gini for classification
#                               min.node.size=seq(1, 5, 2),
#                               stringsAsFactors = F)
# 
# 
# # Initialize results storage
# model_list <- list()
# probabilities_list <- list()
# threshold_list <- list()
# F1_score_list <- list()
# metrics_list <- list()
# results <- data.frame()
# 
# # Detect cores on system and create clusters
# cl <- makeCluster(detectCores() - 1)
# 
# #  Manual tuning loop
# for (i in 1:nrow(rf_gridsearch)) {
#   cat('Iteration',i, 'out of', nrow(rf_gridsearch))
#   i <- i
#   params <- rf_gridsearch[i, ]
#   
#   # Train the model
#   rf <- ranger(
#     formula = Fire_Value ~ .,
#     data = train_set,
#     probability = T,  # to get class probabilities
#     classification = T,
#     importance = 'permutation',
#     oob.error = T,
#     num.threads = detectCores() - 1,
#     mtry = params$mtry,
#     splitrule = params$splitrule,
#     min.node.size = params$min.node.size,
#     num.trees = 500,
#     verbose = T,
#     seed = 1
#   )
#   
#   options(scipen = 999)  # Prevents scientific notation
#   
#   # Predict probabilities on validation set
#   probs <- predict(rf, data = val_set[,colnames(val_set) != 'Fire_Value'])$predictions
#   actual_class <- val_set$Fire_Value # extract known response variable from validation set
#   
#   # probs[which(actual_class==1),]
#   # probs[which(actual_class==1),'1'] |> hist()
#   # density(probs[,'1'])
#   # summary(probs[,'1'])
#   
#   threshold <- seq(min(probs[,'1']), max(probs[,'1']), by = 0.002) # generate a sequence of threshold to classify response variable based on probability class
# 
#     # To allow parallel processing in pbsapply export items used in the function to the cluster
#   clusterExport(cl, varlist = c("probs",'threshold','actual_class')) 
#   # Loading relevant package on cluster
#   clusterEvalQ(cl, library(caret))
#   
#   # This returns the F1 scores for each threshold 
#   F1_SCORES <- pbsapply(seq_along(threshold), function (x){
#     pred_class <- ifelse(probs[, '1'] >= threshold[x], 1, 0) |> as.factor()
#     return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
#   
#   # # This returns the F1 scores for each threshold
#   # F1_SCORES <- pbsapply(seq_along(threshold), function (x){
#   # pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= threshold[x], 1, 0) |> as.factor()
#   # return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
# 
#  
#   # F1_SCORES[is.nan(F1_SCORES)] <- 0 # replace NaN with 0
#   optimal_threshold <- threshold[which.max(F1_SCORES)] # which threshold has led to the maximum F1 score
#   optimal_f1_score <- max(F1_SCORES[!is.nan(F1_SCORES)]) # extract the maximum F1 score (omitting NaN if there's any)
#   final_pred_class <- ifelse(probs[, '1'] >= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
#   # final_pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
#   # plot(threshold, F1_SCORES,
#   #      type = 'l',
#   #      # pch = 19,
#   #      cex.main = .9,
#   #      cex.lab = .9,
#   #      cex.axis = .9,
#   #      # cex = .3,
#   #      col = 'red',
#   #      xlab = 'Threshold',
#   #      ylab = 'F1 Score',
#   #      # xlim = c(min(threshold), 0.01)
#   #      )
#   # points(threshold, F1_SCORES, pch = 19, cex = .2, col = 'red')
#   MCC <- mcc(preds = final_pred_class, actuals = actual_class) # computing Matthew's correlation coefficient
#   Metrics <- confusionMatrix(final_pred_class, actual_class, positive = '1', mode = 'everything') # generate other metrics from confusion matrix
#   
#   prediction <- prediction(as.numeric(final_pred_class)-1, actual_class)
#   AUC_ROC <- performance(prediction, measure = 'auc')@y.values[[1]] # AUC_ROC
#   AUC_PR <- performance(prediction, measure = 'aucpr')@y.values[[1]] # AUC_PR
#   
#   # # Visualising AUC ROC curve
#   # plot(performance(prediction, 'tpr', 'fpr'), colorize = T, xlab = '1-Specificity', ylab = 'Recall')
#   # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
#   # 
#   # # Visualising AUC PR curve
#   # plot(performance(prediction, 'prec', 'rec'), colorize = T)
#   # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
#   
#   model_list[[i]] <- rf # appending each model to a list
#   probabilities_list[[i]] <- probs # appending each model's probability to a list
#   threshold_list[[i]] <- threshold # appending the threshold generated from the probabilities to a list
#   F1_score_list[[i]] <- F1_SCORES # appending each F1 score generated from the respective threshold to a list
#   metrics_list[[i]] <- Metrics # appending each metric from each model to a list
#   results <- rbind(results, cbind(params, 
#                                   optimal_threshold = optimal_threshold,
#                                   precision = Metrics$byClass['Precision'][[1]],
#                                   recall = Metrics$byClass['Recall'][[1]],
#                                   F1_score = optimal_f1_score, 
#                                   AUC_ROC = AUC_ROC,
#                                   AUC_PR = AUC_PR,
#                                   MCC = MCC))
#   
# }
# 
# stopCluster(cl)
# # save all content from model
# # save(model_list, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_2014_2022_resampled_dataset.Rdata')
# # save(probabilities_list, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_probabilities_2014_2022_resampled_dataset.Rdata')
# # save(threshold_list, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_thresholds_2014_2022_resampled_dataset.Rdata')
# # save(F1_score_list, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_F1_scores_2014_2022_resampled_dataset.Rdata')
# # save(metrics_list, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_metrics_2014_2022_resampled_dataset.Rdata')
# # save(results, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_results_2014_2022_resampled_dataset.Rdata')
# 
# 
# 
# results
# which.max(results$AUC_ROC)
# which.max(results$AUC_PR)
# which.max(results$MCC)
# which.max(results$F1_score)
# metrics_list[[which.max(results$AUC_PR)]]
# 
# optimal_model <- model_list[[which.max(results$AUC_PR)]] # extracting optimal model from list using AUC_PR as metrics of choice
# { # Variable importance plot
#   # par(mar = c(4.1, 7, 1, 0.2)) 
#   # importance(optimal_model)|> sort(decreasing = F) |> barplot(horiz = T, las = 1) # base R plot
#   optimal_model_IMP <- importance(optimal_model) |> as.data.frame() # convert importance into a data frame
#   rownames(optimal_model_IMP) <- c('Longitude', 'Latitude', 'Year', 'Month',
#                                    'LULC', 'NDVI', 'NDMI', 'NBR', 'TP',
#                                    'AMT', 'ANSWS', 'ARH', 'Elevation',
#                                    'Slope', 'Aspect') # rename variables
#   colnames(optimal_model_IMP) <- 'Importance' # change column names
#   optimal_model_IMP$Importance <- optimal_model_IMP$Importance*100 # convert importance to percentage
#   optimal_model_IMP_plot <- ggplot(optimal_model_IMP, aes(x = reorder(rownames(optimal_model_IMP), Importance), y = Importance, 
#                                                           fill = -Importance)) +
#     geom_bar(stat='identity') +
#     ggtitle('Variable Importance\n from RFM 1')+
#     xlab('')+
#     ylab('Overall \nImportance (%)') +
#     theme_classic() +
#     coord_flip()+
#     theme(legend.position = '', axis.title = element_text(size = 8), plot.title = element_text(size = 9),
#           axis.text = element_text(size = 8)) 
#   optimal_model_IMP_plot
# }
# 
# opt_probs <- probabilities_list[[which.max(results$AUC_PR)]]
# threshold_from_optimal_model <- threshold_list[[which.max(results$AUC_PR)]] # generate a sequence of threshold to classify response variable based on probability class
# F1_scores_from_optimal_model <- F1_score_list[[which.max(results$AUC_PR)]]
# 
# {
#   par(mar = c(4.1, 4, 1, 0.2)) # customised margin
#   plot(threshold_from_optimal_model, F1_scores_from_optimal_model,
#        type = 'l',
#        # pch = 19,
#        # main = 'Chosen Threshold from Optimal RF Model',
#        # cex.main = .9,
#        cex.lab = .8,
#        cex.axis = .8,
#        # cex = .3,
#        col = 'seagreen',
#        xlab = paste0('Threshold'),
#        ylab = 'F1 Score',
#        # xlim = c(min(threshold), 0.01)
#   )
#   points(threshold_from_optimal_model, 
#          F1_scores_from_optimal_model, 
#          pch = 19, cex = .1, col = 'seagreen')
#   points(results$optimal_threshold[which.max(results$AUC_PR)], 
#          results$F1_score[which.max(results$AUC_PR)], 
#          pch = 19, cex = .5, col = 'greenyellow')
#   abline(v=results$optimal_threshold[which.max(results$AUC_PR)],
#          h=results$F1_score[which.max(results$AUC_PR)],
#          lty = "dashed",
#          col= 'greenyellow')
#   text(results$optimal_threshold[which.max(results$AUC_PR)]+.02, 
#        results$F1_score[which.max(results$AUC_PR)]-.03, 
#        labels=paste("Threshold = ", results$optimal_threshold[which.max(results$AUC_PR)]),
#        cex=.6,
#        col="seagreen",
#        srt=270)
#   text(results$optimal_threshold[which.max(results$AUC_PR)]-.3, 
#        results$F1_score[which.max(results$AUC_PR)]-.0015, 
#        labels=paste("F1 Score = ", results$F1_score[which.max(results$AUC_PR)]|>round(3)),
#        cex=.6,
#        col="seagreen")
# }
# 
# # prediction probabilities for each class on test set
# test_probs <- predict(optimal_model, data = test_set[,colnames(test_set) != 'Fire_Value'])$predictions
# test_optimal_threshold <- results[which.max(results$AUC_PR),]$optimal_threshold # extracting the optimal threshold used in the optimal model
# test_pred_class <- ifelse(test_probs[, '1'] >= test_optimal_threshold, 1, 0) |> as.factor()
# test_metrics <- confusionMatrix(test_pred_class, test_set$Fire_Value, positive = '1', mode = 'everything')
# test_f1_score <- test_metrics$byClass['F1'][[1]]
# test_precision <- test_metrics$byClass['Precision'][[1]]
# test_recall <- test_metrics$byClass['Recall'][[1]]
# test_prediction <- prediction(as.numeric(test_pred_class)-1, test_set$Fire_Value)
# test_AUC_ROC <- performance(test_prediction, measure = 'auc')@y.values[[1]] # AUC_ROC
# test_AUC_PR <- performance(test_prediction, measure = 'aucpr')@y.values[[1]] # AUC_PR
# test_MCC <- mcc(preds = test_pred_class, actuals = test_set$Fire_Value) # computing Matthew's correlation coefficient
# test_accuracy <- cbind(optimal_threshold = test_optimal_threshold,
#       precision = test_precision|>round(3),
#       recall = test_recall|>round(3),
#       F1_score = test_f1_score|>round(3), 
#       AUC_ROC = test_AUC_ROC|>round(3),
#       AUC_PR = test_AUC_PR|>round(3),
#       MCC = test_MCC|>round(3)) |> as_tibble()
# 
# 
# WS_visualisation_from_RF(df = test_set, year = 2022, month = 1, 
#                          test_probs = test_probs, test_pred_class = test_pred_class, 
#                          classes_breaks_method = 'natural_breaks')
# WS_visualisation_from_RF(df = test_set, year = 2022, month = 1, 
#                          test_probs = test_probs, test_pred_class = test_pred_class, 
#                          classes_breaks_method = 'quantile')
# 
# pblapply(1:12, function(x){
#   WS_visualisation_from_RF(df = test_set, year = 2022, month = x, 
#                            test_probs = test_probs, test_pred_class = test_pred_class, 
#                            classes_breaks_method = 'quantile')
# })
# 


# RF MODEL 1 --------------------------------------------------------------
# Importing structured and normalised data for modelling using RF --------
RF_2014to2022_Train1 <- fully_resampled_buffered_dfnorm_2014_2022_training_set # random forest training set from 2014 to 2022 dataframe
RF_2014to2022_Train1$Month <- as.factor(RF_2014to2022_Train1$Month) # converting month to factor
RF_2014to2022_Train1$Lagged_Fire_Value <- as.factor(RF_2014to2022_Train1$Lagged_Fire_Value) # converting lagged fire value to factor
RF_2014to2022_Train1$Fire_Value <- as.factor(RF_2014to2022_Train1$Fire_Value) # converting fire value to factor

RF_2014to2022_Val1 <- dfnorm_2014_2022_validation_set  # random forest validation set from 2014 to 2022 dataframe
RF_2014to2022_Val1$Month <- as.factor(RF_2014to2022_Val1$Month) # converting month to factor
RF_2014to2022_Val1$Lagged_Fire_Value <- as.factor(RF_2014to2022_Val1$Lagged_Fire_Value) # converting lagged fire value to factor
RF_2014to2022_Val1$Fire_Value <- as.factor(RF_2014to2022_Val1$Fire_Value) # converting fire value to factor

RF_2014to2022_Test1 <- dfnorm_2014_2022_test_set # random forest test set from 2014 to 2022 dataframe
RF_2014to2022_Test1$Month <- as.factor(RF_2014to2022_Test1$Month) # converting month to factor
RF_2014to2022_Test1$Lagged_Fire_Value <- as.factor(RF_2014to2022_Test1$Lagged_Fire_Value) # converting lagged fire value to factor
RF_2014to2022_Test1$Fire_Value <- as.factor(RF_2014to2022_Test1$Fire_Value) # converting fire value to factor

# full dataset from 2014 to 2022 timeframe
train_set1 <- RF_2014to2022_Train1 # 2014-2018
prop.table(table(train_set1$Fire_Value))*100 # calculate proportion of imbalance after resampling
val_set1   <- RF_2014to2022_Val1 # 2019-2020
prop.table(table(val_set1$Fire_Value))*100 # calculate proportion of imbalance 
test_set1  <- RF_2014to2022_Test1 # 2021-2022
prop.table(table(test_set1$Fire_Value))*100 # calculate proportion of imbalance 

# save(RF_2014to2022_Train1,
#      RF_2014to2022_Val1,
#      RF_2014to2022_Test1,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/RF Model 1 input data/bufferedTrain20142018_Val20192020_Test20212022.Rdata')

# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/RF Model 1 input data/bufferedTrain20142018_Val20192020_Test20212022.Rdata')

# save(RF_2014to2022_Train1,
#      RF_2014to2022_Val1,
#      RF_2014to2022_Test1,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Buffered Model/Input data/bufferedTrain20142018_Val20192020_Test20212022.Rdata')



# create combinations of hyperparameters
rf_gridsearch <-  expand.grid(mtry = 2:(ncol(train_set1) - 1),
                              splitrule = c('gini', 'hellinger'), # gini for classification
                              min.node.size=seq(1, 5, 2),
                              stringsAsFactors = F)


# Initialize results storage
model_list1 <- list()
probabilities_list1 <- list()
# threshold_list1 <- list()
# F1_score_list1 <- list()
MCC_score_list1 <- list()
metrics_list1 <- list()
results1 <- data.frame()

# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)

#  Manual tuning loop
for (i in 1:nrow(rf_gridsearch)) {
  cat('Iteration',i, 'out of', nrow(rf_gridsearch))
  i <- i
  params <- rf_gridsearch[i, ]
  
  # Train the model
  rf <- ranger(
    formula = Fire_Value ~ .,
    data = train_set1,
    probability = T,  # to get class probabilities
    classification = T,
    importance = 'permutation',
    oob.error = T,
    num.threads = detectCores() - 1,
    mtry = params$mtry,
    splitrule = params$splitrule,
    min.node.size = params$min.node.size,
    num.trees = 500,
    verbose = T,
    seed = 1
  )
  
  options(scipen = 999)  # Prevents scientific notation
  
  # Predict probabilities on validation set
  probs <- predict(rf, data = val_set1[,colnames(val_set1) != 'Fire_Value'])$predictions
  actual_class <- val_set1$Fire_Value # extract known response variable from validation set
  
  # probs[which(actual_class==1),]
  # probs[which(actual_class==1),'1'] |> hist()
  # density(probs[,'1'])
  # summary(probs[,'1'])
  
  # threshold <- seq(min(probs[,'1']), max(probs[,'1']), by = 0.01) # generate a sequence of threshold to classify response variable based on probability class
  threshold <- seq(0.5, 0.9, 0.05)

  # To allow parallel processing in pbsapply export items used in the function to the cluster
  clusterExport(cl, varlist = c("probs",'actual_class','threshold')) 
  # Loading relevant package on cluster
  clusterEvalQ(cl, {
    library(caret)
    library(mltools)
    })
  
  # This returns the F1 scores for each threshold 
  MCC_SCORES <- pbsapply(seq_along(threshold), function (x){
    pred_class <- ifelse(probs[, '1'] >= threshold[x], 1, 0) |> as.factor()
    # CM <- confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')
    return(mcc(pred_class,actual_class)) # return mcc score
    },
    cl = cl) # apply threshold on positive class; 1 in this case
  
  # # This returns the F1 scores for each threshold
  # F1_SCORES <- pbsapply(seq_along(threshold), function (x){
  # pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= threshold[x], 1, 0) |> as.factor()
  # return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
  
  
  # F1_SCORES[is.nan(F1_SCORES)] <- 0 # replace NaN with 0
  optimal_threshold <- threshold[which.max(MCC_SCORES)] # which threshold has led to the maximum MCC score
  optimal_mcc_score <- max(MCC_SCORES[!is.nan(MCC_SCORES)]) # extract the maximum MCC score (omitting NaN if there's any)
  final_pred_class <- ifelse(probs[, '1'] >= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
  # final_pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
  # plot(threshold, MCC_SCORES,
  #      type = 'l',
  #      # pch = 19,
  #      cex.main = .9,
  #      cex.lab = .9,
  #      cex.axis = .9,
  #      # cex = .3,
  #      col = 'red',
  #      xlab = 'Threshold',
  #      ylab = 'MCC Score',
  #      # xlim = c(min(threshold), 0.01)
  #      )
  # points(threshold, MCC_SCORES, pch = 19, cex = .2, col = 'red')
  # MCC <- mcc(preds = final_pred_class, actuals = actual_class) # computing Matthew's correlation coefficient
  Metrics <- confusionMatrix(final_pred_class, actual_class, positive = '1', mode = 'everything') # generate other metrics from confusion matrix
  
  prediction <- prediction(as.numeric(final_pred_class)-1, actual_class)
  AUC_ROC <- performance(prediction, measure = 'auc')@y.values[[1]] # AUC_ROC
  AUC_PR <- performance(prediction, measure = 'aucpr')@y.values[[1]] # AUC_PR
  
  # # Visualising AUC ROC curve
  # plot(performance(prediction, 'tpr', 'fpr'), colorize = T, xlab = '1-Specificity', ylab = 'Recall')
  # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
  # 
  # # Visualising AUC PR curve
  # plot(performance(prediction, 'prec', 'rec'), colorize = T)
  # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
  
  model_list1[[i]] <- rf # appending each model to a list
  probabilities_list1[[i]] <- probs # appending each model's probability to a list
  # threshold_list1[[i]] <- threshold # appending the threshold generated from the probabilities to a list
  MCC_score_list1[[i]] <- MCC_SCORES # appending each F1 score generated from the respective threshold to a list
  metrics_list1[[i]] <- Metrics # appending each metric from each model to a list
  
  results1 <- rbind(results1, cbind(params, 
                                  optimal_threshold = optimal_threshold,
                                  binary_accuracy = Metrics$overall['Accuracy'][[1]],
                                  recall = Metrics$byClass['Recall'][[1]],
                                  precision = Metrics$byClass['Precision'][[1]],
                                  specificity =  Metrics$byClass['Specificity'][[1]],
                                  F1_score = Metrics$byClass['F1'][[1]], 
                                  MCC = optimal_mcc_score,
                                  AUC_ROC = AUC_ROC,
                                  AUC_PR = AUC_PR,
                                  FN = Metrics$table[1,2],
                                  FP = Metrics$table[2,1],
                                  TN = Metrics$table[1,1],
                                  TP = Metrics$table[2,2]
                                  ))
  
}

stopCluster(cl)
# save all content from RF model 1
# save(model_list1,
#      probabilities_list1,
#      MCC_score_list1,
#      metrics_list1,
#      results1,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_output_2014_2022_resampled_buffered_dataset.Rdata')

# save(model_list1,
#      probabilities_list1,
#      MCC_score_list1,
#      metrics_list1,
#      results1,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Buffered Model/Training metrics/rf_models_training_output_2014_2022_resampled_buffered_dataset.Rdata')

View(results1)
optimal_model1 <- model_list1[[which.max(results1$MCC)]] # extracting optimal model from list using MCC as metrics of choice
{ # Variable importance plot
  # par(mar = c(4.1, 7, 1, 0.2)) 
  # importance(optimal_model1)|> sort(decreasing = F) |> barplot(horiz = T, las = 1)
  optimal_model1_IMP <- importance(optimal_model1) |> as.data.frame() # convert importance into a data frame
  rownames(optimal_model1_IMP) <- c('Longitude', 'Latitude', 'Year', 'Month',
                                   'LULC', 'NDVI', 'NDMI', 'TP',
                                   'AMT', 'ANSWS', 'ARH', 'Elevation',
                                   'Slope', 'Aspect', 'Lagged Fire') # rename variables
  colnames(optimal_model1_IMP) <- 'Importance' # change column names
  optimal_model1_IMP$Importance <- optimal_model1_IMP$Importance*100 # convert importance to percentage
  optimal_model1_IMP_plot <- ggplot(optimal_model1_IMP, aes(x = reorder(rownames(optimal_model1_IMP), Importance), y = Importance, 
                                                          fill = -Importance)) +
    geom_bar(stat='identity') +
    ggtitle('Variable Importance\n from RFM 1')+
    xlab('')+
    ylab('Overall \nImportance (%)') +
    theme_classic() +
    coord_flip()+
    theme(legend.position = '', axis.title = element_text(size = 8), plot.title = element_text(size = 9),
          axis.text = element_text(size = 8))  
  optimal_model1_IMP_plot
}
opt_probs1 <- probabilities_list1[[which.max(results1$MCC)]]
RFM1_training_accuracy <- results1[which.max(results1$MCC),]
MCC_from_optimal_model1 <- MCC_score_list1[[which.max(results1$MCC)]] # list of MCCs for each threshold
RFM1_optimal_MCC <- max(MCC_from_optimal_model1) # chosen MCC for best outcome
RFM1_optimal_threshold <- RFM1_training_accuracy[,'optimal_threshold']

{
  par(mar = c(4.1, 4, .2, 0.2)) # customised margin
  plot(threshold, MCC_from_optimal_model1,
       type = 'b',
       # pch = 19,
       # main = 'Chosen Threshold from Optimal RF Model',
       # cex.main = .9,
       cex.lab = .8,
       cex.axis = .8,
       # cex = .3,
       col = 'seagreen',
       xlab = 'Threshold',
       ylab = 'Validation MCC Score',
       # xlim = c(min(threshold), 0.01)
  )
  points(RFM1_optimal_threshold,
         RFM1_optimal_MCC,
         pch = 19, cex = .75, col = 'seagreen')
  # points(results1$optimal_threshold[which.max(results1$AUC_PR)], 
  #        results1$F1_score[which.max(results1$AUC_PR)], 
  #        pch = 19, cex = .5, col = 'greenyellow')
  # abline(v=results1$optimal_threshold[which.max(results1$AUC_PR)],
  #        h=results1$F1_score[which.max(results1$AUC_PR)],
  #        lty = "dashed",
  #        col= 'greenyellow')
  # text(results1$optimal_threshold[which.max(results1$AUC_PR)]+.02, 
  #      results1$F1_score[which.max(results1$AUC_PR)]-.03, 
  #      labels=paste("Threshold = ", results1$optimal_threshold[which.max(results1$AUC_PR)]),
  #      cex=.6,
  #      col="seagreen",
  #      srt=270)
  # text(results1$optimal_threshold[which.max(results1$AUC_PR)]-.3, 
  #      results1$F1_score[which.max(results1$AUC_PR)]-.0015, 
  #      labels=paste("F1 Score = ", results1$F1_score[which.max(results1$AUC_PR)]|>round(3)),
  #      cex=.6,
  #      col="seagreen")
}

# prediction probabilities for each class on test set
test_probs1 <- predict(optimal_model1, data = test_set1[,colnames(test_set1) != 'Fire_Value'])$predictions
test_optimal_threshold1 <- RFM1_optimal_threshold # extracting the optimal threshold used in the optimal model
test_pred_class1 <- ifelse(test_probs1[, '1'] >= test_optimal_threshold1, 1, 0) |> as.factor()
test_metrics1 <- confusionMatrix(test_pred_class1, test_set1$Fire_Value, positive = '1', mode = 'everything')
test_prediction1 <- prediction(as.numeric(test_pred_class1)-1, test_set1$Fire_Value)
test_AUC_ROC1 <- performance(test_prediction1, measure = 'auc')@y.values[[1]] # AUC_ROC
test_AUC_PR1 <- performance(test_prediction1, measure = 'aucpr')@y.values[[1]] # AUC_PR
test_MCC1 <- mcc(preds = test_pred_class1, actuals = test_set1$Fire_Value) # computing Matthew's correlation coefficient
test_accuracy1 <- cbind(optimal_threshold = test_optimal_threshold1,
                        binary_accuracy = test_metrics1$overall['Accuracy'][[1]],
                        recall = test_metrics1$byClass['Recall'][[1]],
                        precision = test_metrics1$byClass['Precision'][[1]],
                        specificity = test_metrics1$byClass['Specificity'][[1]],
                        F1_score = test_metrics1$byClass['F1'][[1]], 
                        MCC = test_MCC1,
                        AUC_ROC = test_AUC_ROC1,
                        AUC_PR = test_AUC_PR1,
                        FN = test_metrics1$table[1,2],
                        FP = test_metrics1$table[2,1],
                        TN = test_metrics1$table[1,1],
                        TP = test_metrics1$table[2,2]) |> as.data.frame(); test_accuracy1

# save(test_probs1,test_optimal_threshold1,test_pred_class1,test_metrics1,
#      test_prediction1,test_AUC_ROC1,test_AUC_PR1,test_MCC1,test_accuracy1,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Buffered Model/Test metrics/rf_models_test_output_2014_2022_resampled_buffered_dataset.Rdata')

# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Buffered Model/Test metrics/rf_models_test_output_2014_2022_resampled_buffered_dataset.Rdata')

# WS_visualisation_from_RF(df = test_set1, year = 2022, month = 3, 
#                          test_probs = test_probs1, test_pred_class = test_pred_class1, 
#                          classes_breaks_method = 'natural_breaks')
WS_visualisation_from_RF(df = test_set1, year = 2022, month = 3, 
                         test_probs = test_probs1, test_pred_class = test_pred_class1, 
                         classes_breaks_method = 'quantile')

pblapply(1:12, function(x){
  WS_visualisation_from_RF(df = test_set1, year = 2021, month = x, 
                           test_probs = test_probs1, test_pred_class = test_pred_class1, 
                           classes_breaks_method = 'quantile')
})

pblapply(1:12, function(x){
  WS_visualisation_from_RF(df = test_set1, year = 2022, month = x, 
                           test_probs = test_probs1, test_pred_class = test_pred_class1, 
                           classes_breaks_method = 'quantile')
})




# RF MODEL 2 --------------------------------------------------------------
# Importing structured and normalised data for modelling using RF --------
RF_2014to2022_Train2 <- fully_resampled_non_buffered_dfnorm_2014_2022_training_set # random forest training set from 2014 to 2022 dataframe
RF_2014to2022_Train2$Month <- as.factor(RF_2014to2022_Train2$Month) # converting month to factor
RF_2014to2022_Train2$Lagged_Fire_Value <- as.factor(RF_2014to2022_Train2$Lagged_Fire_Value) # converting lagged fire value to factor
RF_2014to2022_Train2$Fire_Value <- as.factor(RF_2014to2022_Train2$Fire_Value) # converting fire value to factor

RF_2014to2022_Val2 <- dfnorm_2014_2022_validation_set  # random forest validation set from 2014 to 2022 dataframe
RF_2014to2022_Val2$Month <- as.factor(RF_2014to2022_Val2$Month) # converting month to factor
RF_2014to2022_Val2$Lagged_Fire_Value <- as.factor(RF_2014to2022_Val2$Lagged_Fire_Value) # converting lagged fire value to factor
RF_2014to2022_Val2$Fire_Value <- as.factor(RF_2014to2022_Val2$Fire_Value) # converting fire value to factor

RF_2014to2022_Test2 <- dfnorm_2014_2022_test_set # random forest test set from 2014 to 2022 dataframe
RF_2014to2022_Test2$Month <- as.factor(RF_2014to2022_Test2$Month) # converting month to factor
RF_2014to2022_Test2$Lagged_Fire_Value <- as.factor(RF_2014to2022_Test2$Lagged_Fire_Value) # converting lagged fire value to factor
RF_2014to2022_Test2$Fire_Value <- as.factor(RF_2014to2022_Test2$Fire_Value) # converting fire value to factor

# RF_2002to2022_Train <- dfnorm_2002_2022_training_set # random forest training set from 2002 to 2022 dataframe
# RF_2002to2022_Train$Month <- as.factor(RF_2002to2022_Train$Month) # converting month to factor
# RF_2002to2022_Train$Fire_Value <- as.factor(RF_2002to2022_Train$Fire_Value) # converting fire value to factor
# 
# RF_2002to2022_Val<- dfnorm_2002_2022_validation_set # random forest validation set from 2002 to 2022 dataframe
# RF_2002to2022_Val$Month <- as.factor(RF_2002to2022_Val$Month) # converting month to factor
# RF_2002to2022_Val$Fire_Value <- as.factor(RF_2002to2022_Val$Fire_Value) # converting fire value to factor
# 
# RF_2002to2022_Test <- dfnorm_2002_2022_test_set # random forest test set from 2002 to 2022 dataframe
# RF_2002to2022_Test$Month <- as.factor(RF_2002to2022_Test$Month) # converting month to factor
# RF_2002to2022_Test$Fire_Value <- as.factor(RF_2002to2022_Test$Fire_Value) # converting fire value to factor

# # full dataset from 2014 to 2022 timeframe
train_set2 <- RF_2014to2022_Train2 # 2014-2018
prop.table(table(train_set2$Fire_Value))*100 # calculate proportion of imbalance after resampling
val_set2   <- RF_2014to2022_Val2 # 2019-2020
prop.table(table(val_set2$Fire_Value))*100 # calculate proportion of imbalance after resampling
test_set2  <- RF_2014to2022_Test2 # 2021-2022
prop.table(table(test_set2$Fire_Value))*100 # calculate proportion of imbalance after resampling

# save(RF_2014to2022_Train2,
#      RF_2014to2022_Val2,
#      RF_2014to2022_Test2,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/RF Model 2 input data/non_bufferedTrain20142018_Val20192020_Test20212022.Rdata')

load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/RF Model 2 input data/non_bufferedTrain20142018_Val20192020_Test20212022.Rdata')

# save(RF_2014to2022_Train2,
#      RF_2014to2022_Val2,
#      RF_2014to2022_Test2,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Non-Buffered Model/Input data/non_bufferedTrain20142018_Val20192020_Test20212022.Rdata')

# create combinations of hyperparameters
rf2_gridsearch <-  expand.grid(mtry = 2:(ncol(train_set2) - 1),
                              splitrule = c('gini', 'hellinger'), # gini for classification
                              min.node.size=seq(1, 5, 2),
                              stringsAsFactors = F)


# Initialize results storage
model_list2 <- list()
probabilities_list2 <- list()
# threshold_list2 <- list()
# F1_score_list2 <- list()
MCC_score_list2 <- list()
metrics_list2 <- list()
results2 <- data.frame()

# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)

#  Manual tuning loop
for (i in 1:nrow(rf2_gridsearch)) {
  cat('Iteration',i, 'out of', nrow(rf2_gridsearch), '\n')
  i <- i
  params <- rf2_gridsearch[i, ]
  
  # Train the model
  rf <- ranger(
    formula = Fire_Value ~ .,
    data = train_set2,
    probability = T,  # to get class probabilities
    classification = T,
    importance = 'permutation',
    oob.error = T,
    num.threads = detectCores() - 1,
    mtry = params$mtry,
    splitrule = params$splitrule,
    min.node.size = params$min.node.size,
    num.trees = 500,
    verbose = T,
    seed = 1
  )
  
  options(scipen = 999)  # Prevents scientific notation
  
  # Predict probabilities on validation set
  probs <- predict(rf, data = val_set2[,colnames(val_set2) != 'Fire_Value'])$predictions
  actual_class <- val_set2$Fire_Value # extract known response variable from validation set
  
  # probs[which(actual_class==1),]
  # probs[which(actual_class==1),'1'] |> hist()
  # density(probs[,'1'])
  # summary(probs[,'1'])
  
  threshold <- seq(0.5, 0.9, 0.05) # generate a sequence of threshold to classify response variable 
  
  # To allow parallel processing in pbsapply export items used in the function to the cluster
  clusterExport(cl, varlist = c("probs",'threshold','actual_class')) 
  # Loading relevant package on cluster
  clusterEvalQ(cl,  {
    library(caret)
    library(mltools)
  })
  
  # This returns the F1 scores for each threshold 
  MCC_SCORES <- pbsapply(seq_along(threshold), function (x){
    pred_class <- ifelse(probs[, '1'] >= threshold[x], 1, 0) |> as.factor()
    return(mcc(pred_class,actual_class))# apply threshold on positive class; 1 in this case
    }, cl = cl) # apply threshold on positive class; 1 in this case
  
  # # This returns the F1 scores for each threshold
  # F1_SCORES <- pbsapply(seq_along(threshold), function (x){
  # pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= threshold[x], 1, 0) |> as.factor()
  # return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
  
  
  # F1_SCORES[is.nan(F1_SCORES)] <- 0 # replace NaN with 0
  optimal_threshold <- threshold[which.max(MCC_SCORES)] # which threshold has led to the maximum F1 score
  optimal_mcc_score <- max(MCC_SCORES[!is.nan(MCC_SCORES)]) # extract the maximum F1 score (omitting NaN if there's any)
  final_pred_class <- ifelse(probs[, '1'] >= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
  # final_pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
  # plot(threshold, MCC_SCORES,
  #      type = 'l',
  #      # pch = 19,
  #      cex.main = .9,
  #      cex.lab = .9,
  #      cex.axis = .9,
  #      # cex = .3,
  #      col = 'red',
  #      xlab = 'Threshold',
  #      ylab = 'MCC Score',
  #      # xlim = c(min(threshold), 0.01)
  #      )
  # points(threshold, MCC_SCORES, pch = 19, cex = .2, col = 'red')
  MCC <- mcc(preds = final_pred_class, actuals = actual_class) # computing Matthew's correlation coefficient
  Metrics <- confusionMatrix(final_pred_class, actual_class, positive = '1', mode = 'everything') # generate other metrics from confusion matrix
  
  prediction <- prediction(as.numeric(final_pred_class)-1, actual_class)
  AUC_ROC <- performance(prediction, measure = 'auc')@y.values[[1]] # AUC_ROC
  AUC_PR <- performance(prediction, measure = 'aucpr')@y.values[[1]] # AUC_PR
  
  # # Visualising AUC ROC curve
  # plot(performance(prediction, 'tpr', 'fpr'), colorize = T, xlab = '1-Specificity', ylab = 'Recall')
  # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
  # 
  # # Visualising AUC PR curve
  # plot(performance(prediction, 'prec', 'rec'), colorize = T)
  # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
  
  model_list2[[i]] <- rf # appending each model to a list
  probabilities_list2[[i]] <- probs # appending each model's probability to a list
  # threshold_list2[[i]] <- threshold # appending the threshold generated from the probabilities to a list
  MCC_score_list2[[i]] <- MCC_SCORES # appending each F1 score generated from the respective threshold to a list
  metrics_list2[[i]] <- Metrics # appending each metric from each model to a list
  results2 <- rbind(results2, cbind(params, 
                                    optimal_threshold = optimal_threshold,
                                    binary_accuracy = Metrics$overall['Accuracy'][[1]],
                                    recall = Metrics$byClass['Recall'][[1]],
                                    precision = Metrics$byClass['Precision'][[1]],
                                    specificity =  Metrics$byClass['Specificity'][[1]],
                                    F1_score = Metrics$byClass['F1'][[1]], 
                                    MCC = optimal_mcc_score,
                                    AUC_ROC = AUC_ROC,
                                    AUC_PR = AUC_PR,
                                    FN = Metrics$table[1,2],
                                    FP = Metrics$table[2,1],
                                    TN = Metrics$table[1,1],
                                    TP = Metrics$table[2,2]
                                    ))
  
}

stopCluster(cl)
# save all content from model
# save(model_list2,
#      probabilities_list2,
#      MCC_score_list2,
#      metrics_list2,
#      results2,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Models/rf_models_output_2014_2022_resampled_non_buffered_dataset.Rdata')


# save(model_list2,
#      probabilities_list2,
#      MCC_score_list2,
#      metrics_list2,
#      results2,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Non-Buffered Model/Training metrics/rf_models_training_output_2014_2022_resampled_non_buffered_dataset.Rdata')
# 

results2

optimal_model2 <- model_list2[[which.max(results2$MCC)]] # extracting optimal model from list using MCC as metrics of choice
{ # Variable importance plot
  # par(mar = c(4.1, 7, 1, 0.2)) 
  # importance(optimal_model2)|> sort(decreasing = F) |> barplot(horiz = T, las = 1)
  optimal_model2_IMP <- importance(optimal_model2) |> as.data.frame() # convert importance into a data frame
  rownames(optimal_model2_IMP) <- c('Longitude', 'Latitude', 'Year', 'Month',
                                    'LULC', 'NDVI', 'NDMI', 'TP',
                                    'AMT', 'ANSWS', 'ARH', 'Elevation',
                                    'Slope', 'Aspect', 'Lagged Fire') # rename variables
  colnames(optimal_model2_IMP) <- 'Importance' # change column names
  optimal_model2_IMP$Importance <- optimal_model2_IMP$Importance*100 # convert importance to percentage
  optimal_model2_IMP_plot <- ggplot(optimal_model2_IMP, aes(x = reorder(rownames(optimal_model2_IMP), Importance), y = Importance, 
                                                            fill = -Importance)) +
    geom_bar(stat='identity') +
    ggtitle('Variable Importance\n from RFM 2')+
    xlab('')+
    ylab('Overall \nImportance (%)') +
    theme_classic() +
    coord_flip()+
    theme(legend.position = '', axis.title = element_text(size = 8), plot.title = element_text(size = 9),
          axis.text = element_text(size = 8))
  optimal_model2_IMP_plot
}
opt_probs2 <- probabilities_list2[[which.max(results2$MCC)]]
RFM2_training_accuracy <- results2[which.max(results2$MCC),]
MCC_from_optimal_model2 <- MCC_score_list2[[which.max(results2$MCC)]] # list of MCCs for each threshold
RFM2_optimal_MCC <- max(MCC_from_optimal_model2) # chosen MCC for best outcome
RFM2_optimal_threshold <- RFM2_training_accuracy[,'optimal_threshold']

{
  par(mar = c(4.1, 4, .2, 0.2)) # customised margin
  plot(threshold, MCC_from_optimal_model2,
       type = 'b',
       # pch = 19,
       # main = 'Chosen Threshold from Optimal RF Model',
       # cex.main = .9,
       cex.lab = .8,
       cex.axis = .8,
       # cex = .3,
       col = 'seagreen',
       xlab = paste0('Threshold'),
       ylab = 'MCC Score',
       # xlim = c(min(threshold), 0.01)
  )
  points(RFM2_optimal_threshold, 
         RFM2_optimal_MCC, 
         pch = 19, cex = .75, col = 'seagreen')
  # points(results2$optimal_threshold[which.max(results2$AUC_PR)], 
  #        results2$F1_score[which.max(results2$AUC_PR)], 
  #        pch = 19, cex = .5, col = 'greenyellow')
  # abline(v=results2$optimal_threshold[which.max(results2$AUC_PR)],
  #        h=results2$F1_score[which.max(results2$AUC_PR)],
  #        lty = "dashed",
  #        col= 'greenyellow')
  # text(results2$optimal_threshold[which.max(results2$AUC_PR)]+.02, 
  #      results2$F1_score[which.max(results2$AUC_PR)]-.03, 
  #      labels=paste("Threshold = ", results2$optimal_threshold[which.max(results2$AUC_PR)]|>round(3)),
  #      cex=.6,
  #      col="seagreen",
  #      srt=270)
  # text(results2$optimal_threshold[which.max(results2$AUC_PR)]-.3, 
  #      results2$F1_score[which.max(results2$AUC_PR)]-.0015, 
  #      labels=paste("F1 Score = ", results2$F1_score[which.max(results2$AUC_PR)]|>round(3)),
  #      cex=.6,
  #      col="seagreen")
}

# prediction probabilities for each class on test set
test_probs2 <- predict(optimal_model2, data = test_set2[,colnames(test_set2) != 'Fire_Value'])$predictions
test_optimal_threshold2 <- RFM2_optimal_threshold # extracting the optimal threshold used in the optimal model
test_pred_class2 <- ifelse(test_probs2[, '1'] >= test_optimal_threshold2, 1, 0) |> as.factor()
test_metrics2 <- confusionMatrix(test_pred_class2, test_set2$Fire_Value, positive = '1', mode = 'everything')
test_prediction2 <- prediction(as.numeric(test_pred_class2)-1, test_set2$Fire_Value)
test_AUC_ROC2<- performance(test_prediction2, measure = 'auc')@y.values[[1]] # AUC_ROC
test_AUC_PR2 <- performance(test_prediction2, measure = 'aucpr')@y.values[[1]] # AUC_PR
test_MCC2 <- mcc(preds = test_pred_class2, actuals = test_set2$Fire_Value) # computing Matthew's correlation coefficient
test_accuracy2 <- cbind(optimal_threshold = test_optimal_threshold2,
                        binary_accuracy = test_metrics2$overall['Accuracy'][[1]],
                        recall = test_metrics2$byClass['Recall'][[1]],
                        precision = test_metrics2$byClass['Precision'][[1]],
                        specificity = test_metrics2$byClass['Specificity'][[1]],
                        F1_score = test_metrics2$byClass['F1'][[1]], 
                        MCC = test_MCC2,
                        AUC_ROC = test_AUC_ROC2,
                        AUC_PR = test_AUC_PR2,
                        FN = test_metrics2$table[1,2],
                        FP = test_metrics2$table[2,1],
                        TN = test_metrics2$table[1,1],
                        TP = test_metrics2$table[2,2]) |> as.data.frame(); test_accuracy2

# # saving test accuracy
# save(test_probs2,test_optimal_threshold2,test_pred_class2,test_metrics2,
#      test_prediction2,test_AUC_ROC2,test_AUC_PR2,test_MCC2,test_accuracy2,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Non-Buffered Model/Test metrics/rf_models_test_output_2014_2022_resampled_non_buffered_dataset.Rdata')

load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/FINAL MODELS OUTPUT/Random Forest/Non-Buffered Model/Test metrics/rf_models_test_output_2014_2022_resampled_non_buffered_dataset.Rdata')

# WS_visualisation_from_RF(df = test_set2, year = 2021, month = 4, 
#                          test_probs = test_probs2, test_pred_class = test_pred_class2, 
#                          classes_breaks_method = 'natural_breaks')
WS_visualisation_from_RF(df = test_set2, year = 2022, month = 03,
                         test_probs = test_probs2, test_pred_class = test_pred_class2, 
                         classes_breaks_method = 'quantile')

pblapply(1:12, function(x){
  WS_visualisation_from_RF(df = test_set2, year = 2021, month = x, 
                           test_probs = test_probs2, test_pred_class = test_pred_class2, 
                           classes_breaks_method = 'quantile')
})

pblapply(1:12, function(x){
  WS_visualisation_from_RF(df = test_set2, year = 2022, month = x, 
                           test_probs = test_probs2, test_pred_class = test_pred_class2, 
                           classes_breaks_method = 'quantile')
})


# # Final model results from resampled buffered training set [2014-2019], validation set [2020-2021], test set [2022]
# results[which.max(results$AUC_PR),] # training accuracy for Model 0
# test_accuracy # test accuracy from Model 0
# 
# # Final model results from resampled buffered training set [2014-2018], validation set [2019-2020], test set [2021-2022]
# results1[which.max(results1$AUC_PR),] # training accuracy for Model 1
# test_accuracy1 # test accuracy from Model 1
# 
# # Final model results from resampled non-buffered training set [2014-2018], validation set [2019-2020], test set [2021-2022]
# results2[which.max(results2$AUC_PR),] # training accuracy for Model 2
# test_accuracy2 # test accuracy from Model 2
# 
# # Random forest models training accuracy 
# cbind(Model = c('RFM0 (Training set: buffered+undersampled+normalised-2014 to 2019| Val set: normalised-2020 to 2021| Test set: normalised-2022', 
#                 'RFM1 (Training set: buffered+undersampled+normalised-2014 to 2018| Val set: normalised-2019 to 2020| Test set: normalised-2021 to 2022',
#                 'RFM2 (Training set: non-buffered+undersampled+normalised-2014 to 2018| Val set: normalised-2019 to 2020| Test set: normalised-2021 to 2022'),
#       rbind(results[which.max(results$AUC_PR),],
#       results1[which.max(results1$AUC_PR),],
#       results2[which.max(results2$AUC_PR),]))
# 
# # Random forest models test accuracy 
# cbind(Model = c('RFM0 (Training set: buffered+undersampled+normalised-2014 to 2019| Val set: normalised-2020 to 2021| Test set: normalised-2022', 
#                 'RFM1 (Training set: buffered+undersampled+normalised-2014 to 2018| Val set: normalised-2019 to 2020| Test set: normalised-2021 to 2022',
#                 'RFM2 (Training set: non-buffered+undersampled+normalised-2014 to 2018| Val set: normalised-2019 to 2020| Test set: normalised-2021 to 2022'),
#       rbind(test_accuracy,
#       test_accuracy1,
#       test_accuracy2))

VARIMPPLOT <- cowplot::plot_grid(optimal_model1_IMP_plot, optimal_model2_IMP_plot,
                   labels = '', ncol = 2, nrow = 1)
ggsave('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/variable_importance_plots.pdf', plot = VARIMPPLOT,  width = 6.56, height = 3)














 