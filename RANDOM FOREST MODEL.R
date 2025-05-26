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
}

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Importing structured and normalised data for modelling using RF --------

RF_2014to2022_Train <- dfnorm_2014_2022_training_set # random forest training set from 2014 to 2022 dataframe
RF_2014to2022_Train$Month <- as.factor(RF_2014to2022_Train$Month) # converting month to factor
RF_2014to2022_Train$Fire_Value <- as.factor(RF_2014to2022_Train$Fire_Value) # converting fire value to factor

RF_2014to2022_Val <- dfnorm_2014_2022_validation_set  # random forest validation set from 2014 to 2022 dataframe
RF_2014to2022_Val$Month <- as.factor(RF_2014to2022_Val$Month) # converting month to factor
RF_2014to2022_Val$Fire_Value <- as.factor(RF_2014to2022_Val$Fire_Value) # converting fire value to factor

RF_2014to2022_Test <- dfnorm_2014_2022_test_set # random forest test set from 2014 to 2022 dataframe
RF_2014to2022_Test$Month <- as.factor(RF_2014to2022_Test$Month) # converting month to factor
RF_2014to2022_Test$Fire_Value <- as.factor(RF_2014to2022_Test$Fire_Value) # converting fire value to factor



RF_2002to2022_Train <- dfnorm_2002_2022_training_set # random forest training set from 2002 to 2022 dataframe
RF_2002to2022_Train$Month <- as.factor(RF_2002to2022_Train$Month) # converting month to factor
RF_2002to2022_Train$Fire_Value <- as.factor(RF_2002to2022_Train$Fire_Value) # converting fire value to factor

RF_2002to2022_Val<- dfnorm_2002_2022_validation_set # random forest validation set from 2002 to 2022 dataframe
RF_2002to2022_Val$Month <- as.factor(RF_2002to2022_Val$Month) # converting month to factor
RF_2002to2022_Val$Fire_Value <- as.factor(RF_2002to2022_Val$Fire_Value) # converting fire value to factor

RF_2002to2022_Test <- dfnorm_2002_2022_test_set # random forest test set from 2002 to 2022 dataframe
RF_2002to2022_Test$Month <- as.factor(RF_2002to2022_Test$Month) # converting month to factor
RF_2002to2022_Test$Fire_Value <- as.factor(RF_2002to2022_Test$Fire_Value) # converting fire value to factor


subsetTrain <- RF_2014to2022_Train %>%
  filter(Year==2018 & Month==2) %>%
  dplyr::select(-Month)

subsetVal <- RF_2014to2022_Train %>%
  filter(Year==2019 & Month==2) %>%
  dplyr::select(-Month)

subsetTest <- RF_2014to2022_Val %>%
  filter(Year==2020 & Month==2) %>%
  dplyr::select(-Month)


# Applying random forest on subset of my data but applied in the same way we would apply it to our whole dataset

# Split the data
train_set <- subsetTrain
val_set   <- subsetVal
test_set  <- subsetTest

# # full dataset from 2014 to 2022 timeframe
# train_set <- RF_2014to2022_Train
# val_set   <- RF_2014to2022_Val
# test_set  <- RF_2014to2022_Test

# create combinations of hyperparameters
rf_gridsearch <- expand.grid(mtry = 2:(ncol(train_set) - 1),
                             splitrule = c('gini', 'hellinger'), # gini for classification
                             min.node.size=seq(1, 15, 5),
                             stringsAsFactors = F)

# Initialize results storage
results <- data.frame()
probabilities_list <- list()
model_list <- list()

# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)

#  Manual tuning loop
for (i in 1:nrow(rf_gridsearch)) {
  # print(i)
  i <- 1
  params <- rf_gridsearch[i, ]
  
  # Train the model
  rf <- ranger(
    formula = Fire_Value ~ .,
    data = train_set,
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
  probs <- predict(rf, data = val_set[,colnames(val_set) != 'Fire_Value'])$predictions
  actual_class <- val_set$Fire_Value # extract known response variable from validation set
  probs[which(actual_class==1),'1']
  summary(probs[,'1'])
  threshold <- seq(min(probs[,'1']), max(probs[,'1']), by = 0.0001) # generate a sequence of threshold to classify response variable based on probability class
  
  # To allow parallel processing in pbsapply export items used in the function to the cluster
  clusterExport(cl, varlist = c("probs",'threshold','actual_class')) 
  # Loading relevant package on cluster
  clusterEvalQ(cl, library(caret))
  # This returns the F1 scores for each threshold
  F1_SCORES <- pbsapply(seq_along(threshold), function (x){
  pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= threshold[x], 1, 0) |> as.factor()
  return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
  F1_SCORES[is.nan(F1_SCORES)] <- 0 # replace NaN with 0
  optimal_threshold <- threshold[which.max(F1_SCORES)] # which threshold has led to the maximum F1 score
  optimal_f1_score <- max(F1_SCORES[!is.nan(F1_SCORES)]) # extract the maximum F1 score (omitting NaN if there's any)
  final_pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
  plot(threshold, F1_SCORES,
       type = 'l',
       # pch = 19,
       cex.main = .9,
       cex.lab = .9,
       cex.axis = .9,
       # cex = .3,
       col = 'red',
       xlab = 'Threshold',
       ylab = 'F1 Score',
       xlim = c(min(threshold), 0.01)
       )
  points(threshold, F1_SCORES, pch = 19, cex = .2, col = 'red')
  MCC <- mcc(preds = final_pred_class, actuals = actual_class) # computing Matthew's correlation coefficient
  Metrics <- confusionMatrix(final_pred_class, actual_class, positive = '1', mode = 'everything') # generate other metrics from confusion matrix
  
  prediction <- prediction(as.numeric(final_pred_class)-1, actual_class)
  AUC_ROC <- performance(prediction, measure = 'auc')@y.values[[1]] # AUC
  
  # Visualising AUC ROC curve
  # plot(performance(prediction, 'tpr', 'fpr'), colorize = T, xlab = '1-Specificity', ylab = 'Recall')
  # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')

  probabilities_list[[i]] <- probs
  model_list[[i]] <- rf # appending each model to a list
  results <- rbind(results, cbind(params, 
                                  optimal_threshold = optimal_threshold,
                                  precision = Metrics$byClass['Precision'][[1]],
                                  recall = Metrics$byClass['Recall'][[1]],
                                  F1_score = optimal_f1_score, 
                                  AUC_ROC = AUC_ROC,
                                  MCC = MCC))
  
}
stopCluster(cl)
results

optimal_model <- model_list[[which.max(results$AUC_ROC)]] # extracting optimal model from list using AUC_ROC as metrics of choice
opt_probs <- probabilities_list[[which.max(results$AUC_ROC)]]
threshold <- seq(min(opt_probs[,'1']), max(opt_probs[,'1']), by = 0.0001) # generate a sequence of threshold to classify response variable based on probability class
actual_class <- val_set$Fire_Value # extract known response variable from validation set
# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)
# To allow parallel processing in pbsapply export items used in the function to the cluster
clusterExport(cl, varlist = c("opt_probs",'threshold','actual_class')) 
# Loading relevant package on cluster
clusterEvalQ(cl, library(caret))
# This returns the F1 scores for each threshold
F1_SCORES_from_opt_model <- pbsapply(seq_along(threshold), function (x){
  opt_pred_class <- ifelse(opt_probs[, '1'] >= threshold[1] & opt_probs[, '1'] <= threshold[x], 1, 0) |> as.factor()
  return(confusionMatrix(opt_pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
stopCluster(cl)
F1_SCORES_from_opt_model[is.nan(F1_SCORES_from_opt_model)] <- 0 # replace NaN with 0
plot(threshold, F1_SCORES_from_opt_model,
     type = 'l',
     # pch = 19,
     main = 'Chosen Threshold from Optimal RF Model',
     cex.main = .9,
     cex.lab = .9,
     cex.axis = .9,
     # cex = .3,
     col = 'red',
     xlab = 'Threshold',
     ylab = 'F1 Score',
     xlim = c(min(threshold), 0.01))
points(threshold, F1_SCORES_from_opt_model, pch = 19, cex = .2, col = 'red')
# prediction probabilities for each class on test set
test_probs <- predict(optimal_model, data = test_set[,colnames(test_set) != 'Fire_Value'])$predictions
test_optimal_threshold <- results[which.max(results$AUC_ROC),]$optimal_threshold # extracting the optimal threshold used in the optimal model
test_pred_class <- ifelse(test_probs[, '1'] >= test_optimal_threshold, 1, 0) |> as.factor()
test_metrics <- confusionMatrix(test_pred_class, test_set$Fire_Value, positive = '1', mode = 'everything')
test_f1_score <- test_metrics$byClass['F1'][[1]]
test_precision <- test_metrics$byClass['Precision'][[1]]
test_recall <- test_metrics$byClass['Recall'][[1]]
test_prediction <- prediction(as.numeric(test_pred_class)-1, test_set$Fire_Value)
test_AUC_ROC <- performance(test_prediction, measure = 'auc')@y.values[[1]] # AUC
test_MCC <- mcc(preds = test_pred_class, actuals = test_set$Fire_Value) # computing Matthew's correlation coefficient
cbind(optimal_threshold = test_optimal_threshold,
      precision = test_precision|>round(3),
      recall = test_recall|>round(3),
      F1_score = test_f1_score|>round(3), 
      AUC_ROC = test_AUC_ROC|>round(3),
      MCC = test_MCC|>round(3))
test_pred_class

x <- test_set %>%
  # filter(Month==1)%>%
  dplyr::select(x,y,Fire_Value)

xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
# Set color based on the condition
fire_color_condition <- if (all(values(xx) %>% na.omit() == 0)) {
  "lightgray"
} else {
  c("lightgray", "red")
}
plot(xx, col = fire_color_condition, main = 'True-2022/01')
# plot(roi_trans, col = 'transparent', border = 'black', add = T)

x <- cbind(test_set[,1:2], as_tibble(test_pred_class))|>as_tibble()
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = fire_color_condition, cex.main = .9, main = 'Predicted-2022/01 [whole training set from 2014 to 2022 timeframe was used]')
# plot(roi_trans, col = 'transparent', border = 'black', add = T)

# Random forest on undersampled dataset -----------------------------------

# undersampled dataset from 2014 to 2022 timeframe
train_set <- RF_2014to2022_Train_under$data |> as_tibble()
val_set   <- RF_2014to2022_Val_under$data |> as_tibble()
test_set  <- RF_2014to2022_Test

# create combinations of hyperparameters
rf_gridsearch <- expand.grid(mtry = 2:(ncol(train_set) - 1),
                             splitrule = c('gini', 'hellinger'), # gini for classification
                             min.node.size=seq(1, 5, 2),
                             stringsAsFactors = F)

# Initialize results storage
results <- data.frame()
probabilities_list <- list()
model_list <- list()

# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)

#  Manual tuning loop
for (i in 1:nrow(rf_gridsearch)) {
  # print(i)
  i <- 1
  params <- rf_gridsearch[i, ]
  
  # Train the model
  rf <- ranger(
    formula = Fire_Value ~ .,
    data = train_set,
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
  probs <- predict(rf, data = val_set[,colnames(val_set) != 'Fire_Value'])$predictions
  actual_class <- val_set$Fire_Value # extract known response variable from validation set
  summary(probs[,'1']) # probabilities obtained from positive class from RF model
  probs[which(actual_class==1),'1']|>summary() # probabilities where positive is actually true
  
  threshold <- seq(0.01, 1, by = 0.001) # generate a sequence of threshold to classify response variable based on probability class
  
  # To allow parallel processing in pbsapply export items used in the function to the cluster
  clusterExport(cl, varlist = c("probs",'threshold','actual_class')) 
  # Loading relevant package on cluster
  clusterEvalQ(cl, library(caret))
  # This returns the F1 scores for each threshold
  F1_SCORES <- pbsapply(seq_along(threshold), function (x){
    pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= threshold[x], 1, 0) |> as.factor()
    return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
  # F1_SCORES <- pbsapply(seq_along(threshold), function (x){
  #   pred_class <- ifelse(probs[, '1'] >= threshold[x], 1, 0) |> as.factor()
  #   return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case
  
  F1_SCORES[is.nan(F1_SCORES)] <- 0 # replace NaN with 0
  F1_SCORES[is.na(F1_SCORES)] <- 0 # replace NA with 0
  optimal_threshold <- threshold[which.max(F1_SCORES)] # which threshold has led to the maximum F1 score
  optimal_f1_score <- max(F1_SCORES[!is.nan(F1_SCORES)]) # extract the maximum F1 score (omitting NaN if there's any)
  final_pred_class <- ifelse(probs[, '1'] >= threshold[1] & probs[, '1'] <= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
  plot(threshold, F1_SCORES,
       type = 'l',
       # pch = 19,
       cex.main = .9,
       cex.lab = .9,
       cex.axis = .9,
       # cex = .3,
       col = 'red',
       xlab = 'Threshold',
       ylab = 'F1 Score',
       # xlim = c(min(threshold), 0.01)
  )
  points(threshold, F1_SCORES, pch = 19, cex = .2, col = 'red')
  MCC <- mcc(preds = final_pred_class, actuals = actual_class) # computing Matthew's correlation coefficient
  Metrics <- confusionMatrix(final_pred_class, actual_class, positive = '1', mode = 'everything') # generate other metrics from confusion matrix
  
  prediction <- prediction(as.numeric(final_pred_class)-1, actual_class)
  AUC_ROC <- performance(prediction, measure = 'auc')@y.values[[1]] # AUC
  
  # Visualising AUC ROC curve
  # plot(performance(prediction, 'tpr', 'fpr'), colorize = T, xlab = '1-Specificity', ylab = 'Recall')
  # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
  
  probabilities_list[[i]] <- probs
  model_list[[i]] <- rf # appending each model to a list
  results <- rbind(results, cbind(params, 
                                  optimal_threshold = optimal_threshold,
                                  precision = Metrics$byClass['Precision'][[1]],
                                  recall = Metrics$byClass['Recall'][[1]],
                                  F1_score = optimal_f1_score, 
                                  AUC_ROC = AUC_ROC,
                                  MCC = MCC))
  
}
stopCluster(cl)
results

optimal_model <- model_list[[which.max(results$AUC_ROC)]] # extracting optimal model from list using AUC_ROC as metrics of choice
opt_probs <- probabilities_list[[which.max(results$AUC_ROC)]]
threshold <- seq(0.01, 1, by = 0.001) # generate a sequence of threshold to classify response variable based on probability class
actual_class <- val_set$Fire_Value # extract known response variable from validation set
# Detect cores on system and create clusters
cl <- makeCluster(detectCores() - 1)
# To allow parallel processing in pbsapply export items used in the function to the cluster
clusterExport(cl, varlist = c("opt_probs",'threshold','actual_class')) 
# Loading relevant package on cluster
clusterEvalQ(cl, library(caret))

# # This returns the F1 scores for each threshold
# F1_SCORES_from_opt_model <- pbsapply(seq_along(threshold), function (x){
#   opt_pred_class <- ifelse(opt_probs[, '1'] >= threshold[1] & opt_probs[, '1'] <= threshold[x], 1, 0) |> as.factor()
#   return(confusionMatrix(opt_pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case

# This returns the F1 scores for each threshold
F1_SCORES_from_opt_model <- pbsapply(seq_along(threshold), function (x){
  opt_pred_class <- ifelse(opt_probs[, '1'] >= threshold[x], 1, 0) |> as.factor()
  return(confusionMatrix(opt_pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])}, cl = cl)# apply threshold on positive class; 1 in this case

stopCluster(cl)
F1_SCORES_from_opt_model[is.nan(F1_SCORES_from_opt_model)] <- 0 # replace NaN with 0
F1_SCORES_from_opt_model[is.na(F1_SCORES_from_opt_model)] <- 0 # replace NaN with 0
plot(threshold, F1_SCORES_from_opt_model,
     type = 'l',
     # pch = 19,
     main = 'Chosen Threshold from Optimal RF Model',
     cex.main = .9,
     cex.lab = .9,
     cex.axis = .9,
     # cex = .3,
     col = 'red',
     xlab = 'Threshold',
     ylab = 'F1 Score',
     # xlim = c(min(threshold), 0.01)
     )
points(threshold, F1_SCORES_from_opt_model, pch = 19, cex = .2, col = 'red')
# prediction probabilities for each class on test set
test_probs <- predict(optimal_model, data = test_set[,colnames(test_set) != 'Fire_Value'])$predictions
test_optimal_threshold <- results[which.max(results$AUC_ROC),]$optimal_threshold # extracting the optimal threshold used in the optimal model
test_pred_class <- ifelse(test_probs[, '1'] >= threshold[1] & test_probs[, '1'] <= test_optimal_threshold, 1, 0) |> as.factor()
test_metrics <- confusionMatrix(test_pred_class, test_set$Fire_Value, positive = '1', mode = 'everything')
test_f1_score <- test_metrics$byClass['F1'][[1]]
test_precision <- test_metrics$byClass['Precision'][[1]]
test_recall <- test_metrics$byClass['Recall'][[1]]
test_prediction <- prediction(as.numeric(test_pred_class)-1, test_set$Fire_Value)
test_AUC_ROC <- performance(test_prediction, measure = 'auc')@y.values[[1]] # AUC
test_MCC <- mcc(preds = test_pred_class, actuals = test_set$Fire_Value) # computing Matthew's correlation coefficient
cbind(optimal_threshold = test_optimal_threshold,
      precision = test_precision|>round(3),
      recall = test_recall|>round(3),
      F1_score = test_f1_score|>round(3), 
      AUC_ROC = test_AUC_ROC|>round(3),
      MCC = test_MCC|>round(3))
test_pred_class
which(test_set$Fire_Value==1)
test_probs[which(test_set$Fire_Value==1),'1']
x <- test_set %>%
  filter(Month==1)%>%
  dplyr::select(x,y,Fire_Value)

xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
# Set color based on the condition
fire_color_condition <- if (all(values(xx) %>% na.omit() == 0)) {
  "lightgray"
} else {
  c("lightgray", "red")
}
plot(xx, col = fire_color_condition, main = 'True-2022/01')
# plot(roi_trans, col = 'transparent', border = 'black', add = T)

x <- cbind(test_set[,1:2], as_tibble(test_pred_class))|>as_tibble()
xx <- rasterFromXYZ(x, res = c(30,30), crs = crs(roi_trans))
plot(xx, col = fire_color_condition, cex.main = .9, main = 'Predicted-2022/01 [whole training set from 2014 to 2022 timeframe was used]')
# plot(roi_trans, col = 'transparent', border = 'black', add = T)

























#-----------------------------------------------------------------------------------------------------------------------------
# 
# 
# # Applying random forest on Penguin data but applied in the same way we would apply it to our dataset
# library(palmerpenguins)  # Penguins dataset
# data(penguins, package = "palmerpenguins")
# # Remove rows with missing data for simplicity
# penguins <- na.omit(penguins)
# 
# str(penguins)
# penguins$year|>table()
# View(penguins)
# 
# 
# # make problem into only interested in predicting the Gentoo species (i.e, turn dataset into a binary problem)
# penguins_binary <- penguins %>%
#   mutate(Species_bn = as.factor(case_when(species=='Gentoo'~1, # make Gentoo = 1...
#                                           T ~ 0))) %>% # Adelie and Chinstrap are set to 0
#   dplyr::select(-species)
# 
# str(penguins_binary)
# 
# penguins_binary$year|>table()
# # Split the data
# train_set <- penguins_binary%>%filter(year==2009)
# val_set   <- penguins_binary%>%filter(year==2008)
# test_set  <- penguins_binary%>%filter(year==2007)
# 
# # create combinations of hyperparameters
# rf_gridsearch_trial <- expand.grid(mtry = 2:(ncol(penguins_binary) - 1),
#                                    splitrule = c('gini', 'hellinger'), 
#                                    min.node.size=c(1, 5, 10, 20),
#                                    stringsAsFactors = F)
# 
# # 4. Initialize results storage
# results <- data.frame()
# 
# model_list <- list()
# # 5. Manual tuning loop
# for (i in 1:nrow(rf_gridsearch_trial)) {
#   print(i)
#   params <- rf_gridsearch_trial[i, ]
#   
#   # Train the model
#   rf <- ranger(
#     formula = Species_bn ~ .,
#     data = train_set,
#     probability = T,  # to get class probabilities
#     classification = T,
#     oob.error = F,
#     mtry = params$mtry,
#     splitrule = params$splitrule,
#     min.node.size = params$min.node.size,
#     num.trees = 500,
#     verbose = T,
#     seed = 1
#   )
#   
#   # Predict probabilities on validation set
#   probs <- predict(rf, data = val_set[,colnames(val_set) != 'Species_bn'])$predictions
#   actual_class <- val_set$Species_bn # extract known response variable
#   
#   threshold <- seq(0.2,0.6, by = 0.005) # generate a sequence of threshold to classify response variable based on probability class
#   # This returns the F1 scores for each threshold
#   F1_SCORES <- pbsapply(seq_along(threshold), function (x){pred_class <- ifelse(probs[, '1'] >= threshold[x], 1, 0) |> as.factor()
#   return(confusionMatrix(pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])})# apply threshold on positive class; 1 in this case
#   
#   optimal_threshold <- threshold[which.max(F1_SCORES)] # which threshold has led to the maximum F1 score
#   optimal_f1_score <- max(F1_SCORES) # extract the maximum F1 score
#   final_pred_class <- ifelse(probs[, '1'] >= optimal_threshold, 1, 0) |> as.factor() # recalculate the final predicted class again using the optimal threshold
#   # plot(threshold, F1_SCORES,
#   #      type = 'b',
#   #      pch = 19,
#   #      cex = .3,
#   #      col = 'red',
#   #      xlab = 'Threshold',
#   #      ylab = 'F1 Score')
#   MCC <- mcc(preds = final_pred_class, actuals = actual_class) # computing Matthew's correlation coefficient
#   Metrics <- confusionMatrix(final_pred_class, actual_class, positive = '1', mode = 'everything') # generate other metrics from confusion matrix
#   
#   prediction <- prediction(as.numeric(final_pred_class)-1, actual_class)
#   AUC_ROC <- performance(prediction, measure = 'auc')@y.values[[1]] # AUC
#   
#   # Visualising AUC ROC curve
#   # plot(performance(prediction, 'tpr', 'fpr'), colorize = F, xlab = '1-Specificity', ylab = 'Recall')
#   # lines(c(0,1), c(0,1), lty = 'dotted', col = 'darkgray')
#   
#   model_list[[i]] <- rf # appending each model to a list
#   results <- rbind(results, cbind(params, 
#                                   optimal_threshold = optimal_threshold,
#                                   precision = Metrics$byClass['Precision'][[1]]|>round(3),
#                                   recall = Metrics$byClass['Recall'][[1]]|>round(3),
#                                   F1_score = optimal_f1_score|>round(3), 
#                                   AUC_ROC = AUC_ROC|>round(3),
#                                   MCC = MCC|>round(3)))
#   
#   
# }
# 
# results
# 
# optimal_model <- model_list[[which.max(results$MCC)]] # extracting optimal model from list using MCC as metrics of choice
# opt_probs <- predict(optimal_model, data = val_set[,colnames(val_set) != 'Species_bn'])$predictions # rerun prediction for optimal model
# # This returns the F1 scores for each threshold
# F1_SCORES_from_opt_model <- pbsapply(seq_along(threshold), function (x){opt_pred_class <- ifelse(opt_probs[, '1'] >= threshold[x], 1, 0) |> as.factor()
# return(confusionMatrix(opt_pred_class, actual_class, positive = '1', mode = 'everything')$byClass['F1'][[1]])})# apply threshold on positive class; 1 in this case
# plot(threshold, F1_SCORES_from_opt_model,
#      type = 'b',
#      pch = 19,
#      main = 'Chosen Threshold from Optimal RF Model',
#      cex.main = .9,
#      cex.lab = .9,
#      cex.axis = .9,
#      cex = .3,
#      col = 'red',
#      xlab = 'Threshold',
#      ylab = 'F1 Score')
# 
# # prediction probabilities for each class
# test_probs <- predict(optimal_model, data = test_set[,colnames(test_set) != 'Species_bn'])$predictions
# test_optimal_threshold <- results[which.max(results$MCC),]$optimal_threshold # extracting the optimal threshold used in the optimal model
# test_pred_class <- ifelse(test_probs[, '1'] >= test_optimal_threshold, 1, 0) |> as.factor()
# test_metrics <- confusionMatrix(test_pred_class, test_set$Species_bn, positive = '1', mode = 'everything')
# test_f1_score <- test_metrics$byClass['F1'][[1]]
# test_precision <- test_metrics$byClass['Precision'][[1]]
# test_recall <- test_metrics$byClass['Recall'][[1]]
# test_prediction <- prediction(as.numeric(test_pred_class)-1, test_set$Species_bn)
# test_AUC_ROC <- performance(test_prediction, measure = 'auc')@y.values[[1]] # AUC
# test_MCC <- mcc(preds = test_pred_class, actuals = test_set$Species_bn) # computing Matthew's correlation coefficient
# 
# 
# cbind(optimal_threshold = test_optimal_threshold,
#       precision = test_precision|>round(3),
#       recall = test_recall|>round(3),
#       F1_score = test_f1_score|>round(3), 
#       AUC_ROC = test_AUC_ROC|>round(3),
#       MCC = test_MCC|>round(3))
#       
      
      