{
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
  library(performanceEstimation)
  library(leaflet)
  library(leafem)
  library(mapview)
}

# read aerial imagery and training samples filenames
aerial_imagery_filenames <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Unprocessed Variables/Aerial Imagery 2014-2023 (with interpolation)', pattern = 'tif')
training_samples_filenames <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Training Samples 2014-2023', pattern = '\\.shp$')

RF_supervised_classification <- function(index, plotPCAImagery=NULL, plottrainingsamples=NULL, Screeplot=NULL, plotLULC=NULL){
  index <- index # call for specific raster
  # read rasterBrick
  aerial_imagery <- rast(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Unprocessed Variables/Aerial Imagery 2014-2023 (with interpolation)/', aerial_imagery_filenames[index]))
  names(aerial_imagery) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename layers

  
  # IMAGE ENHANCEMENT/REDUCTION THROUGH PCA ---------------------------------
  
  # convert raster to data frame to apply pca
  aerial_imagery_df <- as.data.frame(aerial_imagery, xy = T) |> na.omit()
  pca <- prcomp(aerial_imagery_df[,c(-1,-2)], scale. = T) # apply pca
  # pca$rotation # loadings
  # summary(pca)
  # cor(aerial_imagery_df[,c(-1,-2)]) %>% mean() # check if pca was valid (all values were close to 0 showing independence- therefore valid)
  if(Screeplot==T){
    par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
    print(fviz_eig(pca))
  }
  
  # convert applied pca dataframe to raster
  aerial_imagery_pca <- rasterFromXYZ(cbind(aerial_imagery_df[,1:2], 
                                            pca$x[,1:3]), # extract 3 PCs as it explains most of the data
                                      crs = crs(roi_trans))
  aerial_imagery_pca@file@name <- gsub("\\.tif$", "", aerial_imagery_filenames[index]) # rename raster
  names(aerial_imagery_pca) <- c('PC1_B', 'PC2_G', 'PC2_R') # rename raster layers
  
  training_samples <- readOGR(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Training Samples 2014-2023/', training_samples_filenames[index])) # import this way to ignore any Z value
  training_samples_df <- st_as_sf(training_samples) %>%  # convert to spatial dataframe
    st_buffer(dist = 0) %>% # correct for geometry if there's any error
    mutate(Classname= as.factor(Classname), # convert class label into factor
           Classcode=1:nrow(training_samples)) 
  
  # if all classes are present
  if(length(levels(training_samples_df$Classname)) == 7){
    LULCpal <-  c('#883C07', '#CCCCCC', '#00734C', '#D1FF73', '#000000', '#70A800', '#00A9E6')
    # if cloud cover is not part of the training samples
  }else if(length(levels(training_samples_df$Classname)) == 6 & levels(training_samples_df$Classname)[2] != 'Cloud Cover'){
    LULCpal <-  c('#883C07', '#00734C', '#D1FF73', '#000000', '#70A800', '#00A9E6')
    # if shadow is not part of the training samples
  }else if(length(levels(training_samples_df$Classname)) == 6 & levels(training_samples_df$Classname)[5] != 'Shadow'){
    LULCpal <-   c('#883C07', '#CCCCCC', '#00734C', '#D1FF73', '#70A800', '#00A9E6')
  }else if(length(levels(training_samples_df$Classname)) == 5){
    LULCpal <- c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6')
  }
  
  # plot PCA enhanced imagery with training samples
  if(plotPCAImagery==T){
    par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
    # Visualisation of PCA version of aerial imagery
    print(
      plotRGB(aerial_imagery_pca, r=3 , g=2 , b=1, 
              stretch = 'lin', 
              margin = T,
              main = as.Date(gsub("\\.tif$", "", aerial_imagery_filenames[index]), format = "%Y%m%d"))
    )
    
    if(plottrainingsamples==T){
      print(plot(training_samples_df, add = T, col = training_samples_df$Classname))
            print(legend("topleft", 
                   legend = training_samples_df$Classname,
                   fill = training_samples_df$Classname,
                   cex = .7))
    }
    
  }
  
  # Extraction of RasterLayer values
  # extract raster values at each polygon
  extract <- extract(aerial_imagery_pca, training_samples_df, df = T)
  extractMerged <- inner_join(extract,training_samples_df,by= c("ID"="Classcode")) %>%
    select(ID, PC1_B, PC2_G, PC2_R, Classname) %>% # select only relevant columns
    na.omit() # omit NA cells
  
  # Data Partitioning
  set.seed(123) 
  extractMerged_shuffle <- extractMerged[sample(1:nrow(extractMerged)), ] # shuffle rows in dataset
  trainIndex <- caret::createDataPartition(extractMerged_shuffle$ID,list = FALSE,p=0.8)
  trainData <- extractMerged_shuffle[trainIndex,]  # 80% for training Data
  testData <- extractMerged_shuffle[-trainIndex,] # 20% for testing Data
  
  # Model Training
  respVar <- c("Classname")
  predVar <- c("PC1_B","PC2_G","PC2_R")
  
  set.seed(123)
  cvControl <- caret::trainControl(method = 'cv',
                                   number = 5,  # 5 fold CV
                                   savePredictions = T,
                                   verboseIter = T)
  
  # Train model using random Forest algorithm
  set.seed(123)
  rfModel <- caret::train(trainData[,predVar],
                          trainData[,respVar],
                          method="rf",
                          metric = "Kappa",
                          ntree= 500,
                          trControl= cvControl,
                          tuneLength=6,
                          importance=TRUE)
  
  # Model Evaluation using Test Data
  rfPredict <- predict(rfModel,testData)
  accuracy_assessment <- confusionMatrix(rfPredict,testData$Classname)
  
  # Spatial Prediction
  LULCPredictions<- raster::predict(aerial_imagery_pca, rfModel) 
  LULCPredictions@file@name <- paste0('LULC ', gsub("\\.tif$", "", aerial_imagery_filenames[index])) # rename LULC layer
  
  
  if(plotLULC==T){
    # visualise LULC
    print(
      tm_shape(LULCPredictions)+
        tm_raster(style = "cat", title = "", palette = LULCpal)+ 
        tm_layout(main.title= paste0(as.Date(gsub("\\.tif$", "", aerial_imagery_filenames[index]), format = "%Y%m%d"), ' LULC'),
                  main.title.size =.9,
                  main.title.position = c("center", "top"))+
        tm_graticules(lines = F)
    )

  }
  
  return(list(PCA_Summary = summary(pca), # return PCA info.
              random_forest_model = rfModel, # return rfModel
              Test_accuracy = accuracy_assessment$overall[1:2], # return overall and kappa accuracy
              LULCRaster = LULCPredictions, # return LULC raster
              LULC_nclass = nlevels(training_samples_df$Classname))) # number of classes for the LULC
} 
# apply random forest for lulc classification and calculate relevant metrics
testingRF <- RF_supervised_classification(index = 57, 
                             plotPCAImagery=T, 
                             plottrainingsamples=T, 
                             Screeplot=T, 
                             plotLULC=T)

# apply random forest for lulc classification and calculate relevant metrics in bulk
LULC_raster_list <- pblapply(seq_along(training_samples_filenames), function(x){RF_supervised_classification(index = x, 
                                                                                                             plotPCAImagery=T, 
                                                                                                             plottrainingsamples=T, 
                                                                                                             Screeplot=T, 
                                                                                                             plotLULC=T)})

# Save object
# save(LULC_raster_list, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (with interpolation)/LULC_raster_list.Rdata')

# extracting the LULC rasters only
LULC <- lapply(seq_along(training_samples_filenames), function(x){LULC_raster_list[[x]]$LULCRaster})

# {
#   # Save rasters in one folder on local machine or hard drive
#   Save_raster <- function(data, index, path){
#     
#     file_path <- paste0(path, data[[index]]@file@name)
#     
#     return(writeRaster(data[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
#   
#   # Bulk Save!!!!
#   pblapply(seq_along(training_samples_filenames),
#            function(x) {Save_raster(data = LULC,
#                                     index = x,
#                                     path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (with interpolation)/')})
#   
# }



# Removing shadows and cloud cover, then interpolating --------------------
# Check which LULCs have shadow
sapply(seq_along(LULC), function (x){
  any(LULC[[x]]@data@attributes[[1]]$value == 'Shadow') # Is any of the classes labelled as shadows?
}) # all LULCs contain shadows

#Check which LULCs have cloud cover
LULC_CloudCover_index <- sapply(seq_along(LULC), function (x){
  any(LULC[[x]]@data@attributes[[1]]$value == 'Cloud Cover') # Is any of the classes labelled as cloud cover?
})|>which() # which of the LULC contains cloud cover

# extract the remaining indices/LULC which contain shadows but not cloud cover
LULC_NOCloudCover_index <- which(!seq_along(LULC) %in% LULC_CloudCover_index)

# Quick visualisation of all the LULC containing shadows and cloud cover
lapply(seq_along(LULC_CloudCover_index), function(x){
  tm_shape(LULC[[LULC_CloudCover_index[x]]])+
    tm_raster(style = "cat", title = "", palette = 
                if(nrow(LULC[[1]]@data@attributes[[1]]) == 7){
                  LULCpal <-  c('#883C07', '#CCCCCC', '#00734C', '#D1FF73', '#000000', '#70A800', '#00A9E6')
                  # if cloud cover is not part of the training samples
                }else if(nrow(LULC[[1]]@data@attributes[[1]]) == 6 & x@data@attributes[[1]]$value[2] != 'Cloud Cover'){
                  LULCpal <-  c('#883C07', '#00734C', '#D1FF73', '#000000', '#70A800', '#00A9E6')
                  # if shadow is not part of the training samples
                }else if(nrow(LULC[[1]]@data@attributes[[1]]) == 6 & levels(training_samples_df$Classname)[5] != 'Shadow'){
                  LULCpal <-   c('#883C07', '#CCCCCC', '#00734C', '#D1FF73', '#70A800', '#00A9E6')
                }else if(nrow(LULC[[1]]@data@attributes[[1]]) == 5){
                  LULCpal <- c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6')
                }
    )+
    tm_layout(main.title= paste0('LULC ', gsub("\\.tif$", "", aerial_imagery_filenames[LULC_CloudCover_index[x]])), # rename LULC layer,
              main.title.size =.9,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5)+
    tm_graticules(lines = F)
})

# FUNCTION THAT REMOVE SHADOWS AND REPLACE IT BY NEAREST NEIGHBOUR PIXELS
Imputating_shadows_from_7classesLULC <- function(index, plt_mask=NULL, plt_imputed = NULL){
  index <- index
  initial_LULC <- LULC[[LULC_CloudCover_index[index]]];levels(initial_LULC) # read in LULC data
  shadow_index <- which(values(initial_LULC)==5) # this only applies for LULC with 7 classes (i.e, also containing cloud cover)
  initial_LULC_mask <- mask(initial_LULC, initial_LULC, maskvalue = which(levels(initial_LULC)[[1]]$value == 'Shadow')) # mask shadow pixels
  levels(initial_LULC_mask)[[1]] <- levels(initial_LULC_mask)[[1]] %>% slice(-which(levels(initial_LULC_mask)[[1]]$value == 'Shadow')) # redefine levels (i.e, exclude shadows)
  # levels(initial_LULC_mask)
  
  if(plt_mask==T){
    print(
      # Visualise masked LULC
      tm_shape(initial_LULC_mask)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', '#CCCCCC', '#00734C', '#D1FF73', 'white','#70A800', '#00A9E6'))+
        tm_layout(main.title= paste0('LULC ', gsub("\\.tif$", "", aerial_imagery_filenames[LULC_CloudCover_index[index]]), ' (masked)') ,
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }

  
  # Convert the masked LULC into a dataframe to modify the classes
  initial_LULC_mask_df <- as.data.frame(initial_LULC_mask, xy = T)%>%
    mutate(layer_value = factor(case_when(layer_value=='Bare Land' ~ 1,
                                          layer_value=='Cloud Cover'~2,
                                          layer_value=='Forest & Thicket'~3,
                                          layer_value=='Grass Land'~4,
                                          # layer_value=='Shadow'~5,
                                          layer_value=='Shrub Land'~5,
                                          layer_value=='Water Bodies'~6))) 
  
  # str(initial_LULC_mask_df)
  
  # Convert the updated LULC dataframe into raster again
  initial_LULC_mask_raster <- rasterFromXYZ(initial_LULC_mask_df) 
  crs(initial_LULC_mask_raster) <- crs(roi_trans) # redefine crs
  initial_LULC_mask_raster <- ratify(initial_LULC_mask_raster) # make raster as a factor raster
  levels(initial_LULC_mask_raster) <- data.frame(ID = c(1,2,3,4,5,6), Classes = c('Bare Land', 'Cloud Cover', 'Forest & Thicket', 'Grass Land', 'Shrub Land', 'Water Bodies')) # redefine levels
  
  # Define a function to find the mode (most frequent value) in the neighborhood
  mode_fun <- function(x) {
    tab <- table(x, useNA = "no")  # Count class occurrences (ignore NA)
    if (length(tab) == 0) return(NA)  # Return NA if all neighbors are NA
    as.numeric(names(tab)[which.max(tab)])  # Return most frequent class
  }
  
  # Impute masked shadows with modal neighbouring values 
  raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T); cat('Iteration:', 1, '\n')
  raster_imputation_mask <- raster_imputation |> crop(roi_trans)|>mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
  
  if(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){ # Is there still any of the shadow pixels which are NAs- if so, keep imputing by taking the previously imputed raster
    # Iterate imputation process until all shadow pixels are imputed
    i = 2
    cat('Iteration:', i, '\n')
    while(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){
      
      initial_LULC_mask_raster <- raster_imputation_mask
      # Impute masked shadows with modal neighbouring values 
      raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T)
      raster_imputation_mask <- raster_imputation |> crop(roi_trans)|>mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
      i = i+1; cat('Iteration:', i, '\n')
      
      # sometimes masked pixels get too little that the dimension of the matrix above is too big and must change to a smaller one.
      if(i==10){ # after 10 iterations resize the kernel to make it smaller
        # Iterate imputation process until all shadow pixels are imputed
        while(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){
          
          initial_LULC_mask_raster <- raster_imputation_mask
          # Impute masked shadows with modal neighbouring values 
          raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 3, 3), fun = mode_fun, NAonly = T)
          raster_imputation_mask <- raster_imputation |> crop(roi_trans)|>mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
          i = i+1; cat('Iteration:', i, '\n')
          
          if(i==15){ # if imputation does not stop-i.e, failed to impute with neighbouring pixels-default to value 5 representing shrubland
            while(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){
              values(raster_imputation_mask)[shadow_index][which(is.na(values(raster_imputation_mask)[shadow_index]))] <- 5
            }
          }
        }
      }
    }
    
  }else {raster_imputation_mask}
  
  # any(is.na(values(raster_imputation_mask)[shadow_index])) # Is there still any of the shadow pixels which are NAs
  # sum(is.na(values(raster_imputation_mask)[shadow_index]))
  
  raster_imputation_mask <- raster_imputation_mask|>ratify() # make raster a factor raster again
  levels(raster_imputation_mask) <- data.frame(ID = c(1,2,3,4,5,6), Classes = c('Bare Land', 'Cloud Cover', 'Forest & Thicket', 'Grass Land', 'Shrub Land', 'Water Bodies')) # redefine levels
  raster_imputation_mask@file@name <- paste0('LULC ', gsub("\\.tif$", "", aerial_imagery_filenames[LULC_CloudCover_index[index]]))  # rename LULC layer
  
  if(plt_imputed==T){
    print(
      # Visualise imputed LULC
      tm_shape(raster_imputation_mask)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', '#CCCCCC', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
        tm_layout(main.title= raster_imputation_mask@file@name,
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }
  return(raster_imputation_mask)
}

# List to output removal of shadow first from LULC which contained shadows and still contain cloud cover
LULC_imputed_from_7classesLULC_list <- pblapply(seq_along(LULC_CloudCover_index), function(x){
  Imputating_shadows_from_7classesLULC(index = x, plt_mask=T, plt_imputed = T)
})

# # Save output as an .Rdata file
# save(LULC_imputed_from_7classesLULC_list,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (post-processing)/LULC_imputed_from_7classesLULC_list.Rdata')

# FUNCTION THAT REMOVE CLOUD COVER AND REPLACE IT BY NEAREST NEIGHBOUR PIXELS- taking output from previous function
Imputing_cloudcover_from_7classesLULC <- function(index, plt_mask=NULL, plt_imputed = NULL){
  index <- index
  initial_LULC <- LULC_imputed_from_7classesLULC_list[[index]];levels(initial_LULC) # read in LULC data
  CC_index <- which(values(initial_LULC)==2) # this only applies for LULC originally having 7 classes containing cloud cover
  initial_LULC_mask <- mask(initial_LULC, initial_LULC, maskvalue = which(levels(initial_LULC)[[1]]$Classes == 'Cloud Cover')) # mask cloud cover pixels
  levels(initial_LULC_mask)[[1]] <- levels(initial_LULC_mask)[[1]] %>% slice(-which(levels(initial_LULC_mask)[[1]]$Classes == 'Cloud Cover')) # redefine levels (i.e, exclude cloud cover)
 
  if(plt_mask==T){
    print(
      # Visualise masked LULC
      tm_shape(initial_LULC_mask)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', 'white', '#00734C', '#D1FF73','#70A800', '#00A9E6'))+
        tm_layout(main.title= paste0(LULC_imputed_from_7classesLULC_list[[index]]@file@name, ' (masked)'),
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }
  
  
  # Convert the masked LULC into a dataframe to modify the classes
  initial_LULC_mask_df <- as.data.frame(initial_LULC_mask, xy = T)%>%
    mutate(layer_Classes = factor(case_when(layer_Classes=='Bare Land' ~ 1,
                                            # layer_Classes=='Cloud Cover'~2,
                                            layer_Classes=='Forest & Thicket'~2,
                                            layer_Classes=='Grass Land'~3,
                                            layer_Classes=='Shrub Land'~4,
                                            layer_Classes=='Water Bodies'~5))) 
  
  # str(initial_LULC_mask_df)
  
  # Convert the updated LULC dataframe into raster again
  initial_LULC_mask_raster <- rasterFromXYZ(initial_LULC_mask_df) 
  crs(initial_LULC_mask_raster) <- crs(roi_trans) # redefine crs
  initial_LULC_mask_raster <- ratify(initial_LULC_mask_raster) # make raster as a factor raster
  levels(initial_LULC_mask_raster) <- data.frame(ID = c(1,2,3,4,5), Classes = c('Bare Land', 'Forest & Thicket', 'Grass Land', 'Shrub Land', 'Water Bodies')) # redefine levels
  
  # Define a function to find the mode (most frequent value) in the neighborhood
  mode_fun <- function(x) {
    tab <- table(x, useNA = "no")  # Count class occurrences (ignore NA)
    if (length(tab) == 0) return(NA)  # Return NA if all neighbors are NA
    as.numeric(names(tab)[which.max(tab)])  # Return most frequent class
  }
  
  # Impute masked cloud cover with modal neighbouring values 
  raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T); cat('Iteration:', 1, '\n')
  raster_imputation_mask <- raster_imputation |> crop(roi_trans)|>mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
  
  if(any(is.na(values(raster_imputation_mask)[CC_index]))==T){ # Is there still any of the cloud cover pixels which are NAs- if so, keep imputing by taking the previously imputed raster
    # Iterate imputation process until all cloud cover pixels are imputed
    i = 2
    cat('Iteration:', i, '\n')
    while(any(is.na(values(raster_imputation_mask)[CC_index]))==T){
      
      initial_LULC_mask_raster <- raster_imputation_mask
      # Impute masked cloud cover with modal neighbouring values 
      raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T)
      raster_imputation_mask <- raster_imputation |> crop(roi_trans)|> mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
      i = i+1; cat('Iteration:', i, '\n')
      
      # sometimes masked pixels get too little that the dimension of the matrix above is too big and must change to a smaller one.
      if(i==15){ # after 15 iterations resize the kernel to make it smaller
        # Iterate imputation process until all cloud cover pixels are imputed
        while(any(is.na(values(raster_imputation_mask)[CC_index]))==T){
          
          initial_LULC_mask_raster <- raster_imputation_mask
          # Impute masked cloud cover with modal neighbouring values 
          raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 3, 3), fun = mode_fun, NAonly = T)
          raster_imputation_mask <- raster_imputation |> crop(roi_trans)|>mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
          i = i+1; cat('Iteration:', i, '\n')
          
          if(i==20){ # if imputation does not stop-i.e, failed to impute with neighbouring pixels-default to value 4 representing shrubland
            while(any(is.na(values(raster_imputation_mask)[CC_index]))==T){
              values(raster_imputation_mask)[CC_index][which(is.na(values(raster_imputation_mask)[CC_index]))] <- 4
            }
          }
        }
      }
    }
    
  }else {raster_imputation_mask}
  
  # any(is.na(values(raster_imputation_mask)[CC_index])) # Is there still any of the cloud cover pixels which are NAs
  # sum(is.na(values(raster_imputation_mask)[CC_index]))
  
  raster_imputation_mask <- raster_imputation_mask|>ratify() # make raster a factor raster again
  levels(raster_imputation_mask) <- data.frame(ID = c(1,2,3,4,5), Classes = c('Bare Land', 'Forest & Thicket', 'Grass Land', 'Shrub Land', 'Water Bodies')) # redefine levels
  raster_imputation_mask@file@name <- LULC_imputed_from_7classesLULC_list[[index]]@file@name  # rename LULC layer
  
  if(plt_imputed==T){
    print(
      # Visualise imputed LULC
      tm_shape(raster_imputation_mask)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
        tm_layout(main.title= raster_imputation_mask@file@name,
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }
  return(raster_imputation_mask)
}

# List to output removal of cloud cover after removal of shadows
LULC_imputed_NO_shadows_CC_list <- pblapply(seq_along(LULC_CloudCover_index), function(x){
  Imputing_cloudcover_from_7classesLULC(index = x, plt_mask=T, plt_imputed = T)
})

# Save output as an .Rdata file
# save(LULC_imputed_NO_shadows_CC_list,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (post-processing)/LULC_imputed_NO_shadows_CC_list.Rdata')

# FUNCTION THAT REMOVE SHADOWS AND REPLACE IT BY NEAREST NEIGHBOUR PIXELS- on remaining LULC which do not contain cloud cover
Imputing_shadows_from_6classesLULC <- function(index, plt_mask=NULL, plt_imputed = NULL){
  index <- index
  initial_LULC <- LULC[[LULC_NOCloudCover_index[index]]];levels(initial_LULC) # read in LULC data
  shadow_index <- which(values(initial_LULC)==4) # this only applies for LULC with 6 classes (i.e, also containing cloud cover)
  initial_LULC_mask <- mask(initial_LULC, initial_LULC, maskvalue = which(levels(initial_LULC)[[1]]$value == 'Shadow')) # mask shadow pixels
  levels(initial_LULC_mask)[[1]] <- levels(initial_LULC_mask)[[1]] %>% slice(-which(levels(initial_LULC_mask)[[1]]$value == 'Shadow')) # redefine levels (i.e, exclude shadows)
  # levels(initial_LULC_mask)
  
  if(plt_mask==T){
    print(
      # Visualise masked LULC
      tm_shape(initial_LULC_mask)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', 'white','#70A800', '#00A9E6'))+
        tm_layout(main.title= paste0('LULC ', gsub("\\.tif$", "", aerial_imagery_filenames[LULC_NOCloudCover_index[index]]), ' (masked)') ,
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }
  
  
  # Convert the masked LULC into a dataframe to modify the classes
  initial_LULC_mask_df <- as.data.frame(initial_LULC_mask, xy = T)%>%
    mutate(layer_value = factor(case_when(layer_value=='Bare Land' ~ 1,
                                          layer_value=='Forest & Thicket'~2,
                                          layer_value=='Grass Land'~3,
                                          # layer_value=='Shadow'~4,
                                          layer_value=='Shrub Land'~4,
                                          layer_value=='Water Bodies'~5))) 
  
  # str(initial_LULC_mask_df)
  
  # Convert the updated LULC dataframe into raster again
  initial_LULC_mask_raster <- rasterFromXYZ(initial_LULC_mask_df) 
  crs(initial_LULC_mask_raster) <- crs(roi_trans) # redefine crs
  initial_LULC_mask_raster <- ratify(initial_LULC_mask_raster) # make raster as a factor raster
  levels(initial_LULC_mask_raster) <- data.frame(ID = c(1,2,3,4,5), Classes = c('Bare Land', 'Forest & Thicket', 'Grass Land', 'Shrub Land', 'Water Bodies')) # redefine levels
  
  # Define a function to find the mode (most frequent value) in the neighborhood
  mode_fun <- function(x) {
    tab <- table(x, useNA = "no")  # Count class occurrences (ignore NA)
    if (length(tab) == 0) return(NA)  # Return NA if all neighbors are NA
    as.numeric(names(tab)[which.max(tab)])  # Return most frequent class
  }
  
  # Impute masked shadows with modal neighbouring values 
  raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T); cat('Iteration:', 1, '\n')
  raster_imputation_mask <- raster_imputation |> crop(roi_trans)|> mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
  
  if(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){ # Is there still any of the shadow pixels which are NAs- if so, keep imputing by taking the previously imputed raster
    # Iterate imputation process until all shadow pixels are imputed
    i = 2
    cat('Iteration:', i, '\n')
    while(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){
      
      initial_LULC_mask_raster <- raster_imputation_mask
      # Impute masked shadows with modal neighbouring values 
      raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T)
      raster_imputation_mask <- raster_imputation |> crop(roi_trans)|> mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
      i = i+1; cat('Iteration:', i, '\n')
      
      # sometimes masked pixels get too little that the dimension of the matrix above is too big and must change to a smaller one.
      if(i==10){ # after 10 iterations resize the kernel to make it smaller
        # Iterate imputation process until all shadow pixels are imputed
        while(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){
          
          initial_LULC_mask_raster <- raster_imputation_mask
          # Impute masked shadows with modal neighbouring values 
          raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 3, 3), fun = mode_fun, NAonly = T)
          raster_imputation_mask <- raster_imputation |> crop(roi_trans)|>mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
          i = i+1; cat('Iteration:', i, '\n')
          
          if(i==15){ # if imputation does not stop-i.e, failed to impute with neighbouring pixels-default to value 4 representing shrubland
            while(any(is.na(values(raster_imputation_mask)[shadow_index]))==T){
              values(raster_imputation_mask)[shadow_index][which(is.na(values(raster_imputation_mask)[shadow_index]))] <- 4
            }
          }
        }
      }
    }
    
  }else {raster_imputation_mask}
  
  # any(is.na(values(raster_imputation_mask)[shadow_index])) # Is there still any of the shadow pixels which are NAs
  # sum(is.na(values(raster_imputation_mask)[shadow_index]))
  
  raster_imputation_mask <- raster_imputation_mask|>ratify() # make raster a factor raster again
  levels(raster_imputation_mask) <- data.frame(ID = c(1,2,3,4,5), Classes = c('Bare Land', 'Forest & Thicket', 'Grass Land', 'Shrub Land', 'Water Bodies')) # redefine levels
  raster_imputation_mask@file@name <- paste0('LULC ', gsub("\\.tif$", "", aerial_imagery_filenames[LULC_NOCloudCover_index[index]]))  # rename LULC layer
  
  if(plt_imputed==T){
    print(
      # Visualise imputed LULC
      tm_shape(raster_imputation_mask)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
        tm_layout(main.title= raster_imputation_mask@file@name,
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }
  return(raster_imputation_mask)
}


# List to output removal of shadows (Those LULC were free from cloud cover!)
LULC_imputed_NO_shadows_never_had_CC_list <- pblapply(seq_along(LULC_NOCloudCover_index), function(x){
  Imputing_shadows_from_6classesLULC(index = x, plt_mask=T, plt_imputed = T)
})


# # Save output as an .Rdata file
# save(LULC_imputed_NO_shadows_never_had_CC_list,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (post-processing)/LULC_imputed_NO_shadows_never_had_CC_list.Rdata')


# Combining imputed rasters in one list
LULC_noCloudCover_noShadow_full_list <- vector('list', length(LULC)) # create an empty list with the same size as the number of LULC

# Append the LULC to the empty list created above to rearrange them in order according to their dates
lapply(seq_along(LULC_CloudCover_index), function(x){
  LULC_noCloudCover_noShadow_full_list[[LULC_CloudCover_index[x]]]<<- LULC_imputed_NO_shadows_CC_list[[x]]
})

# Append the LULC to the empty list created above to rearrange them in order according to their dates
lapply(seq_along(LULC_NOCloudCover_index), function(x){
  LULC_noCloudCover_noShadow_full_list[[LULC_NOCloudCover_index[x]]] <<- LULC_imputed_NO_shadows_never_had_CC_list[[x]]
})

# check if LULC order is correct in the list
# sapply(seq_along(LULC_noCloudCover_noShadow_full_list), function (x) {LULC_noCloudCover_noShadow_full_list[[x]]@file@name})

# Visualisation by trial and error the coordinates that enclosed the actual waterbodies on TMNR
par(mar = c(0.2, 0.1, 1.8, 0.1))
plotRGB(L8_20150412_original, r=3 , g=2 , b=1,
        margin = T,
        stretch = 'lin',
        cex.main = .6)
abline(v = coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[225,]['x'], col = 'red') # ≥
abline(v = coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[180,]['x'], col = 'yellow') # ≤
abline(h = coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[116000,]['y'], col = 'blue') # ≤
abline(h = coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[87000,]['y'], col = 'cyan') # ≥

# Define thresholds to exclude actual waterbodies- This is set, so do not change!
threshold1 <- which(coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[,1]>coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[225,]['x'])
threshold2 <- which(coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[,1]<coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[180,]['x'])
threshold3 <- which(coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[,2]<coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[116000,]['y'])
threshold4 <- which(coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[,2]>coordinates(LULC_noCloudCover_noShadow_full_list[[1]])[87000,]['y'])

# Combine threshold without repeating index
outside_enclosure_index <- unique(c(threshold1, threshold2, threshold3, threshold4))

# FUNCTION TO REMOVE MISCLASSIFIED WATER BODIES, MASK AND IMPUTE THEM WITH MORE REALISTIC CLASS
impute_misclassified_waterbodies <- function(index, plt_mask=NULL, plt_imputed = NULL){
  index <- index
  initial_LULC <- LULC_noCloudCover_noShadow_full_list[[index]] # call in LULC after shadows and cloud cover have been removed!
  
  # Convert the latter into a dataframe and add an index column to keep track of the pixel index
  initial_LULC_df <- as.data.frame(initial_LULC, xy = T) %>% mutate(index = 1:length(initial_LULC))
  
  # Select misclassified waterbodies pixels indices outside the enclosed well-classified waterbodies pixel
  misclassified_waterbodies_index_df <- initial_LULC_df[outside_enclosure_index,] %>% 
    filter(layer_Classes=='Water Bodies') %>%
    select(index)
  
  # extract misclassified waterbodies pixels as a vector
  misclassified_waterbodies_index <- misclassified_waterbodies_index_df$index
  
  # Apply mask (set to NA only where condition is met)
  initial_LULC[misclassified_waterbodies_index] <- NA
  initial_LULC_mask_raster <- initial_LULC
  
  if(plt_mask==T){
    print(
      # Visualise masked LULC
      tm_shape(initial_LULC_mask_raster)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73','#70A800', '#00A9E6'))+
        tm_layout(main.title= paste0(LULC_noCloudCover_noShadow_full_list[[index]]@file@name, ' (masked)'),
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }
  
  # Define a function to find the mode (most frequent value) in the neighborhood
  mode_fun <- function(x) {
    tab <- table(x, useNA = "no")  # Count class occurrences (ignore NA)
    if (length(tab) == 0) return(NA)  # Return NA if all neighbors are NA
    as.numeric(names(tab)[which.max(tab)])  # Return most frequent class
  }
  
  # Impute masked misclassified waterbodies with modal neighbouring values 
  raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T); cat('Iteration:', 1, '\n')
  raster_imputation_mask <- raster_imputation |> crop(roi_trans)|> mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
  
  if(any(is.na(values(raster_imputation_mask)[misclassified_waterbodies_index]))==T){ # Is there still any of the masked misclassified waterbodies pixels which are NAs- if so, keep imputing by taking the previously imputed raster
    # Iterate imputation process until all masked misclassified waterbodies pixels are imputed
    i = 2
    cat('Iteration:', i, '\n')
    while(any(is.na(values(raster_imputation_mask)[misclassified_waterbodies_index]))==T){
      
      initial_LULC_mask_raster <- raster_imputation_mask
      # Impute masked misclassified waterbodies with modal neighbouring values 
      raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 7, 7), fun = mode_fun, NAonly = T)
      raster_imputation_mask <- raster_imputation |> crop(roi_trans)|> mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
      i = i+1; cat('Iteration:', i, '\n')
      
      # sometimes masked pixels get too little that the dimension of the matrix above is too big and must change to a smaller one.
      if(i==10){ # after 10 iterations resize the kernel to make it smaller
        # Iterate imputation process until all masked misclassified waterbodies pixels are imputed
        while(any(is.na(values(raster_imputation_mask)[misclassified_waterbodies_index]))==T){
          
          initial_LULC_mask_raster <- raster_imputation_mask
          # Impute masked misclassified waterbodies with modal neighbouring values 
          raster_imputation <- focal(x=initial_LULC_mask_raster, w=matrix(1, 3, 3), fun = mode_fun, NAonly = T)
          raster_imputation_mask <- raster_imputation |> crop(roi_trans)|>mask(roi_trans) # The raster swells on the edge a bit- crop the raster to ROI again.
          i = i+1; cat('Iteration:', i, '\n')
          
          if(i==15){ # if imputation does not stop-i.e, failed to impute with neighbouring pixels-default to value 4 representing shrubland
            while(any(is.na(values(raster_imputation_mask)[misclassified_waterbodies_index]))==T){
              values(raster_imputation_mask)[misclassified_waterbodies_index][which(is.na(values(raster_imputation_mask)[misclassified_waterbodies_index]))] <- 4
            }
          }
        }
      }
    }
    
  }else {raster_imputation_mask}
  
  # any(is.na(values(raster_imputation_mask)[misclassified_waterbodies_index])) # Is there still any of the misclassified waterbodies pixels which are NAs
  # sum(is.na(values(raster_imputation_mask)[misclassified_waterbodies_index]))
  
  raster_imputation_mask <- raster_imputation_mask|>ratify() # make raster a factor raster again
  levels(raster_imputation_mask) <- data.frame(ID = c(1,2,3,4,5), Classes = c('Bare Land', 'Forest & Thicket', 'Grass Land', 'Shrub Land', 'Water Bodies')) # redefine levels
  raster_imputation_mask@file@name <- LULC_noCloudCover_noShadow_full_list[[index]]@file@name  # rename LULC layer
  
  if(plt_imputed==T){
    print(
      # Visualise imputed LULC
      tm_shape(raster_imputation_mask)+
        tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
        tm_layout(main.title= raster_imputation_mask@file@name,
                  main.title.size =.9,
                  main.title.position = c("center", "top"),
                  legend.outside = F,
                  legend.text.size = .5)+
        tm_graticules(lines = F)
    )
  }
  return(raster_imputation_mask)
}

FINAL_LULC <- pblapply(seq_along(LULC_noCloudCover_noShadow_full_list), function(x){
  impute_misclassified_waterbodies(index=x, plt_mask=T, plt_imputed = T)
})

# # Save output as an .Rdata file
# save(FINAL_LULC,
#      file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (post-processing)/FINAL_LULC.Rdata')

# {
#   Save_raster <- function(data, index, path){
#     
#     file_path <- paste0(path, data[[index]]@file@name)
#     
#     return(writeRaster(data[[index]],
#                        filename = file_path, format = "GTiff", overwrite = TRUE))
#   }
#   
#   # Bulk Save!!!!
#   pblapply(seq_along(FINAL_LULC),
#            function(x) {Save_raster(data = FINAL_LULC,
#                                     index = x,
#                                     path = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (post-processing)/')})
# }  

# EDA ---------------------------------------------------------------------

# Extracting accuracy assessment
LULC_accuracy_assessment <- lapply(seq_along(LULC_raster_list), function(x) LULC_raster_list[[x]]$Test_accuracy)

# Extracting dates from LULC rasters
LULC_dates <- pbsapply(seq_along(LULC_raster_list), function(index){sub("LULC ", "", LULC_raster_list[[index]]$LULCRaster@file@name)}) %>% 
  as.Date("%Y%m%d")

# Create a dataframe for the accuracy assessment
LULC_accuracy_assessment_df <- data.frame(Date = LULC_dates, do.call('rbind', LULC_accuracy_assessment))
LULC_accuracy_assessment_df$Year <- year(LULC_accuracy_assessment_df$Date) # extract year from date
mean(LULC_accuracy_assessment_df$Accuracy) # mean value for overall accuracy
mean(LULC_accuracy_assessment_df$Kappa) # mean value for kappa coefficient

# Convert to long format
LULC_accuracy_assessment_df_long <- pivot_longer(LULC_accuracy_assessment_df, col = c('Accuracy','Kappa'), names_to = 'Metrics_name', values_to = 'Metrics_value')
LULC_accuracy_assessment_df_long$Metrics_name <- as.factor(LULC_accuracy_assessment_df_long$Metrics_name) # convert column into factor
LULC_accuracy_assessment_df_long$Year <- as.factor(LULC_accuracy_assessment_df_long$Year) # convert column into factor

LULC_AA_boxplot <- ggplot(LULC_accuracy_assessment_df_long, aes(x = Year, y = Metrics_value, fill = Metrics_name)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(LULC_accuracy_assessment_df$Accuracy), 
             linetype = 'dashed',
             linewidth = .2,
             color = 'red')+ # mean value for overall accuracy
  geom_hline(yintercept = mean(LULC_accuracy_assessment_df$Kappa), 
             linetype = 'dashed', 
             linewidth = .2,
             color = 'blue')+ # mean value for kappa coefficient
  annotate('text', 
           x=7-.1, 
           y=mean(LULC_accuracy_assessment_df$Accuracy), 
           label = paste('Mean Overall Accuracy: \n', mean(LULC_accuracy_assessment_df$Accuracy)|>round(3)),
           size = 2, 
           color = 'red')+
  annotate('text',
            x=7+.3, 
            y=mean(LULC_accuracy_assessment_df$Kappa), 
            label = paste('Mean Kappa Coefficient: \n', mean(LULC_accuracy_assessment_df$Kappa)|>round(3)),
            size = 2,
            color = 'blue')+
  scale_fill_discrete(labels = c('Overall Accuracy', 'Kappa Coefficient')) +
  xlab('Period') +
  ylab('LULC Accuracy Assessment') +
  labs(fill = '')+
  theme_light()+
  theme(legend.position = 'bottom')

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/LULC_AA_boxplot.pdf", 
       plot = LULC_AA_boxplot, width = 6.56, height = 3.5)

# Plot example of the geoimputation process
LULC_with_shadow_plot <- tm_shape(LULC[[119]])+ # leave the index as 119 here!
  tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#000000', '#70A800', '#00A9E6'))+
  tm_layout(main.title= paste0(LULC[[119]]@file@name,' (original)'), #...and here!
            main.title.size =.6,
            main.title.position = c("center", "top"),
            legend.outside = F,
            legend.text.size = .5)+
  tm_graticules(lines = F); LULC_with_shadow_plot

LULC_with_shadowMasked_plot <- tm_shape(initial_LULC_mask)+
  tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', 'white','#70A800', '#00A9E6'))+
  tm_layout(main.title= paste0(LULC[[119]]@file@name, ' (shadows masked)') ,
            main.title.size =.6,
            main.title.position = c("center", "top"),
            legend.outside = F,
            legend.text.size = .5)+
  tm_graticules(lines = F); LULC_with_shadowMasked_plot

LULC_with_shadowImputed_plot <- tm_shape(LULC_imputed_NO_shadows_never_had_CC_list[[94]])+
  tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
  tm_layout(main.title= paste0(LULC_imputed_NO_shadows_never_had_CC_list[[94]]@file@name, ' (shadows imputed)'),
            main.title.size =.6,
            main.title.position = c("center", "top"),
            legend.outside = F,
            legend.text.size = .5)+
  tm_graticules(lines = F); LULC_with_shadowImputed_plot

LULC_with_misclassified_waterbodies_masked_plot <- tm_shape(initial_LULC_mask_raster)+
  tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
  tm_layout(main.title= paste0(LULC_imputed_NO_shadows_never_had_CC_list[[94]]@file@name, ' (misclassified water bodies masked)'),
            main.title.size =.6,
            main.title.position = c("center", "top"),
            legend.outside = F,
            legend.text.size = .5)+
  tm_graticules(lines = F); LULC_with_misclassified_waterbodies_masked_plot

LULC_final_imputation_plot <-  tm_shape(FINAL_LULC[[119]])+
  tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
  tm_layout(main.title= paste0(LULC_imputed_NO_shadows_never_had_CC_list[[94]]@file@name, ' (final imputation)'),
            main.title.size =.6,
            main.title.position = c("center", "top"),
            legend.outside = F,
            legend.text.size = .5)+
  tm_graticules(lines = F); LULC_final_imputation_plot

geoimputation_plot <- tmap_arrange(LULC_with_shadow_plot, LULC_with_shadowMasked_plot,
                                   LULC_with_shadowImputed_plot, LULC_with_misclassified_waterbodies_masked_plot, 
                                   LULC_final_imputation_plot, nrow = 3, ncol = 2)
tmap_save(geoimputation_plot, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/geoimputation_plot.pdf", width = 6, height = 7, dpi = 600)





