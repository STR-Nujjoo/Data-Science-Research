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
RF_supervised_classification(index = 57, 
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


# EDA ---------------------------------------------------------------------

# Visualisation of PCA and LULC for my appendix
aerial_imagery_pca <- projectRaster(aerial_imagery_pca, crs = "+proj=longlat +datum=WGS84 +no_defs", method = 'ngb')
par(mar = c(2, 2, 1.0, 0.1)) # customised margin
plotRGB(aerial_imagery_pca, r=3 , g=2 , b=1, 
        stretch = 'lin', 
        margin = T,
        main = paste0(as.Date(gsub("\\.tif$", "", aerial_imagery_filenames[index]), format = "%Y%m%d"), ' PCA'))
# Add longitude (x-axis) and latitude (y-axis) labels
axis(1, at = seq(extent(aerial_imagery_pca)[1], extent(aerial_imagery_pca)[2], length.out = 23), 
     labels = seq(extent(aerial_imagery_pca)[1], extent(aerial_imagery_pca)[2],length.out = 23)%>%round(2))
axis(2, at = seq(extent(aerial_imagery_pca)[3], extent(aerial_imagery_pca)[4], length.out = 23), 
     labels = seq(extent(aerial_imagery_pca)[3], extent(aerial_imagery_pca)[4], length.out = 23)%>%round(2))


LULCexample <- tm_shape(LULCPredictions)+
  tm_raster(style = "cat", title = "", palette = LULCpal)+ 
  tm_layout(main.title= paste0(as.Date(gsub("\\.tif$", "", aerial_imagery_filenames[index]), format = "%Y%m%d"), ' LULC'),
            main.title.size =.9,
            main.title.position = c("center", "top"),
            legend.outside = F,
            legend.text.size = .5)+
  tm_graticules(lines = F)


# tmap_save(LULCexample, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/test.pdf", width = 6.56, height = 3)





