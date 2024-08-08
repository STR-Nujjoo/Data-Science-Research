# rm(list = ls()) # clear environment

{
  library(raster)
  library(sp)
  library(tidyverse)
  library(rgdal)
  library(sf)
  library(stars)
  library(naniar)
}


# DATA ORGANISATION FOR FURTHER ANALYSES ----------------------------------
# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/Shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs ')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Defining column and row names of data frame
year <- 2002:2023
month <- month.abb

df_empty <- data.frame(matrix(nrow = length(year), ncol = length(month)), row.names = year) # create empty dataframe
names(df_empty) <- month # rename columns

df <- df_empty;df
row.names(df)

# LANDSAT 7 SR ------------------------------------------------------------
# Read in Landsat 7 files names in the R environment
L7SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Landsat 7',
                                    pattern = '.tif')

# Function to read in all possible Landsat 7 imagery downloaded and plot the image
L7SR_image_collection_import <- function(index){
  
  image <- brick(paste0('Raw Data/Aerial Imagery/Landsat 7/', L7SR_image_collection[index]))
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  mask_image@file@name <- str_extract(image@file@name, "\\d{8}") # append imagery date to masked imagery
  dropped_bands_mask_image <- dropLayer(mask_image, c(7:19)) # remove unneccesary bands for consistency with Landsat bands
  
  return(dropped_bands_mask_image)
}
  
L7 <- lapply(1:length(L7SR_image_collection), L7SR_image_collection_import) # import imageries 

# Appending the collected imageries to the empty data frame created above
# 2002 series
df$Jan[row.names(df)=='2002'] <- list(L7[[1]]) # 20020126
df$Mar[row.names(df)=='2002'] <- list(list(L7[[2]], L7[[3]])) # 20020315 & 20020331
df$Apr[row.names(df)=='2002'] <- list(L7[[4]]) # 20020416
df$May[row.names(df)=='2002'] <- list(L7[[5]]) # 20020518
df$Jun[row.names(df)=='2002'] <- list(L7[[6]]) # 20020603
df$Jul[row.names(df)=='2002'] <- list(L7[[7]]) # 20020721
df$Sep[row.names(df)=='2002'] <- list(L7[[8]]) # 20020923
df$Nov[row.names(df)=='2002'] <- list(L7[[9]]) # 20021110

# 2003 series
df$Jan[row.names(df)=='2003'] <- list(L7[[10]]) # 20030113
df$May[row.names(df)=='2003'] <- list(L7[[11]]) # 20030521
df$Nov[row.names(df)=='2003'] <- list(list(L7[[12]], L7[[13]])) # 20031113 & 20031129
df$Dec[row.names(df)=='2003'] <- list(L7[[14]]) # 20031215

# 2004 series
df$Mar[row.names(df)=='2004'] <- list(L7[[15]]) # 20040320
df$Jul[row.names(df)=='2004'] <- list(L7[[16]]) # 20040710
df$Aug[row.names(df)=='2004'] <- list(list(L7[[17]], L7[[18]])) # 20040811 & 20040827
df$Nov[row.names(df)=='2004'] <- list(L7[[19]]) # 20041115

# # Manipulating duplicate data
# setClass("RasterBrick_with_alias", contains="RasterBrick", slots=list(alias="character"))
# L7[[20]] <-  as(L7[[20]], "RasterBrick_with_alias") # create a new subclass attribute to identify which year are we replacing this data with.
# L7[[20]]@alias <- '200412 replacement' # Define the year at which the substitution occurred as an alias. Note: original name/date of capture is still maintained.
# df$Dec[row.names(df)=='2004'] <- list(L7[[20]]) # 20050102

# 2005 series
df$Jan[row.names(df)=='2005'] <- list(list(L7[[20]],L7[[21]])) # 20050102 & 20050118
df$Mar[row.names(df)=='2005'] <- list(L7[[22]]) # 20050323
df$Apr[row.names(df)=='2005'] <- list(L7[[23]]) # 20050408
df$May[row.names(df)=='2005'] <- list(L7[[24]]) # 20050510
df$Jun[row.names(df)=='2005'] <- list(L7[[25]]) # 20050611
df$Dec[row.names(df)=='2005'] <- list(L7[[26]]) # 20051204

# # 2006 series
# # Manipulating duplicate data
# L7[[27]] <- as(L7[[27]], "RasterBrick_with_alias") # create a new subclass attribute to identify which year are we replacing this data with.
# L7[[27]]@alias <- '200601 replacement' # Define the year at which the substitution occurred as an alias. Note: original name/date of capture is still maintained.
# df$Jan[row.names(df)=='2006'] <- list(L7[[27]]) # 20060206
df$Feb[row.names(df)=='2006'] <- list(list(L7[[27]], L7[[28]])) # 20060206 & 20060222
df$Mar[row.names(df)=='2006'] <- list(L7[[29]]) # 20060310
df$Jun[row.names(df)=='2006'] <- list(L7[[30]]) # 20060630
df$Aug[row.names(df)=='2006'] <- list(L7[[31]]) # 20060817
df$Nov[row.names(df)=='2006'] <- list(L7[[32]]) # 20061105

# 2007 series
df$Feb[row.names(df)=='2007'] <- list(L7[[33]]) # 20070225
df$Mar[row.names(df)=='2007'] <- list(L7[[34]]) # 20070313
# # Manipulating duplicate data
# L7[[35]] <- as(L7[[35]], "RasterBrick_with_alias") # create a new subclass attribute to identify which year are we replacing this data with.
# L7[[35]]@alias <- '200705 replacement' # Define the year at which the substitution occurred as an alias. Note: original name/date of capture is still maintained.
# df$May[row.names(df)=='2007'] <- list(L7[[35]]) # 20070601
df$Jun[row.names(df)=='2007'] <- list(list(L7[[35]], L7[[36]])) # 20070601 & 20070617
df$Jul[row.names(df)=='2007'] <- list(L7[[37]]) # 20070703
df$Nov[row.names(df)=='2007'] <- list(L7[[38]]) # 20071124

# 2008 series
df$Jan[row.names(df)=='2008'] <- list(L7[[39]]) # 20080127
df$May[row.names(df)=='2008'] <- list(L7[[40]]) # 20080502
# # Manipulating duplicate data
# L7[[41]] <- as(L7[[41]], "RasterBrick_with_alias") # create a new subclass attribute to identify which year are we replacing this data with.
# L7[[41]]@alias <- '200807 replacement' # Define the year at which the substitution occurred as an alias. Note: original name/date of capture is still maintained.
# df$Jul[row.names(df)=='2008'] <- list(L7[[41]]) # 20080806
df$Aug[row.names(df)=='2008'] <- list(list(L7[[41]], L7[[42]])) # 20080806 & 20080822
df$Oct[row.names(df)=='2008'] <- list(L7[[43]]) # 20081025
df$Nov[row.names(df)=='2008'] <- list(list(L7[[44]], L7[[45]])) # 20081110 & 20081126

# 2009 series
df$May[row.names(df)=='2009'] <- list(L7[[46]]) # 20090505
df$Jul[row.names(df)=='2009'] <- list(L7[[47]]) # 20090724
df$Aug[row.names(df)=='2009'] <- list(list(L7[[48]], L7[[49]])) # 20090809 & 20090825

# 2010 series
df$Jan[row.names(df)=='2010'] <- list(L7[[50]]) # 20100116
df$Jun[row.names(df)=='2010'] <- list(L7[[51]]) # 20100625
df$Aug[row.names(df)=='2010'] <- list(L7[[52]]) # 20100812
df$Sep[row.names(df)=='2010'] <- list(L7[[53]]) # 20100913
df$Oct[row.names(df)=='2010'] <- list(L7[[54]]) # 20101031
df$Nov[row.names(df)=='2010'] <- list(L7[[55]]) # 20101116
df$Dec[row.names(df)=='2010'] <- list(L7[[56]]) # 20101218

# 2011 series
df$Jan[row.names(df)=='2011'] <- list(L7[[57]]) # 20110103
df$Apr[row.names(df)=='2011'] <- list(L7[[58]]) # 20110409
df$May[row.names(df)=='2011'] <- list(L7[[59]]) # 20110527
df$Jun[row.names(df)=='2011'] <- list(L7[[60]]) # 20110612
df$Aug[row.names(df)=='2011'] <- list(L7[[61]]) # 20110815
df$Sep[row.names(df)=='2011'] <- list(L7[[62]]) # 20110916

# 2012 series
df$Jan[row.names(df)=='2012'] <- list(L7[[63]]) # 20120106
df$Mar[row.names(df)=='2012'] <- list(L7[[64]]) # 20120326
df$Apr[row.names(df)=='2012'] <- list(L7[[65]]) # 20120411
df$Sep[row.names(df)=='2012'] <- list(L7[[66]]) # 20120902
df$Oct[row.names(df)=='2012'] <- list(L7[[67]]) # 20121020
df$Dec[row.names(df)=='2012'] <- list(L7[[68]]) # 20121223

# 2013 series
df$Feb[row.names(df)=='2013'] <- list(L7[[69]]) # 20130225
df$Apr[row.names(df)=='2013'] <- list(L7[[70]]) # 20130430
df$Jun[row.names(df)=='2013'] <- list(L7[[71]]) # 20130617
df$Jul[row.names(df)=='2013'] <- list(L7[[72]]) # 20130703
df$Aug[row.names(df)=='2013'] <- list(L7[[73]]) # 20130804
df$Nov[row.names(df)=='2013'] <- list(L7[[74]]) # 20131124
# df$Dec[row.names(df)=='2013'] <- list(L7[[75]]) # 20131226

# Example of how to extract an image from the create dataframe for visualisation
{
  plotRGB(df$May[row.names(df)=='2009'][[1]], r='SR_B3' , g='SR_B2' , b='SR_B1', 
          stretch = 'lin', 
          margin = T,
          main = as.Date(df$May[row.names(df)=='2009'][[1]]@file@name, format = "%Y%m%d"))
  #plot(roi_trans, col = 'transparent', border = 'limegreen', lwd = 2, add = T)
}


# LANDSAT 8 SR ------------------------------------------------------------

# Read in Landsat 8 files names in the R environment
L8SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Landsat 8',
                                    pattern = '.tif')

# Function to read in all possible Landsat 8 imagery downloaded and plot the image
L8SR_image_collection_import <- function(index){
  
  image <- brick(paste0('Raw Data/Aerial Imagery/Landsat 8/', L8SR_image_collection[index]))
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  mask_image@file@name <- str_extract(image@file@name, "\\d{8}") # append imagery date to masked imagery
  dropped_bands_mask_image <- dropLayer(mask_image, c(1,8:19)) # remove unnecessary bands for consistency with Landsat bands
  
  return(dropped_bands_mask_image)
}

L8 <- lapply(1:length(L8SR_image_collection), L8SR_image_collection_import) # import imageries 

# 2013 series
# df$Oct[row.names(df)=='2013'] <- list(L8[[1]]) # 20131015: very hazy- consider removing this one!! REMOVED!
df$Dec[row.names(df)=='2013'] <- list(list(L8[[2]], L7[[75]])) # 20131218 & 20131226

# 2014 series
df$Feb[row.names(df)=='2014'] <- list(L8[[3]]) # 20140204 
df$Apr[row.names(df)=='2014'] <- list(list(L8[[4]], L8[[5]])) # 20140409 & 20140425 
df$Jun[row.names(df)=='2014'] <- list(list(L8[[6]], L8[[7]])) # 20140612 & 20140628 
df$Jul[row.names(df)=='2014'] <- list(list(L8[[8]], L8[[9]])) # 20140714 & 20140730
df$Aug[row.names(df)=='2014'] <- list(L8[[10]]) # 20140831 
# # Manipulating duplicate data
# L8[[11]] <- as(L8[[11]], "RasterBrick_with_alias") # create a new subclass attribute to identify which year are we replacing this data with.
# L8[[11]]@alias <- '201409 replacement' # Define the year at which the substitution occurred as an alias. Note: original name/date of capture is still maintained.
# df$Sep[row.names(df)=='2014'] <- list(L8[[11]]) # 20141002 
df$Oct[row.names(df)=='2014'] <- list(list(L8[[11]], L8[[12]])) # 20141002 & 20141018 
df$Nov[row.names(df)=='2014'] <- list(L8[[13]]) # 20141119 
df$Dec[row.names(df)=='2014'] <- list(L8[[14]]) # 20141205 

# 2015 series
df$Jan[row.names(df)=='2015'] <- list(list(L8[[15]], L8[[16]])) # 20150106 & 20150122 
df$Feb[row.names(df)=='2015'] <- list(list(L8[[17]], L8[[18]])) # 20150207 & 20150223 
df$Mar[row.names(df)=='2015'] <- list(L8[[19]]) # 20150311 
df$Apr[row.names(df)=='2015'] <- list(L8[[20]]) # 20150412 
df$Aug[row.names(df)=='2015'] <- list(L8[[22]]) # 20150802 
df$Sep[row.names(df)=='2015'] <- list(list(L8[[23]], L8[[24]])) # 20150903 & 20150919 

# 2016 series
df$Jan[row.names(df)=='2016'] <- list(L8[[25]]) # 20160109 
df$Feb[row.names(df)=='2016'] <- list(L8[[26]]) # 20160210 
df$Jul[row.names(df)=='2016'] <- list(L8[[27]]) # 20160703 
df$Oct[row.names(df)=='2016'] <- list(L8[[28]]) # 20161023 
df$Dec[row.names(df)=='2016'] <- list(list(L8[[29]], L8[[30]])) # 20161210 & 20161226

# 2017 series
df$Jan[row.names(df)=='2017'] <- list(L8[[31]]) # 20170111 
df$Feb[row.names(df)=='2017'] <- list(L8[[32]]) # 20170228 
df$Mar[row.names(df)=='2017'] <- list(L8[[33]]) # 20170316 
df$Apr[row.names(df)=='2017'] <- list(L8[[34]]) # 20170417 
df$May[row.names(df)=='2017'] <- list(L8[[35]]) # 20170519 
# df$Jul[row.names(df)=='2017'] <- list(L8[[36]]) # 20170706: very hazy- consider removing this one!!
df$Aug[row.names(df)=='2017'] <- list(L8[[37]]) # 20170807 
df$Oct[row.names(df)=='2017'] <- list(L8[[38]]) # 20171010 
df$Nov[row.names(df)=='2017'] <- list(L8[[39]]) # 20171127 
df$Dec[row.names(df)=='2017'] <- list(L8[[40]]) # 20171229 

# 2018 series
df$Jan[row.names(df)=='2018'] <- list(L8[[41]]) # 20180114 
df$Feb[row.names(df)=='2018'] <- list(L8[[42]]) # 20180215 
df$Mar[row.names(df)=='2018'] <- list(list(L8[[43]], L8[[44]])) # 20180303 & 20180319
df$Apr[row.names(df)=='2018'] <- list(L8[[45]]) # 20180404 
df$Jul[row.names(df)=='2018'] <- list(L8[[46]]) # 20180709 
df$Sep[row.names(df)=='2018'] <- list(L8[[47]]) # 20180911 
df$Oct[row.names(df)=='2018'] <- list(L8[[48]]) # 20181013 
df$Nov[row.names(df)=='2018'] <- list(list(L8[[49]], L8[[50]])) # 20181114 & 20181130
df$Dec[row.names(df)=='2018'] <- list(L8[[51]]) # 20181216 

# 2019 series
df$Feb[row.names(df)=='2019'] <- list(L8[[52]]) # 20190218 
df$Mar[row.names(df)=='2019'] <- list(L8[[53]]) # 20190306 
df$Apr[row.names(df)=='2019'] <- list(L8[[54]]) # 20190407 
df$May[row.names(df)=='2019'] <- list(L8[[55]]) # 20190509 
df$Sep[row.names(df)=='2019'] <- list(L8[[56]]) # 20190914 
df$Oct[row.names(df)=='2019'] <- list(L8[[57]]) # 20191016 

# 2020 series
df$Jan[row.names(df)=='2020'] <- list(L8[[58]]) # 20200104 
df$Feb[row.names(df)=='2020'] <- list(L8[[59]]) # 20200221 
df$Apr[row.names(df)=='2020'] <- list(list(L8[[60]], L8[[61]])) # 20200409 & 20200425
df$May[row.names(df)=='2020'] <- list(L8[[62]]) # 20200511 
df$Dec[row.names(df)=='2020'] <- list(L8[[63]]) # 20201205 

# 2021 series
df$Jan[row.names(df)=='2021'] <- list(list(L8[[64]], L8[[65]])) # 20210106 & 20210122 
df$Feb[row.names(df)=='2021'] <- list(L8[[66]]) # 20210223 
df$Apr[row.names(df)=='2021'] <- list(L8[[67]]) # 20210412 
df$Jul[row.names(df)=='2021'] <- list(L8[[68]]) # 20210717 
df$Aug[row.names(df)=='2021'] <- list(L8[[69]]) # 20210802 
df$Oct[row.names(df)=='2021'] <- list(L8[[70]]) # 20211005 
df$Dec[row.names(df)=='2021'] <- list(list(L8[[71]], L8[[72]])) # 20211208 & 20211224

# Example of how to extract an image from the create dataframe for visualisation
{
  plotRGB(df$Dec[row.names(df)=='2017'][[1]], r='SR_B4' , g='SR_B3' , b='SR_B2', 
          stretch = 'lin', 
          margin = T,
          main = as.Date(df$Dec[row.names(df)=='2017'][[1]]@file@name, format = "%Y%m%d"))
  #plot(roi_trans, col = 'transparent', border = 'limegreen', lwd = 2, add = T)
}

# SENTINEL 2 TOA ----------------------------------------------------------
# Read in Sentinel 2 files names in the R environment
S2TOA_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Sentinel 2 TOA',
                                    pattern = '.tif')

# Function to read in all possible Sentinel 2 imagery downloaded and plot the image
S2TOA_image_collection_import <- function(index){
  
  image <- brick(paste0('Raw Data/Aerial Imagery/Sentinel 2 TOA/', S2TOA_image_collection[index]))
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  mask_image@file@name <- str_extract(image@file@name, "\\d{8}") # append imagery date to masked imagery
  dropped_bands_mask_image <- dropLayer(mask_image, c(1,5:7,9:11,14:16)) # remove unneccesary bands for consistency with Landsat bands
  
  return(dropped_bands_mask_image)
}

S2TOA <- lapply(1:length(S2TOA_image_collection), S2TOA_image_collection_import) # import imageries 
S2TOA[[1]]
# 2015 series
df$Dec[row.names(df)=='2015'] <- list(S2TOA[[1]]) # 20151218 

# 2016 series
df$Apr[row.names(df)=='2016'] <- list(S2TOA[[2]]) # 20160406 
df$May[row.names(df)=='2016'] <- list(S2TOA[[3]]) # 20160526 
df$Jun[row.names(df)=='2016'] <- list(S2TOA[[4]]) # 20160605 
df$Aug[row.names(df)=='2016'] <- list(S2TOA[[5]]) # 20160824 
df$Sep[row.names(df)=='2016'] <- list(S2TOA[[6]]) # 20160913 
df$Nov[row.names(df)=='2016'] <- list(S2TOA[[7]]) # 20161122 

# Example of how to extract an image from the create dataframe for visualisation
{
  plotRGB(df$Nov[row.names(df)=='2016'][[1]], r='B4' , g='B3' , b='B2', 
          stretch = 'lin', 
          margin = T,
          main = as.Date(df$Nov[row.names(df)=='2016'][[1]]@file@name, format = "%Y%m%d"))
  #plot(roi_trans, col = 'transparent', border = 'limegreen', lwd = 2, add = T)
}


# SENTINEL 2 SR -----------------------------------------------------------
# Read in Sentinel 2 files names in the R environment
S2SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Sentinel 2 SR',
                                     pattern = '.tif')

# Function to read in all possible Sentinel 2 imagery downloaded and plot the image
S2SR_image_collection_import <- function(index){
  
  image <- brick(paste0('Raw Data/Aerial Imagery/Sentinel 2 SR/', S2SR_image_collection[index]))
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  mask_image@file@name <- str_extract(image@file@name, "\\d{8}") # append imagery date to masked imagery
  dropped_bands_mask_image <- dropLayer(mask_image, c(1,5:7,9,10,13:23)) # remove unneccesary bands for consistency with Landsat bands
  
  return(dropped_bands_mask_image)
}
S2SR <- lapply(1:length(S2SR_image_collection), S2SR_image_collection_import) # import imageries 

names(S2SR[[1]])
# 2019 series
df$Jan[row.names(df)=='2019'] <- list(S2SR[[1]]) # 20190126 
df$Jun[row.names(df)=='2019'] <- list(S2SR[[2]]) # 20190615 
df$Jul[row.names(df)=='2019'] <- list(S2SR[[3]]) # 20190710 
df$Aug[row.names(df)=='2019'] <- list(S2SR[[4]]) # 20190824 
df$Nov[row.names(df)=='2019'] <- list(S2SR[[5]]) # 20191122 
df$Dec[row.names(df)=='2019'] <- list(S2SR[[6]]) # 20191222 

# 2020 series
df$Mar[row.names(df)=='2020'] <- list(S2SR[[7]]) # 20200316 
df$Jun[row.names(df)=='2020'] <- list(S2SR[[8]]) # 20200624 
df$Jul[row.names(df)=='2020'] <- list(S2SR[[9]]) # 20200719 
df$Sep[row.names(df)=='2020'] <- list(S2SR[[10]]) # 20200922 
df$Oct[row.names(df)=='2020'] <- list(S2SR[[11]]) # 20201022 
df$Nov[row.names(df)=='2020'] <- list(S2SR[[12]]) # 20201111 

# 2021 series
df$Mar[row.names(df)=='2021'] <- list(S2SR[[13]]) # 20210321 
df$Jun[row.names(df)=='2021'] <- list(S2SR[[14]]) # 20210619 
df$Sep[row.names(df)=='2021'] <- list(S2SR[[15]]) # 20210922 
df$Nov[row.names(df)=='2021'] <- list(S2SR[[16]]) # 20211111 

# 2022 series
df$Apr[row.names(df)=='2022'] <- list(S2SR[[17]]) # 20220410 
df$Aug[row.names(df)=='2022'] <- list(S2SR[[18]]) # 20220823 
df$Oct[row.names(df)=='2022'] <- list(S2SR[[19]]) # 20221022 
df$Dec[row.names(df)=='2022'] <- list(S2SR[[20]]) # 20221221 

# 2023 series
df$Jan[row.names(df)=='2023'] <- list(S2SR[[21]]) # 20230125 
df$Feb[row.names(df)=='2023'] <- list(S2SR[[22]]) # 20230209 
df$Mar[row.names(df)=='2023'] <- list(S2SR[[23]]) # 20230316 
df$Apr[row.names(df)=='2023'] <- list(S2SR[[24]]) # 20230405 
df$Jul[row.names(df)=='2023'] <- list(S2SR[[25]]) # 20230724 
df$Sep[row.names(df)=='2023'] <- list(S2SR[[26]]) # 20230927 
df$Nov[row.names(df)=='2023'] <- list(S2SR[[27]]) # 20231126 

# Example of how to extract an image from the create dataframe for visualisation
{
  plotRGB(df$Jul[row.names(df)=='2023'][[1]], r='B4' , g='B3' , b='B2', 
          stretch = 'lin', 
          margin = T,
          main = as.Date(df$Jul[row.names(df)=='2023'][[1]]@file@name, format = "%Y%m%d"))
  #plot(roi_trans, col = 'transparent', border = 'limegreen', lwd = 2, add = T)
}

# LANDSAT 9 SR ------------------------------------------------------------

# Read in Landsat 9 files names in the R environment
L9SR_image_collection <- list.files(path = 'Raw Data/Aerial Imagery/Landsat 9',
                                    pattern = '.tif')

# Function to read in all possible Landsat 8 imagery downloaded and plot the image
L9SR_image_collection_import <- function(index){
  
  image <- brick(paste0('Raw Data/Aerial Imagery/Landsat 9/', L9SR_image_collection[index]))
  mask_image <- mask(image, roi_trans) # restrict imagery to roi-its like clipping in a way
  mask_image@file@name <- str_extract(image@file@name, "\\d{8}") # append imagery date to masked imagery
  dropped_bands_mask_image <- dropLayer(mask_image, c(1, 8:19)) # remove unnecessary bands for consistency with Landsat bands
  
  return(dropped_bands_mask_image)
}

L9 <- lapply(1:length(L9SR_image_collection), L9SR_image_collection_import) # import imageries 

# 2022 series
df$Jan[row.names(df)=='2022'] <- list(L9[[1]]) # 20220117 
df$Feb[row.names(df)=='2022'] <- list(L9[[2]]) # 20220218 
df$Mar[row.names(df)=='2022'] <- list(L9[[3]]) # 20220306 
df$May[row.names(df)=='2022'] <- list(L9[[4]]) # 20220509 
df$Jun[row.names(df)=='2022'] <- list(list(L9[[5]], L9[[6]])) # 20220610 & 20220626
df$Jul[row.names(df)=='2022'] <- list(L9[[7]]) # 20220728 
df$Sep[row.names(df)=='2022'] <- list(L9[[8]]) # 20220914 
df$Nov[row.names(df)=='2022'] <- list(L9[[9]]) # 20221101 

# 2023 series
df$May[row.names(df)=='2023'] <- list(L9[[10]]) # 20230528 
df$Aug[row.names(df)=='2023'] <- list(L9[[11]]) # 20230816 
df$Oct[row.names(df)=='2023'] <- list(list(L9[[12]], L9[[13]])) #  20231003 & 20231019 
df$Dec[row.names(df)=='2023'] <- list(list(L9[[14]], L9[[15]])) # 20231206 & 20231222 

# Example of how to extract an image from the create dataframe for visualisation
{
  plotRGB(df$Jan[row.names(df)=='2022'][[1]], r='SR_B4' , g='SR_B3' , b='SR_B2', 
          stretch = 'lin', 
          margin = T,
          main = as.Date(df$Jan[row.names(df)=='2022'][[1]]@file@name, format = "%Y%m%d"))
  #plot(roi_trans, col = 'transparent', border = 'limegreen', lwd = 2, add = T)
}

# as.Date(S2SR[[14]]@file@name, format="%Y%m%d") - as.Date(S2SR[[13]]@file@name, format="%Y%m%d")

# Some extraction examples
L7[[15]]@file@name
df$Jan[row.names(df)=='2006'][[1]]@alias
df$Jan[row.names(df)=='2006'][[1]]@file@name
df[row.names(df)=='2013',] # yearly extraction

# No. of missing cells/aerial imageries in data frame
df %>% miss_var_summary() %>% tally(n_miss)

# Extract imageries and put them in column format for further analyses (if necessary!)
df_long_withoutNAs <- df %>%
  mutate(year = year) %>%
  pivot_longer(cols = -year, names_to = 'month', values_to = "Imageries") %>%
  filter(!is.na(Imageries))

# head(as.data.frame(pivot_wider(df_long, names_from = 'month', values_from = Imageries), row.names = year))

# Extract the dates in the same order as acquired
dates_extraction <- NULL
for(i in 1:length(unlist(df_long_withoutNAs$Imageries))){
  dates_extraction[i] <- unlist(df_long_withoutNAs$Imageries)[[i]]@file@name
}

# Space between set of pairs of observation
days_between_imagery_obs <- diff(as.Date(dates_extraction, format="%Y%m%d"))

# Average space between set of pairs of observation
ave_days_between_imagery_obs <- floor(mean(days_between_imagery_obs)) # ~approximately

quantile(days_between_imagery_obs)
(which(days_between_imagery_obs > 32 & days_between_imagery_obs <= 48 ))

days_between_imagery_obs[which(days_between_imagery_obs > 32 & days_between_imagery_obs <= 48)]

# HEURISTIC FOR HORIZONTAL INTERPOLATION ----------------------------------
df_long_withNAs <- df %>%
  mutate(year = year) %>%
  pivot_longer(cols = -year, names_to = 'month', values_to = "Imageries")

# Function to find midpoint of 2 dates
find_midpoint_date <- function(date1, date2) {
  # Ensure the input dates are in Date format
  date1 <- as.Date(date1, format="%Y%m%d")
  date2 <- as.Date(date2, format="%Y%m%d")
  
  # Calculate the midpoint
  midpoint <- date1 + (difftime(date2, date1) / 2)
  
  return(midpoint)
}
find_midpoint_date(df[1,1][[1]]@file@name, df[1,3][[1]][[1]]@file@name)
x <- (df[1,1][[1]] + df[1,3][[1]][[1]])/2
x@file@name <- as.character(find_midpoint_date(df[1,1][[1]]@file@name, df[1,3][[1]][[1]]@file@name))
find_midpoint_date('20041115', '20050102')

trimmed_df <- df[1:22,]

for (row in 1:nrow(trimmed_df)) {
  for (col in 1:ncol(trimmed_df)) {
    # For each cell, get the next cell in the same row
    if (col < ncol(trimmed_df)) {
      # First element in the table remains unchange
      if(row == 1 & col == 1){
        trimmed_df[row, col] <- list(trimmed_df[row, col]) # cell remain unchanged
        # First element in each row if they are Null apart from first row
      }else if(row != 1 & col == 1){
        # If the latter is not null... 
        if(!is.na(trimmed_df[row, col])){
          trimmed_df[row, col] <- list(trimmed_df[row, col]) # cell remain unchanged
          # If the latter is null...
        }else if(is.na(trimmed_df[row, col]) && 
                 !is.na(trimmed_df[row-1, 12]) &&
                 !is.na(trimmed_df[row, col+1])){
          # when previous cell contains 2 imageries and next next cell contains 2 imageries
          if(length(trimmed_df[row-1, 12][[1]])>1 & length(trimmed_df[row, col+1][[1]])>1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row-1, 12][[1]][[2]] + trimmed_df[row, col+1][[1]][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            # 
            # when previous cell contains 2 imageries and next next cell contains 1 imagery
          }else if(length(trimmed_df[row-1, 12][[1]])>1 & length(trimmed_df[row, col+1][[1]])==1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row-1, 12][[1]][[2]] + trimmed_df[row, col+1][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            # 
            # when previous cell contains 1 imagery and next next cell contains 2 imageries
          }else if(length(trimmed_df[row-1, 12][[1]])==1 & length(trimmed_df[row, col+1][[1]])>1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row-1, 12][[1]] + trimmed_df[row, col+1][[1]][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            # 
             # when previous cell contains 1 imagery and next next cell contains 1 imagery
          }else if(length(trimmed_df[row-1, 12][[1]])==1 & length(trimmed_df[row, col+1][[1]])==1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row-1, 12][[1]] + trimmed_df[row, col+1][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
          }
          # trimmed_df[row, col] <- 'Interpolate'
        }else(trimmed_df[row, col] <- NA)
        
        # If cell contains 1 or more than a raster then keep it
      }else if(length(trimmed_df[row, col][[1]]) == 1 & !is.na(trimmed_df[row, col]) || length(trimmed_df[row, col][[1]]) > 1){
        trimmed_df[row, col] <- list(trimmed_df[row, col]) # cell remain unchanged
        # If cell is null...
      }else if(is.na(trimmed_df[row, col])) {
        if(!is.na(trimmed_df[row, col-1]) & !is.na(trimmed_df[row, col+1])){
          # when previous cell contains 2 imageries and next next cell contains 2 imageries
          if(length(trimmed_df[row, col-1][[1]]) > 1 & length(trimmed_df[row, col+1][[1]]) > 1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]][[2]] + trimmed_df[row, col+1][[1]][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            
            # when previous cell contains 2 imageries and next next cell contains 1 imagery
          }else if(length(trimmed_df[row, col-1][[1]]) > 1 & length(trimmed_df[row, col+1][[1]]) == 1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]][[2]] + trimmed_df[row, col+1][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            
            # when previous cell contains 1 imagery and next next cell contains 2 imageries
          }else if(length(trimmed_df[row, col-1][[1]]) == 1 & length(trimmed_df[row, col+1][[1]]) > 1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]] + trimmed_df[row, col+1][[1]][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            
            # when previous cell contains 1 imagery and next next cell contains 1 imagery
          }else if(length(trimmed_df[row, col-1][[1]]) == 1 & length(trimmed_df[row, col+1][[1]]) == 1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]] + trimmed_df[row, col+1][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
          }
          # trimmed_df[row, col] <- 'Interpolate'
        }else (trimmed_df[row, col] <- NA)
      }
      # For the last cell in the row, get the first cell of the next row  
    }else if (col == ncol(trimmed_df) && row < nrow(trimmed_df)) {
      # If cell contains 1 or more than a raster then keep it
      if(length(trimmed_df[row, col][[1]]) == 1 & !is.na(trimmed_df[row, col]) || length(trimmed_df[row, col][[1]]) > 1){
        trimmed_df[row, col] <- list(trimmed_df[row, col]) # cell remain unchanged
        # If cell is null
      }else if(is.na(trimmed_df[row, col])){
        if(!is.na(trimmed_df[row, col-1]) & !is.na(trimmed_df[row+1, 1])){ 
          # when previous cell contains 2 imageries and next next cell contains 2 imageries
          if(length(trimmed_df[row, col-1][[1]])>1 & length(trimmed_df[row+1, 1][[1]])>1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]][[2]] + trimmed_df[row+1, 1][[1]][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            
            # when previous cell contains 2 imageries and next next cell contains 1 imagery
          }else if(length(trimmed_df[row, col-1][[1]])>1 & length(trimmed_df[row+1, 1][[1]])==1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]][[2]] + trimmed_df[row+1, 1][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            # 
            # when previous cell contains 1 imagery and next next cell contains 2 imageries
          }else if(length(trimmed_df[row, col-1][[1]])==1 & length(trimmed_df[row+1, 1][[1]])>1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]] + trimmed_df[row+1, 1][[1]][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
            
            # when previous cell contains 1 imagery and next next cell contains 1 imagery
          }else if(length(trimmed_df[row, col-1][[1]])==1 & length(trimmed_df[row+1, 1][[1]])==1){
            trimmed_df[row, col][[1]] <- list((trimmed_df[row, col-1][[1]] + trimmed_df[row+1, 1][[1]])/2) # Horizontal Interpolation
            # interpolated_raster <- trimmed_df[row, col][[1]]
            # names(interpolated_raster) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2') # rename bands
          }
          # trimmed_df[row, col] <- 'Interpolate'
        }else (trimmed_df[row, col] <- NA)
      } 
    }
    print(c(row, col)) # track progress
  }
}







trimmed_df[1,7][[1]]
trimmed_df[13,2][[1]]






# Extract the raster object
raster_obj <- trimmed_df[13, 1][[1]]

# Rename the layers
names(raster_obj) <- c('B', 'G', 'R', 'NIR', 'SWIR1', 'SWIR2')




















































































