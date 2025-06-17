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
}


# Load data if necessary! -------------------------------------------------
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/LULC 2014-2023 (post-processing)/FINAL_LULC.Rdata')



# Data preparation --------------------------------------------------------
# identifying dupicates aerial imageries from 2014 to 2022
duplicate_aerial_imageries_to_remove <- c('20140425', '20140612', '20140714', '20141002', '20150122', '20150223', '20150903',
                              '20161226', '20180319', '20181130', '20200425', '20210106', '20211224', '20220610', '20231003',
                              '20231206')


# LULC --------------------------------------------------------------------
# reading all the file names
final_lulc_names <- sapply(seq_along(FINAL_LULC), function (x){sub('LULC ', '', FINAL_LULC[[x]]@file@name)})

# removing the duplicate LULC
LULC <- lapply(seq_along(which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)), 
function (x) {FINAL_LULC[[which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from LULC- we're only dealing with 108 periods now from 2014 to 2022
LULC_2014_2022 <- lapply(1:108, function (x) {LULC[[x]]})

lapply(1:108, function (x) {names(LULC_2014_2022[[x]]) <- LULC_2014_2022[[x]]@file@name
names(LULC_2014_2022[[x]]) <<- gsub('[.]','', names(LULC_2014_2022[[x]]))}) # rename layers
# save(LULC_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/LULC_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/LULC_2014_2022.Rdata')

# 2023 period only from LULC (just in case!)
LULC_2023 <- lapply(109:120, function (x) {LULC[[x]]})

lapply(1:12, function (x) {names(LULC_2023[[x]]) <- LULC_2023[[x]]@file@name
names(LULC_2023[[x]]) <<- gsub('[.]','', names(LULC_2023[[x]]))}) # rename layers
# save(LULC_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/LULC_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/LULC_2023.Rdata')

# # Visualising the LULC to check if everything is in order
# pblapply(seq_along(LULC_2023), function (x) {
#   tm_shape(LULC_2023[[x]])+
#     tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
#     tm_layout(main.title= LULC_2023[[x]]@file@name,
#               main.title.size =.6,
#               main.title.position = c("center", "top"),
#               legend.outside = F,
#               legend.text.size = .5)+
#     tm_graticules(lines = F)
# })

{
  # calculate the average days between consecutive LULC after removal of duplicate LULC (duplicate as in 2 LULC for the same month)
  ave_d <- as.Date(sapply(seq_along(LULC_2014_2022), function (x){sub('LULC ', '', LULC_2014_2022[[x]]@file@name)}), '%Y%m%d') |> 
    diff() |> mean() |> ceiling()
  
  # Visualisation of the days between consecutive pairs of LULC after removal of duplicate LULC (duplicate as in 2 LULC for the same month)
  p1 <- as.Date(sapply(seq_along(LULC_2014_2022), function (x){sub('LULC ', '', LULC_2014_2022[[x]]@file@name)}), '%Y%m%d') |> 
    diff() |> 
    as.numeric() |> 
    hist(xlab = '',
         ylab = '',
         main = '',
         # main = 'Range of Days between Consecutive\n Pairs of Observation (2014-2023)',
         xaxt = 'n',
         #cex.main = .9,
         cex.axis = .6,
         #cex.sub = .8,
         # col = color_ramp(max(p1$counts))[p1$counts],
         col = 'bisque') # apply colour ramp on histogram
  axis(side = 1, at = p1$breaks, cex.axis = .6) # re-adjust the x-axis ticks and values
  text(p1$mids,
       p1$counts,
       labels=p1$counts, 
       adj=c(0.5, -0.5),
       cex = .6) # label each bin with their respective frequency
  title(ylab="Frequency", line=2, cex.lab=.8) # make y axis label closer to the y-axis
  title(xlab = "Range of Days", line= 2, cex.lab = .8) # make x axis label closer to the y-axis
  title(sub = "(post-interpolation & duplication removal)", line= 2.8, cex.sub = .8) # make x-axis sub label closer to the x-axis
  abline(v = ave_d,
         lty = 2,
         col = 'red') # add dotted line to represent mean value
  text(x = ave_d - 1.7, 
       y = 25, 
       srt = 90,
       label = substitute(paste(phantom() %~~% phantom(), ave_d, " days"), 
                          list(ave_d = ave_d)),
       col = 'red',
       cex = .7) # position the mean value text
  # SAVE  PLOT AT 4.15 X 4.09 inches
} # histogram for consecutive pairs after removal of duplication

# NDVI --------------------------------------------------------------------
# reading all the file names
NDVI_names <- sapply(seq_along(NDVI_rasters_after_interpolation), function (x){
  gsub('-','', sub('NDVI: ','', NDVI_rasters_after_interpolation[[x]]@file@name))})

# removing the duplicate NDVI
NDVI <- lapply(seq_along(which(!NDVI_names %in% duplicate_aerial_imageries_to_remove)), 
               function (x) {NDVI_rasters_after_interpolation[[which(!NDVI_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from NDVI- we're only dealing with 108 periods now from 2014 to 2022
NDVI_2014_2022 <- lapply(1:108, function (x) {NDVI[[x]]})
lapply(1:108, function (x) {names(NDVI_2014_2022[[x]]) <- NDVI_2014_2022[[x]]@file@name
names(NDVI_2014_2022[[x]]) <<- gsub('[.]','', names(NDVI_2014_2022[[x]]))}) # rename layers
# save(NDVI_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/NDVI_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/NDVI_2014_2022.Rdata')

# 2023 period only from NDVI (just in case!)
NDVI_2023 <- lapply(109:120, function (x) {NDVI[[x]]})

lapply(1:12, function (x) {names(NDVI_2023[[x]]) <- NDVI_2023[[x]]@file@name
names(NDVI_2023[[x]]) <<- gsub('[.]','', names(NDVI_2023[[x]]))}) # rename layers
# save(NDVI_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/NDVI_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/NDVI_2023.Rdata')


# # Visualising the NDVI to check if everything is in order
# lapply(seq_along(NDVI_2014_2022), function(x) {plot(NDVI_2014_2022[[x]],
#                                                     col = NDVI_colour_ramp,
#                                                     main = NDVI_2014_2022[[x]]@file@name)})


# NDMI --------------------------------------------------------------------
# reading all the file names
NDMI_names <- sapply(seq_along(NDMI_rasters_after_interpolation), function (x){
  gsub('-','', sub('NDMI: ','', NDMI_rasters_after_interpolation[[x]]@file@name))})

# removing the duplicate NDMI
NDMI <- lapply(seq_along(which(!NDMI_names %in% duplicate_aerial_imageries_to_remove)), 
               function (x) {NDMI_rasters_after_interpolation[[which(!NDMI_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from NDMI- we're only dealing with 108 periods now from 2014 to 2022
NDMI_2014_2022 <- lapply(1:108, function (x) {NDMI[[x]]})

lapply(1:108, function (x) {names(NDMI_2014_2022[[x]]) <- NDMI_2014_2022[[x]]@file@name
names(NDMI_2014_2022[[x]]) <<- gsub('[.]','', names(NDMI_2014_2022[[x]]))}) # rename layers
# save(NDMI_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/NDMI_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/NDMI_2014_2022.Rdata')

# 2023 period only from NDMI (just in case!)
NDMI_2023 <- lapply(109:120, function (x) {NDMI[[x]]})

lapply(1:12, function (x) {names(NDMI_2023[[x]]) <- NDMI_2023[[x]]@file@name
names(NDMI_2023[[x]]) <<- gsub('[.]','', names(NDMI_2023[[x]]))}) # rename layers
# save(NDMI_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/NDMI_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/NDMI_2023.Rdata')

# # Visualising the NDMI to check if everything is in order
# lapply(seq_along(NDMI_2014_2022), function(x) {plot(NDMI_2014_2022[[x]],
#                                                     col = NDMI_colour_ramp,
#                                                     main = NDMI_2014_2022[[x]]@file@name)})


# NBR ---------------------------------------------------------------------

# reading all the file names
NBR_names <- sapply(seq_along(NBR_rasters_after_interpolation), function (x){
  gsub('-','', sub('NBR: ','', NBR_rasters_after_interpolation[[x]]@file@name))})

# removing the duplicate NBR
NBR <- lapply(seq_along(which(!NBR_names %in% duplicate_aerial_imageries_to_remove)), 
               function (x) {NBR_rasters_after_interpolation[[which(!NBR_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from NBR- we're only dealing with 108 periods now from 2014 to 2022
NBR_2014_2022 <- lapply(1:108, function (x) {NBR[[x]]})

lapply(1:108, function (x) {names(NBR_2014_2022[[x]]) <- NBR_2014_2022[[x]]@file@name
names(NBR_2014_2022[[x]]) <<- gsub('[.]','', names(NBR_2014_2022[[x]]))}) # rename layers
# save(NBR_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/NBR_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/NBR_2014_2022.Rdata')

# 2023 period only from NBR (just in case!)
NBR_2023 <- lapply(109:120, function (x) {NBR[[x]]})

lapply(1:12, function (x) {names(NBR_2023[[x]]) <- NBR_2023[[x]]@file@name
names(NBR_2023[[x]]) <<- gsub('[.]','', names(NBR_2023[[x]]))}) # rename layers
# save(NBR_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/NBR_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/NBR_2023.Rdata')

# # Visualising the NBR to check if everything is in order
# lapply(seq_along(NBR_2014_2022), function(x) {plot(NBR_2014_2022[[x]],
#                                                     col = NBR_colour_ramp,
#                                                     main = NBR_2014_2022[[x]]@file@name)})

# ATP ---------------------------------------------------------------------

# exclude 2023 period from ATP- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
ATP_2002_2022 <- lapply(1:252, function(x) {WorldClimCHIRPS_precipitation_raster_list[[x]]})
# save(ATP_2002_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/ATP_2002_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/ATP_2002_2022.Rdata')

# exclude 2023 period from ATP- we're only dealing with 108 periods now from 2014 to 2022
ATP_2014_2022 <- lapply(145:252, function(x) {ATP_2002_2022[[x]]})
# save(ATP_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/ATP_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/ATP_2014_2022.Rdata')

# 2023 period only from ATP (just in case!)
ATP_2023 <- lapply(253:264, function(x) {WorldClimCHIRPS_precipitation_raster_list[[x]]})
# save(ATP_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/ATP_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/ATP_2023.Rdata')


# # Visualising the ATP to check if everything is in order
# lapply(seq_along(ATP_2014_2022), function (x) {plot(ATP_2014_2022[[x]], 
#                                                     main = names(ATP_2014_2022[[x]]), 
#                                                     col = blue_ramp)})

# AMT ---------------------------------------------------------------------

# exclude 2023 period from AMT- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
AMT_2002_2022 <- lapply(1:252, function(x) {WorldClim_S3LST_temperature_raster_list[[x]]})

lapply(241:252, function(x){
  names(AMT_2002_2022[[x]]) <- names(ATP_2002_2022[[x]])
  names(AMT_2002_2022[[x]]) <<- sub('TP','AMT', names(AMT_2002_2022[[x]]))
}) # rename last few layers

# In 2022, there is a slight resolution and extent difference due to acquisition from 2 different platform. 
# Resample the latter for consistency in the data
lapply(241:252, function (x) {AMT_2002_2022[[x]] <<- resample(AMT_2002_2022[[x]], AMT_2002_2022[[240]], method = 'ngb')})
# save(AMT_2002_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/AMT_2002_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/AMT_2002_2022.Rdata')

# exclude 2023 period from AMT- we're only dealing with 108 periods now from 2014 to 2022
AMT_2014_2022 <- lapply(145:252, function(x) {AMT_2002_2022[[x]]})
# save(AMT_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/AMT_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/AMT_2014_2022.Rdata')

# 2023 period only from AMT (just in case!)
AMT_2023 <- lapply(253:264, function(x) {WorldClim_S3LST_temperature_raster_list[[x]]})

lapply(1:12, function(x){
  names(AMT_2023[[x]]) <- names(ATP_2023[[x]])
  names(AMT_2023[[x]]) <<- sub('TP','AMT', names(AMT_2023[[x]]))
}) # rename last few layers
# In 2023, there is a slight resolution and extent difference due to acquisition from 2 different platform. 
# Resample the latter for consistency in the data
lapply(1:12, function (x) {AMT_2023[[x]] <<- resample(AMT_2023[[x]], AMT_2002_2022[[240]], method = 'ngb')})
# save(AMT_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/AMT_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/AMT_2023.Rdata')

# # Visualising the AMT to check if everything is in order
# lapply(seq_along(AMT_2014_2022), function (x) {plot(AMT_2014_2022[[x]],
#                                                     main = names(AMT_2014_2022[[x]]),
#                                                     col = red_ramp)})


# ANSWS -------------------------------------------------------------------

# exclude 2023 period from ANSWS- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
ANSWS_2002_2022 <- lapply(1:252, function(x) {windspeed_raster_list[[x]]})
# save(ANSWS_2002_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/ANSWS_2002_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/ANSWS_2002_2022.Rdata')

# exclude 2023 period from ANSWS- we're only dealing with 108 periods now from 2014 to 2022
ANSWS_2014_2022 <- lapply(145:252, function(x) {ANSWS_2002_2022[[x]]})
# save(ANSWS_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/ANSWS_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/ANSWS_2014_2022.Rdata')

# 2023 period only from ANSWS (just in case!)
ANSWS_2023 <- lapply(253:264, function(x) {windspeed_raster_list[[x]]})
# save(ANSWS_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/ANSWS_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/ANSWS_2023.Rdata')

# # Visualising the ANSWS to check if everything is in order
# lapply(seq_along(ANSWS_2014_2022), function (x) {plot(ANSWS_2014_2022[[x]],
#                                                     main = names(ANSWS_2014_2022[[x]]),
#                                                     col = wind_color_ramp)})

# ARH ---------------------------------------------------------------------

# exclude 2023 period from ARH- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
ARH_2002_2022 <- lapply(1:252, function(x) {RH_raster_list[[x]]})
# save(ARH_2002_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/ARH_2002_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/ARH_2002_2022.Rdata')


# exclude 2023 period from ARH- we're only dealing with 108 periods now from 2014 to 2022
ARH_2014_2022 <- lapply(145:252, function(x) {ARH_2002_2022[[x]]})
# save(ARH_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/ARH_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/ARH_2014_2022.Rdata')

# 2023 period only from ARH (just in case!)
ARH_2023 <- lapply(253:264, function(x) {RH_raster_list[[x]]})
# save(ARH_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/ARH_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/ARH_2023.Rdata')


# # Visualising the ARH to check if everything is in order
# lapply(seq_along(ARH_2014_2022), function (x) {plot(ARH_2014_2022[[x]],
#                                                     main = names(ARH_2014_2022[[x]]),
#                                                     col = RH_color_ramp)})

# Elevation ---------------------------------------------------------------

# elevation is a static variable therefore replicated to match the 2002 to 2022 period
elevation_replicated_for_2002_to_2022 <- replicate(252, elevation_raster_trans)

lapply(seq_along(elevation_replicated_for_2002_to_2022), function(x){
  names(elevation_replicated_for_2002_to_2022[[x]]) <- names(ATP_2002_2022[[x]])
  names(elevation_replicated_for_2002_to_2022[[x]]) <<- sub('TP','Elev', names(elevation_replicated_for_2002_to_2022[[x]]))
}) # rename layers although variables are static
# save(elevation_replicated_for_2002_to_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/elevation_replicated_for_2002_to_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/elevation_replicated_for_2002_to_2022.Rdata')

# elevation is a static variable therefore replicated to match the 2014 to 2022 period
elevation_replicated_for_2014_to_2022 <- lapply(145:252, function(x) {elevation_replicated_for_2002_to_2022[[x]]})
# save(elevation_replicated_for_2014_to_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/elevation_replicated_for_2014_to_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/elevation_replicated_for_2014_to_2022.Rdata')

# generate 2023 period for Elevation (just in case!)
elevation_replicated_for_2023 <- replicate(12, elevation_raster_trans)
lapply(seq_along(elevation_replicated_for_2023), function(x){
  names(elevation_replicated_for_2023[[x]]) <- names(ATP_2023[[x]])
  names(elevation_replicated_for_2023[[x]]) <<- sub('TP','Elev', names(elevation_replicated_for_2023[[x]]))
}) # rename layers although variables are static
# save(elevation_replicated_for_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/elevation_replicated_for_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/elevation_replicated_for_2023.Rdata')


# Slope -------------------------------------------------------------------

# slope is a static variable therefore replicated to match the 2002 to 2022 period
slope_replicated_for_2002_to_2022 <- replicate(252, slope_raster_trans)

lapply(seq_along(slope_replicated_for_2002_to_2022), function(x){
  names(slope_replicated_for_2002_to_2022[[x]]) <- names(ATP_2002_2022[[x]])
  names(slope_replicated_for_2002_to_2022[[x]]) <<- sub('TP','Slope', names(slope_replicated_for_2002_to_2022[[x]]))
}) # rename layers although variables are static
# save(slope_replicated_for_2002_to_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/slope_replicated_for_2002_to_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/slope_replicated_for_2002_to_2022.Rdata')


# slope is a static variable therefore replicated to match the 2014 to 2022 period
slope_replicated_for_2014_to_2022 <- lapply(145:252, function(x) {slope_replicated_for_2002_to_2022[[x]]})
# save(slope_replicated_for_2014_to_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/slope_replicated_for_2014_to_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/slope_replicated_for_2014_to_2022.Rdata')

# generate 2023 period for slope (just in case!)
slope_replicated_for_2023 <- replicate(12, slope_raster_trans)
lapply(seq_along(slope_replicated_for_2023), function(x){
  names(slope_replicated_for_2023[[x]]) <- names(ATP_2023[[x]])
  names(slope_replicated_for_2023[[x]]) <<- sub('TP','Slope', names(slope_replicated_for_2023[[x]]))
}) # rename layers although variables are static
# save(slope_replicated_for_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/slope_replicated_for_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/slope_replicated_for_2023.Rdata')

# Aspect ------------------------------------------------------------------

# aspect is a static variable therefore replicated to match the 2002 to 2022 period
aspect_replicated_for_2002_to_2022 <- replicate(252, aspect_raster_trans)

lapply(seq_along(aspect_replicated_for_2002_to_2022), function(x){
  names(aspect_replicated_for_2002_to_2022[[x]]) <- names(ATP_2002_2022[[x]])
  names(aspect_replicated_for_2002_to_2022[[x]]) <<- sub('TP','Aspect', names(aspect_replicated_for_2002_to_2022[[x]]))
}) # rename layers although variables are static
# save(aspect_replicated_for_2002_to_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/aspect_replicated_for_2002_to_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/aspect_replicated_for_2002_to_2022.Rdata')


# aspect is a static variable therefore replicated to match the 2014 to 2022 period
aspect_replicated_for_2014_to_2022 <- lapply(145:252, function(x) {aspect_replicated_for_2002_to_2022[[x]]})
# save(aspect_replicated_for_2014_to_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/aspect_replicated_for_2014_to_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/aspect_replicated_for_2014_to_2022.Rdata')

# generate 2023 period for aspect (just in case!)
aspect_replicated_for_2023 <- replicate(12, aspect_raster_trans)
lapply(seq_along(aspect_replicated_for_2023), function(x){
  names(aspect_replicated_for_2023[[x]]) <- names(ATP_2023[[x]])
  names(aspect_replicated_for_2023[[x]]) <<- sub('TP','Aspect', names(aspect_replicated_for_2023[[x]]))
}) # rename layers although variables are static
# save(aspect_replicated_for_2023, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/aspect_replicated_for_2023.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Individual raster format (not normalised)/aspect_replicated_for_2023.Rdata')


# Fire --------------------------------------------------------------------

# exclude 2023 period from fire data- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
FIRE_2002_2022 <- lapply(1:252, function(x) {FIRE_DATA[[x]]})
# save(FIRE_2002_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/FIRE_2002_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Individual raster format (not normalised)/FIRE_2002_2022.Rdata')


# exclude 2023 period from fire data- we're only dealing with 108 periods now from 2014 to 2022
FIRE_2014_2022 <- lapply(145:252, function(x) {FIRE_2002_2022[[x]]})
# save(FIRE_2014_2022, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/FIRE_2014_2022.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Individual raster format (not normalised)/FIRE_2014_2022.Rdata')


# # Visualising the fire data to check if everything is in order
# lapply(seq_along(FIRE_2014_2022), function (x) {
#   # Set color based on the condition
#   fire_color_condition <- if (all(values(FIRE_2014_2022[[x]]) %>% na.omit() == 0)) {
#     "lightgray"
#   } else {
#     c("lightgray", "red")
#   }
#   plot(FIRE_2014_2022[[x]], 
#        col = fire_color_condition,
#        main = names(FIRE_2014_2022[[x]]), 
#        legend = F)})

# Data Stacking -----------------------------------------------------------
# stack raster for each variable (2014 to 2022) - 11 predictor variables (shorter timeframe with more predictor variables)
# Note: the variables differ in dimension slightly by 1 or 2 pixels and resampling is necessary to ensure consistency: LULC was the chosen baseline for resampling
LULC_2014_2022_stack <- stack(LULC_2014_2022)
# save(LULC_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/LULC_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/LULC_2014_2022_stack.Rdata')
NDVI_2014_2022_stack <- stack(NDVI_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(NDVI_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/NDVI_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/NDVI_2014_2022_stack.Rdata')
NDMI_2014_2022_stack <- stack(NDMI_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(NDMI_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/NDMI_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/NDMI_2014_2022_stack.Rdata')
NBR_2014_2022_stack <- stack(NBR_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(NBR_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/NBR_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/NBR_2014_2022_stack.Rdata')
ATP_2014_2022_stack <- stack(ATP_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(ATP_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/ATP_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/ATP_2014_2022_stack.Rdata')
AMT_2014_2022_stack <- stack(AMT_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(AMT_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/AMT_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/AMT_2014_2022_stack.Rdata')
ANSWS_2014_2022_stack <- stack(ANSWS_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(ANSWS_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/ANSWS_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/ANSWS_2014_2022_stack.Rdata')
ARH_2014_2022_stack <- stack(ARH_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(ARH_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/ARH_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/ARH_2014_2022_stack.Rdata')
elevation_replicated_for_2014_to_2022_stack <- stack(elevation_replicated_for_2014_to_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(elevation_replicated_for_2014_to_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/elevation_replicated_for_2014_to_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/elevation_replicated_for_2014_to_2022_stack.Rdata')
slope_replicated_for_2014_to_2022_stack <- stack(slope_replicated_for_2014_to_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(slope_replicated_for_2014_to_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/slope_replicated_for_2014_to_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/slope_replicated_for_2014_to_2022_stack.Rdata')
aspect_replicated_for_2014_to_2022_stack <- stack(aspect_replicated_for_2014_to_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(aspect_replicated_for_2014_to_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/aspect_replicated_for_2014_to_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/aspect_replicated_for_2014_to_2022_stack.Rdata')

# response variable 2014 to 2022
FIRE_2014_2022_stack <- stack(FIRE_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(FIRE_2014_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/FIRE_2014_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/FIRE_2014_2022_stack.Rdata')

RESAMPLED_FIRE_2014_2022_training_stack <- stack(RESAMPLED_FIRE_2014_2022_training_list) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(RESAMPLED_FIRE_2014_2022_training_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/RESAMPLED_FIRE_2014_2022_training_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/RESAMPLED_FIRE_2014_2022_training_stack.Rdata')

RESAMPLED_FIRE_2014_2022_buffered_training_stack <- stack(RESAMPLED_FIRE_2014_2022_buffered_training_list) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(RESAMPLED_FIRE_2014_2022_buffered_training_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/RESAMPLED_FIRE_2014_2022_buffered_training_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/RESAMPLED_FIRE_2014_2022_buffered_training_stack.Rdata')

RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_stack <- stack(RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_list) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Not-Normalised/RESAMPLED_NON_BUFFERED_FIRE_2014_2022_training_stack.Rdata')


# stack raster for each variable (2002 to 2022) - 7 predictor variables (longer timeframe with less predictor variable)
ATP_2002_2022_stack <- stack(ATP_2002_2022)
# save(ATP_2002_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/ATP_2002_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/ATP_2002_2022_stack.Rdata')
AMT_2002_2022_stack <- stack(AMT_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
# save(AMT_2002_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/AMT_2002_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/AMT_2002_2022_stack.Rdata')
ANSWS_2002_2022_stack <- stack(ANSWS_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
# save(ANSWS_2002_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/ANSWS_2002_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/ANSWS_2002_2022_stack.Rdata')
ARH_2002_2022_stack <- stack(ARH_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
# save(ARH_2002_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/ARH_2002_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/ARH_2002_2022_stack.Rdata')
elevation_replicated_for_2002_to_2022_stack <- stack(elevation_replicated_for_2002_to_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
# save(elevation_replicated_for_2002_to_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/elevation_replicated_for_2002_to_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/elevation_replicated_for_2002_to_2022_stack.Rdata')
slope_replicated_for_2002_to_2022_stack <- stack(slope_replicated_for_2002_to_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
# save(slope_replicated_for_2002_to_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/slope_replicated_for_2002_to_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/slope_replicated_for_2002_to_2022_stack.Rdata')
aspect_replicated_for_2002_to_2022_stack <- stack(aspect_replicated_for_2002_to_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
# save(aspect_replicated_for_2002_to_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/aspect_replicated_for_2002_to_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/aspect_replicated_for_2002_to_2022_stack.Rdata')


# response variable
FIRE_2002_2022_stack <- stack(FIRE_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
# save(FIRE_2002_2022_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/FIRE_2002_2022_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Not-Normalised/FIRE_2002_2022_stack.Rdata')

# stack raster for each variable (2023)- just in case! - 11 predictor variables (shorter timeframe with more predictor variables)
LULC_2023_stack <- stack(LULC_2023)
# save(LULC_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/LULC_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/LULC_2023_stack.Rdata')
NDVI_2023_stack <- stack(NDVI_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(NDVI_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/NDVI_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/NDVI_2023_stack.Rdata')

NDMI_2023_stack <- stack(NDMI_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(NDMI_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/NDMI_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/NDMI_2023_stack.Rdata')

NBR_2023_stack <- stack(NBR_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(NBR_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/NBR_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/NBR_2023_stack.Rdata')

ATP_2023_stack <- stack(ATP_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(ATP_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/ATP_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/ATP_2023_stack.Rdata')

AMT_2023_stack <- stack(AMT_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(AMT_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/AMT_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/AMT_2023_stack.Rdata')

ANSWS_2023_stack <- stack(ANSWS_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(ANSWS_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/ANSWS_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/ANSWS_2023_stack.Rdata')

ARH_2023_stack <- stack(ARH_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(ARH_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/ARH_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/ARH_2023_stack.Rdata')

elevation_replicated_for_2023_stack <- stack(elevation_replicated_for_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(elevation_replicated_for_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/elevation_replicated_for_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/elevation_replicated_for_2023_stack.Rdata')

slope_replicated_for_2023_stack <- stack(slope_replicated_for_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(slope_replicated_for_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/slope_replicated_for_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/slope_replicated_for_2023_stack.Rdata')

aspect_replicated_for_2023_stack <- stack(aspect_replicated_for_2023) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
# save(aspect_replicated_for_2023_stack, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/aspect_replicated_for_2023_stack.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Not-Normalised/aspect_replicated_for_2023_stack.Rdata')

# Applying min-max normalisation to stack raster --------------------------

# Function to be applied on the raster values; return: rasterLayer object
raster_stack_minmax_norm <- function(stack_raster, index) {
  
  data <- stack_raster # raster stack
  min_val <-  min(minValue(data)) # global minimum of raster stack
  max_val <- max(maxValue(data)) # global maximum of raster stack
  index <- index # raster index
  val <- data[[index]] # relevant raster only
  
  x <- (val - min_val) / (max_val - min_val) # normalisation calculation
  x[is.na(values(x))] <- 0 # convert all NA values after normalisation to 0
  x[x < 0] <- 0     # correct tiny negative values due to floating point error to 0
  x[x > 1] <- 1     # similarly just in case of overshoots restrict value to 1
  
  return(x)
}

# Normalising the 2014 to 2022 stack using the min-max normalisation function with all NAs converted to 0
# Normalising after splitting to training, validation and test set
LULC_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: LULC training set 
                                       function(x) {LULC_2014_2022_stack[[x]]}) |> stack()
LULC_2014_2018_stack_norm_train <- pblapply(seq_along(LULC_2014_2018_stack_train@layers), # 2014-2018: LULC training set normalised
                                            function(x) {raster_stack_minmax_norm(LULC_2014_2018_stack_train, x)}) |> stack()
# save(LULC_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/LULC_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/LULC_2014_2018_stack_norm_train.Rdata')

LULC_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: LULC validation set 
                                          function(x) {LULC_2014_2022_stack[[x]]}) |> stack()
LULC_2019_2020_stack_norm_val <- pblapply(seq_along(LULC_2019_2020_stack_val@layers), # 2019-2020: LULC validation set normalised
                                            function(x) {raster_stack_minmax_norm(LULC_2019_2020_stack_val, x)}) |> stack()
# save(LULC_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/LULC_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/LULC_2019_2020_stack_norm_val.Rdata')

LULC_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: LULC test set 
                                           function(x) {LULC_2014_2022_stack[[x]]}) |> stack()
LULC_2021_2022_stack_norm_test <- pblapply(seq_along(LULC_2021_2022_stack_test@layers), # 2021-2022: LULC test set normalised
                                      function(x) {raster_stack_minmax_norm(LULC_2021_2022_stack_test, x)}) |> stack()
# save(LULC_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/LULC_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/LULC_2021_2022_stack_norm_test.Rdata')

NDVI_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: NDVI training set 
                                       function(x) {NDVI_2014_2022_stack[[x]]}) |> stack()
NDVI_2014_2018_stack_norm_train <- pblapply(seq_along(NDVI_2014_2018_stack_train@layers), # 2014-2018: NDVI training set normalised
                                            function(x) {raster_stack_minmax_norm(NDVI_2014_2018_stack_train, x)}) |> stack()

# save(NDVI_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/NDVI_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/NDVI_2014_2018_stack_norm_train.Rdata')

NDVI_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: NDVI validation set 
                                     function(x) {NDVI_2014_2022_stack[[x]]}) |> stack()
NDVI_2019_2020_stack_norm_val <- pblapply(seq_along(NDVI_2019_2020_stack_val@layers), # 2019-2020: NDVI validation set normalised
                                          function(x) {raster_stack_minmax_norm(NDVI_2019_2020_stack_val, x)}) |> stack()

# save(NDVI_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/NDVI_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/NDVI_2019_2020_stack_norm_val.Rdata')

NDVI_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: NDVI test set 
                                      function(x) {NDVI_2014_2022_stack[[x]]}) |> stack()
NDVI_2021_2022_stack_norm_test <- pblapply(seq_along(NDVI_2021_2022_stack_test@layers), # 2021-2022: NDVI test set normalised
                                           function(x) {raster_stack_minmax_norm(NDVI_2021_2022_stack_test, x)}) |> stack()

# save(NDVI_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/NDVI_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/NDVI_2021_2022_stack_norm_test.Rdata')


NDMI_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: NDMI training set 
                                       function(x) {NDMI_2014_2022_stack[[x]]}) |> stack()
NDMI_2014_2018_stack_norm_train <- pblapply(seq_along(NDMI_2014_2018_stack_train@layers), # 2014-2018: NDMI training set normalised
                                            function(x) {raster_stack_minmax_norm(NDMI_2014_2018_stack_train, x)}) |> stack()

# save(NDMI_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/NDMI_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/NDMI_2014_2018_stack_norm_train.Rdata')

NDMI_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: NDMI validation set 
                                     function(x) {NDMI_2014_2022_stack[[x]]}) |> stack()
NDMI_2019_2020_stack_norm_val <- pblapply(seq_along(NDMI_2019_2020_stack_val@layers), # 2019-2020: NDMI validation set normalised
                                          function(x) {raster_stack_minmax_norm(NDMI_2019_2020_stack_val, x)}) |> stack()

# save(NDMI_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/NDMI_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/NDMI_2019_2020_stack_norm_val.Rdata')

NDMI_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: NDMI test set 
                                      function(x) {NDMI_2014_2022_stack[[x]]}) |> stack()
NDMI_2021_2022_stack_norm_test <- pblapply(seq_along(NDMI_2021_2022_stack_test@layers), # 2021-2022: NDMI test set normalised
                                           function(x) {raster_stack_minmax_norm(NDMI_2021_2022_stack_test, x)}) |> stack()

# save(NDMI_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/NDMI_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/NDMI_2021_2022_stack_norm_test.Rdata')

NBR_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: NBR training set 
                                       function(x) {NBR_2014_2022_stack[[x]]}) |> stack()
NBR_2014_2018_stack_norm_train <- pblapply(seq_along(NBR_2014_2018_stack_train@layers), # 2014-2018: NBR training set normalised
                                            function(x) {raster_stack_minmax_norm(NBR_2014_2018_stack_train, x)}) |> stack()

# save(NBR_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/NBR_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/NBR_2014_2018_stack_norm_train.Rdata')

NBR_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: NBR validation set 
                                     function(x) {NBR_2014_2022_stack[[x]]}) |> stack()
NBR_2019_2020_stack_norm_val <- pblapply(seq_along(NBR_2019_2020_stack_val@layers), # 2019-2020: NBR validation set normalised
                                          function(x) {raster_stack_minmax_norm(NBR_2019_2020_stack_val, x)}) |> stack()

# save(NBR_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/NBR_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/NBR_2019_2020_stack_norm_val.Rdata')

NBR_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: NBR test set 
                                      function(x) {NBR_2014_2022_stack[[x]]}) |> stack()
NBR_2021_2022_stack_norm_test <- pblapply(seq_along(NBR_2021_2022_stack_test@layers), # 2021-2022: NBR test set normalised
                                           function(x) {raster_stack_minmax_norm(NBR_2021_2022_stack_test, x)}) |> stack()

# save(NBR_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/NBR_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/NBR_2021_2022_stack_norm_test.Rdata')

ATP_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: ATP training set 
                                      function(x) {ATP_2014_2022_stack[[x]]}) |> stack()
ATP_2014_2018_stack_norm_train <- pblapply(seq_along(ATP_2014_2018_stack_train@layers), # 2014-2018: ATP training set normalised
                                           function(x) {raster_stack_minmax_norm(ATP_2014_2018_stack_train, x)}) |> stack()

# save(ATP_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ATP_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ATP_2014_2018_stack_norm_train.Rdata')

ATP_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: ATP validation set 
                                    function(x) {ATP_2014_2022_stack[[x]]}) |> stack()
ATP_2019_2020_stack_norm_val <- pblapply(seq_along(ATP_2019_2020_stack_val@layers), # 2019-2020: ATP validation set normalised
                                         function(x) {raster_stack_minmax_norm(ATP_2019_2020_stack_val, x)}) |> stack()

# save(ATP_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ATP_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ATP_2019_2020_stack_norm_val.Rdata')

ATP_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: ATP test set 
                                     function(x) {ATP_2014_2022_stack[[x]]}) |> stack()
ATP_2021_2022_stack_norm_test <- pblapply(seq_along(ATP_2021_2022_stack_test@layers), # 2021-2022: ATP test set normalised
                                          function(x) {raster_stack_minmax_norm(ATP_2021_2022_stack_test, x)}) |> stack()

# save(ATP_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ATP_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ATP_2021_2022_stack_norm_test.Rdata')

AMT_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: AMT training set 
                                      function(x) {AMT_2014_2022_stack[[x]]}) |> stack()
AMT_2014_2018_stack_norm_train <- pblapply(seq_along(AMT_2014_2018_stack_train@layers), # 2014-2018: AMT training set normalised
                                           function(x) {raster_stack_minmax_norm(AMT_2014_2018_stack_train, x)}) |> stack()

# save(AMT_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/AMT_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/AMT_2014_2018_stack_norm_train.Rdata')

AMT_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: AMT validation set 
                                    function(x) {AMT_2014_2022_stack[[x]]}) |> stack()
AMT_2019_2020_stack_norm_val <- pblapply(seq_along(AMT_2019_2020_stack_val@layers), # 2019-2020: AMT validation set normalised
                                         function(x) {raster_stack_minmax_norm(AMT_2019_2020_stack_val, x)}) |> stack()

# save(AMT_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/AMT_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/AMT_2019_2020_stack_norm_val.Rdata')

AMT_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: AMT test set 
                                     function(x) {AMT_2014_2022_stack[[x]]}) |> stack()
AMT_2021_2022_stack_norm_test <- pblapply(seq_along(AMT_2021_2022_stack_test@layers), # 2021-2022: AMT test set normalised
                                          function(x) {raster_stack_minmax_norm(AMT_2021_2022_stack_test, x)}) |> stack()

# save(AMT_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/AMT_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/AMT_2021_2022_stack_norm_test.Rdata')


ANSWS_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: ANSWS training set 
                                      function(x) {ANSWS_2014_2022_stack[[x]]}) |> stack()
ANSWS_2014_2018_stack_norm_train <- pblapply(seq_along(ANSWS_2014_2018_stack_train@layers), # 2014-2018: ANSWS training set normalised
                                           function(x) {raster_stack_minmax_norm(ANSWS_2014_2018_stack_train, x)}) |> stack()

# save(ANSWS_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ANSWS_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ANSWS_2014_2018_stack_norm_train.Rdata')

ANSWS_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: ANSWS validation set 
                                    function(x) {ANSWS_2014_2022_stack[[x]]}) |> stack()
ANSWS_2019_2020_stack_norm_val <- pblapply(seq_along(ANSWS_2019_2020_stack_val@layers), # 2019-2020: ANSWS validation set normalised
                                         function(x) {raster_stack_minmax_norm(ANSWS_2019_2020_stack_val, x)}) |> stack()

# save(ANSWS_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ANSWS_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ANSWS_2019_2020_stack_norm_val.Rdata')

ANSWS_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: ANSWS test set 
                                     function(x) {ANSWS_2014_2022_stack[[x]]}) |> stack()
ANSWS_2021_2022_stack_norm_test <- pblapply(seq_along(ANSWS_2021_2022_stack_test@layers), # 2021-2022: ANSWS test set normalised
                                          function(x) {raster_stack_minmax_norm(ANSWS_2021_2022_stack_test, x)}) |> stack()

# save(ANSWS_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ANSWS_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ANSWS_2021_2022_stack_norm_test.Rdata')

ARH_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: ARH training set 
                                        function(x) {ARH_2014_2022_stack[[x]]}) |> stack()
ARH_2014_2018_stack_norm_train <- pblapply(seq_along(ARH_2014_2018_stack_train@layers), # 2014-2018: ARH training set normalised
                                             function(x) {raster_stack_minmax_norm(ARH_2014_2018_stack_train, x)}) |> stack()

# save(ARH_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ARH_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ARH_2014_2018_stack_norm_train.Rdata')

ARH_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: ARH validation set 
                                      function(x) {ARH_2014_2022_stack[[x]]}) |> stack()
ARH_2019_2020_stack_norm_val <- pblapply(seq_along(ARH_2019_2020_stack_val@layers), # 2019-2020: ARH validation set normalised
                                           function(x) {raster_stack_minmax_norm(ARH_2019_2020_stack_val, x)}) |> stack()

# save(ARH_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ARH_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ARH_2019_2020_stack_norm_val.Rdata')

ARH_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: ARH test set 
                                       function(x) {ARH_2014_2022_stack[[x]]}) |> stack()
ARH_2021_2022_stack_norm_test <- pblapply(seq_along(ARH_2021_2022_stack_test@layers), # 2021-2022: ARH test set normalised
                                            function(x) {raster_stack_minmax_norm(ARH_2021_2022_stack_test, x)}) |> stack()

# save(ARH_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ARH_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ARH_2021_2022_stack_norm_test.Rdata')

ELEV_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: ELEV training set 
                                      function(x) {elevation_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
ELEV_2014_2018_stack_norm_train <- pblapply(seq_along(ELEV_2014_2018_stack_train@layers), # 2014-2018: ELEV training set normalised
                                           function(x) {raster_stack_minmax_norm(ELEV_2014_2018_stack_train, x)}) |> stack()

# save(ELEV_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ELEV_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ELEV_2014_2018_stack_norm_train.Rdata')

ELEV_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: ELEV validation set 
                                    function(x) {elevation_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
ELEV_2019_2020_stack_norm_val <- pblapply(seq_along(ELEV_2019_2020_stack_val@layers), # 2019-2020: ELEV validation set normalised
                                         function(x) {raster_stack_minmax_norm(ELEV_2019_2020_stack_val, x)}) |> stack()

# save(ELEV_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ELEV_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ELEV_2019_2020_stack_norm_val.Rdata')

ELEV_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: ELEV test set 
                                     function(x) {elevation_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
ELEV_2021_2022_stack_norm_test <- pblapply(seq_along(ELEV_2021_2022_stack_test@layers), # 2021-2022: ELEV test set normalised
                                          function(x) {raster_stack_minmax_norm(ELEV_2021_2022_stack_test, x)}) |> stack()

# save(ELEV_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ELEV_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ELEV_2021_2022_stack_norm_test.Rdata')

SLOPE_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: SLOPE training set 
                                       function(x) {slope_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
SLOPE_2014_2018_stack_norm_train <- pblapply(seq_along(SLOPE_2014_2018_stack_train@layers), # 2014-2018: SLOPE training set normalised
                                            function(x) {raster_stack_minmax_norm(SLOPE_2014_2018_stack_train, x)}) |> stack()

# save(SLOPE_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/SLOPE_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/SLOPE_2014_2018_stack_norm_train.Rdata')

SLOPE_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: SLOPE validation set 
                                     function(x) {slope_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
SLOPE_2019_2020_stack_norm_val <- pblapply(seq_along(SLOPE_2019_2020_stack_val@layers), # 2019-2020: SLOPE validation set normalised
                                          function(x) {raster_stack_minmax_norm(SLOPE_2019_2020_stack_val, x)}) |> stack()

# save(SLOPE_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/SLOPE_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/SLOPE_2019_2020_stack_norm_val.Rdata')

SLOPE_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: SLOPE test set 
                                      function(x) {slope_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
SLOPE_2021_2022_stack_norm_test <- pblapply(seq_along(SLOPE_2021_2022_stack_test@layers), # 2021-2022: SLOPE test set normalised
                                           function(x) {raster_stack_minmax_norm(SLOPE_2021_2022_stack_test, x)}) |> stack()

# save(SLOPE_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/SLOPE_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/SLOPE_2021_2022_stack_norm_test.Rdata')

ASPECT_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: ASPECT training set 
                                        function(x) {aspect_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
ASPECT_2014_2018_stack_norm_train <- pblapply(seq_along(ASPECT_2014_2018_stack_train@layers), # 2014-2018: ASPECT training set normalised
                                             function(x) {raster_stack_minmax_norm(ASPECT_2014_2018_stack_train, x)}) |> stack()

# save(ASPECT_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ASPECT_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/ASPECT_2014_2018_stack_norm_train.Rdata')

ASPECT_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: ASPECT validation set 
                                      function(x) {aspect_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
ASPECT_2019_2020_stack_norm_val <- pblapply(seq_along(ASPECT_2019_2020_stack_val@layers), # 2019-2020: ASPECT validation set normalised
                                           function(x) {raster_stack_minmax_norm(ASPECT_2019_2020_stack_val, x)}) |> stack()

# save(ASPECT_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ASPECT_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/ASPECT_2019_2020_stack_norm_val.Rdata')

ASPECT_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: ASPECT test set 
                                       function(x) {aspect_replicated_for_2014_to_2022_stack[[x]]}) |> stack()
ASPECT_2021_2022_stack_norm_test <- pblapply(seq_along(ASPECT_2021_2022_stack_test@layers), # 2021-2022: ASPECT test set normalised
                                            function(x) {raster_stack_minmax_norm(ASPECT_2021_2022_stack_test, x)}) |> stack()

# save(ASPECT_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ASPECT_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/ASPECT_2021_2022_stack_norm_test.Rdata')


FIRE_2014_2018_stack_train <- pblapply(1:60, # 2014-2018: FIRE training set 
                                      function(x) {FIRE_2014_2022_stack[[x]]}) |> stack()
FIRE_2014_2018_stack_norm_train <- pblapply(seq_along(FIRE_2014_2018_stack_train@layers), # 2014-2018: FIRE training set normalised
                                           function(x) {raster_stack_minmax_norm(FIRE_2014_2018_stack_train, x)}) |> stack()

# save(FIRE_2014_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/FIRE_2014_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Training Set/FIRE_2014_2018_stack_norm_train.Rdata')

FIRE_2019_2020_stack_val <- pblapply(61:84, # 2019-2020: FIRE validation set 
                                    function(x) {FIRE_2014_2022_stack[[x]]}) |> stack()
FIRE_2019_2020_stack_norm_val <- pblapply(seq_along(FIRE_2019_2020_stack_val@layers), # 2019-2020: FIRE validation set normalised
                                         function(x) {raster_stack_minmax_norm(FIRE_2019_2020_stack_val, x)}) |> stack()

# save(FIRE_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/FIRE_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Validation Set/FIRE_2019_2020_stack_norm_val.Rdata')

FIRE_2021_2022_stack_test <- pblapply(85:108, # 2021-2022: FIRE test set 
                                     function(x) {FIRE_2014_2022_stack[[x]]}) |> stack()
FIRE_2021_2022_stack_norm_test <- pblapply(seq_along(FIRE_2021_2022_stack_test@layers), # 2021-2022: FIRE test set normalised
                                          function(x) {raster_stack_minmax_norm(FIRE_2021_2022_stack_test, x)}) |> stack()

# save(FIRE_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/FIRE_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/Test Set/FIRE_2021_2022_stack_norm_test.Rdata')

#------------------------------------------------------------------------------------------------------------
# Normalising the 2002 to 2022 stack using the min-max normalisation function with all NAs converted to 0
ATP_2002_2018_stack_norm_train <- pblapply(1:204, 
                                     function(x) {raster_stack_minmax_norm(ATP_2002_2022_stack, x)}) |> stack()
save(ATP_2002_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ATP_2002_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ATP_2002_2018_stack_norm_train.Rdata')

ATP_2019_2020_stack_norm_val <- pblapply(205:228, 
                                           function(x) {raster_stack_minmax_norm(ATP_2002_2022_stack, x)}) |> stack()
save(ATP_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ATP_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ATP_2019_2020_stack_norm_val.Rdata')

ATP_2021_2022_stack_norm_test <- pblapply(229:252, 
                                         function(x) {raster_stack_minmax_norm(ATP_2002_2022_stack, x)}) |> stack()

save(ATP_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ATP_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ATP_2021_2022_stack_norm_test.Rdata')

ANSWS_2002_2018_stack_norm_train <- pblapply(1:204, 
                                           function(x) {raster_stack_minmax_norm(ANSWS_2002_2022_stack, x)}) |> stack()
save(ANSWS_2002_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ANSWS_2002_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ANSWS_2002_2018_stack_norm_train.Rdata')

ANSWS_2019_2020_stack_norm_val <- pblapply(205:228, 
                                         function(x) {raster_stack_minmax_norm(ANSWS_2002_2022_stack, x)}) |> stack()
save(ANSWS_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ANSWS_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ANSWS_2019_2020_stack_norm_val.Rdata')

ANSWS_2021_2022_stack_norm_test <- pblapply(229:252, 
                                          function(x) {raster_stack_minmax_norm(ANSWS_2002_2022_stack, x)}) |> stack()

save(ANSWS_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ANSWS_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ANSWS_2021_2022_stack_norm_test.Rdata')

ARH_2002_2018_stack_norm_train <- pblapply(1:204, 
                                             function(x) {raster_stack_minmax_norm(ARH_2002_2022_stack, x)}) |> stack()
save(ARH_2002_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ARH_2002_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ARH_2002_2018_stack_norm_train.Rdata')

ARH_2019_2020_stack_norm_val <- pblapply(205:228, 
                                           function(x) {raster_stack_minmax_norm(ARH_2002_2022_stack, x)}) |> stack()
save(ARH_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ARH_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ARH_2019_2020_stack_norm_val.Rdata')

ARH_2021_2022_stack_norm_test <- pblapply(229:252, 
                                            function(x) {raster_stack_minmax_norm(ARH_2002_2022_stack, x)}) |> stack()

save(ARH_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ARH_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ARH_2021_2022_stack_norm_test.Rdata')

ELEV_2002_2018_stack_norm_train <- pblapply(1:204, 
                                            function(x) {raster_stack_minmax_norm(elevation_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(ELEV_2002_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ELEV_2002_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ELEV_2002_2018_stack_norm_train.Rdata')

ELEV_2019_2020_stack_norm_val <- pblapply(205:228, 
                                            function(x) {raster_stack_minmax_norm(elevation_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(ELEV_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ELEV_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ELEV_2019_2020_stack_norm_val.Rdata')

ELEV_2021_2022_stack_norm_test <- pblapply(229:252, 
                                           function(x) {raster_stack_minmax_norm(elevation_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(ELEV_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ELEV_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ELEV_2021_2022_stack_norm_test.Rdata')

SLOPE_2002_2018_stack_norm_train <- pblapply(1:204, 
                                            function(x) {raster_stack_minmax_norm(slope_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(SLOPE_2002_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/SLOPE_2002_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/SLOPE_2002_2018_stack_norm_train.Rdata')

SLOPE_2019_2020_stack_norm_val <- pblapply(205:228, 
                                          function(x) {raster_stack_minmax_norm(slope_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(SLOPE_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/SLOPE_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/SLOPE_2019_2020_stack_norm_val.Rdata')

SLOPE_2021_2022_stack_norm_test <- pblapply(229:252, 
                                           function(x) {raster_stack_minmax_norm(slope_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(SLOPE_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/SLOPE_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/SLOPE_2021_2022_stack_norm_test.Rdata')

ASPECT_2002_2018_stack_norm_train <- pblapply(1:204, 
                                             function(x) {raster_stack_minmax_norm(aspect_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(ASPECT_2002_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ASPECT_2002_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/ASPECT_2002_2018_stack_norm_train.Rdata')

ASPECT_2019_2020_stack_norm_val <- pblapply(205:228, 
                                           function(x) {raster_stack_minmax_norm(aspect_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(ASPECT_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ASPECT_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/ASPECT_2019_2020_stack_norm_val.Rdata')

ASPECT_2021_2022_stack_norm_test <- pblapply(229:252, 
                                            function(x) {raster_stack_minmax_norm(aspect_replicated_for_2002_to_2022_stack, x)}) |> stack()
save(ASPECT_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ASPECT_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/ASPECT_2021_2022_stack_norm_test.Rdata')

FIRE_2002_2018_stack_norm_train <- pblapply(1:204, 
                                           function(x) {raster_stack_minmax_norm(FIRE_2002_2022_stack, x)}) |> stack()
save(FIRE_2002_2018_stack_norm_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/FIRE_2002_2018_stack_norm_train.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Training Set/FIRE_2002_2018_stack_norm_train.Rdata')

FIRE_2019_2020_stack_norm_val <- pblapply(205:228, 
                                         function(x) {raster_stack_minmax_norm(FIRE_2002_2022_stack, x)}) |> stack()
save(FIRE_2019_2020_stack_norm_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/FIRE_2019_2020_stack_norm_val.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Validation Set/FIRE_2019_2020_stack_norm_val.Rdata')

FIRE_2021_2022_stack_norm_test <- pblapply(229:252, 
                                          function(x) {raster_stack_minmax_norm(FIRE_2002_2022_stack, x)}) |> stack()

save(FIRE_2021_2022_stack_norm_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/FIRE_2021_2022_stack_norm_test.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2002-2022/Rasterstack format/Normalised/Test Set/FIRE_2021_2022_stack_norm_test.Rdata')

# Normalising the 2023 stack using the min-max normalisation function with all NAs converted to 0 (just in case!)
LULC_2023_stack_norm <- pblapply(1:nlayers(LULC_2023_stack), 
                                 function(x) {raster_stack_minmax_norm(LULC_2023_stack, x)}) |> stack()
save(LULC_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/LULC_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/LULC_2023_stack_norm.Rdata')

NDVI_2023_stack_norm <- pblapply(1:nlayers(NDVI_2023_stack), 
                                 function(x) {raster_stack_minmax_norm(NDVI_2023_stack, x)}) |> stack()
save(NDVI_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/NDVI_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/NDVI_2023_stack_norm.Rdata')

NDMI_2023_stack_norm <- pblapply(1:nlayers(NDMI_2023_stack), 
                                 function(x) {raster_stack_minmax_norm(NDMI_2023_stack, x)}) |> stack()
save(NDMI_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/NDMI_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/NDMI_2023_stack_norm.Rdata')

NBR_2023_stack_norm <- pblapply(1:nlayers(NBR_2023_stack), 
                                 function(x) {raster_stack_minmax_norm(NBR_2023_stack, x)}) |> stack()
save(NBR_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/NBR_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/NBR_2023_stack_norm.Rdata')

ATP_2023_stack_norm <- pblapply(1:nlayers(ATP_2023_stack), 
                                function(x) {raster_stack_minmax_norm(ATP_2023_stack, x)}) |> stack()
save(ATP_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ATP_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ATP_2023_stack_norm.Rdata')

AMT_2023_stack_norm <- pblapply(1:nlayers(AMT_2023_stack), 
                                function(x) {raster_stack_minmax_norm(AMT_2023_stack, x)}) |> stack()
save(AMT_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/AMT_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/AMT_2023_stack_norm.Rdata')

ANSWS_2023_stack_norm <- pblapply(1:nlayers(ANSWS_2023_stack), 
                                function(x) {raster_stack_minmax_norm(ANSWS_2023_stack, x)}) |> stack()
save(ANSWS_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ANSWS_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ANSWS_2023_stack_norm.Rdata')

ARH_2023_stack_norm <- pblapply(1:nlayers(ARH_2023_stack), 
                                  function(x) {raster_stack_minmax_norm(ARH_2023_stack, x)}) |> stack()
save(ARH_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ARH_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ARH_2023_stack_norm.Rdata')

ELEV_2023_stack_norm <- pblapply(1:nlayers(elevation_replicated_for_2023_stack), 
                                function(x) {raster_stack_minmax_norm(elevation_replicated_for_2023_stack, x)}) |> stack()
save(ELEV_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ELEV_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ELEV_2023_stack_norm.Rdata')

SLOPE_2023_stack_norm <- pblapply(1:nlayers(slope_replicated_for_2023_stack), 
                                 function(x) {raster_stack_minmax_norm(slope_replicated_for_2023_stack, x)}) |> stack()
save(SLOPE_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/SLOPE_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/SLOPE_2023_stack_norm.Rdata')

ASPECT_2023_stack_norm <- pblapply(1:nlayers(aspect_replicated_for_2023_stack), 
                                 function(x) {raster_stack_minmax_norm(aspect_replicated_for_2023_stack, x)}) |> stack()
save(ASPECT_2023_stack_norm, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ASPECT_2023_stack_norm.Rdata')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2023/Rasterstack format/Normalised/ASPECT_2023_stack_norm.Rdata')


# Reshape for ConvLSTM format  --------------------------------------------
# Full training set
predictor_variables_2014_2018_train <- abind(LULC_2014_2018_stack_norm_train|> as.array(),
                                       NDVI_2014_2018_stack_norm_train|> as.array(),
                                       NDMI_2014_2018_stack_norm_train|> as.array(),
                                       NBR_2014_2018_stack_norm_train|> as.array(),
                                       ATP_2014_2018_stack_norm_train|> as.array(),
                                       AMT_2014_2018_stack_norm_train|> as.array(),
                                       ANSWS_2014_2018_stack_norm_train|> as.array(),
                                       ARH_2014_2018_stack_norm_train|> as.array(),
                                       ELEV_2014_2018_stack_norm_train|> as.array(),
                                       SLOPE_2014_2018_stack_norm_train|> as.array(),
                                       ASPECT_2014_2018_stack_norm_train|> as.array(),
                                       along = 4) |> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format

dim(predictor_variables_2014_2018_train)
predictor_variables_2014_2018_train <- array(predictor_variables_2014_2018_train, dim = c(1, dim(predictor_variables_2014_2018_train))) # Adjust dimension to include sample dimension to be 1
dim(predictor_variables_2014_2018_train)

# save(predictor_variables_2014_2018_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2014_2018_train.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2014_2018_train.RData')

response_variable_2014_2018_train <- abind(FIRE_2014_2018_stack_norm_train|>as.array(), 
                                     along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format)
dim(response_variable_2014_2018_train)
response_variable_2014_2018_train <- array(response_variable_2014_2018_train, dim = c(1, dim(response_variable_2014_2018_train))) # Adjust dimension to include sample dimension to be 1
dim(response_variable_2014_2018_train)

# save(response_variable_2014_2018_train, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2014_2018_train.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2014_2018_train.RData')


fire_class_imbalance <- table(response_variable_2014_2018_train) # fire class imbalance
fire_class_imbalance_prop <- prop.table(table(response_variable_2014_2018_train)) # fire class imbalance proportion
calculated_class_weights <- max(fire_class_imbalance)/fire_class_imbalance # class weights to be applied to convLSTM


# Full validation set
predictor_variables_2019_2020_val <- abind(LULC_2019_2020_stack_norm_val|> as.array(),
                                             NDVI_2019_2020_stack_norm_val|> as.array(),
                                             NDMI_2019_2020_stack_norm_val|> as.array(),
                                             NBR_2019_2020_stack_norm_val|> as.array(),
                                             ATP_2019_2020_stack_norm_val|> as.array(),
                                             AMT_2019_2020_stack_norm_val|> as.array(),
                                             ANSWS_2019_2020_stack_norm_val|> as.array(),
                                             ARH_2019_2020_stack_norm_val|> as.array(),
                                             ELEV_2019_2020_stack_norm_val|> as.array(),
                                             SLOPE_2019_2020_stack_norm_val|> as.array(),
                                             ASPECT_2019_2020_stack_norm_val|> as.array(),
                                             along = 4) |> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format

dim(predictor_variables_2019_2020_val)
predictor_variables_2019_2020_val <- array(predictor_variables_2019_2020_val, dim = c(1, dim(predictor_variables_2019_2020_val))) # Adjust dimension to include sample dimension to be 1
dim(predictor_variables_2019_2020_val)
# save(predictor_variables_2019_2020_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2019_2020_val.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2019_2020_val.RData')

response_variable_2019_2020_val <- abind(FIRE_2019_2020_stack_norm_val|>as.array(), 
                                           along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format)
dim(response_variable_2019_2020_val)
response_variable_2019_2020_val <- array(response_variable_2019_2020_val, dim = c(1, dim(response_variable_2019_2020_val))) # Adjust dimension to include sample dimension to be 1
dim(response_variable_2019_2020_val)
# save(response_variable_2019_2020_val, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2019_2020_val.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2019_2020_val.RData')

# Full test set
predictor_variables_2021_2022_test <- abind(LULC_2021_2022_stack_norm_test|> as.array(),
                                           NDVI_2021_2022_stack_norm_test|> as.array(),
                                           NDMI_2021_2022_stack_norm_test|> as.array(),
                                           NBR_2021_2022_stack_norm_test|> as.array(),
                                           ATP_2021_2022_stack_norm_test|> as.array(),
                                           AMT_2021_2022_stack_norm_test|> as.array(),
                                           ANSWS_2021_2022_stack_norm_test|> as.array(),
                                           ARH_2021_2022_stack_norm_test|> as.array(),
                                           ELEV_2021_2022_stack_norm_test|> as.array(),
                                           SLOPE_2021_2022_stack_norm_test|> as.array(),
                                           ASPECT_2021_2022_stack_norm_test|> as.array(),
                                           along = 4) |> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format

dim(predictor_variables_2021_2022_test)
predictor_variables_2021_2022_test <- array(predictor_variables_2021_2022_test, dim = c(1, dim(predictor_variables_2021_2022_test))) # Adjust dimension to include sample dimension to be 1
dim(predictor_variables_2021_2022_test)
# save(predictor_variables_2021_2022_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2021_2022_test.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/predictor_variables_2021_2022_test.RData')

response_variable_2021_2022_test <- abind(FIRE_2021_2022_stack_norm_test|>as.array(), 
                                         along = 4)|> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format)
dim(response_variable_2021_2022_test)
response_variable_2021_2022_test <- array(response_variable_2021_2022_test, dim = c(1, dim(response_variable_2021_2022_test))) # Adjust dimension to include sample dimension to be 1
dim(response_variable_2021_2022_test)
# save(response_variable_2021_2022_test, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2021_2022_test.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/response_variable_2021_2022_test.RData')

# subset predictor variables data to test convLSTM

# subset training set
predictor_variables_2014_2018_train_subset <- abind(
                                             # LULC_2014_2018_stack_norm_train|> as.array(),
                                             NDVI_2014_2018_stack_norm_train|> as.array(),
                                             NDMI_2014_2018_stack_norm_train|> as.array(),
                                             # NBR_2014_2018_stack_norm_train|> as.array(),
                                             ATP_2014_2018_stack_norm_train|> as.array(),
                                             AMT_2014_2018_stack_norm_train|> as.array(),
                                             ANSWS_2014_2018_stack_norm_train|> as.array(),
                                             # ARH_2014_2018_stack_norm_train|> as.array(),
                                             # ELEV_2014_2018_stack_norm_train|> as.array(),
                                             # SLOPE_2014_2018_stack_norm_train|> as.array(),
                                             # ASPECT_2014_2018_stack_norm_train|> as.array(),
                                             along = 4) |> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format

dim(predictor_variables_2014_2018_train_subset)
predictor_variables_2014_2018_train_subset <- array(predictor_variables_2014_2018_train_subset, dim = c(1, dim(predictor_variables_2014_2018_train_subset))) # Adjust dimension to include sample dimension to be 1
dim(predictor_variables_2014_2018_train_subset)

save(predictor_variables_2014_2018_train_subset, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/Subset data/predictor_variables_2014_2018_train_subset.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/Subset data/predictor_variables_2014_2018_train_subset.RData')

# subset validation set
predictor_variables_2019_2020_val_subset <- abind(
                                           # LULC_2019_2020_stack_norm_val|> as.array(),
                                           NDVI_2019_2020_stack_norm_val|> as.array(),
                                           NDMI_2019_2020_stack_norm_val|> as.array(),
                                           # NBR_2019_2020_stack_norm_val|> as.array(),
                                           ATP_2019_2020_stack_norm_val|> as.array(),
                                           AMT_2019_2020_stack_norm_val|> as.array(),
                                           ANSWS_2019_2020_stack_norm_val|> as.array(),
                                           # ARH_2019_2020_stack_norm_val|> as.array(),
                                           # ELEV_2019_2020_stack_norm_val|> as.array(),
                                           # SLOPE_2019_2020_stack_norm_val|> as.array(),
                                           # ASPECT_2019_2020_stack_norm_val|> as.array(),
                                           along = 4) |> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format

dim(predictor_variables_2019_2020_val_subset)
predictor_variables_2019_2020_val_subset <- array(predictor_variables_2019_2020_val_subset, dim = c(1, dim(predictor_variables_2019_2020_val_subset))) # Adjust dimension to include sample dimension to be 1
dim(predictor_variables_2019_2020_val_subset)
# save(predictor_variables_2019_2020_val_subset, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/Subset data/predictor_variables_2019_2020_val_subset.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/Subset data/predictor_variables_2019_2020_val_subset.RData')

# subset test set
predictor_variables_2021_2022_test_subset <- abind(
                                            # LULC_2021_2022_stack_norm_test|> as.array(),
                                            NDVI_2021_2022_stack_norm_test|> as.array(),
                                            NDMI_2021_2022_stack_norm_test|> as.array(),
                                            # NBR_2021_2022_stack_norm_test|> as.array(),
                                            ATP_2021_2022_stack_norm_test|> as.array(),
                                            AMT_2021_2022_stack_norm_test|> as.array(),
                                            ANSWS_2021_2022_stack_norm_test|> as.array(),
                                            # ARH_2021_2022_stack_norm_test|> as.array(),
                                            # ELEV_2021_2022_stack_norm_test|> as.array(),
                                            # SLOPE_2021_2022_stack_norm_test|> as.array(),
                                            # ASPECT_2021_2022_stack_norm_test|> as.array(),
                                            along = 4) |> # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)
  aperm(c(3,1,2,4))  # Reorder shape: (time_steps, height, width, variables)- channels_last format

dim(predictor_variables_2021_2022_test_subset)
predictor_variables_2021_2022_test_subset <- array(predictor_variables_2021_2022_test_subset, dim = c(1, dim(predictor_variables_2021_2022_test_subset))) # Adjust dimension to include sample dimension to be 1
dim(predictor_variables_2021_2022_test_subset)
# save(predictor_variables_2021_2022_test_subset, file = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/Subset data/predictor_variables_2021_2022_test_subset.RData')
# load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/All variables (.Rdata)/2014-2022/Rasterstack format/Normalised/ConvLSTM data format/Subset data/predictor_variables_2021_2022_test_subset.RData')























