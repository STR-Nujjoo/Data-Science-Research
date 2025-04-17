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
                              '20161226', '20180319', '20181130', '20200425', '20210106', '20211224', '20220610')


# LULC --------------------------------------------------------------------
# reading all the file names
final_lulc_names <- sapply(seq_along(FINAL_LULC), function (x){sub('LULC ', '', FINAL_LULC[[x]]@file@name)})

# removing the duplicate LULC
LULC <- lapply(seq_along(which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)), 
function (x) {FINAL_LULC[[which(!final_lulc_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from LULC- we're only dealing with 108 periods now from 2014 to 2022
LULC_2014_2022 <- lapply(1:108, function (x) {LULC[[x]]})

# # Visualising the LULC to check if everything is in order
# pblapply(seq_along(LULC_2014_2022), function (x) {
#   tm_shape(LULC_2014_2022[[x]])+
#     tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
#     tm_layout(main.title= LULC_2014_2022[[x]]@file@name,
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

# Visualising the NDMI to check if everything is in order
lapply(seq_along(NDMI_2014_2022), function(x) {plot(NDMI_2014_2022[[x]],
                                                    col = NDMI_colour_ramp,
                                                    main = NDMI_2014_2022[[x]]@file@name)})

# NBR ---------------------------------------------------------------------

# reading all the file names
NBR_names <- sapply(seq_along(NBR_rasters_after_interpolation), function (x){
  gsub('-','', sub('NBR: ','', NBR_rasters_after_interpolation[[x]]@file@name))})

# removing the duplicate NBR
NBR <- lapply(seq_along(which(!NBR_names %in% duplicate_aerial_imageries_to_remove)), 
               function (x) {NBR_rasters_after_interpolation[[which(!NBR_names %in% duplicate_aerial_imageries_to_remove)[x]]]})

# exclude 2023 period from NBR- we're only dealing with 108 periods now from 2014 to 2022
NBR_2014_2022 <- lapply(1:108, function (x) {NBR[[x]]})

# # Visualising the NBR to check if everything is in order
# lapply(seq_along(NBR_2014_2022), function(x) {plot(NBR_2014_2022[[x]],
#                                                     col = NBR_colour_ramp,
#                                                     main = NBR_2014_2022[[x]]@file@name)})

# ATP ---------------------------------------------------------------------

# exclude 2023 period from ATP- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
ATP_2002_2022 <- lapply(1:252, function(x) {WorldClimCHIRPS_precipitation_raster_list[[x]]})

# exclude 2023 period from ATP- we're only dealing with 108 periods now from 2014 to 2022
ATP_2014_2022 <- lapply(145:252, function(x) {ATP_2002_2022[[x]]})

# # Visualising the ATP to check if everything is in order
# lapply(seq_along(ATP_2014_2022), function (x) {plot(ATP_2014_2022[[x]], 
#                                                     main = names(ATP_2014_2022[[x]]), 
#                                                     col = blue_ramp)})

# AMT ---------------------------------------------------------------------

# exclude 2023 period from AMT- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
AMT_2002_2022 <- lapply(1:252, function(x) {WorldClim_S3LST_temperature_raster_list[[x]]})

# In 2022, there is a slight resolution and extent difference due to acquisition from 2 different platform. 
# Resample the latter for consistency in the data
lapply(241:252, function (x) {AMT_2002_2022[[x]] <<- resample(AMT_2002_2022[[x]], AMT_2002_2022[[240]], method = 'ngb')})


# exclude 2023 period from AMT- we're only dealing with 108 periods now from 2014 to 2022
AMT_2014_2022 <- lapply(145:252, function(x) {AMT_2002_2022[[x]]})

# # Visualising the AMT to check if everything is in order
# lapply(seq_along(AMT_2014_2022), function (x) {plot(AMT_2014_2022[[x]],
#                                                     main = names(AMT_2014_2022[[x]]),
#                                                     col = red_ramp)})


# ANSWS -------------------------------------------------------------------

# exclude 2023 period from ANSWS- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
ANSWS_2002_2022 <- lapply(1:252, function(x) {windspeed_raster_list[[x]]})

# exclude 2023 period from ANSWS- we're only dealing with 108 periods now from 2014 to 2022
ANSWS_2014_2022 <- lapply(145:252, function(x) {ANSWS_2002_2022[[x]]})

# # Visualising the ANSWS to check if everything is in order
# lapply(seq_along(ANSWS_2014_2022), function (x) {plot(ANSWS_2014_2022[[x]],
#                                                     main = names(ANSWS_2014_2022[[x]]),
#                                                     col = wind_color_ramp)})

# ARH ---------------------------------------------------------------------

# exclude 2023 period from ARH- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
ARH_2002_2022 <- lapply(1:252, function(x) {RH_raster_list[[x]]})

# exclude 2023 period from ARH- we're only dealing with 108 periods now from 2014 to 2022
ARH_2014_2022 <- lapply(145:252, function(x) {ARH_2002_2022[[x]]})

# # Visualising the ARH to check if everything is in order
# lapply(seq_along(ARH_2014_2022), function (x) {plot(ARH_2014_2022[[x]],
#                                                     main = names(ARH_2014_2022[[x]]),
#                                                     col = RH_color_ramp)})

# Elevation ---------------------------------------------------------------

# elevation is a static variable therefore replicated to match the 2002 to 2014 period
elevation_replicated_for_2002_to_2022 <- replicate(252, elevation_raster_trans)

# elevation is a static variable therefore replicated to match the 2002 to 2014 period
elevation_replicated_for_2014_to_2022 <- replicate(108, elevation_raster_trans)

# Slope -------------------------------------------------------------------

# slope is a static variable therefore replicated to match the 2002 to 2014 period
slope_replicated_for_2002_to_2022 <- replicate(252, slope_raster_trans)

# slope is a static variable therefore replicated to match the 2002 to 2014 period
slope_replicated_for_2014_to_2022 <- replicate(108, slope_raster_trans)

# Aspect ------------------------------------------------------------------

# aspect is a static variable therefore replicated to match the 2002 to 2014 period
aspect_replicated_for_2002_to_2022 <- replicate(252, aspect_raster_trans)

# aspect is a static variable therefore replicated to match the 2002 to 2014 period
aspect_replicated_for_2014_to_2022 <- replicate(108, aspect_raster_trans)

# Fire --------------------------------------------------------------------

# exclude 2023 period from fire data- we will deal with 252 periods from 2002 to 2022 as a form of sensitivity analysis at a later stage
FIRE_2002_2022 <- lapply(1:252, function(x) {FIRE_DATA[[x]]})

# exclude 2023 period from fire data- we're only dealing with 108 periods now from 2014 to 2022
FIRE_2014_2022 <- lapply(145:252, function(x) {FIRE_2002_2022[[x]]})

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
NDVI_2014_2022_stack <- stack(NDVI_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
NDMI_2014_2022_stack <- stack(NDMI_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
NBR_2014_2022_stack <- stack(NBR_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
ATP_2014_2022_stack <- stack(ATP_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
AMT_2014_2022_stack <- stack(AMT_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
ANSWS_2014_2022_stack <- stack(ANSWS_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
ARH_2014_2022_stack <- stack(ARH_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
elevation_replicated_for_2014_to_2022_stack <- stack(elevation_replicated_for_2014_to_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
slope_replicated_for_2014_to_2022_stack <- stack(slope_replicated_for_2014_to_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()
aspect_replicated_for_2014_to_2022 <- stack(aspect_replicated_for_2014_to_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()

# response variable
FIRE_2014_2022_stack <- stack(FIRE_2014_2022) |> resample(LULC_2014_2022[[1]], method = 'ngb') |> stack()

# stack raster for each variable (2002 to 2022) - 7 predictor variables (longer timeframe with less predictor variable)
ATP_2002_2022_stack <- stack(ATP_2002_2022)
AMT_2002_2022_stack <- stack(AMT_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
ANSWS_2002_2022_stack <- stack(ANSWS_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
ARH_2002_2022_stack <- stack(ARH_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
elevation_replicated_for_2002_to_2022_stack <- stack(elevation_replicated_for_2002_to_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
slope_replicated_for_2002_to_2022_stack <- stack(slope_replicated_for_2002_to_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()
aspect_replicated_for_2002_to_2022_stack <- stack(aspect_replicated_for_2002_to_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()

# response variable
FIRE_2002_2022_stack <- stack(FIRE_2002_2022) |> resample(ATP_2002_2022[[1]], method = 'ngb') |> stack()

# Reshape for ConvLSTM format  --------------------------------------------
predictor_variables_2014_2022 <- abind(LULC_2014_2022_stack|> as.array(),
                        NDVI_2014_2022_stack|> as.array(),
                        NDMI_2014_2022_stack|> as.array(),
                        NBR_2014_2022_stack|> as.array(),
                        ATP_2014_2022_stack|> as.array(),
                        AMT_2014_2022_stack|> as.array(),
                        ANSWS_2014_2022_stack|> as.array(),
                        ARH_2014_2022_stack|> as.array(),
                        elevation_replicated_for_2014_to_2022_stack|> as.array(),
                        slope_replicated_for_2014_to_2022_stack|> as.array(),
                        aspect_replicated_for_2014_to_2022|> as.array(),
                        along = 4) # Shape: ([1] height/row, [2] width/column, [3] time_steps, [4] variables/channels)



predictor_variables_2014_2022[, , 1, 1]
predictor_variables_2014_2022[is.na(predictor_variables_2014_2022)] <- 0
NDVI_2014_2022[[1]]|> is.na()
predictor_variables_2014_2022
predictor_variables_2014_2022|> dim()
