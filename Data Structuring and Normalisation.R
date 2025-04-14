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

# Visualising the LULC to check if everything is in order
pblapply(seq_along(LULC_2014_2022), function (x) {
  tm_shape(LULC_2014_2022[[x]])+
    tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
    tm_layout(main.title= LULC_2014_2022[[x]]@file@name,
              main.title.size =.6,
              main.title.position = c("center", "top"),
              legend.outside = F,
              legend.text.size = .5)+
    tm_graticules(lines = F)
})


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
NDVI_rasters_after_interpolation

# NDMI --------------------------------------------------------------------


# NBR ---------------------------------------------------------------------


# ATP ---------------------------------------------------------------------


# AMT ---------------------------------------------------------------------


# ANSWS -------------------------------------------------------------------


# ARH ---------------------------------------------------------------------


# Elevation ---------------------------------------------------------------


# Slope -------------------------------------------------------------------


# Aspect ------------------------------------------------------------------


# Fire --------------------------------------------------------------------






# Data Stacking -----------------------------------------------------------
# stack raster for each variable
LULC_2014_2022_stack <- stack(LULC_2014_2022)
str(LULC_2014_2022_stack)





