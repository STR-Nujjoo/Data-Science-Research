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
}

# Combined visualisation of both FIRMS and SANPARKs fire hotspots detected
# SANPARK years = 2002[1], 2003[2], 2004[3], 2005[4], 2006[5], 2007[6], ..., 2021[20], 2022[21] :index in []
# FIRMS years = 2003[1], 2004[2], 2005[3], 2006[4], 2007[5], 2009[6], 2012[7], 2015[8], 2016[9], 2017[10], 2018[11],
# 2019[12], 2020[13], 2021[14], 2022[15], 2023[16]:index in []
{
  par(mfrow = c(5,5))
  # 2002
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 1, plot = F)
  # FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 14), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  # plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2003
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 2, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 1), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2004
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 3, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 2), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2005
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 4, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 3), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2006
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 5, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 4), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2007
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 6, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 5), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2008
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 7, plot = F)
  # FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 5), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  # plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2009
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 8, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 6), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # 2010
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 9, plot = F)
  # FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 6), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  # plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2011
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 10, plot = F)
  # FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 6), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  # plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2012
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 11, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 7), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2013
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 12, plot = F)
  # FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 7), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  # plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2014
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 13, plot = F)
  # FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 7), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  # plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2015
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 14, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 8), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2016
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 15, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 9), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2017
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 16, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 10), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2018
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 17, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 11), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2019
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 18, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 12), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2020
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 19, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 13), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2021
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 20, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 14), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2022
  SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 21, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 15), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = SANPARK@data$YEAR[1])
  plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  #2023
  # SANPARK <- yearly_sanpark_fire_shpfile(data = sanpark_fire_shpfile_list, index = 21, plot = F)
  FIRMS <- as(firms_fire_shpfile_trans_df_2002_2023_yearly(data = firms_fire_shpfile_trans_df_2002_2023, index = 16), 'Spatial')
  
  par(mar = c(0.1, 0.1, 1.0, 0.1)) # customised margin
  plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
       main = FIRMS@data$ACQ_YEAR[1])
  # plot(SANPARK, col = alpha('red',.3), border = 'red', add = T)
  plot(FIRMS, pch = 16, cex = .5, col = 'brown', add = T)
  
  # Visualisation of hotspots from 2002 to 2023
    plot(roi_trans, col = 'transparent', border = 'black', lwd = 1, 
         main = '2002-2023')
    plot(sanpark_fire_shpfile_combind_list_trans_intersect, col = alpha('red',.3), border = 'red', add = T)
    plot(as(firms_fire_shpfile_trans_df_2002_2023, 'Spatial'), pch = 16, cex = .5, col = 'red', add = T)
  
}















