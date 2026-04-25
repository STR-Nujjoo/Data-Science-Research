{ # load libraries
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
  library(corrplot)
}

# load data
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Relative humidity/RH_raster_list.Rdata')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Wind Speed/windspeed_raster_list.Rdata')
load('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Processed Variables/Precipitation/Combined precipitation data/WorldClimCHIRPS_precipitation_raster_list.Rdata')

# inspect data
RH_raster_list
windspeed_raster_list
WorldClimCHIRPS_precipitation_raster_list

# RH ----------------------------------------------------------------------
# Trim data from 2014 to 2023
RH_raster_list_2014_2023 <- RH_raster_list[145:264]

# Calculate the median value of RH per raster
median_RH_values <- pbsapply(seq_along(RH_raster_list_2014_2023), function(index){
  values(RH_raster_list_2014_2023[[index]]) |>
    na.omit() |>
    median()
})


# Add the median values to a dataframe
RH_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                        median_RH = median_RH_values) 


RH_EDA_df$year <- year(RH_EDA_df$date) # extract year from date and create a year column
RH_EDA_df$median_RH <- RH_EDA_df$median_RH * 100 # express median RH as %

# ANSWS -------------------------------------------------------------------
# Calculate the median value of ANSWS per raster
median_ANSWS_values <- pbsapply(seq_along(windspeed_raster_list_2014_2023_bilinear), function(index){
  values(windspeed_raster_list_2014_2023_bilinear[[index]]) |>
    na.omit() |>
    median()
})

# Add the median values to a dataframe
ANSWS_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                           median_ANSWS = median_ANSWS_values) 


ANSWS_EDA_df$year <- year(ANSWS_EDA_df$date) # extract year from date and create a year column
ANSWS_EDA_df$month <- month(ANSWS_EDA_df$date) # extract year from date and create a month column

# TP ----------------------------------------------------------------------

# Trim data from 2014 to 2023
WorldClimCHIRPS_2014_2023_precipitation_raster_list <- WorldClimCHIRPS_precipitation_raster_list[145:264]

# Calculate the median value of precipitation per raster
median_precipitation_values <- pbsapply(seq_along(WorldClimCHIRPS_2014_2023_precipitation_raster_list), function(index){
  values(WorldClimCHIRPS_2014_2023_precipitation_raster_list[[index]]) |>
    na.omit() |>
    median()
})


# Add the median values to a dataframe
prec_EDA_df <- data.frame(date = seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month"),
                          median_prec = median_precipitation_values) 


prec_EDA_df$year <- year(prec_EDA_df$date) # extract year from date and create a year column


{
  # data in dataframe format
  # relative humidity
  RH_EDA_df
  head(RH_EDA_df) # inspect data
  par(mfrow=c(3,1))
  plot(RH_EDA_df$date, RH_EDA_df$median_RH, type='l', col = 'orange', main = 'Relative Humidity',
       xlab='Period',
       ylab = 'ARH (%)') # plot data
  
  # wind speed
  ANSWS_EDA_df
  head(ANSWS_EDA_df) # inspect data
  plot(ANSWS_EDA_df$date, ANSWS_EDA_df$median_ANSWS, type='l', col = 'green', main = 'Wind Speed',
       xlab='Period',
       ylab = 'ANSWS (m/s)') # plot data
  
  # precipitation
  prec_EDA_df 
  head(prec_EDA_df) # inspect data
  
  plot(prec_EDA_df$date, prec_EDA_df$median_prec, type='l', col = 'steelblue', main = 'Precipitation',
       xlab='Period',
       ylab = 'TP (mm)') # plot data
}


# Correlation -------------------------------------------------------------

RH_ANSWS_TP_df = data.frame(RH=RH_EDA_df$median_RH, ANSWS=ANSWS_EDA_df$median_ANSWS, TP=prec_EDA_df$median_prec)
head(RH_ANSWS_TP_df)

par(mfrow=c(3,1))
cor(RH_ANSWS_TP_df, method='pearson') |> corrplot(method='number', type='upper')
cor(RH_ANSWS_TP_df, method='kendall') |> corrplot(method='number', type='upper')
cor(RH_ANSWS_TP_df, method='spearman') |> corrplot(method='number', type='upper')
par(mfrow=c(1,1)) # reset

cor.test(RH_ANSWS_TP_df$RH,RH_ANSWS_TP_df$ANSWS, method='pearson')
cor.test(RH_ANSWS_TP_df$RH,RH_ANSWS_TP_df$ANSWS, method='kendall')
cor.test(RH_ANSWS_TP_df$RH,RH_ANSWS_TP_df$ANSWS, method='spearman')

cor.test(RH_ANSWS_TP_df$RH,RH_ANSWS_TP_df$TP, method='pearson')
cor.test(RH_ANSWS_TP_df$RH,RH_ANSWS_TP_df$TP, method='kendall')
cor.test(RH_ANSWS_TP_df$RH,RH_ANSWS_TP_df$TP, method='spearman')












































