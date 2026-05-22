
# Detecting abnormal prediction from convLSTM [2014-2022]- 2021-10 which i suspect the 2017-10 fire had an influence on that.
# Hence, check similariity between few predictor variables between 2017-10 and 2021-10.
NDVI_2014_2022
NDVI_2014_2022[[94]]|>summary() # 2021-10
NDVI_2014_2022[[46]]|>summary() # 2017-10

plot(NDVI_2014_2022[[94]])
plot(NDVI_2014_2022[[46]])

# Extracting the NDVI for October only
NDVI_octobers <- lapply(seq(10,108, by=12), function(x){NDVI_2014_2022[[x]]})
NDVI_2017_10 <- NDVI_2014_2022[[46]] # reference= 2017-10

NDVI_oct_error <- sapply(seq_along(NDVI_octobers), function(x){(values(NDVI_2017_10)|>na.omit()|>median()-values(NDVI_octobers[[x]])|>na.omit()|>median())|>abs()}) # absolute median error

# converting error data into dataframe
NDVI_oct_error_df <- tibble(Period = sapply(seq_along(NDVI_octobers), function(x) {paste0(str_extract(names(NDVI_octobers[[x]]), '\\d{4}'),'-10')})|>as.factor(), 
                            Error = NDVI_oct_error)

NDVI_oct_error_plot <- ggplot(data = NDVI_oct_error_df, aes(x = Period, y = Error))+
  geom_segment(aes(x=Period, xend=Period, y=0, yend=Error), color = 'grey')+
  geom_point(color=c('black', 'black', 'black', 'red', 'black', 'black', 'black', 'green', 'black'), size=3) +
  annotate('text',
           x=4, 
           y=0.001, 
           label = 'Suspect',
           size = 4,
           color = 'red')+ 
  annotate('text',
           x=8, 
           y=0.0015, 
           label = 'Target',
           size = 4,
           color = 'green')+ 
  theme_light()+
  ylab('Absolute Median Error')+
  ggtitle('NDVI')
  

# Use this in case they want to see the boxplot as well!
# NDVI_octobers_df <- as.data.frame(NDVI_octobers|>stack(), xy = F) %>%
#   # convert dataframe into long format where there is only one fire column
#   pivot_longer(
#     cols = starts_with("NDVI"),
#     names_to = "NDVI_Date",
#     values_to = "NDVI_Value"
#   ) %>%
#   na.omit() %>%
#   mutate(NDVI_Year = str_extract(NDVI_Date,'\\d{4}')|>as.factor()) %>%
#   select(NDVI_Year,NDVI_Value)
  
# ggplot(NDVI_octobers_df, aes(x = NDVI_Year, y = NDVI_Value, fill = NDVI_Year)) +
#   geom_boxplot(outliers = F) 


NDMI_2014_2022
NDMI_2014_2022[[94]]|>summary() # 2021-10
NDMI_2014_2022[[46]]|>summary() # 2017-10

plot(NDMI_2014_2022[[94]])
plot(NDMI_2014_2022[[46]])

# Extracting the NDMI for October only
NDMI_octobers <- lapply(seq(10,108, by=12), function(x){NDMI_2014_2022[[x]]})
NDMI_2017_10 <- NDMI_2014_2022[[46]] # reference= 2017-10

NDMI_oct_error <- sapply(seq_along(NDMI_octobers), function(x){(values(NDMI_2017_10)|>na.omit()|>median()-values(NDMI_octobers[[x]])|>na.omit()|>median())|>abs()}) # absolute median error

# converting error data into dataframe
NDMI_oct_error_df <- tibble(Period = sapply(seq_along(NDMI_octobers), function(x) {paste0(str_extract(names(NDMI_octobers[[x]]), '\\d{4}'),'-10')})|>as.factor(), 
                            Error = NDMI_oct_error)

NDMI_oct_error_plot <- ggplot(data = NDMI_oct_error_df, aes(x = Period, y = Error))+
  geom_segment(aes(x=Period, xend=Period, y=0, yend=Error), color = 'grey')+
  geom_point(color=c('black', 'black', 'black', 'red', 'black', 'black', 'black', 'green', 'black'), size=3) +
  annotate('text',
           x=4, 
           y=0.0025, 
           label = 'Suspect',
           size = 4,
           color = 'red')+ 
  annotate('text',
           x=8, 
           y=0.009, 
           label = 'Target',
           size = 4,
           color = 'green')+ 
  theme_light()+
  ylab('Absolute Median Error')+
  ggtitle('NDMI')

october_NDVI_NDMI_error_plots <- plot_grid(NDVI_oct_error_plot,NDMI_oct_error_plot, nrow = 2, ncol = 1)
ggsave('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/october_NDVI_NDMI_error_plots.pdf', plot = october_NDVI_NDMI_error_plots,  width = 6.56, height = 6)


# # Use this in case they want to see the boxplot as well!
# NDMI_octobers_df <- as.data.frame(NDMI_octobers|>stack(), xy = F) %>%
#   # convert dataframe into long format where there is only one fire column
#   pivot_longer(
#     cols = starts_with("NDMI"),
#     names_to = "NDMI_Date",
#     values_to = "NDMI_Value"
#   ) %>%
#   na.omit() %>%
#   mutate(NDMI_Year = str_extract(NDMI_Date,'\\d{4}')|>as.factor()) %>%
#   select(NDMI_Year,NDMI_Value)
# 
# ggplot(NDMI_octobers_df, aes(x = NDMI_Year, y = NDMI_Value, fill = NDMI_Year)) +
#   geom_boxplot(outliers = F)

# ARH_2014_2022
# ARH_2014_2022[[94]]|>summary() # 2021-10
# ARH_2014_2022[[46]]|>summary() # 2017-10
# 
# plot(ARH_2014_2022[[94]])
# plot(ARH_2014_2022[[46]])
# 
# # Extracting the ARH for October only
# ARH_octobers <- lapply(seq(10,108, by=12), function(x){ARH_2014_2022[[x]]})
# ARH_2017_10 <- ARH_2014_2022[[46]] # reference= 2017-10
# 
# ARH_oct_error <- sapply(seq_along(ARH_octobers), function(x){(values(ARH_2017_10)|>na.omit()|>median()-values(ARH_octobers[[x]])|>na.omit()|>median())|>abs()}) # absolute median error
# 
# # converting error data into dataframe
# ARH_oct_error_df <- tibble(Period = sapply(seq_along(ARH_octobers), function(x) {paste0(str_extract(names(ARH_octobers[[x]]), '\\d{4}'),'-10')})|>as.factor(), 
#                             Error = ARH_oct_error)
# 
# ARH_oct_error_plot <- ggplot(data = ARH_oct_error_df, aes(x = Period, y = Error))+
#   geom_segment(aes(x=Period, xend=Period, y=0, yend=Error), color = 'grey')+
#   geom_point(color=c('black', 'black', 'black', 'red', 'black', 'black', 'black', 'green', 'black'), size=3) +
#   annotate('text',
#            x=4, 
#            y=0.001, 
#            label = 'Suspect',
#            size = 4,
#            color = 'red')+ 
#   annotate('text',
#            x=8, 
#            y=0.0075, 
#            label = 'Target',
#            size = 4,
#            color = 'green')+ 
#   theme_light()+
#   ylab('Absolute Median Error')+
#   ggtitle('ARH')
# 
# 
# # # Use this in case they want to see the boxplot as well!
# # ARH_octobers_df <- as.data.frame(ARH_octobers|>stack(), xy = F) %>%
# #   # convert dataframe into long format where there is only one fire column
# #   pivot_longer(
# #     cols = starts_with("ARH"),
# #     names_to = "ARH_Date",
# #     values_to = "ARH_Value"
# #   ) %>%
# #   na.omit() %>%
# #   mutate(ARH_Year = str_extract(ARH_Date,'\\d{4}')|>as.factor()) %>%
# #   select(ARH_Year,ARH_Value)
# # 
# # ggplot(ARH_octobers_df, aes(x = ARH_Year, y = ARH_Value, fill = ARH_Year)) +
# #   geom_boxplot(outliers = F)


WS_RASTERS <- function(index, true_raster, raster_with_probabilities, raster_factor){
  period_name <- sub("^Fire\\s*", "", timesteps_labels[index])
  # Subdivision types
  quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
  
  # Susceptibility quantile classes- makes more sense
  wildfire_susceptibility_quantile_classes <- matrix(c(
    -0.1, quantile_subdivisions[2], 1, # very low
    quantile_subdivisions[2], quantile_subdivisions[3], 2, # low
    quantile_subdivisions[3], quantile_subdivisions[4], 3, # moderate
    quantile_subdivisions[4], quantile_subdivisions[5], 4, # high
    quantile_subdivisions[5], 1, 5 # very high
  ), ncol = 3, byrow = TRUE)
  
  # Reclassify raster accordingly
  classified_raster <- classify(raster_with_probabilities|>rast(), wildfire_susceptibility_quantile_classes)
  levels(classified_raster) <- data.frame(
    ID = 1:5,
    Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
  )
  
  # Update levels
  classified_raster <- droplevels(classified_raster)
  
  # Update levels of other rasters
  levels(true_raster) <- data.frame(
    ID = 0:1,
    fire_status = c('No Fire', 'Fire')
  )
  true_raster <- droplevels(true_raster|>rast())
  
  levels(raster_factor) <- data.frame(
    ID = 0:1,
    fire_status = c('No Fire', 'Fire')
  )
  raster_factor <- droplevels(raster_factor|>rast())
  
  return(list(true_raster,raster_factor,raster_with_probabilities,classified_raster))
}


# CONVLSTM 1 OUTPUT -------------------------------------------------------

timesteps_labels
true_test_raster_list
predicted_raster_list
y_pred_raster_list


WS_RASTERS_list <- pblapply(seq_along(timesteps_labels), function(x){
  WS_RASTERS(index = x,
             true_raster = true_test_raster_list[[x]], 
             raster_with_probabilities = predicted_raster_list[[x]], 
             raster_factor = y_pred_raster_list[[x]])
})


# testing plot grid
tm_shape(WS_RASTERS_list[[24]][[4]])+
  tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(WS_RASTERS_list[[24]][[4]])[[1]]$ID)])+
  tm_layout(main.title= paste0(period_name[24],': True Fire Status'),
            main.title.size =.55,
            main.title.position = 0.26,
            legend.outside = F,
            legend.text.size = .3
            # legend.outside.position = 'bottom'
  )+
  tm_graticules(labels.size = 0.4, n.x = 2, n.y = 4, lines = F)


WSM_combined_plot <- function(data, r1_index, r2_index, r3_index, r4_index){
  period_name <- c('2021-01', '2021-02', '2021-03', '2021-04', '2021-05', '2021-06', '2021-07', '2021-08', '2021-09', '2021-10', '2021-11', '2021-12',
                   '2022-01', '2022-02', '2022-03', '2022-04', '2022-05', '2022-06', '2022-07', '2022-08', '2022-09', '2022-10', '2022-11', '2022-12')
  
  # define a color palette for the wildfire susceptibility class
  WS_palette <- c('#007206', '#7DB810', '#F2FE1E', '#FFAC12','#FC3B09')
  
  
  # Row 1 in full layout
  # Visualising the fire data used as testY
  r1p1 <- tm_shape(data[[r1_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r1_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r1_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r1p2 <- tm_shape(data[[r1_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r1_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r1_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  
  # # Visualise the sd of wildfire probabilities raster
  # r1p3 <- tm_shape(data[[r1_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r1_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  
  
  
  # Visualise the classified raster
  r1p4 <- tm_shape(data[[r1_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r1_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r1_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # Row 2 in full layout
  # Visualising the fire data used as testY
  r2p1 <- tm_shape(data[[r2_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r2_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r2_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r2p2 <- tm_shape(data[[r2_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r2_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r2_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  
  # # Visualise the sd of wildfire probabilities raster
  # r2p3 <- tm_shape(data[[r2_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r2_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  
  
  
  # Visualise the classified raster
  r2p4 <- tm_shape(data[[r2_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r2_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r2_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # Row 3 in full layout
  # Visualising the fire data used as testY
  r3p1 <- tm_shape(data[[r3_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r3_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r3_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r3p2 <- tm_shape(data[[r3_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r3_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r3_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  
  # # Visualise the sd of wildfire probabilities raster
  # r3p3 <- tm_shape(data[[r3_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r3_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  # 
  
  
  # Visualise the classified raster
  r3p4 <- tm_shape(data[[r3_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r3_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r3_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # Row 4 in full layout
  # Visualising the fire data used as testY
  r4p1 <- tm_shape(data[[r4_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r4_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r4_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r4p2 <- tm_shape(data[[r4_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r4_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r4_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # # Visualise the sd of wildfire probabilities raster
  # r4p3 <- tm_shape(data[[r4_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r4_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  
  # Visualise the classified raster
  r4p4 <- tm_shape(data[[r4_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r4_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r4_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3,
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  return(
    tmap_arrange(r1p1,r1p2,r1p4,
                 r2p1,r2p2,r2p4,
                 r3p1,r3p2,r3p4,
                 r4p1,r4p2,r4p4,
                 nrow = 4, ncol = 3)
  ) 

    

}

# 2021
# Plotting and saving output from convlstm [2014-2022]: Jan-Apr 2021
convlstm_wsm_2021_01_to_2021_04_20142022df <- WSM_combined_plot(data = WS_RASTERS_list,
                                                                r1_index = 1,
                                                                r2_index = 2,
                                                                r3_index = 3,
                                                                r4_index = 4)

tmap_save(convlstm_wsm_2021_01_to_2021_04_20142022df, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/convlstm_wsm_2021_01_to_2021_04_20142022df.pdf", width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2014-2022]: May-Aug 2021
convlstm_wsm_2021_05_to_2021_08_20142022df <- WSM_combined_plot(data = WS_RASTERS_list,
                                                                r1_index = 5,
                                                                r2_index = 6,
                                                                r3_index = 7,
                                                                r4_index = 8)

tmap_save(convlstm_wsm_2021_05_to_2021_08_20142022df, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/convlstm_wsm_2021_05_to_2021_08_20142022df.pdf", width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2014-2022]: Sep-Dec 2021
convlstm_wsm_2021_09_to_2021_12_20142022df <- WSM_combined_plot(data = WS_RASTERS_list,
                                                                r1_index = 9,
                                                                r2_index = 10,
                                                                r3_index = 11,
                                                                r4_index = 12)

tmap_save(convlstm_wsm_2021_09_to_2021_12_20142022df, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/convlstm_wsm_2021_09_to_2021_12_20142022df.pdf", width = 6.56, height = 8.50)

# 2022
# Plotting and saving output from convlstm [2014-2022]: Jan-Apr 2022
convlstm_wsm_2022_01_to_2022_04_20142022df <- WSM_combined_plot(data = WS_RASTERS_list,
                                                                r1_index = 13,
                                                                r2_index = 14,
                                                                r3_index = 15,
                                                                r4_index = 16)

tmap_save(convlstm_wsm_2022_01_to_2022_04_20142022df, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/convlstm_wsm_2022_01_to_2022_04_20142022df.pdf", width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2014-2022]: May-Aug 2022
convlstm_wsm_2022_05_to_2022_08_20142022df <- WSM_combined_plot(data = WS_RASTERS_list,
                                                                r1_index = 17,
                                                                r2_index = 18,
                                                                r3_index = 19,
                                                                r4_index = 20)

tmap_save(convlstm_wsm_2022_05_to_2022_08_20142022df, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/convlstm_wsm_2022_05_to_2022_08_20142022df.pdf", width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2014-2022]: Sep-Dec 2022
convlstm_wsm_2022_09_to_2022_12_20142022df <- WSM_combined_plot(data = WS_RASTERS_list,
                                                                r1_index = 21,
                                                                r2_index = 22,
                                                                r3_index = 23,
                                                                r4_index = 24)

tmap_save(convlstm_wsm_2022_09_to_2022_12_20142022df, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/convlstm_wsm_2022_09_to_2022_12_20142022df.pdf", width = 6.56, height = 8.50)


# Performing further analysis with the WSM- temporal trend in classes

Area_per_classes_WSM <- do.call(rbind,lapply(1:24,function(x){freq(WS_RASTERS_list[[x]][[4]])%>%
  mutate(area_in_ha = (count*30*30)/10000,
         date = str_extract(timesteps_labels[x],"\\d{4}-\\d{2}"))%>%
  select(value,area_in_ha,date)}))

Area_per_classes_WSM$date <- factor(Area_per_classes_WSM$date) # converting column to factor
Area_per_classes_WSM$value <- factor(Area_per_classes_WSM$value,
                                     levels = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS"))
str(Area_per_classes_WSM)

# Define custom colors
my_colors <- c(
  "Very Low WS" = '#007206',  # dark green
  "Low WS"      = '#7DB810',  # green
  "Moderate WS" = '#F2FE1E',  # yellow
  "High WS"     = '#FFAC12',  # orange
  "Very High WS"= '#FC3B09'   # red
)

ymin <- 0
ymax <- max(Area_per_classes_WSM$area_in_ha)  # slightly above max for padding


# 1st option of the plot
options(scipen = 999)
ggplot(Area_per_classes_WSM , aes(x = date, y = area_in_ha, colour = value, group = value)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    x = "Period",
    y = "Area (ha)",
    colour = "WS Class"
  ) +
  scale_color_manual(values = my_colors)+
  theme_light() +
  annotate(
    "rect",
    xmin = "2021-11",
    xmax = '2021-12',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-01",
    xmax = '2021-02',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-03",
    xmax = '2021-05',
    ymin = ymin,
    ymax = ymax,
    fill = "yellow", # for autumn
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-11",
    xmax = '2022-12',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-01",
    xmax = '2022-02',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-03",
    xmax = '2022-05',
    ymin = ymin,
    ymax = ymax,
    fill = "yellow", # for autumn
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-12",
    xmax = '2022-01',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-02",
    xmax = '2021-03',
    ymin = ymin,
    ymax = ymax,
    fill = "darkorange", # for summer transitioning to early autumn
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-02",
    xmax = '2022-03',
    ymin = ymin,
    ymax = ymax,
    fill = "darkorange", # for summer transitioning to early autumn
    alpha = 0.2
  )+
  scale_y_log10() + # scale plot if necessary
  facet_wrap(~ value, scales = "free_y", ncol = 1)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


# 2nd option of the plot is to preserve the area- therefore plot separately and combine them into 1

# generate a dataset with continuous 5 classes per period
all_months <- seq.Date(
  from = as.Date("2021-01-01"),
  to   = as.Date("2022-12-01"),
  by   = "month"
) |> format("%Y-%m")

all_classes <- c(
  "Very Low WS",
  "Low WS",
  "Moderate WS",
  "High WS",
  "Very High WS"
)

df_ref <- expand_grid(
  date  = all_months,
  value = all_classes
)

# convert WSM area df to tibble
Area_per_classes_WSM <- as_tibble(Area_per_classes_WSM)

# join both df to see which period did not contain those class
Area_per_classes_WSM_modified <- left_join(df_ref,Area_per_classes_WSM, by = c('date','value'))
Area_per_classes_WSM_modified <- Area_per_classes_WSM_modified %>%
  mutate(area_in_ha = ifelse(is.na(Area_per_classes_WSM_modified$area_in_ha),0,Area_per_classes_WSM_modified$area_in_ha)) # convert any NA to 0
# View(Area_per_classes_WSM_modified)
Area_per_classes_WSM_modified$value <- factor(Area_per_classes_WSM_modified$value,
                                              levels = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS"))

Area_per_classes_WSM_modified %>%
  filter(value=="Very High WS")%>%
  summary()

ymin <- min(Area_per_classes_WSM_modified$area_in_ha)
ymax <- max(Area_per_classes_WSM_modified$area_in_ha) # slightly above max for padding

# 1st option of the plot
options(scipen = 999)
timeseries_WS_class_plot <- ggplot(Area_per_classes_WSM_modified , aes(x = date, y = area_in_ha, colour = value, group = value)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    x = "Period",
    y = "Area (ha)",
    colour = "WS Class"
  ) +
  # geom_smooth(method='lm', se = F, linewidth = 0.3)+
  scale_color_manual(values = my_colors)+
  theme_light(base_size = 9) +
  annotate(
    "rect",
    xmin = "2021-11",
    xmax = '2021-12',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-01",
    xmax = '2021-02',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-03",
    xmax = '2021-05',
    ymin = ymin,
    ymax = ymax,
    fill = "yellow", # for autumn
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-11",
    xmax = '2022-12',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-01",
    xmax = '2022-02',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-03",
    xmax = '2022-05',
    ymin = ymin,
    ymax = ymax,
    fill = "yellow", # for autumn
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-12",
    xmax = '2022-01',
    ymin = ymin,
    ymax = ymax,
    fill = "red", # for summer
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2021-02",
    xmax = '2021-03',
    ymin = ymin,
    ymax = ymax,
    fill = "darkorange", # for summer transitioning to early autumn
    alpha = 0.2
  )+
  annotate(
    "rect",
    xmin = "2022-02",
    xmax = '2022-03',
    ymin = ymin,
    ymax = ymax,
    fill = "darkorange", # for summer transitioning to early autumn
    alpha = 0.2
  )+
  # scale_y_log10() + # scale plot if necessary
  # facet_wrap(~ value, scales = "free_y", ncol = 1)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank()
  )

timeseries_WS_class_plot

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/timeseries_WS_class_plot.pdf", 
       plot = timeseries_WS_class_plot, width = 6.56, height = 3.8)

# this one plots all the labels- separate plots
WS_class_timeseries <- function(data,class,y_axis_title_col, title){
  class_df <- Area_per_classes_WSM_modified%>%filter(value==class)
  ymin <- min(class_df$area_in_ha)
  ymax <- max(class_df$area_in_ha)  
  
  ggplot(class_df, aes(x = date, y = area_in_ha, colour = value, group = value))+
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    labs(
      x = "Period",
      y = "Area (ha)",
      colour = "WS Class"
    ) +
    geom_smooth(method = "loess", se=F, linewidth = 0.2)+
    ggtitle(title)+
    scale_color_manual(values = my_colors)+
    theme_light() +
    annotate(
      "rect",
      xmin = "2021-11",
      xmax = '2021-12',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-01",
      xmax = '2021-02',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-03",
      xmax = '2021-05',
      ymin = ymin,
      ymax = ymax,
      fill = "yellow", # for autumn
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-11",
      xmax = '2022-12',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-01",
      xmax = '2022-02',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-03",
      xmax = '2022-05',
      ymin = ymin,
      ymax = ymax,
      fill = "yellow", # for autumn
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-12",
      xmax = '2022-01',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-02",
      xmax = '2021-03',
      ymin = ymin,
      ymax = ymax,
      fill = "darkorange", # for summer transitioning to early autumn
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-02",
      xmax = '2022-03',
      ymin = ymin,
      ymax = ymax,
      fill = "darkorange", # for summer transitioning to early autumn
      alpha = 0.2
    )+
    theme_light(base_size = 9)+
    theme(legend.position = 'none', 
          plot.title = element_text(size= 7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0.5),
          axis.title.y = element_text(color=y_axis_title_col))
  
}

# this one plots all the labels- separate plots but without x-axis label
WS_class_timeseries_no_x_axis_label <- function(data,class,y_axis_title_col, title){
  class_df <- Area_per_classes_WSM_modified%>%filter(value==class)
  ymin <- min(class_df$area_in_ha)
  ymax <- max(class_df$area_in_ha)  
  
  ggplot(class_df, aes(x = date, y = area_in_ha, colour = value, group = value))+
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    labs(
      # x = "Period",
      y = "Area (ha)",
      colour = "WS Class"
    ) +
    geom_smooth(method = "loess", se=F, linewidth = 0.2)+
    ggtitle(title)+
    scale_color_manual(values = my_colors)+
    theme_light() +
    annotate(
      "rect",
      xmin = "2021-11",
      xmax = '2021-12',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-01",
      xmax = '2021-02',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-03",
      xmax = '2021-05',
      ymin = ymin,
      ymax = ymax,
      fill = "yellow", # for autumn
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-11",
      xmax = '2022-12',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-01",
      xmax = '2022-02',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-03",
      xmax = '2022-05',
      ymin = ymin,
      ymax = ymax,
      fill = "yellow", # for autumn
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-12",
      xmax = '2022-01',
      ymin = ymin,
      ymax = ymax,
      fill = "red", # for summer
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2021-02",
      xmax = '2021-03',
      ymin = ymin,
      ymax = ymax,
      fill = "darkorange", # for summer transitioning to early autumn
      alpha = 0.2
    )+
    annotate(
      "rect",
      xmin = "2022-02",
      xmax = '2022-03',
      ymin = ymin,
      ymax = ymax,
      fill = "darkorange", # for summer transitioning to early autumn
      alpha = 0.2
    )+
    theme_light(base_size = 9)+
    theme(legend.position = 'none', 
          plot.title = element_text(size= 7),
          axis.text.x = element_blank(), axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0.5),
          axis.title.y = element_text(color=y_axis_title_col))
  
}

plt_VLWS <- WS_class_timeseries_no_x_axis_label(Area_per_classes_WSM_modified, "Very Low WS","white","Very Low WS")

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/VLWS_timeseries.pdf", 
       plot = plt_VLWS, width = 6.56, height = 2)
# # Save above plot
# ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/VLWS_timeseries.png", 
#        plot = plt_VLWS, width = 6.56, height = 2)

plt_LWS <-  WS_class_timeseries_no_x_axis_label(Area_per_classes_WSM_modified, "Low WS","white","Low WS")
# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/LWS_timeseries.pdf", 
       plot = plt_LWS, width = 6.56, height = 2)
# # Save above plot
# ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/LWS_timeseries.png", 
#        plot = plt_LWS, width = 6.56, height = 2)



plt_MWS <- WS_class_timeseries_no_x_axis_label(Area_per_classes_WSM_modified, "Moderate WS","black","Moderate WS")
# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/MWS_timeseries.pdf", 
       plot = plt_MWS, width = 6.56, height = 2)
# # Save above plot
# ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/MWS_timeseries.png", 
#        plot = plt_MWS, width = 6.56, height = 2)

plt_HWS <- WS_class_timeseries_no_x_axis_label(Area_per_classes_WSM_modified, "High WS","white","High WS")
# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/HWS_timeseries.pdf", 
       plot = plt_HWS, width = 6.56, height = 2)
# # Save above plot
# ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/HWS_timeseries.png", 
#        plot = plt_HWS, width = 6.56, height = 2)

plt_VHWS <-  WS_class_timeseries(Area_per_classes_WSM_modified, "Very High WS","white","Very High WS")

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/VHWS_timeseries.pdf", 
       plot = plt_VHWS, width = 6.56, height = 2.64)
# # Save above plot
# ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/VHWS_timeseries.png", 
#        plot = plt_VHWS, width = 6.56, height = 2.64)

WSM_timeseries_combined <- plot_grid(plt_VLWS,plt_LWS,plt_MWS,plt_HWS,plt_VHWS, nrow = 5, ncol = 1)

# Save above plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/results plot/WSM_timeseries_combined.pdf", 
       plot = WSM_timeseries_combined, width = 6.56, height = 8.50)

# CONVLSTM 2 OUTPUTS ------------------------------------------------------


WS_RASTERS_2002_2022_list <- pblapply(seq_along(timesteps_labels), function(x){
  WS_RASTERS(index = x,
             true_raster = true_test_raster_list_long[[x]], 
             raster_with_probabilities = predicted_raster_list_long[[x]], 
             raster_factor = y_pred_raster_list_long[[x]])
})


# 2021
# Plotting and saving output from convlstm [2002-2022]: Jan-Apr 2021
convlstm_wsm_2021_01_to_2021_04_20022022df <- WSM_combined_plot(data = WS_RASTERS_2002_2022_list,
                                                                r1_index = 1,
                                                                r2_index = 2,
                                                                r3_index = 3,
                                                                r4_index = 4)

tmap_save(convlstm_wsm_2021_01_to_2021_04_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/convlstm_wsm_2021_01_to_2021_04_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2002-2022]: May-Aug 2021
convlstm_wsm_2021_05_to_2021_08_20022022df <- WSM_combined_plot(data = WS_RASTERS_2002_2022_list,
                                                                r1_index = 5,
                                                                r2_index = 6,
                                                                r3_index = 7,
                                                                r4_index = 8)

tmap_save(convlstm_wsm_2021_05_to_2021_08_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/convlstm_wsm_2021_05_to_2021_08_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2002-2022]: Sep-Dec 2021
convlstm_wsm_2021_09_to_2021_12_20022022df <- WSM_combined_plot(data = WS_RASTERS_2002_2022_list,
                                                                r1_index = 9,
                                                                r2_index = 10,
                                                                r3_index = 11,
                                                                r4_index = 12)

tmap_save(convlstm_wsm_2021_09_to_2021_12_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/convlstm_wsm_2021_09_to_2021_12_20022022df.pdf', width = 6.56, height = 8.50)

# 2022
# Plotting and saving output from convlstm [2002-2022]: Jan-Apr 2022
convlstm_wsm_2022_01_to_2022_04_20022022df <- WSM_combined_plot(data = WS_RASTERS_2002_2022_list,
                                                                r1_index = 13,
                                                                r2_index = 14,
                                                                r3_index = 15,
                                                                r4_index = 16)

tmap_save(convlstm_wsm_2022_01_to_2022_04_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/convlstm_wsm_2022_01_to_2022_04_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2002-2022]: May-Aug 2022
convlstm_wsm_2022_05_to_2022_08_20022022df <- WSM_combined_plot(data = WS_RASTERS_2002_2022_list,
                                                                r1_index = 17,
                                                                r2_index = 18,
                                                                r3_index = 19,
                                                                r4_index = 20)

tmap_save(convlstm_wsm_2022_05_to_2022_08_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/convlstm_wsm_2022_05_to_2022_08_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from convlstm [2002-2022]: Sep-Dec 2022
convlstm_wsm_2022_09_to_2022_12_20022022df <- WSM_combined_plot(data = WS_RASTERS_2002_2022_list,
                                                                r1_index = 21,
                                                                r2_index = 22,
                                                                r3_index = 23,
                                                                r4_index = 24)

tmap_save(convlstm_wsm_2022_09_to_2022_12_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/convlstm_wsm_2022_09_to_2022_12_20022022df.pdf', width = 6.56, height = 8.50)



# RFM 1 OUTPUTS -----------------------------------------------------------

# creating a function for visualisation
WS_RASTERS_RF <- function(df, year, month, test_probs, test_pred_class){
  
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
  
  # Update levels of other rasters
  levels(xx_true) <- data.frame(
    ID = 0:1,
    fire_status = c('No Fire', 'Fire')
  )
  xx_true <- droplevels(xx_true|>rast())
  
  
  # Extract PREDICTED fire event we want to visualise
  xx_pred <- x %>%
    dplyr::select(x,y,test_pred_class) %>%
    rasterFromXYZ(res = c(30,30), crs = crs(roi_trans))
  # names(xx_pred) <- paste0('Predicted Fire Events: ', unique(x$Year), '-',unique(x$Month))
  levels(xx_pred) <- data.frame(
    ID = 0:1,
    fire_status = c('No Fire', 'Fire')
  )
  xx_pred <- droplevels(xx_pred|>rast())
  
  xx_pred_prob <- x %>%
    dplyr::select(x,y,test_probs) %>%
    rasterFromXYZ(res = c(30,30), crs = crs(roi_trans))
  # names(xx_pred_prob) <- paste0('WSM: ', unique(x$Year), '-',unique(x$Month))
  
  # Subdivision types
  quantile_subdivisions <- quantile(0:1, probs = seq(0,1,1/5))
  # Susceptibility quantile classes- makes more sense
  wildfire_susceptibility_quantile_classes <- matrix(c(
    -0.1, quantile_subdivisions[2], 1, # very low
    quantile_subdivisions[2], quantile_subdivisions[3], 2, # low
    quantile_subdivisions[3], quantile_subdivisions[4], 3, # moderate
    quantile_subdivisions[4], quantile_subdivisions[5], 4, # high
    quantile_subdivisions[5], 1, 5 # very high
  ), ncol = 3, byrow = TRUE)
  
  
    # Reclassify raster accordingly
    classified_raster <- classify(rast(xx_pred_prob), wildfire_susceptibility_quantile_classes)
    
    levels(classified_raster) <- data.frame(
      ID = 1:5,
      Susceptibility = c("Very Low WS", "Low WS", "Moderate WS", "High WS", "Very High WS")
    )
    
    # Update levels
    classified_raster <- droplevels(classified_raster)

  return(
    list(xx_true, 
         xx_pred, 
         xx_pred_prob, 
         classified_raster)
  ) 
}

WS_RASTERS_RFM1_2021_list <- pblapply(1:12, function(x){WS_RASTERS_RF(df = test_set1,
                                    year = 2021, month = x,
                                    test_probs = test_probs1, 
                                    test_pred_class = test_pred_class1)})


WS_RASTERS_RFM1_2022_list <- pblapply(1:12, function(x){WS_RASTERS_RF(df = test_set1,
                                                                      year = 2022, month = x,
                                                                      test_probs = test_probs1, 
                                                                      test_pred_class = test_pred_class1)})


RF_WSM_combined_plot <- function(data, period_name_2021 = T, r1_index, r2_index, r3_index, r4_index){
  if(period_name_2021==T){
    period_name <- c('2021-01', '2021-02', '2021-03', '2021-04', '2021-05', '2021-06', '2021-07', '2021-08', '2021-09', '2021-10', '2021-11', '2021-12')
  }else{
    period_name <- c('2022-01', '2022-02', '2022-03', '2022-04', '2022-05', '2022-06', '2022-07', '2022-08', '2022-09', '2022-10', '2022-11', '2022-12')
  }
  
                   
  
  # define a color palette for the wildfire susceptibility class
  WS_palette <- c('#007206', '#7DB810', '#F2FE1E', '#FFAC12','#FC3B09')
  
  
  # Row 1 in full layout
  # Visualising the fire data used as testY
  r1p1 <- tm_shape(data[[r1_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r1_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r1_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r1p2 <- tm_shape(data[[r1_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r1_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r1_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  
  # # Visualise the sd of wildfire probabilities raster
  # r1p3 <- tm_shape(data[[r1_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r1_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  
  
  
  # Visualise the classified raster
  r1p4 <- tm_shape(data[[r1_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r1_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r1_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # Row 2 in full layout
  # Visualising the fire data used as testY
  r2p1 <- tm_shape(data[[r2_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r2_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r2_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r2p2 <- tm_shape(data[[r2_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r2_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r2_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  
  # # Visualise the sd of wildfire probabilities raster
  # r2p3 <- tm_shape(data[[r2_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r2_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  
  
  
  # Visualise the classified raster
  r2p4 <- tm_shape(data[[r2_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r2_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r2_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # Row 3 in full layout
  # Visualising the fire data used as testY
  r3p1 <- tm_shape(data[[r3_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r3_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r3_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r3p2 <- tm_shape(data[[r3_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r3_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r3_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  
  # # Visualise the sd of wildfire probabilities raster
  # r3p3 <- tm_shape(data[[r3_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r3_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  # 
  
  
  # Visualise the classified raster
  r3p4 <- tm_shape(data[[r3_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r3_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r3_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # Row 4 in full layout
  # Visualising the fire data used as testY
  r4p1 <- tm_shape(data[[r4_index]][[1]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r4_index]][[1]]))+
    tm_layout(main.title= paste0(period_name[r4_index],': True Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  r4p2 <- tm_shape(data[[r4_index]][[2]])+
    tm_raster(style = "cat", title = "", palette = fire_color_condition_func(data[[r4_index]][[2]]))+
    tm_layout(main.title= paste0(period_name[r4_index],': Predicted Fire Status'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  # # Visualise the sd of wildfire probabilities raster
  # r4p3 <- tm_shape(data[[r4_index]][[3]])+
  #   tm_raster(style = "sd", title = "", palette = '-RdBu')+
  #   tm_layout(main.title= paste0(period_name[r4_index],': Standard Deviation Map'),
  #             main.title.size =.55,
  #             main.title.position = 0.26,
  #             legend.outside = F,
  #             legend.text.size = .3
  #             # legend.outside.position = 'bottom'
  #   )+
  #   tm_graticules(lines = F)
  
  # Visualise the classified raster
  r4p4 <- tm_shape(data[[r4_index]][[4]])+
    tm_raster(style = "cat", title = "", palette = WS_palette[c(levels(data[[r4_index]][[4]])[[1]]$ID)])+
    tm_layout(main.title= paste0(period_name[r4_index],': WSM'),
              main.title.size =.55,
              main.title.position = 0.26,
              legend.outside = F,
              legend.text.size = .3,
              # legend.outside.position = 'bottom'
    )+
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)
  
  return(
    tmap_arrange(r1p1,r1p2,r1p4,
                 r2p1,r2p2,r2p4,
                 r3p1,r3p2,r3p4,
                 r4p1,r4p2,r4p4,
                 nrow = 4, ncol = 3)
  ) 
  
  
  
}




# 2021
# Plotting and saving output from RF [2014-2022]: Jan-Apr 2021
rf_wsm_2021_01_to_2021_04_20142022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM1_2021_list,
                                                             period_name_2021 = T,
                                                             r1_index = 1,
                                                             r2_index = 2,
                                                             r3_index = 3,
                                                             r4_index = 4)

tmap_save(rf_wsm_2021_01_to_2021_04_20142022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2021_01_to_2021_04_20142022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2014-2022]: May-Aug 2021
rf_wsm_2021_05_to_2021_08_20142022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM1_2021_list,
                                                             period_name_2021 = T,
                                                          r1_index = 5,
                                                          r2_index = 6,
                                                          r3_index = 7,
                                                          r4_index = 8)

tmap_save(rf_wsm_2021_05_to_2021_08_20142022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2021_05_to_2021_08_20142022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2014-2022]: Sep-Dec 2021
rf_wsm_2021_09_to_2021_12_20142022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM1_2021_list,
                                                             period_name_2021 = T,
                                                          r1_index = 9,
                                                          r2_index = 10,
                                                          r3_index = 11,
                                                          r4_index = 12)

tmap_save(rf_wsm_2021_09_to_2021_12_20142022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2021_09_to_2021_12_20142022df.pdf', width = 6.56, height = 8.50)

# 2022
# Plotting and saving output from RF [2014-2022]: Jan-Apr 2022
rf_wsm_2022_01_to_2022_04_20142022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM1_2022_list,
                                                             period_name_2021 = F,
                                                          r1_index = 1,
                                                          r2_index = 2,
                                                          r3_index = 3,
                                                          r4_index = 4)

tmap_save(rf_wsm_2022_01_to_2022_04_20142022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2022_01_to_2022_04_20142022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2014-2022]: May-Aug 2022
rf_wsm_2022_05_to_2022_08_20142022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM1_2022_list,
                                                             period_name_2021 = F,
                                                          r1_index = 5,
                                                          r2_index = 6,
                                                          r3_index = 7,
                                                          r4_index = 8)

tmap_save(rf_wsm_2022_05_to_2022_08_20142022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2022_05_to_2022_08_20142022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2014-2022]: Sep-Dec 2022
rf_wsm_2022_09_to_2022_12_20142022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM1_2022_list,
                                                             period_name_2021 = F,
                                                          r1_index = 9,
                                                          r2_index = 10,
                                                          r3_index = 11,
                                                          r4_index = 12)

tmap_save(rf_wsm_2022_09_to_2022_12_20142022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2022_09_to_2022_12_20142022df.pdf', width = 6.56, height = 8.50)



# RFM 2 OUTPUTS -----------------------------------------------------------

WS_RASTERS_RFM2_2021_list <- pblapply(1:12, function(x){WS_RASTERS_RF(df = test_set2,
                                                                      year = 2021, month = x,
                                                                      test_probs = test_probs2, 
                                                                      test_pred_class = test_pred_class2)})


WS_RASTERS_RFM2_2022_list <- pblapply(1:12, function(x){WS_RASTERS_RF(df = test_set2,
                                                                      year = 2022, month = x,
                                                                      test_probs = test_probs2, 
                                                                      test_pred_class = test_pred_class2)})



# 2021
# Plotting and saving output from convlstm [2002-2022]: Jan-Apr 2021
rf_wsm_2021_01_to_2021_04_20022022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM2_2021_list,
                                                             period_name_2021 = T,
                                                             r1_index = 1,
                                                             r2_index = 2,
                                                             r3_index = 3,
                                                             r4_index = 4)

tmap_save(rf_wsm_2021_01_to_2021_04_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2021_01_to_2021_04_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2002-2022]: May-Aug 2021
rf_wsm_2021_05_to_2021_08_20022022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM2_2021_list,
                                                             period_name_2021 = T,
                                                             r1_index = 5,
                                                             r2_index = 6,
                                                             r3_index = 7,
                                                             r4_index = 8)

tmap_save(rf_wsm_2021_05_to_2021_08_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2021_05_to_2021_08_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2002-2022]: Sep-Dec 2021
rf_wsm_2021_09_to_2021_12_20022022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM2_2021_list,
                                                             period_name_2021 = T,
                                                             r1_index = 9,
                                                             r2_index = 10,
                                                             r3_index = 11,
                                                             r4_index = 12)

tmap_save(rf_wsm_2021_09_to_2021_12_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2021_09_to_2021_12_20022022df.pdf', width = 6.56, height = 8.50)

# 2022
# Plotting and saving output from RF [2002-2022]: Jan-Apr 2022
rf_wsm_2022_01_to_2022_04_20022022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM2_2022_list,
                                                             period_name_2021 = F,
                                                             r1_index = 1,
                                                             r2_index = 2,
                                                             r3_index = 3,
                                                             r4_index = 4)

tmap_save(rf_wsm_2022_01_to_2022_04_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2022_01_to_2022_04_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2002-2022]: May-Aug 2022
rf_wsm_2022_05_to_2022_08_20022022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM2_2022_list,
                                                             period_name_2021 = F,
                                                             r1_index = 5,
                                                             r2_index = 6,
                                                             r3_index = 7,
                                                             r4_index = 8)

tmap_save(rf_wsm_2022_05_to_2022_08_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2022_05_to_2022_08_20022022df.pdf', width = 6.56, height = 8.50)

# Plotting and saving output from RF [2002-2022]: Sep-Dec 2022
rf_wsm_2022_09_to_2022_12_20022022df <- RF_WSM_combined_plot(data = WS_RASTERS_RFM2_2022_list,
                                                             period_name_2021 = F,
                                                             r1_index = 9,
                                                             r2_index = 10,
                                                             r3_index = 11,
                                                             r4_index = 12)

tmap_save(rf_wsm_2022_09_to_2022_12_20022022df, filename = '/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/Appendix plots/rf_wsm_2022_09_to_2022_12_20022022df.pdf', width = 6.56, height = 8.50)










