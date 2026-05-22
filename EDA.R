# Aerial Imagery Data Collection EDA --------------------------------------

# Load necessary libraries
library(tidyverse)
library(cowplot)

# Creating date range
start_date <- as.Date("2002-01-01")
end_date <- as.Date("2023-12-31")
all_dates <- seq(start_date, end_date, by = "day")

# Creating dataset
df_aerial_imagery <- all_dates %>%
  as_tibble() %>% # convert to tibble
  rename(date = value) %>% # rename column as date
  mutate(year = year(date), # extracting year component from date
         month = month(all_dates, label = T), # extracting month component from date
         day = day(all_dates), # extracting days component from date
         has_data = factor(case_when(date %in% c(
                                          as.Date('2002-01-26'), as.Date('2002-03-15'), as.Date('2002-03-31'), as.Date('2002-04-16'),
                                          as.Date('2002-05-18'), as.Date('2002-06-03'), as.Date('2002-07-21'), as.Date('2002-09-23'),
                                          as.Date('2002-11-10'), # 2002 series
                                          
                                          as.Date('2003-01-13'), as.Date('2003-05-21'), as.Date('2003-11-13'), as.Date('2003-11-29'), 
                                          as.Date('2003-12-15'), # 2003 series
                                          
                                          as.Date('2004-03-20'), as.Date('2004-07-10'), as.Date('2004-08-11'), as.Date('2004-08-27'),
                                          as.Date('2004-11-15'), # 2004 series
                                          
                                          as.Date('2005-01-02'), as.Date('2005-01-18'), as.Date('2005-03-23'), as.Date('2005-04-08'),
                                          as.Date('2005-05-10'), as.Date('2005-06-11'), as.Date('2005-12-04'), # 2005 series
                                          
                                          as.Date('2006-02-06'), as.Date('2006-02-22'), as.Date('2006-03-10'), as.Date('2006-06-30'),
                                          as.Date('2006-08-17'), as.Date('2006-11-05'), # 2006 series
                                          
                                          as.Date('2007-02-25'), as.Date('2007-03-13'), as.Date('2007-06-01'), as.Date('2007-06-17'), 
                                          as.Date('2007-07-03'), as.Date('2007-11-24'), # 2007 series
                                          
                                          as.Date('2008-01-27'), as.Date('2008-05-02'), as.Date('2008-08-06'), as.Date('2008-08-22'),
                                          as.Date('2008-10-25'), as.Date('2008-11-10'), as.Date('2008-11-26'), # 2008 series
                                          
                                          as.Date('2009-05-05'), as.Date('2009-07-24'), as.Date('2009-08-09'), as.Date('2009-08-25'), # 2009 series
                                          
                                          as.Date('2010-01-16'), as.Date('2010-06-25'), as.Date('2010-08-12'), as.Date('2010-09-13'),
                                          as.Date('2010-10-31'), as.Date('2010-11-16'), as.Date('2010-12-18'), # 2010 series
                                          
                                          as.Date('2011-01-03'), as.Date('2011-04-09'), as.Date('2011-05-27'), as.Date('2011-06-12'),
                                          as.Date('2011-08-15'), as.Date('2011-09-16'), # 2011 series
                                          
                                          as.Date('2012-01-06'), as.Date('2012-03-26'), as.Date('2012-04-11'), as.Date('2012-09-02'),
                                          as.Date('2012-10-20'), as.Date('2012-12-23'), # 2012 series
                                          
                                          as.Date('2013-02-25'), as.Date('2013-04-30'), as.Date('2013-06-17'), as.Date('2013-07-03'),
                                          as.Date('2013-08-04'), as.Date('2013-11-24'), as.Date('2013-12-26') # 2013 series
                                          ) ~ 'Landsat 7 SR', # all the above imageries collected from L7 SR satellite
                                     
                                     date %in% c(
                                          as.Date('2013-12-18'), # 2013 series
                                                 
                                          as.Date('2014-02-04'), as.Date('2014-04-09'), as.Date('2014-04-25'), as.Date('2014-06-12'), 
                                          as.Date('2014-06-28'), as.Date('2014-07-14'), as.Date('2014-07-30'), as.Date('2014-08-31'),
                                          as.Date('2014-10-02'), as.Date('2014-10-18'), as.Date('2014-11-19'), as.Date('2014-12-05'), # 2014 series
                                          
                                          as.Date('2015-01-06'), as.Date('2015-01-22'), as.Date('2015-02-07'), as.Date('2015-02-23'),
                                          as.Date('2015-03-11'), as.Date('2015-04-12'), as.Date('2015-08-02'),
                                          as.Date('2015-09-03'), as.Date('2015-09-19'), # 2015 series
                                          
                                          as.Date('2016-01-09'), as.Date('2016-02-10'), as.Date('2016-07-03'), as.Date('2016-10-23'),
                                          as.Date('2016-12-10'), as.Date('2016-12-26'), # 2016 series
                                          
                                          as.Date('2017-01-11'), as.Date('2017-02-28'), as.Date('2017-03-16'), as.Date('2017-04-17'),
                                          as.Date('2017-05-19'), as.Date('2017-08-07'), as.Date('2017-10-10'),
                                          as.Date('2017-11-27'), as.Date('2017-12-29'), # 2017 series
                                          
                                          as.Date('2018-01-14'), as.Date('2018-02-15'), as.Date('2018-03-03'), as.Date('2018-03-19'),
                                          as.Date('2018-04-04'), as.Date('2018-07-09'), as.Date('2018-09-11'), as.Date('2018-10-13'),
                                          as.Date('2018-11-14'), as.Date('2018-11-30'), as.Date('2018-12-16'), # 2018 series
                                          
                                          as.Date('2019-02-18'), as.Date('2019-03-06'), as.Date('2019-04-07'), as.Date('2019-05-09'),
                                          as.Date('2019-09-14'), as.Date('2019-10-16'), # 2019 series
                                          
                                          as.Date('2020-01-04'), as.Date('2020-02-21'), as.Date('2020-04-09'), as.Date('2020-04-25'),
                                          as.Date('2020-05-11'), as.Date('2020-12-05'), # 2020 series
                                          
                                          as.Date('2021-01-06'), as.Date('2021-01-22'), as.Date('2021-02-23'), as.Date('2021-04-12'), 
                                          as.Date('2021-07-17'), as.Date('2021-08-02'), as.Date('2021-10-05'), as.Date('2021-12-08'), 
                                          as.Date('2021-12-24') # 2021 series
                                          ) ~ 'Landsat 8 SR', # all the above imageries collected from L8 SR satellite
                                     
                                     date %in% c(
                                          as.Date('2022-01-17'), as.Date('2022-02-18'), as.Date('2022-03-06'), 
                                          as.Date('2022-05-09'), as.Date('2022-06-10'), as.Date('2022-06-26'), as.Date('2022-07-28'),
                                          as.Date('2022-09-14'), as.Date('2022-11-01'), # 2022 series
                                          
                                          as.Date('2023-05-28'), as.Date('2023-08-16'), as.Date('2023-10-03'), as.Date('2023-10-19'),
                                          as.Date('2023-12-06'), as.Date('2023-12-22') # 2023 series
                                          ) ~ 'Landsat 9 SR', # all the above imageries collected from L9 SR satellite
                                     
                                     date %in% c(
                                          as.Date('2015-12-18'), # 2015 series
                                          
                                          as.Date('2016-04-06'), as.Date('2016-05-26'), as.Date('2016-06-05'), as.Date('2016-08-24'),
                                          as.Date('2016-09-13'), as.Date('2016-11-22'), as.Date('2016-04-06') # 2016 series
                                          ) ~ 'Sentinel 2 TOA', # all the above imageries collected from S2 TOA satellite
                                     
                                     date %in% c(
                                          as.Date('2019-01-26'), as.Date('2019-06-15'), as.Date('2019-07-10'), as.Date('2019-08-24'),
                                          as.Date('2019-11-22'), as.Date('2019-12-22'), # 2019 series
                                          
                                          as.Date('2020-03-16'), as.Date('2020-06-24'), as.Date('2020-07-19'), as.Date('2020-09-22'),
                                          as.Date('2020-10-22'), as.Date('2020-11-11'), # 2020 series
                                          
                                          as.Date('2021-03-21'), as.Date('2021-06-19'), as.Date('2021-09-22'), as.Date('2021-11-11'), # 2021 series
                                          
                                          as.Date('2022-04-10'), as.Date('2022-08-23'), as.Date('2022-10-22'), as.Date('2022-12-21'),  # 2022 series
                                          
                                          as.Date('2023-01-25'), as.Date('2023-02-09'), as.Date('2023-03-16'), as.Date('2023-04-05'),
                                          as.Date('2023-07-24'), as.Date('2023-09-27'), as.Date('2023-11-26')
                                          ) ~ 'Sentinel 2 SR', # all the above imageries collected from S2 SR satellite
                                     
                              TRUE ~ 'Missing Data'))) # data collection status
  

# levels(df_aerial_imagery$has_data) 

# Plot the above
all_possible_aerial_imagery_plot <- ggplot(df_aerial_imagery, aes(x = day, y = 22, fill = has_data)) +
  geom_tile(color = "lightgray", linewidth = .01) +
  scale_fill_manual(labels= c("Landsat 7 SR", "Landsat 8 SR", "Landsat 9 SR", "Missing Data", "Sentinel 2 SR", "Sentinel 2 TOA"), 
                    values = c("deepskyblue", "blue", 'navyblue', 'white', 'red', 'orange')) +
  facet_grid(year ~ month) +
  theme_void()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(), # remove legend title
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        panel.spacing.x = unit(0, "lines"),  # Remove horizontal space
        panel.spacing.y = unit(0, "lines")) +    # Remove plot margins
  guides(fill = guide_legend(
    keyheight = unit(0.4, "cm"), # resize of icon
    keywidth = unit(.08, "cm"), # resize icon
    nrow = 1  # force icon to unwrap to 1 row
  )) 

all_possible_aerial_imagery_plot

ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/all_possible_aerial_imagery_plot.pdf", 
       plot = all_possible_aerial_imagery_plot, width = 10, height= 5.5)
# width = 6.56, height = 8.5

# Total no. of imagery acquired
df_aerial_imagery %>%
  filter(has_data != 'Missing Data') %>%
  tally()

# No. of imagery acquired for each category
df_aerial_imagery %>%
  group_by(has_data) %>%
  tally() %>%
filter(has_data!='Missing Data') %>%
  summarise(sum(n))

#######################################################################################################################
# EDA for trimmed aerial imagery dataset
# Creating date range
trimed_start_date <- as.Date("2014-01-01")
end_date <- as.Date("2023-12-31")
trimmed_dates <- seq(trimed_start_date, end_date, by = "day")

# Creating trimmed dataset
df_aerial_imagery_2014_2023 <- trimmed_dates %>%
  as_tibble() %>% # convert to tibble
  rename(date = value) %>% # rename column as date
  mutate(year = year(date), # extracting year component from date
         month = month(trimmed_dates, label = T), # extracting month component from date
         day = day(trimmed_dates), # extracting days component from date
         has_data = factor(case_when(date %in% c(

           as.Date('2014-02-04'), as.Date('2014-04-09'), as.Date('2014-04-25'), as.Date('2014-06-12'), 
           as.Date('2014-06-28'), as.Date('2014-07-14'), as.Date('2014-07-30'), as.Date('2014-08-31'),
           as.Date('2014-10-02'), as.Date('2014-10-18'), as.Date('2014-11-19'), as.Date('2014-12-05'), # 2014 series
           
           as.Date('2015-01-06'), as.Date('2015-01-22'), as.Date('2015-02-07'), as.Date('2015-02-23'),
           as.Date('2015-03-11'), as.Date('2015-04-12'), as.Date('2015-08-02'),
           as.Date('2015-09-03'), as.Date('2015-09-19'), # 2015 series
           
           as.Date('2016-01-09'), as.Date('2016-02-10'), as.Date('2016-07-03'), as.Date('2016-10-23'),
           as.Date('2016-12-10'), as.Date('2016-12-26'), # 2016 series
           
           as.Date('2017-01-11'), as.Date('2017-02-28'), as.Date('2017-03-16'), as.Date('2017-04-17'),
           as.Date('2017-05-19'), as.Date('2017-08-07'), as.Date('2017-10-10'),
           as.Date('2017-11-27'), as.Date('2017-12-29'), # 2017 series
           
           as.Date('2018-01-14'), as.Date('2018-02-15'), as.Date('2018-03-03'), as.Date('2018-03-19'),
           as.Date('2018-04-04'), as.Date('2018-07-09'), as.Date('2018-09-11'), as.Date('2018-10-13'),
           as.Date('2018-11-14'), as.Date('2018-11-30'), as.Date('2018-12-16'), # 2018 series
           
           as.Date('2019-02-18'), as.Date('2019-03-06'), as.Date('2019-04-07'), as.Date('2019-05-09'),
           as.Date('2019-09-14'), as.Date('2019-10-16'), # 2019 series
           
           as.Date('2020-01-04'), as.Date('2020-02-21'), as.Date('2020-04-09'), as.Date('2020-04-25'),
           as.Date('2020-05-11'), as.Date('2020-12-05'), # 2020 series
           
           as.Date('2021-01-06'), as.Date('2021-01-22'), as.Date('2021-02-23'), as.Date('2021-04-12'), 
           as.Date('2021-07-17'), as.Date('2021-08-02'), as.Date('2021-10-05'), as.Date('2021-12-08'), 
           as.Date('2021-12-24') # 2021 series
         ) ~ 'Landsat 8 SR', # all the above imageries collected from L8 SR satellite
         
         date %in% c(
           as.Date('2022-01-17'), as.Date('2022-02-18'), as.Date('2022-03-06'), 
           as.Date('2022-05-09'), as.Date('2022-06-10'), as.Date('2022-06-26'), as.Date('2022-07-28'),
           as.Date('2022-09-14'), as.Date('2022-11-01'), # 2022 series
           
           as.Date('2023-05-28'), as.Date('2023-08-16'), as.Date('2023-10-03'), as.Date('2023-10-19'),
           as.Date('2023-12-06'), as.Date('2023-12-22') # 2023 series
         ) ~ 'Landsat 9 SR', # all the above imageries collected from L9 SR satellite
         
         date %in% c(
           as.Date('2015-12-18'), # 2015 series
           
           as.Date('2016-04-06'), as.Date('2016-05-26'), as.Date('2016-06-05'), as.Date('2016-08-24'),
           as.Date('2016-09-13'), as.Date('2016-11-22'), as.Date('2016-04-06') # 2016 series
         ) ~ 'Sentinel 2 TOA', # all the above imageries collected from S2 TOA satellite
         
         date %in% c(
           as.Date('2019-01-26'), as.Date('2019-06-15'), as.Date('2019-07-10'), as.Date('2019-08-24'),
           as.Date('2019-11-22'), as.Date('2019-12-22'), # 2019 series
           
           as.Date('2020-03-16'), as.Date('2020-06-24'), as.Date('2020-07-19'), as.Date('2020-09-22'),
           as.Date('2020-10-22'), as.Date('2020-11-11'), # 2020 series
           
           as.Date('2021-03-21'), as.Date('2021-06-19'), as.Date('2021-09-22'), as.Date('2021-11-11'), # 2021 series
           
           as.Date('2022-04-10'), as.Date('2022-08-23'), as.Date('2022-10-22'), as.Date('2022-12-21'),  # 2022 series
           
           as.Date('2023-01-25'), as.Date('2023-02-09'), as.Date('2023-03-16'), as.Date('2023-04-05'),
           as.Date('2023-07-24'), as.Date('2023-09-27'), as.Date('2023-11-26')
         ) ~ 'Sentinel 2 SR', # all the above imageries collected from S2 SR satellite
         
         date %in% c(
           as.Date(df_2014_2022[1,1][[1]]@file@name, format = '%Y%m%d'),
           as.Date(df_2014_2022[1,3][[1]]@file@name, format = '%Y%m%d'),
           as.Date(df_2014_2022[1,5][[1]]@file@name, format = '%Y%m%d'),
           as.Date(df_2014_2022[1,9][[1]]@file@name, format = '%Y%m%d'), # 2014 series
           
           as.Date(df_2014_2022[3,3][[1]]@file@name, format = '%Y%m%d'), # 2016 series
           as.Date(df_2014_2022[4,9][[1]] @file@name, format = '%Y%m%d'), # 2017 series
           as.Date(df_2014_2022[5,8][[1]]@file@name, format = '%Y%m%d'), # 2018 series
           as.Date(df_2014_2022[7,8][[1]]@file@name, format = '%Y%m%d'), # 2020 series
           as.Date(df_2014_2022[8,5][[1]]@file@name, format = '%Y%m%d'), # 2021 series
           as.Date(df_2014_2022[10,6][[1]]@file@name, format = '%Y%m%d') # 2023 series
         ) ~ 'L1HI', # all the above imageries horizontally interpolated 
         
         date %in% c(
           as.Date(df_2014_2022[2,5][[1]]@file@name, format = '%Y%m%d'), 
           as.Date(df_2014_2022[2,6][[1]]@file@name, format = '%Y%m%d'), 
           as.Date(df_2014_2022[2,7][[1]]@file@name, format = '%Y%m%d'), 
           as.Date(df_2014_2022[2,10][[1]]@file@name, format = '%Y%m%d'), 
           as.Date(df_2014_2022[2,11][[1]]@file@name, format = '%Y%m%d'), # 2015 series
           as.Date(df_2014_2022[4,7][[1]]@file@name, format = '%Y%m%d') # 2017 series
         ) ~ 'L1VI', # all the above imageries vertically interpolated 
         
         date %in% c(
           as.Date(df_2014_2022[4,6][[1]]@file@name, format = '%Y%m%d') # 2017 series
         ) ~ 'L2HI', # all the above imageries horizotally interpolated 
         
         date %in% c(
           as.Date(df_2014_2022[5,6][[1]]@file@name, format = '%Y%m%d') # 2018 series
         ) ~ 'L3VI', # all the above imageries vertically interpolated 
         
         date %in% c(
           as.Date(df_2014_2022[5,5][[1]]@file@name, format = '%Y%m%d') # 2018 series
         ) ~ 'L4HI', # all the above imageries horizontally interpolated 
         
         TRUE ~ 'Missing Data'))) # data collection status

# Plot the above
trimmed_aerial_imagery_plot <- ggplot(df_aerial_imagery_2014_2023, aes(x = day, y = 10, fill = has_data)) +
  geom_tile(color = "lightgray", linewidth = .01) +
  scale_fill_manual(labels= c("L1HI", "L1VI", "L2HI", "L3VI", "L4HI",
                              "Landsat 8 SR", "Landsat 9 SR", "Missing Data", "Sentinel 2 SR", "Sentinel 2 TOA"), 
                    values = c("yellow", 'gray', 'springgreen', 'black', 'yellowgreen',
                               "blue", "navyblue", "white", "red", "orange")) +
  facet_grid(year ~ month) +
  theme_void()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(), # remove legend title
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        panel.spacing.x = unit(0, "lines"),  # Remove horizontal space
        panel.spacing.y = unit(0, "lines")) +    # Remove plot margins
  guides(fill = guide_legend(
    keyheight = unit(0.5, "cm"), # resize of icon
    keywidth = unit(.07, "cm"), # resize icon
    nrow = 1  # force icon to unwrap to 1 row
  )) 

trimmed_aerial_imagery_plot
ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/trimmed_aerial_imagery_plot.pdf", 
       plot = trimmed_aerial_imagery_plot, width = 10, height= 3)



# Wildfire Intuition based on Vegetation Indices and Climatologica --------

# creating dataframe to visualise which month are more prone to wildfire
df_fire_prone_months <- data.frame(var = factor(rep(c('NDVI','NDMI','NBR','TP','AMT','ANSWS','ARH'),each = 12)),
                                   month = factor(rep(month.abb,times = 7))) %>%
  as_tibble() %>%
  mutate(status = factor(case_when(var=='NDVI' & month %in% c('Feb', 'Mar', 'Apr', 'Dec') ~ 'Fire-prone',
                            var=='NDVI' & month %in% c('Jun', 'Jul', 'Aug') ~ 'Non fire-prone',
                            var=='NDMI' & month %in% c('Jan', 'Feb', 'Mar', 'Nov', 'Dec') ~ 'Fire-prone',
                            var=='NDMI' & month %in% c('May','Jun', 'Jul', 'Aug') ~ 'Non fire-prone',
                            var=='NBR' & month %in% c('Jan', 'Feb', 'Mar', 'Nov', 'Dec') ~ 'Fire-prone',
                            var=='NBR' & month %in% c('May','Jun', 'Jul', 'Aug', 'Sep') ~ 'Non fire-prone',
                            var=='TP' & month %in% c('Jan', 'Feb', 'Mar', 'Oct', 'Nov', 'Dec') ~ 'Fire-prone',
                            var=='TP' & month %in% c('May','Jun', 'Jul', 'Aug') ~ 'Non fire-prone',
                            var=='AMT' & month %in% c('Jan', 'Feb', 'Mar', 'Dec') ~ 'Fire-prone',
                            var=='AMT' & month %in% c('May','Jun', 'Jul', 'Aug') ~ 'Non fire-prone',
                            var=='ANSWS' & month %in% c('Jan', 'Feb', 'Mar', 'Dec') ~ 'Fire-prone',
                            var=='ANSWS' & month %in% c('Apr', 'May', 'Jul', 'Aug') ~ 'Non fire-prone',
                            var=='ARH' & month %in% c('Apr', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov') ~ 'Fire-prone',
                            var=='ARH' & month %in% c('Jan', 'Feb', 'Mar', 'Dec') ~ 'Non fire-prone',
                            TRUE ~ 'Neutral')))

# reorder levels for plotting
df_fire_prone_months$var <- factor(df_fire_prone_months$var, levels = rev(c('NDVI','NDMI','NBR','TP','AMT','ANSWS','ARH')), ordered = T) 
df_fire_prone_months$month <- factor(df_fire_prone_months$month, levels = month.abb, ordered = T)

fire_prone_months_plot <- ggplot(df_fire_prone_months, aes(x = month, y = var, fill = status))+
  geom_tile(color = 'lightgray', lwd = .01) +
  scale_fill_manual(labels = c('Fire-prone', 'Neutral', 'Non fire-prone'),
                    values = c('red', 'white', 'green')) +
  xlab('Month') +
  ylab('Variable')+
  theme_minimal()+
  theme(legend.position = 'top', legend.title = element_blank())

ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/fire_prone_months_plot.pdf", 
       plot = fire_prone_months_plot, width = 6.56, height= 3)

# Show an example of an aerial imagery with 5% CC prior to 2014 as compared to a good one after 2014.
AI_name <- list.files('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Unprocessed Variables/Aerial Imagery 2002-2023/')
bad_AI <- brick(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Unprocessed Variables/Aerial Imagery 2002-2023/', AI_name[76]))
good_AI <- brick(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Unprocessed Variables/Aerial Imagery 2002-2023/', AI_name[160]))

# par(mfrow = c(1,2))
par(mar = c(0.2, 0.1, 1.8, 0.1))
plotRGB(bad_AI, r=3 , g=2 , b=1, 
        stretch = 'lin', 
        margin = T,
        main = paste0(as.Date(str_extract(bad_AI@file@name, "\\d{8}"), format = '%Y%m%d')),
        cex.main = .8)

plotRGB(good_AI, r=3 , g=2 , b=1, 
        stretch = 'lin', 
        margin = T,
        main = paste0(as.Date(str_extract(good_AI@file@name, "\\d{8}"), format = '%Y%m%d')),
        cex.main = .8)

# FIRE EDA

# Import TMNR shapefile 
roi <- readOGR('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/SANParks shapefiles/TMNR shapefile/tmnr_boundary.shp')
roi_trans <- spTransform(roi, CRS('+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs')) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)

# Import SANPARK fire data
sanpark_fire_data <- list.files('Raw Data/SANParks fire data/1962-2022', pattern = '.shp')
sanpark_fire_data <- sanpark_fire_data[seq(1, length(sanpark_fire_data), by = 2)]

sanpark_fire_shpfile_list <- pblapply(seq_along(sanpark_fire_data), function(x){
  readOGR(paste0('Raw Data/SANParks fire data/1962-2022/', sanpark_fire_data[x]))
}) # read in all shapefiles in a list


# Rearrange data columns for consistency and assign original coordinate system to shapefiles
sanpark_fire_shpfile_df_list <- pblapply(seq_along(sanpark_fire_data), function(x) {
  proj4string(sanpark_fire_shpfile_list[[x]]) <- '+proj=tmerc +lat_0=0 +lon_0=19 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # Lo19 Hartebbesthoek94
  st_as_sf(sanpark_fire_shpfile_list[[x]])  %>% # convert spatial features to data frame
    select(FIRETYPE, FIRECAUSE, YEAR, 
           STARTDATE,XHECTARES, geometry)})

# Merge all the shapefiles
sanpark_fire_shpfile_combind_list <- do.call(rbind,sanpark_fire_shpfile_df_list)

sanpark_fire_shpfile_combind_list_fixed <- st_buffer(sanpark_fire_shpfile_combind_list, dist = 0) # hack to fix geometry
sanpark_fire_shpfile_combind_list_fixed <- as(sanpark_fire_shpfile_combind_list_fixed, 'Spatial') # convert dataframe back to spatial features
sanpark_fire_shpfile_combind_list_trans <- spTransform(sanpark_fire_shpfile_combind_list_fixed, CRS(proj4string(roi_trans))) # convert coordinate system to EPSG:32734 (WGS 84 / UTM zone 34S)
sanpark_fire_shpfile_combind_list_trans_intersect <- intersect(sanpark_fire_shpfile_combind_list_trans, roi_trans) # crop polygon to ROI

# Cleaning data
sanpark_fire_shpfile_combind_list_trans_intersect <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect) %>% # convert spatial feature to spatial dataframe
  mutate(FIRECAUSE = case_when(FIRECAUSE == "Accident"~"Accident",
                               FIRECAUSE == "Accident - Vagrants"~"Vagrant",
                               FIRECAUSE == "Arson"~"Arson",
                               FIRECAUSE == "Arson - Vagrants" ~ "Vagrant",
                               FIRECAUSE == "Lighting Strike" ~ "Lightning Strike",
                               FIRECAUSE == "Lightning Strike" ~ "Lightning Strike",
                               FIRECAUSE == "Prescribed"~"Prescribed",
                               FIRECAUSE == "Prescribed Burn"~"Prescribed",
                               FIRECAUSE == "Prescribed burn"~"Prescribed",
                               FIRECAUSE == "prescribed burn"~"Prescribed",
                               FIRECAUSE == "unknown"~"Unknown",
                               FIRECAUSE == "Unknown"~"Unknown",
                               FIRECAUSE == "Vagrant"~"Vagrant",
                               FIRECAUSE == "Wildfire"~"Wildfire",
                               FIRECAUSE == "Wild Fire"~"Wildfire",
                               FIRECAUSE == "negligence"~"negligence"),
         
         FIRETYPE = case_when(FIRETYPE=="cigarette"~"Cigarette",
                              FIRETYPE=="Prescribed"~"Prescribed",
                              FIRETYPE == "Prescribed Burn"~"Prescribed",
                              FIRETYPE == "Prescribed burn"~"Prescribed",
                              FIRETYPE=="unknown"~"Unknown",
                              FIRETYPE == "Unknown"~"Unknown",
                              FIRETYPE=="wildfire"~"Wildfire",
                              FIRETYPE=="Wild Fire"~"Wildfire",
                              FIRETYPE=="Wildfire"~"Wildfire",
                              FIRETYPE=="WildFire"~"Wildfire"))  %>%
  
  mutate(STARTDATE = as.Date(STARTDATE, format = '%Y%m%d'), # reformat date
         STARTDATE = case_when(STARTDATE== as.Date('2020-12-17', format = '%Y-%m-%d')~as.Date('2021-04-18', format = '%Y-%m-%d'), # correct erroneous entry
                               T ~ as.Date(STARTDATE, format = '%Y%m%d')),
         YEARMONTH = format(as.Date(STARTDATE), '%Y-%m') |> as.factor(), # extract year and month
         YEAR_extract = year(STARTDATE) |> as.factor(), # extract year only 
         Area_calc_in_ha = as.numeric(st_area(geometry)/10000)) %>% # calculate missing areas in ha
  arrange(STARTDATE) %>% # rearrange date in correct order
  select(-YEAR, -XHECTARES) %>% # remove supplied year as it creates confusion as in the year for2007-11-30 will be 2008 (we want to keep the year!)
  as('Spatial') # convert dataframe to spatial feature again

# View(st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect))

# Removing prescribed burning from burnt area
sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect) %>%
  filter(FIRECAUSE!="Prescribed") %>%
  as('Spatial')

# View(st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed))

rasterise_polygons <- function(study_area, polygon_shapefile, index, resolution, plot = NULL){
  empty_raster <- raster(extent(study_area), res = resolution) # creating empty raster with 30x30 spatial resolution
  crs(empty_raster) <- crs(study_area) # assigning crs to empty raster
  # rasterise polygon shape file with 1s and 0s
  rasterised_polygon <- rasterize(polygon_shapefile[index,], 
                                  empty_raster,
                                  field = 1,
                                  background = 0) |>
    crop(study_area) |>
    mask(study_area) |>
    ratify() # make raster as a factor
  
  levels(rasterised_polygon) <- data.frame(ID = c(0, 1), fire_status = c("No Fire", "Fire")) # redefine levels
  
  if(plot ==T){
    plot(rasterised_polygon, col= c('lightgray','red'), main = polygon_shapefile[index,]@data$YEARMONTH[1], legend = F)
    plot(study_area, col = 'transparent', border = 'black', lwd = 1,
         add = T)
  }
  return(rasterised_polygon)
}

# rasterise all fire polygons
sanpark_all_historical_fire_raster_list <- pblapply(seq_along(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed), function(x){
  rasterise_polygons(study_area = roi_trans,
                     polygon_shapefile = sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed,
                     index = x,
                     resolution = 30,
                     plot = T)
})


# rename SANparks fire rasters 
pblapply(seq_along(sanpark_all_historical_fire_raster_list), function(x){names(sanpark_all_historical_fire_raster_list[[x]]) <<- paste0("Fire Event ", seq_along(sanpark_all_historical_fire_raster_list)[x])})

sanpark_all_historical_fire_raster_stack <- stack(sanpark_all_historical_fire_raster_list)
sanpark_all_historical_fire_raster_stack_df <- as.data.frame(sanpark_all_historical_fire_raster_stack, xy = T, na.rm = T)
str(sanpark_all_historical_fire_raster_stack_df)

# sanpark_all_historical_fire_raster_stack_df <- ifelse(sanpark_all_historical_fire_raster_stack_df == 'No Fire',NA,1)
# str(sanpark_all_historical_fire_raster_stack_df)

# separate the xy coordinates
sanpark_all_historical_fire_raster_stack_dfxy <- sanpark_all_historical_fire_raster_stack_df[,c('x','y')]
# redefine no fire to 0 and fire to 1
sanpark_all_historical_fire_raster_stack_df_var <- sanpark_all_historical_fire_raster_stack_df[,-(1:2)]
sanpark_all_historical_fire_raster_stack_df_var <- ifelse(sanpark_all_historical_fire_raster_stack_df_var=='No Fire',0,1)
# View(sanpark_all_historical_fire_raster_stack_df_var)

# sum the number of times a fire occur at each pixel location
sanpark_all_historical_fire_raster_stack_dfxy$fire_frequency <- rowSums(sanpark_all_historical_fire_raster_stack_df_var,na.rm =T)|>as.factor()
# str(sanpark_all_historical_fire_raster_stack_dfxy$fire_frequency)

unique(sanpark_all_historical_fire_raster_stack_dfxy$fire_frequency)

# ggplot()+
#   geom_raster(data = sanpark_all_historical_fire_raster_stack_dfxy, aes(x = x, y = y, fill = fire_frequency))+
#   scale_fill_viridis_d(option = "plasma", direction = -1) +
#   theme_bw()+
#   theme(legend.title = element_blank())


library(viridisLite)
plasma_mod <- plasma(12,direction = -1) # importing palette
plasma_mod[1] <- 'lightgreen' # customising palette

historical_fire_frequency_map <- tm_shape(rasterFromXYZ(sanpark_all_historical_fire_raster_stack_dfxy, res = c(30,30), crs = crs(roi_trans))) +
  tm_raster(
    col = "fire_frequency",
    palette = plasma_mod,   
    style = "cat",          # categorical style (discrete)
    title = "1964-2022\nFire Frequency"
  ) +
  tm_graticules(labels.size = 0.5, n.x = 3, n.y = 3, lines = F)+
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.6)

# Save the map as a PDF
# tmap_save(historical_fire_frequency_map, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/historical_fire_frequency_map.pdf", width = 4, height = 4)


sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_2014_2022 <- st_as_sf(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed) %>%
  filter(YEAR_extract %in% 2014:2022) %>%
  as('Spatial') # convert dataframe to spatial feature again

sanpark_20142022_historical_fire_raster_list <- pblapply(seq_along(sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_2014_2022), function(x){
  rasterise_polygons(study_area = roi_trans,
                     polygon_shapefile = sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_2014_2022,
                     index = x,
                     resolution = 30,
                     plot = F)
})

# rename SANparks fire rasters 
pblapply(seq_along(sanpark_20142022_historical_fire_raster_list), function(x){names(sanpark_20142022_historical_fire_raster_list[[x]]) <<- paste0("Fire Event ", seq_along(sanpark_20142022_historical_fire_raster_list)[x])})

sanpark_20142022_historical_fire_raster_stack <- stack(sanpark_20142022_historical_fire_raster_list)
sanpark_20142022_historical_fire_raster_stack_df <- as.data.frame(sanpark_20142022_historical_fire_raster_stack, xy = T, na.rm = T)
# str(sanpark_20142022_historical_fire_raster_stack_df)

# separate the xy coordinates
sanpark_20142022_historical_fire_raster_stack_dfxy <- sanpark_20142022_historical_fire_raster_stack_df[,c('x','y')]

# redefine no fire to 0 and fire to 1
sanpark_20142022_historical_fire_raster_stack_df_var <- sanpark_20142022_historical_fire_raster_stack_df[,-(1:2)]
sanpark_20142022_historical_fire_raster_stack_df_var <- ifelse(sanpark_20142022_historical_fire_raster_stack_df_var=='No Fire',0,1)
# View(sanpark_20142022_historical_fire_raster_stack_df_var)

# sum the number of times a fire occur at each pixel location
sanpark_20142022_historical_fire_raster_stack_dfxy$fire_frequency <- rowSums(sanpark_20142022_historical_fire_raster_stack_df_var, na.rm = T)|>as.factor()
str(sanpark_20142022_historical_fire_raster_stack_dfxy)

unique(sanpark_20142022_historical_fire_raster_stack_dfxy$fire_frequency)


historical_2014_2022_fire_frequency_map <- tm_shape(rasterFromXYZ(sanpark_20142022_historical_fire_raster_stack_dfxy, res = c(30,30), crs = crs(roi_trans))) +
  tm_raster(
    col = "fire_frequency",
    palette = plasma_mod,   
    style = "cat",          # categorical style (discrete)
    title = "2014-2022\nFire Frequency"
  ) +
  tm_graticules(labels.size = 0.5, n.x = 3, n.y = 3, lines = F)+
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.6)

# Save the map as a PDF
# tmap_save(historical_2014_2022_fire_frequency_map, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/historical_2014_2022_fire_frequency_map.pdf", width = 4, height = 4)


LULC_ref_map <- tm_shape(FINAL_LULC[[122]])+
  tm_raster(style = "cat", title = "", palette = c('#883C07', '#00734C', '#D1FF73', '#70A800', '#00A9E6'))+
  tm_layout(
    # main.title= FINAL_LULC[[122]]@file@name,
            # main.title.size =.9,
            # main.title.position = c("center", "top"),
            legend.outside = F,
            legend.text.size = 0.5)+
  tm_graticules(lines = F)


fff_map <- tmap_arrange(historical_fire_frequency_map,
             historical_2014_2022_fire_frequency_map,
             LULC_ref_map,
             ncol = 2,
             nrow=2)
tmap_save(fff_map, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/fff_map_map.pdf", width = 6.56, height = 6)

ff_map <- tmap_arrange(historical_fire_frequency_map,
                        historical_2014_2022_fire_frequency_map,
                        # LULC_ref_map,
                        ncol = 2,
                        nrow=1)
tmap_save(ff_map, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/ff_map.pdf", width = 6.56, height = 2.9)



# dataset preparation
all_monthly_fire_list <- pblapply(c(1:6,8:12), function(x){ # note that July never had a fire
  sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed%>%
    st_as_sf()%>%
    mutate(MONTH = as.numeric(format(STARTDATE, "%m"))) %>%
    filter(MONTH == x) %>%
    as('Spatial')
})

# Function to calculate frequency of fire over a timeframe per month
FIRE_FREQ <- function(data, month){
  # rasterise all fire polygons on a monthly basis
  rasterising_poly <- pblapply(seq_along(data[[month]]), function(x){
    rasterise_polygons(study_area = roi_trans,
                       polygon_shapefile = data[[month]],
                       index = x,
                       resolution = 30,
                       plot = T)
  })
  # rename SANparks fire rasters 
  pblapply(seq_along(rasterising_poly), function(x){names(rasterising_poly[[x]]) <<- paste0("Fire Event ", seq_along(rasterising_poly)[x])})
  # stack rasterised poly
  rasterising_poly_stack <- stack(rasterising_poly)
  # convert stacked raster to dataframe
  rasterising_poly_stack_df <- as.data.frame(rasterising_poly_stack, xy = T, na.rm = T)
  # separate the xy coordinates from the dataframe
  rasterising_poly_stack_dfxy <- rasterising_poly_stack_df[,c('x','y')]
  # separate the variables
  rasterising_poly_stack_df_var <- rasterising_poly_stack_df[,-(1:2)]
  # redefine no fire to 0 and fire to 1
  rasterising_poly_stack_df_var <- ifelse(rasterising_poly_stack_df_var=='No Fire',0,1)

    if(ncol(rasterising_poly_stack_df)!=3){ # if there only multiple fire events
    # sum the number of times a fire occur at each pixel location
    rasterising_poly_stack_dfxy$fire_frequency <- rowSums(rasterising_poly_stack_df_var, na.rm = T)|>as.factor()
    # convert frequency to raster
    frequency_raster <- rasterFromXYZ(rasterising_poly_stack_dfxy, res = c(30,30), crs = crs(roi_trans))
  }else{ # if there;s one fire events
    rasterising_poly_stack_df$Fire.Event.1_fire_status <- ifelse(rasterising_poly_stack_df$Fire.Event.1_fire_status=='No Fire',0,1)
    # convert frequency to raster
    frequency_raster <- rasterFromXYZ(rasterising_poly_stack_df, res = c(30,30), crs = crs(roi_trans))
  }

   return(frequency_raster)
}

 

# FIRE_FREQ(data = all_monthly_fire_list, month = 3)

# note that although index range from 1 to 11; 1 to 6 is correct month but 7 onwards add 1 as July had no fire throughout
monthly_fire_frequency_rasters_1964_2022 <- pblapply(seq_along(all_monthly_fire_list), function(x) {FIRE_FREQ(data = all_monthly_fire_list, month = x)})
#creating a dummy raster with 0 populated for months where no fire were detected
dummy_df_for_no_fire <- as.data.frame(monthly_fire_frequency_rasters_1964_2022[[8]], xy = T, na.rm = T)[,-3]
dummy_df_for_no_fire$fire_frequency <- 0
dummy_df_for_no_fire_raster <- rasterFromXYZ(dummy_df_for_no_fire, res = c(30,30), crs = crs(roi_trans))


{
  p1 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[1]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "January", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p2 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[2]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "February", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p3 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[3]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "March", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p4 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[4]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "April", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p5 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[5]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "May", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p6 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[6]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "June", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  dummy_df_for_no_fire_raster
  p7 <- tm_shape(dummy_df_for_no_fire_raster) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "July", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p8 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[7]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "August", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p9 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[8]]) +
    tm_raster(
      col = "Fire.Event.1_fire_status",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "September", main.title.size = 0.7 ,main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p10 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[9]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "October", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p11 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[10]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "November", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
  p12 <- tm_shape(monthly_fire_frequency_rasters_1964_2022[[11]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "1964-2022\nFire Frequency"
    ) +
    tm_graticules(lines = F)+
    tm_layout(main.title = "December", main.title.size = 0.7, main.title.position = 0.26, legend.text.size = 0.45, legend.title.size = 0.5)
  
}

fire_freq_2964_2022_monthy_combined_maps <- tmap_arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
             ncol = 3,
             nrow=4)

tmap_save(fire_freq_2964_2022_monthy_combined_maps, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/fire_freq_2964_2022_monthy_combined_maps.pdf", width = 6.56, height = 8.50)


# dataset preparation
monthly_2014_2022_fire_list <-  pblapply(c(1:5,9:12), function(x){ # note that June-July-August: No fire detected
 sanpark_fire_shpfile_combind_list_trans_intersect_without_prescribed_2014_2022%>%
    st_as_sf()%>%
    mutate(MONTH = as.numeric(format(STARTDATE, "%m"))) %>%
    filter(MONTH == x) %>%
    as('Spatial')
})

# note that although index range from 1 to 9; 1 to 5 is correct month but 6 onwards add 3 as June-July-Augus had no fire throughout
monthly_fire_frequency_rasters_2014_2022 <- pblapply(seq_along(monthly_2014_2022_fire_list), function(x) {FIRE_FREQ(data = monthly_2014_2022_fire_list, month = x)})


{
  pp1 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[1]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "January", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp2 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[2]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "February", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp3 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[3]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "March", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp4 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[4]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "April", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp5 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[5]]) +
    tm_raster(
      col = "Fire.Event.1_fire_status",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "May", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp6 <- tm_shape(dummy_df_for_no_fire_raster) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "June", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  
  pp7 <- tm_shape(dummy_df_for_no_fire_raster) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "July", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp8 <- tm_shape(dummy_df_for_no_fire_raster) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "August", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp9 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[6]]) +
    tm_raster(
      col = "Fire.Event.1_fire_status",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "September", main.title.size = .55 ,main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp10 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[7]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "October", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp11 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[8]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "November", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
  
  pp12 <- tm_shape(monthly_fire_frequency_rasters_2014_2022[[9]]) +
    tm_raster(
      col = "fire_frequency",
      palette = plasma_mod,   
      style = "cat",          # categorical style (discrete)
      title = "2014-2022\nFire Frequency"
      
    ) +
    tm_graticules(labels.size = 0.4, n.x = 3, n.y = 3, lines = F)+
    tm_layout(main.title = "December", main.title.size = .55, main.title.position = 0.19, legend.text.size = 0.45, legend.title.size = 0.5)
}

fire_freq_2014_2022_monthy_combined_maps <- tmap_arrange(pp1,pp2,pp3,pp4,pp5,pp6,pp7,pp8,pp9,pp10,pp11,pp12,
                                                         ncol = 3,
                                                         nrow=4)

tmap_save(fire_freq_2014_2022_monthy_combined_maps, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/fire_freq_2014_2022_monthy_combined_maps.pdf", width = 6.56, height = 8.50)

# BONUS plot
# trying some animation plot
{
  tmap_save(p1, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_1.pdf", width = 2.5, height = 2.5)
  tmap_save(p2, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_2.pdf", width = 2.5, height = 2.5)
  tmap_save(p3, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_3.pdf", width = 2.5, height = 2.5)
  tmap_save(p4, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_4.pdf", width = 2.5, height = 2.5)
  tmap_save(p5, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_5.pdf", width = 2.5, height = 2.5)
  tmap_save(p6, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_6.pdf", width = 2.5, height = 2.5)
  tmap_save(p7, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_7.pdf", width = 2.5, height = 2.5)
  tmap_save(p8, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_8.pdf", width = 2.5, height = 2.5)
  tmap_save(p9, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_9.pdf", width = 2.5, height = 2.5)
  tmap_save(p10, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_10.pdf", width = 2.5, height = 2.5)
  tmap_save(p11, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_11.pdf", width = 2.5, height = 2.5)
  tmap_save(p12, filename = "/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/sanpark_fire_animation/Rplot_12.pdf", width = 2.5, height = 2.5)
  
}




