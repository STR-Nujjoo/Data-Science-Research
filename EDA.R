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
good_AI <- brick(paste0('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-R Project/Data Science Minor Dissertation/Variables/Unprocessed Variables/Aerial Imagery 2002-2023/', AI_name[197]))

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


