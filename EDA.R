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
                                          
                                          as.Date('2011-01-03'), as.Date('2011-04-09'), as.Date('2011-05-05'), as.Date('2011-06-12'),
                                          as.Date('2011-08-15'), as.Date('2011-09-16'), # 2011 series
                                          
                                          as.Date('2012-01-06'), as.Date('2012-03-26'), as.Date('2012-04-11'), as.Date('2012-09-02'),
                                          as.Date('2012-10-20'), as.Date('2012-12-23'), # 2012 series
                                          
                                          as.Date('2013-02-25'), as.Date('2013-04-30'), as.Date('2013-06-17'), as.Date('2013-07-03'),
                                          as.Date('2013-08-04'), as.Date('2013-11-24'), as.Date('2013-12-26') # 2013 series
                                          ) ~ 'Landsat 7 SR', # all the above imageries collected from L7 SR satellite
                                     
                                     date %in% c(
                                          as.Date('2013-10-15'), as.Date('2013-12-18'), # 2013 series
                                                 
                                          as.Date('2014-02-04'), as.Date('2014-04-09'), as.Date('2014-04-25'), as.Date('2014-06-12'), 
                                          as.Date('2014-06-28'), as.Date('2014-07-14'), as.Date('2014-07-30'), as.Date('2014-08-31'),
                                          as.Date('2014-10-02'), as.Date('2014-10-18'), as.Date('2014-11-10'), as.Date('2014-12-05'), # 2014 series
                                          
                                          as.Date('2015-01-06'), as.Date('2015-01-22'), as.Date('2015-02-07'), as.Date('2015-02-23'),
                                          as.Date('2015-03-11'), as.Date('2015-04-12'), as.Date('2015-07-01'), as.Date('2015-08-02'),
                                          as.Date('2015-09-03'), as.Date('2015-09-19'), # 2015 series
                                          
                                          as.Date('2016-01-09'), as.Date('2016-02-10'), as.Date('2016-07-03'), as.Date('2016-10-23'),
                                          as.Date('2016-12-10'), as.Date('2016-12-26'), # 2016 series
                                          
                                          as.Date('2017-01-11'), as.Date('2017-02-28'), as.Date('2017-03-16'), as.Date('2017-04-17'),
                                          as.Date('2017-05-19'), as.Date('2017-07-06'), as.Date('2017-08-07'), as.Date('2017-10-10'),
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
  geom_tile(color = "black", linewidth = .05) +
  scale_fill_manual(labels= c("Landsat 7 SR", "Landsat 8 SR", "Landsat 9 SR", "Missing Data", "Sentinel 2 SR", "Sentinel 2 TOA"), 
                    values = c("deepskyblue", "blue", 'navyblue', 'white', 'red', 'orange')) +
  facet_grid(year ~ month) +
  theme_void()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(), # remove legend title
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(
    keyheight = unit(0.6, "cm"), # resize of icon
    keywidth = unit(0.08, "cm"), # resize icon
    nrow = 1  # force icon to unwrap to 1 row
  ))

all_possible_aerial_imagery_plot

ggsave("/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/all_possible_aerial_imagery_plot.pdf", 
       plot = all_possible_aerial_imagery_plot, width = 6.56, height= 8.5)


# # No. of imagery acquired
# df_aerial_imagery %>%
#   filter(has_data != 'Missing Data') %>%
#   tally()