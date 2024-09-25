library(readxl)
library(tidyverse)
library(patchwork)
library(plotly)
pop <- read_excel('Raw Data/SA data/Population.xlsx', 
                  skip = 1) # read in population data # https://data.worldbank.org/indicator/SP.POP.TOTL?locations=ZA

firms_fire <- read.csv('Raw Data/SA data/DL_FIRE_M-C61_509290/fire_archive_M-C61_509290.csv') # read fire data

pop_df <- pop[43:nrow(pop),] # trim dataset to deal with the year 2002 onwards
pop_df <- pop_df %>%
  mutate(total_pop_in_millions  = `Total Population`/1000000) %>% # redefine population in millions
  select(Year, total_pop_in_millions)

firms_fire_df <- firms_fire %>%
  mutate(Year = year(acq_date)) %>% # extract year from date
  group_by(Year) %>%
  tally() %>%
  rename(n_fire = n) %>%
  mutate(n_fire = n_fire/1000) %>% # redefine fire occurrences in thousands
  filter(Year %in% 2002:2023)

# Aggregate number of fires as sum over the defined period on a monthly basis
firms_fire_df_monthly <- firms_fire %>%
  mutate(Month = month(acq_date), Year = year(acq_date)) %>% # extract month and year from date
  filter(Year %in% 2002:2023) %>%
  group_by(Month) %>%
  tally() %>%
  rename(n_fire = n) %>%
  mutate(n_fire = n_fire/1000, Month = as.integer(Month)) # redefine fire occurrences in thousands


fire_pop_df <- full_join(firms_fire_df, pop_df, by = 'Year') # combine fire and population data in one dataframe
# fire_pop_df_long <- pivot_longer(fire_pop_df, cols = c(n_fire, total_pop_in_millions),
#              names_to = "Metric",
#              values_to = "Value") %>%
#   mutate(Metric = as.factor(Metric))

# Visualise population data for SA
(
  pop_plot <- ggplot(fire_pop_df, aes(x = Year)) +
    geom_line(aes(y = total_pop_in_millions, colour = 'Total Population')) +
    scale_color_manual(values = c("Total Population" = "blue")) +   # Manually specify the colors for the lines
    ylab('Total Population (in Millions)')+
    theme_light() +
    theme(legend.position = 'none',  
          legend.title = element_blank(),
          plot.tag = element_text(size = 10, face = 'bold'), 
          axis.title = element_text(size = 9))+
    labs(tag = 'A') 
)
ggplotly(pop_plot) # make plot interactive for further analysis

# Visualise fire data for SA
(
  fire_plot <- ggplot(fire_pop_df, aes(x = Year, y = n_fire)) +
    geom_line(aes(y = n_fire, colour = 'Number of Fires')) +
    geom_smooth(method = loess, se = F, color = 'black', linewidth = .3, linetype = 'dashed') +
    scale_color_manual(values = c("Number of Fires" = "red")) +   # Manually specify the colors for the lines
    ylab('Number of Fires  (in Thousands)')+
    theme_light() +
    theme(legend.position = 'none',  
          legend.title = element_blank(),
          plot.tag = element_text(size = 10, face = 'bold'), 
          axis.title = element_text(size = 9)) +
    labs(tag = 'B') 
)
ggplotly(fire_plot) # make plot interactive for further analysis

# Visualise monthly aggregated fire frequency
(
  firefreq_monthly_plot <- ggplot(firms_fire_df_monthly, aes(x = factor(Month), y = n_fire)) +
    xlab('Month')+
    ylab('Number of Fires  (in Thousands)')+
    geom_col(width = 0.3) +
    geom_hline(yintercept = mean(firms_fire_df_monthly$n_fire), linetype = 'dashed', colour = 'red', linewidth = .3)+
    annotate("text", x = 4, y = mean(firms_fire_df_monthly$n_fire), label = paste("Mean Fire Occurrences= ", round(mean(firms_fire_df_monthly$n_fire)*1000)), 
             vjust = -0.5, hjust = 1.1, color = "red", size = 2.5)+
    theme_light()+
    theme(plot.tag = element_text(size = 10, face = 'bold'),
          axis.title = element_text(size = 9))+
    labs(tag = 'C')
)

# Customise layout
(fire_pop_plots <- (pop_plot+fire_plot)/firefreq_monthly_plot)

# Save plot to hard drive
ggsave('/Volumes/Hard Drive (29-08-22)/Data Science 2023-2024/2nd year MSc Data Science/STA5079W-DS Minor Dissertation/Figures/EDA plots/fire_pop_plots.pdf', 
       plot = fire_pop_plots, width = 6.56, height = 5.5)
