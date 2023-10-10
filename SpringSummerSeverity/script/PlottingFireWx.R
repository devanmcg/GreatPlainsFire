pacman::p_load(tidyverse, sf)

load('./data/HistWx.Rdata')

# Example days 

HistWx %>%
  filter(as.character(date) %in% c('2018-05-05', '2018-05-16', '2018-08-13')) %>%
  pivot_wider(names_from = param, 
              values_from = value) %>% View() 

ndawn <- 
read_csv('./data/StreeterStudyPeriodNDAWN.csv')

ndawn %>%
  select(-`Station Name`, -Latitude, -Longitude, -Elevation) %>%
unite(col = 'date', c(Year, Month, Day), sep = '-') %>%
filter(date %in% c('2018-5-5', '2018-5-16', '2018-8-13')) %>%
  pivot_wider(names_from = param, 
              values_from = value) %>% View() 

ndawn %>%
  select(-`Station Name`, -Latitude, -Longitude, -Elevation) %>%
  unite(col = 'date', c(Year, Month, Day), sep = '-') %>%
  filter(date %in% c('2018-5-5', '2018-5-16'),  
         Hour %in% c(1500)) 

ndawn %>%
  select(-`Station Name`, -Latitude, -Longitude, -Elevation) %>%
  unite(col = 'date', c(Year, Month, Day), sep = '-') %>%
  filter(date %in% c('2018-5-5', '2018-5-16'),  
         Hour %in% c(1500)) 


ndawn %>%
  select(-`Station Name`, -Latitude, -Longitude, -Elevation) %>%
  unite(col = 'date', c(Year, Month, Day), sep = '-') %>%
  filter(date %in% c('2018-5-5', '2018-5-16')) %>%  
  group_by(date) %>%
    summarise(RH = min(RH) )


HistWx %>%
  separate(date, c('year', 'month', 'day'), sep = "-", remove = F) %>%
  filter(year == '2018') %>%
  arrange(desc(as.numeric(month)) ) 
  mutate(season = case_when(
    month %in% c('04', '05') ~ 'spring',
    month %in% c('08') ~ 'summer'), 
    day = format(date, '%b-%d')) %>%
  filter(!is.na(season),
         !is.na(value), 
         param %in% c("burn_index", "fmoist_100", "energy_release", "palmer",        
                      "pet_grass", "hdwi" )) %>%
  group_by(param, season, day) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            SEM = sd(value, na.rm = TRUE) / sqrt(n()), 
            .groups = 'drop') %>%
  ggplot() + theme_bw() + 
    geom_smooth(aes(x = day, 
                  y = mean, 
                  group = 1) ) +
  facet_grid(param ~ season, scales = 'free')
