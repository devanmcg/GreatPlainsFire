pacman::p_load(tidyverse, sf, multidplyr, wesanderson)

# Load historical weather data
  load('./data/HistoricalWeather/RawHistWx.Rdata')
  load('./data/HistoricalWeather/RawDayOfWx.Rdata')

# Load fire spatial information
  gis_fp = 'C:/Users/devan.mcgranahan/GithubProjects/GreatPlainsFire/gis'
  load(file=paste0(gis_fp, '/Robjects/GP_WF.Rdata'))

# Add EPA ecoregion columns to historical weather data
  HistWx <-
    GP_WF  %>%
      select(Event_ID, Incid_Name, NA_L2NAME, US_L3NAME) %>%
      unite(event, c(Event_ID, Incid_Name), sep = '_') %>%
      rename(L2 = NA_L2NAME, 
             L3 = US_L3NAME) %>%
      mutate(L2 = str_to_title(L2)) %>%
      as_tibble() %>%
      select(-geometry) %>%
      full_join(HistWx)
  #save(HistWx, file = './data/HistoricalWeather/HistWx.Rdata')
  
  DayOfWx <-
    GP_WF  %>%
    select(Event_ID, Incid_Name, NA_L2NAME, US_L3NAME) %>%
    unite(event, c(Event_ID, Incid_Name), sep = '_') %>%
    rename(L2 = NA_L2NAME, 
           L3 = US_L3NAME) %>%
    mutate(L2 = str_to_title(L2)) %>%
    as_tibble() %>%
    select(-geometry) %>%
    full_join(DayOfWx)
  #save(DayOfWx, file = './data/HistoricalWeather/DayOfWx.Rdata')

# Summarize event-level data to L2 ecoregion
  
  L2_AnnualMeans <-
    HistWx %>%
    filter(!is.na(param)) %>%
      group_by(L2, year, param) %>%
      summarize(Mean = mean(TenDayMean, na.rm = TRUE)) %>%
      ungroup() 
  
  L2_DayOfMeans <-
    DayOfWx %>%
    filter(!is.na(param)) %>%
    group_by(L2, param) %>%
    summarize(Mean = mean(DayOfValue, na.rm = TRUE), 
              SD = sd(DayOfValue, na.rm = TRUE),
              SE = sd(DayOfValue, na.rm = TRUE)/ sqrt(n())) %>%
    ungroup() 

# Plotting 
  ggplot() + theme_bw(14) + 
    geom_boxplot(data = L2_AnnualMeans, 
                 aes(x = L2,
                     y = Mean, 
                     fill = L2),
                 outlier.shape = NA, 
                 color = "darkgrey",
                 alpha = 0.75) +
    geom_errorbar(data = L2_DayOfMeans, 
               aes(x = L2, 
                   ymin = Mean - SE, 
                   ymax = Mean + SE), 
               color = "black", size = 1, width = 0.2) +
    geom_point(data = L2_DayOfMeans, 
               aes(x = L2, 
                   y = Mean), 
               pch = 21, fill = "darkgrey", 
               col = "black", stroke = 1.1, size = 4) +
    labs(y = '', x = '') +
    coord_flip() +
    facet_wrap(~param, scales = 'free', ncol = 2) + 
    scale_fill_manual(name = 'Level 2 ecoregion',
                      values = wes_palette("Zissou1")) +
    theme(panel.grid.major.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank() )
  

  