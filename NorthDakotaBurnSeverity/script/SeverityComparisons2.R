pacman::p+load(tidyverse)

# Load data 
   load('./data/RxPastureSeverity.Rdata') 
   load('./data/WfSeverityLM.Rdata') # wildfire burn indices
   wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF') # Perimeter data

BurnSeverityData <- 
  bind_rows(
    # Rx fires
    RxPastureSeverity %>%
      rename(FireCode = burn) %>%
        filter(complete.cases(.) ) %>%
        group_by(zone, location, FireCode) %>%
        summarise_at(.vars = vars(dNBR),
                     .funs = c(Mean_dNBR="mean", 
                               SEM = function(x) {sd(x)/sqrt(n()) } )) %>%
        ungroup() %>%
        mutate(type = 'Rx') %>%
         select(type, zone, FireCode:SEM) , 
    # Wildfires
      wf_sf %>%
        as_tibble() %>%
        select(FireCode, L3) %>%
      right_join(by = 'FireCode', 
        WfSeverityLM %>%
          group_by(FireCode) %>%
          summarise_at(.vars = vars(dNBR),
                       .funs = c(Mean_dNBR="mean", 
                                 SEM = function(x) {sd(x)/sqrt(n()) } )) ) %>%
         mutate(type = 'Wildfire', 
                zone = case_when(
                  FireCode %in% c('Swather_2021','Hwy31101STWest_2022', 'CB00121_2021', 
                                  'CoalSeamWest_2021', '1806HunkpapaCreek_2021') ~
                    'East', 
                  L3 == 'Northwestern Great Plains' ~ 'West',
                  L3 == 'Northwestern Glaciated Plains' ~ 'West')) %>% 
      select(-L3) ) 

    # save(BurnSeverityData, file = './data/BurnSeverityData.Rdata')

