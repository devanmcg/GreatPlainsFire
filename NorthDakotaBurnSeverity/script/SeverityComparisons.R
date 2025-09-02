FFgrad = wes_palette("FantasticFox1")[c(3,2,4,5)]

BurnSeverityData <- tibble() 
# Load data 
  # HREC & Dayton
   load('./data/RxBurnIndices.Rdata') # 22 Rx burns in sw ND
 # CGREC data from previous paper
   # loading this previously conflicted with the plot-level BurnIndices but it has been renamed
   load("C:/Users/devan.mcgranahan/GitHubProjects/GreatPlainsFire/SpringSummerSeverity/data/BurnIndices.Rdata")
# Wildfires 
   load('./data/WfBurnIndices.Rdata') # wildfire burn indices
   wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF') # Perimeter data

BurnSeverityData <- 
  bind_rows(
    # CGREC
      BurnIndices  %>%
        filter(Season == 'Spring') %>%
        rename(FireCode = fire) %>%
        group_by(FireCode) %>%
        summarise_at(.vars = vars(dNBR, ndvi),
                     .funs = c(Mean="mean", 
                               SEM = function(x) {sd(x)/sqrt(n()) } )) %>%
        ungroup() %>%
        mutate(type = 'Rx', 
               zone = 'east') %>%
        rowid_to_column() %>%
        mutate(burn = paste0('CGREC_', rowid)) %>%
        select(type, zone, FireCode:ndvi_SEM),  
    # HREC & Dayton
      RxBurnIndices  %>%
      rename(FireCode = burn) %>%
        filter(complete.cases(.), 
               dNBR > 0) %>%
        group_by(location, FireCode) %>%
        summarise_at(.vars = vars(dNBR, ndvi),
                     .funs = c(Mean="mean", 
                               SEM = function(x) {sd(x)/sqrt(n()) } )) %>%
        ungroup() %>%
        mutate(type = 'Rx', 
               zone = 'west') %>%
         select(type, zone, FireCode:ndvi_SEM) , 
    # Wildfires
      wf_sf %>%
        as_tibble() %>%
        select(FireCode, L3) %>%
        left_join(by = 'FireCode', 
          WfBurnIndices %>%
            group_by(FireCode) %>%
            summarize(across(c(dNBR, ndvi), 
                             list('Mean' = mean, 
                                  'SEM' = function(x){sd(x)/sqrt(n( ))} ))) ) %>%
         mutate(type = 'Wildfire', 
                zone = case_when(
                  FireCode %in% c('Swather_2021','Hwy31101STWest_2022', 'CB00121_2021', 
                                  'CoalSeamWest_2021', '1806HunkpapaCreek_2021') ~
                    'east', 
                  L3 == 'Northwestern Great Plains' ~ 'west',
                  L3 == 'Northwestern Glaciated Plains' ~ 'east')) %>% 
        select(type, zone, FireCode, dNBR_Mean:ndvi_SEM) )

    # save(BurnSeverityData, file = './data/BurnSeverityData.Rdata')

BurnSeverityData %>%
  group_by(type, zone) %>%
  summarize(Mean = mean(dNBR_Mean))
    

    
    wf_sf %>%
      mutate( L3 = case_when(
               FireCode %in% c('Swather_2021','Hwy31101STWest_2022', 'CB00121_2021', 
                               'CoalSeamWest_2021', '1806HunkpapaCreek_2021') ~
                 'Northwestern Glaciated Plains', 
               TRUE ~ L3)) %>%
      st_transform(albersEAC) %>%
      st_centroid() %>%
      st_intersection(ppt_grd) %>%
      arrange(ppt) %>%
      ggplot(aes(x= reorder(FireCode, ppt, max), y = ppt)) + theme_bw(16) +
      coord_flip() +
      geom_bar(aes(fill = L3), stat='identity')
     
      labs(x = 'Fire type', 
           y = 'Burn severity (dNBR)') +           
      facet_wrap(~L3, scales = 'free_x') +
      theme(panel.grid.major.x = element_blank(), 
            axis.text.x = element_text(color = 'black'))
    


    


  
    wf_sf %>%
      as_tibble() %>%
      select(FireCode, state, L3) %>%
      left_join(by = 'FireCode', 
                WfBurnIndices %>%
                  group_by(FireCode) %>%
                  summarize(across(c(dNBR, ndvi), 
                                   list('Mean' = mean, 
                                        'SEM' = function(x){sd(x)/sqrt(n( ))} ))) ) %>%
      rename(burn = FireCode) %>%
      mutate(type = 'Wildfire') %>%
      filter(L3 == 'Northwestern Glaciated Plains') %>%
      rename(dNBR = dNBR_Mean, NDVI = ndvi_Mean) %>%
      pivot_longer(names_to = 'var', 
                   values_to = 'value', 
                   cols = c(dNBR, NDVI)) %>%
      ggplot() + theme_bw(16) +
      geom_boxplot(aes(x = state, y = value)) + 
        facet_wrap(~var)
  

