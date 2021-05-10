pacman::p_load(tidyverse, sf, climateR)

gis_fp = 'C:/Users/devan.mcgranahan/GithubProjects/GreatPlainsFire/gis'

load(file=paste0(gis_fp, '/Robjects/GP_WF.Rdata'))

# Identify variables to fetch
elements = c(
  'precipitation_amount', 
  'daily_minimum_relative_humidity',
  'daily_mean_specific_humidity',
  'daily_mean_shortwave_radiation_at_surface',
  'daily_maximum_temperature',
  'daily_mean_wind_speed',
  'daily_mean_burning_index_g',
  'daily_mean_energy_release_component-g',
  #'daily_mean_palmer_drought_severity_index',
  'daily_mean_reference_evapotranspiration_grass',
  'daily_mean_vapor_pressure_deficit')

params <- filter(param_meta$gridmet, description %in% elements )$common.name

fires <- 
  GP_WF %>%
    st_centroid() %>% 
      st_transform(4326) %>%
  select(Event_ID, Incid_Name, Ig_Date) %>%
  mutate(start = format(Ig_Date - lubridate::days(5), '%m-%d'), 
         end = format(Ig_Date + lubridate::days(5), '%m-%d'), 
         date = format(Ig_Date, '%Y-%m-%d')) %>%
  unite("event", c(Event_ID, Incid_Name), sep = '_') 

# Download 40 years of historical weather data from each fire location
yr_range = 1980:2020

pacman::p_load(foreach, doParallel)
cores=detectCores()
{
cl <- makeCluster(cores[1]) 
registerDoParallel(cl)

begin = Sys.time()
HistWx <- 
  foreach(f=1:length(fires$event[1:2]), 
          .combine = rbind, 
          .errorhandling = 'remove', 
          .inorder = FALSE) %:% 
  
  foreach(y=1:length(yr_range[1:10]), 
          .combine = 'rbind', 
          .errorhandling = 'remove', 
          .inorder = FALSE, 
          .packages = c('tidyverse', 'sf', 'climateR')) %dopar% {
        fire = slice(fires, f)
        climateR::getGridMET(
          AOI = fire, 
          param = params,
          startDate = paste0(yr_range[y],'-', 
                             as_tibble(fire)['start']), 
          endDate = paste0(yr_range[y],'-', 
                           as_tibble(fire)['end'])
        )  %>% 
       select(-source, -lat, -lon) %>%
          pivot_longer(names_to = "param", 
                       values_to = "value", 
                       -date) %>%
          mutate(event = fire$event, 
                 year = yr_range[y]) %>%
          group_by(event, year, param) %>%
          summarize(TenDayMean = mean(value)) %>%
          ungroup()  
      # closeAllConnections()
  } ; stopCluster(cl)
Sys.time() - begin 
}
