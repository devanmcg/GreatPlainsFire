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

fires_LongLat <- 
  GP_WF %>%
    # slice(1:5) %>%
      st_centroid() %>% 
      st_transform(4326)

# Download 40 years of historical weather data from each fire location

{
HistWx <- tibble()  

begin = Sys.time()
for(i in 1:length(fires_LongLat$Incid_Name)){
  d <-  
    fires_LongLat %>%
    slice(i) 
  for(p in 1:length(params)){
      beepr::beep_on_error(
        climateR::getGridMET(
          AOI = d, 
          param = params[p],
          startDate = '1980-01-01', 
          endDate = '2020-12-31'
            ))  %>% 
          pivot_longer(cols = params[p],
                       names_to = "param", 
                       values_to = "value")%>%
          mutate(incident = d$Incid_Name) %>%
          bind_rows(HistWx) -> HistWx 
  } ; closeAllConnections()
  } ; beepr::beep() ; Sys.time() - begin
      }

# Determine 'normal' weather from 

days_range = seq(-3, 3, 1)
year_range = seq(1980, 2010, 1)

startDate = paste0(year_range[y], '-', 
                    format(d$Ig_Date - 3, '%m-%d'))
endDate = paste0(year_range[y], '-', 
                  format(d$Ig_Date + 3, '%m-%d')) 