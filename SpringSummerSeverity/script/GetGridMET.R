pacman::p_load(tidyverse, sf)
pacman::p_load_gh("devanmcg/climateR") 
# pacman::p_load_gh('fickse/mesowest')

# Identify variables to fetch
  elements = c(
    'precipitation_amount', 
    'daily_minimum_relative_humidity',
    'daily_maximum_temperature',
    'daily_mean_wind_speed',
    'daily_mean_palmer_drought_severity_index',
    'daily_mean_reference_evapotranspiration_grass',
    'dead_fuel_moisture_100hr', 
    'daily_mean_burning_index_g',
    'daily_mean_energy_release_component-g',
    'daily_mean_vapor_pressure_deficit')

params <- filter(param_meta$gridmet, description %in% elements )$common.name



pacman::p_load(foreach, doSNOW)

{
  cgrec_pt <- read_sf('./gis/boundaries/CGREC_bbox_4326.shp') %>% st_centroid()
  yr_range = 1980:2020
  mo_range = c('04', '05', '06', '07', '08', '09', '10')
  
  cores=parallel::detectCores()
  cl <- makeCluster(cores) 
  registerDoSNOW(cl )
  
  begin = Sys.time()
  HistWx <- 
    foreach(y=1:length(yr_range), 
            .combine = bind_rows, 
            .errorhandling = 'remove', 
            .inorder = FALSE) %:% 
    
    foreach(m=1:length(mo_range), 
            .combine = bind_rows, 
            .errorhandling = 'remove', 
            .inorder = FALSE, 
            .packages = c('tidyverse', 'sf', 'climateR')) %dopar% {
              getGridMET(
                AOI = cgrec_pt, 
                param = params,
                startDate = paste0(yr_range[y],'-', 
                                   mo_range[m], '-', 
                                   '01'), 
                endDate = paste0(yr_range[y],'-', 
                                 mo_range[m], '-', 
                                 '30') )  %>% 
                select(-source, -lat, -lon) %>%
                mutate(hdwi = wind_vel * vpd, 
                       tmax = tmax = 275.15, 
                       ) %>%
                pivot_longer(names_to = "param", 
                             values_to = "value", 
                             -date) 
              # closeAllConnections()
            }
  stopCluster(cl)
  Sys.time() - begin 
} 

save(HistWx, file = './data/HistWx.Rdata')