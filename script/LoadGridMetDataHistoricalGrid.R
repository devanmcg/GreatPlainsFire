pacman::p_load(tidyverse, sf, climateR)

load('./gis/Robjects/GP_Wx_SamplePoints.Rdata')

points <- 
  GP_Wx_SamplePoints %>%
    st_transform(4326)

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
  'daily_mean_reference_evapotranspiration_grass',
  'daily_mean_vapor_pressure_deficit')

params <- filter(param_meta$gridmet, description %in% elements )$common.name

# Download 40 years of historical weather data for each sample point
  fast.download <- edit(fast.download) 
# Non=parallel (getGridMET's fast.download modified to use all six cores)
{
  begin = Sys.time()
  PointHistWx <- tibble() 
    for(p in 1:length(points$geometry))  {
              pt = slice(points, p)
              getGridMET(
                AOI = pt, 
                param = params,
                startDate = '1980-01-01', 
                endDate = '2020-12-31') %>% 
                select(-source, -lat, -lon) %>%
                pivot_longer(names_to = "param", 
                             values_to = "value", 
                             -date) %>%
                mutate(date = format(date, '%Y-%m')) %>%
                group_by(date, param) %>%
                summarize(MonthMean = mean(value)) %>%
                ungroup()  %>%
                mutate(point = p) %>%
                bind_rows(PointHistWx) -> PointHistWx
              closeAllConnections()
            } 

  Sys.time() - begin 
}
  
 # save(PointHistWx, file = './data/HistoricalWeather/PointHistWx.Rdata') 
 