pacman::p_load(tidyverse, readxl, RColorBrewer)

seasons <- tibble(season = c('spring', 'summer'), 
                 start = c('04-20', '07-20'), 
                 end = c('06-10', '09-10') ) %>%
          mutate(start = as.Date(start, '%m-%d'), 
                 end = as.Date(end, '%m-%d'))

BurnDays <- 
  read_xlsx('./data/BurnData.xlsx', 'BurnDays') %>%
  filter(is.na(certainty)) %>%
    mutate(date = paste(year, date), 
           date = as.Date(date, '%Y %B %d')) %>%
    select(date) %>%
  distinct() 

WxData <- lst() 
# Get daily data 
  DailyWx <- 
    read_xlsx('./data/CGREC_weather.xlsx', 'daily')
# Identify rainy days
  WxData$Rainfall <-
    DailyWx %>%
    select(Year, Month, Day, Rainfall) %>% 
    unite( c(Month, Day), col = 'day', sep = '-', remove = F) %>%
    mutate(day = as.Date(day, '%m-%d')  , 
           season = case_when(
             between(day, seasons$start[1], seasons$end[1]) ~'Spring', 
             between(day, seasons$start[2], seasons$end[2]) ~'Summer',
             TRUE ~ NA      
           ) ) %>% 
     unite( c(Year, Month, Day), col = 'date', sep = '-')


# Hourly data 
  WxData$HourlyWx <- 
    read_xlsx('./data/CGREC_weather.xlsx', 'hourly') %>%
    filter(between(Hour, 1000, 1700)) %>%
    unite( c(Year, Month, Day), col = 'date', sep = '-', remove = F) %>%
    unite( c(Month, Day), col = 'day', sep = '-', remove = F) %>%
    mutate(day = as.Date(day, '%m-%d'), 
           season = case_when(
             between(day, seasons$start[1], seasons$end[1]) ~'Spring', 
             between(day, seasons$start[2], seasons$end[2]) ~'Summer',
             TRUE ~ NA      
           ) ) %>%
    mutate(e = 6.11 * (10 ^ ( (7.5 * DewPoint)/ (237.3 + DewPoint) ) ), 
           es = 6.11 * (10 ^ ( (7.5 * AirTemp)/ (237.3 + AirTemp) ) ), 
           VPD = es - e) 
# Get burn day weather
  WxData$BurnDayWx <-
    HourlyWx  %>% 
    filter(date %in% BurnDays$date) %>%
    group_by(Year, date, season) %>%
    summarise_at(.vars = vars(c(DewPoint, RelHum, WindSpeed,VPD)),
                 .funs = c("mean")) %>%
    ungroup() %>%
    pivot_longer(names_to = 'variable', 
                 values_to = 'value', 
                 cols = DewPoint:VPD) %>%
    mutate(variable = recode(variable, 
                             'DewPoint' = 'Dew Point (°C)', 
                             'RelHum' = 'Relative Humidity (%)', 
                             'VPD' = 'Vapor Pressure Deficit', 
                             'WindSpeed' = 'Wind speed (m/s)') , 
           Year = as.character(Year)) 
  
  # save(WxData, file = './data/WxData.Rdata')
