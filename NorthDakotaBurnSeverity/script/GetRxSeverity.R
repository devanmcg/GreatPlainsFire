pacman::p_load(tidyverse, magrittr, sf, terra, tidyterra)

# Load Rx fire data
  # burn & imagery dates 
    rx_dates <-read_xlsx("S:/DevanMcG/FireScience/Sentinel/SeverityComparison/ImageDates.xlsx", 
                         'PreBurnPostDatesRxND') %>%
      mutate(across(c(BurnDate:PostBurn), ~ as.character(.x)))
  # spatial & fire data for the thermocouples 
    rx_sf <- st_read('./data/ThermocouplePlacements.gpkg', 
                     'AllBurnData') %>%
              mutate(BurnDate = as.character(date) ) %>%
              select(-date) %>%
              full_join(by = c('location', 'BurnDate'), 
                        rx_dates) %>%
              st_transform(4326)

RxPointSeverity <- tibble() 

{
  begin = Sys.time()
  for(l in 1:length(unique(rx_sf$location))) {
    lc = unique(rx_sf$location)[l]
    loc = filter(rx_sf, location == lc)
    dnbr_fp <- case_when(
                  lc == 'CGREC' ~ './data/RxRasters/CGREC.tiff', 
                  lc == 'HREC' ~ './data/RxRasters/HREC.tiff',
                  lc == 'Dayton' ~ './data/RxRasters/Dayton.tiff'
                )
    dnbr_rast <- rast(dnbr_fp)
  # loop for individual burns
  for(b in 1:length(unique(loc$burn))) {
    bn = unique(loc$burn)[b]
    #bn = 'DAY.1.19'
    burn = filter(loc, burn == bn)
    date = burn$BurnDate
    dnbr <- dnbr_rast %>% select(all_of(date)) 
    names(dnbr) <- 'dNBR'
        # Extract values at points
          terra::extract(dnbr,
                         burn , 
                         fun = mean, 
                         method = 'bilinear',
                         bind = TRUE)  %>% 
          as_tibble() %>% 
            select(location:plot, BurnDate, MaxC:ros, dNBR) %>%
          bind_rows(., RxPointSeverity) -> RxPointSeverity
}
}
  Sys.time() - begin 
}

RxPointSeverity %<>%
  mutate(zone = ifelse(location == 'CGREC', 'East', 'West')) 
  # save(RxPointSeverity, file = './data/RxPointSeverity.Rdata')




