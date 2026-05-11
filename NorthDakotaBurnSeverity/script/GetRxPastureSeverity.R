pacman::p_load(tidyverse, magrittr, sf, terra, tidyterra)
load('../albersEAC.Rdata')

# Load Rx fire data
  rx_perims <- read_sf('./data/SeverityComparison.gpkg', 'rx_perims')


{
  begin = Sys.time()
  
  RxPastureSeverity <- tibble() 
  
  for(b in 1:length(unique(rx_perims$burn))) {
    bn = unique(rx_perims$burn)[b]
    burn = filter(rx_perims, burn == bn)
    lc = burn$location
    # Map to and load severity raster for the location
      dnbr_fp <- case_when(
                    lc == 'CGREC' ~ './data/RxRasters/Pasture/CGREC.tiff', 
                    lc == 'HREC' ~ './data/RxRasters/Pasture/HREC.tiff',
                    lc == 'Dayton' ~ './data/RxRasters/Pasture/Dayton.tiff'
                  )
      dnbr_rast <- rast(dnbr_fp)
    #
    # process the burn
    #
      dp = burn$DeltaPair
      dnbr <- dnbr_rast %>% 
                select(all_of(dp)) %>%
                crop(burn)
      names(dnbr) <- 'dNBR'
    # create sample points 
      buff <- burn %>% 
                st_transform(albersEAC) %>% 
                st_buffer(-20)
      pts <-  buff %>%
                st_make_grid(cellsize = c(diff(st_bbox(.)[c(1, 3)]), 
                                          diff(st_bbox(.)[c(2, 4)]))/ 10) %>% 
                st_centroid() %>%
                st_intersection(buff) %>%
                st_transform(4326)
    # Extract dNBR values at points
      terra::extract(dnbr,
                     pts %>%
                       vect() , 
                     fun = mean, 
                     method = 'bilinear',
                     bind = TRUE)  %>% 
      as_tibble() %>%
      tibble(location = lc, 
             burn = bn, 
             . ) %>% 
      bind_rows(., RxPastureSeverity) -> RxPastureSeverity
}
  Sys.time() - begin 
}

RxPastureSeverity %<>%
  mutate(zone = ifelse(location == 'CGREC', 'East', 'West')) 
  # save(RxPastureSeverity, file = './data/RxPastureSeverity.Rdata')




