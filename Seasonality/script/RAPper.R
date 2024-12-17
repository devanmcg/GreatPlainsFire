pacman::p_load(tidyverse, magrittr, sf, foreach, doSNOW)

load('S:/DevanMcG/Projects/FuelWeatherSeasonality/RangeSectionsSF.Rdata') 

sections <- RangeSectionsSF %>% 
              st_transform(4326)
sec_vec <- terra::vect(sections)
save(sec_vec, file = 'S:/DevanMcG/Projects/FuelWeatherSeasonality/sec_vec.Rdata')

{
  begin = Sys.time()
  # years to sample from 
    years = seq(1986, 2023, 1)

  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
   <- 
    foreach(y=1:length(years), 
            .combine=bind_rows, 
            .errorhandling = 'remove', 
            .packages=c('tidyverse', 'sf')) %dopar% {

      year = unique(years)[y]
    # URL for RAP type + year 
      index_url <- 'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v3/vegetation-cover-v3-' 
      rap_url = paste0('/vsicurl/', index_url, year, '.tif')
      rap_ras <- terra::rast(rap_url)

    # Extract RAP data from URL by feature subset 
      
      td<-
      terra::extract(rap_ras, 
                     sec_vec, 
                     fun = 'mean', 
                     list = FALSE,
                     na.rm = TRUE, 
                     df = TRUE) %>%
        as_tibble() %>%
        mutate(ID = as.character(ID)) %>%
               pivot_longer(names_to = 'col', 
                            values_to = 'value', 
                            - ID) %>%
        tidyr::separate(col, c('type', 'band'), sep = '_') %>%
        mutate(year = year) %>%
        full_join(pts, 
                  . , 
                  by = 'ID') 
            }
  stopCluster(cl)
  Sys.time() - begin 
  }
  
StarbuckPtVegv3 <-
  StarbuckPtVegv3 %>%
  as_tibble() %>%
    mutate(band = case_when( 
      type == 'vc' ~ recode(band, 
                            '1' = 'AFGC',
                            '2' = 'BG', 
                            '3' = 'LTR', 
                            '4' = 'PFGC', 
                            '5' = 'SHR', 
                            '6' = 'TREE' ),
      type == 'ab' ~ recode(band, 
                            '1' = 'AnnProd',
                            '2' = 'PerProd' ) ) )  %>%
  dplyr::select(-geometry) 

# save(StarbuckPtVegv3, file = './data/StarbuckFire/StarbuckPtVegv3.Rdata')

StarbuckPtVegv3 %>%
  pivot_wider(names_from = year, 
              values_from = value) %>%
  mutate(pre = rowMeans(.[,5:9])) %>%
  select(ID, dNBR, type, band, pre, `2017`:`2021`)



  
# save(PointVegNGv3, file = './Rdata/PointVegNGv3.Rdata')
