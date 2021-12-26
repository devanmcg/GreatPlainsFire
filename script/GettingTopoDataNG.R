pacman::p_load(tidyverse, sf, multidplyr)
load('./albersEAC.Rdata')

load('./data/soils/AllotmentSites.Rdata')

AllotmentSites <- 
  AllotmentSites  %>%
                st_transform(albersEAC) %>%
  filter(area_ac >= 10)

# Find dominant ecological sites (summing to at least 75% of allotment's area)
{ 
  begin = Sys.time()
  cl <- new_cluster(80)
  cluster_library(cl, c("tidyverse", "sf"))
  DomES <-
    AllUnits %>%
    filter(!is.na(site), site != "Non-site") %>% 
    mutate(area = st_area(.)) %>%
    group_by(unit, site) %>%
    partition(cl) %>%
    summarize(TotalArea = sum(area) ) %>%
    arrange(desc(TotalArea)) %>%
    mutate(PropArea = TotalArea / sum(TotalArea), 
           c = accumulate(as.numeric(PropArea), ~if_else(.x>=0.75, .y, .x+.y)) )  %>% 
    filter(c >= c[1]) %>%
    select(-c, -PropArea) %>%
    ungroup() %>%
    collect() 
  Sys.time() - begin
                }

{
  pacman::p_load(foreach, doParallel)
  cores=detectCores()
  cl <- makeCluster(cores[1]) 
  registerDoParallel(cl)
  begin = Sys.time()
  
  PointsTopoData <- 
    foreach(p=1:length(unique(samp_sf$unit)), 
            .combine=bind_rows, 
            .packages = c('tidyverse', 'sf')) %dopar% {

      p_sf <-  
        samp_sf %>%
        filter(unit == unique(samp_sf$unit)[p]) 
      dem <- 
        p_sf %>%
          as_Spatial()  %>%
          elevatr::get_elev_raster(z = 13, 
                                   clip = 'bbox', 
                                   verbose = F, 
                                   override_size_check = T) 
       
     full_join(
      terra::terrain(dem, 'slope') %>% 
       terra::rast() %>%
       terra::extract(., 
                      terra::vect(p_sf), 
                      fun = 'mean', 
                      method = 'bilinear', 
                      na.rm = TRUE, 
                      df = TRUE) %>%
          as_tibble() %>%
          rename(point = ID), 
      terra::terrain(dem, 'aspect', 'degrees') %>%
         terra::rast() %>%
         terra::extract(., 
                        terra::vect(p_sf), 
                        fun = 'mean', 
                        method = 'bilinear', 
                        na.rm = TRUE, 
                        df = TRUE) %>%
         as_tibble() %>%
         rename(point = ID) , 
      by = 'point') %>%
       mutate(unit = unique(p_sf$unit)) 
            }
    beepr::beep() 
    stopCluster(cl)
    Sys.time() - begin
}   

# save(PointsTopoData, file = './SpatialData/GreatPlainsNPS/PointsTopoData.Rdata')

