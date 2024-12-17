pacman::p_load(tidyverse, magrittr, sf, foreach, doSNOW)

gp <- read_sf('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg', 'StudyRegion') 

sections <- read_sf('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg', 
                    'gp_state_sections') %>%
              rowid_to_column("sectionID") %>%
              select(-acres)
gp_reeves_r <- raster::raster('S:/DevanMcG/Projects/FuelWeatherSeasonality/StudyRegionReeves.tif')
gp_reeves_ras <- terra::rast('S:/DevanMcG/Projects/FuelWeatherSeasonality/StudyRegionReeves.tif')

reeves_crs = raster::crs(gp_reeves_r)
sections %<>%  st_transform(reeves_crs)

{ 
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeCluster(cores, methods = F, useXDR = F)
  registerDoSNOW(cl)

  RangeSections <- 
    foreach(i=1:length(unique(sections$sectionID)), 
            .combine = 'bind_rows',
            .errorhandling = 'remove', 
            .packages=c('tidyverse', 'sf')) %dopar% {
      sec <- filter(sections, sectionID == i)
      ras <- terra::rast('S:/DevanMcG/Projects/FuelWeatherSeasonality/StudyRegionReeves.tif') %>%
                terra::crop( sec)
      r <- raster::raster(ras)
      sec_sp <- as_Spatial(sec)
      SPDF <- as(r, "SpatialPointsDataFrame")
      SPDF[[1]] <-
        terra::extract(ras, 
                       terra::vect(SPDF), 
                       df = TRUE)[[2]] 
      SPDF <- setNames(SPDF, c("cover"))
      SPDF %>%
        st_as_sf()  %>%
        st_transform(st_crs(gp)) %>%
        filter(cover == 'Rangeland') %>%
          rowid_to_column("pixel") %>%
        st_intersection(sec, .) %>%
        as_tibble()  %>%
        mutate(long = unlist(map(.$geom,1)),
               lat = unlist(map(.$geom,2))) %>%
        select(-geom) 
            }
  stopCluster(cl)
  Sys.time() - begin 
}
