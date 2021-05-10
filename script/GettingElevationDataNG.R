pacman::p_load(tidyverse, sf)
load('./albersEAC.Rdata')

load("C:/Users/devan.mcgranahan/GithubProjects/GreatPlainsFire/gis/Robjects/gpng_perims3k_allots.Rdata")

# NPS shapefiles 

gpng_sf <- gpng_perims3k_allots %>%
                st_transform(albersEAC)

pacman::p_load(foreach, doParallel)
cores=detectCores()

{
  cl <- makeCluster(cores[1]) 
  registerDoParallel(cl)
  begin = Sys.time()
  
    foreach(p=1:length(gp_nps_sf$unit), 
            .combine=bind_rows) %dopar% {
      pacman::p_load(tidyverse, sf)
      p_sf <-  
        gp_nps_sf %>%
        slice(p) 
      p_sf %>%
        as_Spatial()  %>%
        elevatr::get_elev_raster(z = 14, 
                                 clip = 'bbox', 
                                 verbose = F, 
                                 override_size_check = T) %>%
        raster::writeRaster(paste0('S:/DevanMcG/GIS/SpatialData/GreatPlains/DEM/', 
                                   str_replace_all(p_sf$unit, " ", "_"), '.tif'))
            }
    beepr::beep() 
    stopCluster(cl)
    Sys.time() - begin
}   


