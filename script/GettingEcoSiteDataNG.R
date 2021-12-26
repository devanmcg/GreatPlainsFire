pacman::p_load(tidyverse, sf, foreach, doSNOW)

load('./gis/Robjects/gpng_allots.Rdata')

{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  AllotmentSites <-
    foreach(a=1:length(gpng_allots$allot_name), 
            .inorder=FALSE,
            .errorhandling = 'remove',
            .combine=bind_rows, 
            .packages=c('tidyverse', 'sf')) %dopar% {
              
    # Get just one feature from shp
    a_sf <- 
      gpng_allots %>% 
      filter(allot_name == allot_name[a]) 
    
    if (a_sf %>% st_geometry_type(.) %in% ('GEOMETRYCOLLECTION') ) {
      a_sf <- 
        a_sf %>%
        st_cast() %>% 
        filter(!
                 st_geometry_type(.)
               %in% c('LINESTRING') ) %>%
        mutate(area = st_area(.), 
               area = area * 0.0002471052, 
               area = as.numeric(area)) %>% 
        filter(area >= 50) %>%
        select(-area)
    } else {
      a_sf <-
        a_sf %>%
        mutate(area = st_area(.), 
               area = area * 0.0002471052, 
               area = as.numeric(area)) %>% 
        filter(area >= 50) %>%
        select(-area)
    }
    
    # Fetch map units for the feature  
    mukey_sf <-  
      a_sf %>%
      as_Spatial() %>%
      soilDB::SDA_spatialQuery(
        what = 'geom', 
        db = "SSURGO", 
        geomIntersection = TRUE) %>%
      st_as_sf()
    # Define bbox to query from shp
    aoi <- 
      a_sf %>%
      as_Spatial() %>%
      rgeos::writeWKT()
    # Create SQL query string for ecological sites
    es_q = paste0(
      "SELECT 
        component.mukey, component.cokey, component.comppct_r, component.compname, 
        coecoclass.ecoclassname 
       FROM component 
       INNER JOIN coecoclass ON component.cokey = coecoclass.cokey
       WHERE mukey IN (
        SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", aoi, "'))")
    # Query the Soil Data Mart
    unit_data <- 
      soilDB::SDA_query(es_q) %>% 
      as_tibble()  %>%
      select(-cokey) %>%
      group_by(mukey) %>%
      slice(which.max(comppct_r))  %>%
      ungroup() %>% 
      rename(site = ecoclassname) %>%
      full_join(mukey_sf, ., by = 'mukey') %>%
      filter(!is.na(site)) %>%
      mutate(unit = unique(a_sf$unit), 
             allot_name = unique(a_sf$allot_name) ) %>%
      select(unit,  allot_name, site, area_ac)
  }
  stopCluster(cl)
  Sys.time() - begin
  beepr::beep() 
}

# save(AllotmentSites, file = './data/soils/AllotmentSites.Rdata') 
