pacman::p_load(tidyverse, sf, foreach, doParallel)
cores=detectCores()

{
cl <- makeCluster(cores[1]) 
registerDoParallel(cl)
begin = Sys.time()

AllotmentSites <-
  foreach(a=1:length(BurnAllotsGrazing$allot_name),
          .inorder = FALSE, 
          .errorhandling = 'stop',
          .combine=bind_rows) %:%
  foreach(f=1:length(BurnAllotsGrazing$fire), 
          .inorder=FALSE,
          .errorhandling = 'stop',
          .combine=bind_rows, 
          .packages=c('tidyverse', 'sf')) %dopar% {

# Get just one feature from shp
  a_sf <- 
    BurnAllotsGrazing %>% 
    filter(allot_name == allot_name[a], 
           fire == fire[f]) %>%
    slice(1) %>%
    select(-allot_id, -GrazeYear, -grazed, -BurnedAllotment)
   
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
         allot_name = unique(a_sf$allot_name), 
         InFire = unique(a_sf$InFire),
         fire = unique(a_sf$fire) ) %>%
  select(unit, fire, allot_name, InFire, site, area_ac)
          }
  beepr::beep() 
  stopCluster(cl)
  Sys.time() - begin
}



AllFireBuffs %>%
  st_write('./data/soils/GPNG_soils.shp', 
           append = FALSE)

AllFireBuffs <-
  read_sf('./data/soils', 
  'GPNG_soils')



