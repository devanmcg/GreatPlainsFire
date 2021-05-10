pacman::p_load(tidyverse, sf)
source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')

s_gis = 'S:/DevanMcG/GIS/SpatialData/US/USFS'

# Get a boundary for the Great Plains (EPA L3)
  GP_L3 <- 
    read_sf('S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3', 
            'us_eco_l3_state_boundaries') %>%
    filter(NA_L1NAME == 'GREAT PLAINS') %>%
    st_transform(4269)
  GP <- st_union(GP_L3)

# Filter national-level data down to the Great Plains

  gp_ng <- read_sf(paste0(s_gis, '/NationalGrasslands'), 
                    'S_USA.NationalGrassland') %>%
                st_intersection(GP)
  
  gp_perims <- read_sf(paste0(s_gis, '/FinalFirePerimeter'), 
                       'S_USA.FinalFirePerimeter') %>%
                  filter(FIREYEAR >= 1985) %>%
                  mutate(valid = st_is_valid(.)) %>%
                  filter(valid != FALSE) %>%
                  select(-valid) %>%
                  st_intersection(GP)
  
  gpng_perims <- gp_perims %>%
                    st_intersection(gp_ng %>% select(GRASSLANDN)) %>%
                    filter(TOTALACRES >= 247.12) %>% # > 100 ha
                    select(GRASSLANDN, FIRENAME, DISCOVERYD, 
                           SIZECLASS, STATCAUSE, COMMENTS, OWNERAGENC)
  
  gpng_perims3k <- gpng_perims %>%
                    st_transform(26915) %>%
                      st_buffer(3200) %>%
                    st_transform(4269) %>%
                    st_intersection(gp_ng)
  
  gpng_allots <- read_sf(paste0(s_gis, '/RangeAllotments'), 
                    'S_USA.Allotment')  %>%
                    st_intersection(gp_ng %>% select(GRASSLANDN)) %>%
                  select(GRASSLANDN, ALLOTMENT_:ALLOTMEN_2)

  gpng_perims3k_allots <- gpng_perims3k %>%
                            st_intersection(gpng_allots)
  # save(gpng_perims3k_allots, file = './gis/Robjects/gpng_perims3k_allots.Rdata')
  gpng_allots <- 
                gpng_allots %>%
                  mutate(InFireBuff = ifelse(ALLOTMENT1 %in% gpng_perims3k_allots$ALLOTMENT1, 
                                             'Y', 'N'))
  
  ggplot() + theme_map() + 
    geom_sf(data = gp_ng  %>%
              filter(GRASSLANDN == "Little Missouri National Grassland"),
            fill = "lightgrey", color = NA) + 
    geom_sf(data = gpng_perims3k %>%
                    filter(GRASSLANDN == "Little Missouri National Grassland"), 
            fill = "lightblue", color = NA) + 
    geom_sf(data = gpng_perims %>%
              filter(GRASSLANDN == "Little Missouri National Grassland"), 
            fill = "darkblue", color = "white") + 
    geom_sf(data = gpng_allots %>%
              filter(GRASSLANDN == "Little Missouri National Grassland", 
                     InFireBuff == 'Y'), 
            fill = NA, color = "black") +
   labs(caption = "Dark blue = fires (burned areas)
        Light blue = 2 mi buffers around fires (unburned areas)
        solid black lines = allotments that contain fire or buffer
        light grey fill = Little Missouri National Grasslands")
  
# Create dataset for grazing history query
  gpng_perims3k_allots %>%
    mutate(FirstGrazingYear = as.Date(DISCOVERYD, format = '%Y') - lubridate::years(1), 
           FifthGrazingYear = FirstGrazingYear + lubridate::years(6)) %>% 
    mutate(across(FirstGrazingYear:FifthGrazingYear, ~format(., '%Y'))) %>%
    select(GRASSLANDN, ALLOTMENT1, ALLOTMEN_1, 
           FirstGrazingYear, FifthGrazingYear, 
           FIRENAME, DISCOVERYD) %>%
    filter(! st_geometry_type(.) %in% c("LINESTRING","MULTILINESTRING")) %>%
    #as_tibble() %>% select(-geometry) %>% write_csv('./gis/USFS_ManagementBoundaries/NationalGrasslandFireBoundaries/AllotmentsNearBurns.csv')
     st_write('./gis/USFS_ManagementBoundaries/NationalGrasslandFireBoundaries/AllotmentsNearBurns/AllotmentsNearBurns.shp', 
              append = FALSE)


