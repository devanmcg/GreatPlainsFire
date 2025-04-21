pacman::p_load(tidyverse, magrittr, sf)

gp <- read_sf('./gis/GreatPlains.gpkg', 'outline')

ng <- read_sf("S:/DevanMcG/GIS/SpatialData/US/USFS/NationalGrasslands/S_USA.NationalGrassland.shp") %>% 
        st_transform(st_crs(gp)) %>% 
        st_intersection(gp)
allots <- read_sf('S:/DevanMcG/GIS/SpatialData/US/USFS/RangeAllotments/S_USA.Allotment.shp') %>% 
        st_transform(st_crs(gp)) %>%
        select(ALLOTMENT_, ALLOTMENT1, ALLOTMEN_1) %>%
        st_intersection(gp)

ng %>%
  mutate(area = st_area(.), 
         ha = as.numeric(area) * 0.0001) %>%
  summarize(TotalArea = sum(ha) ) 

ng %>%
  filter(GRASSLANDN  %in% c('Cedar River National Grassland', 'Grand River National Grassland', 
                            'Sheyenne National Grassland', 'Little Missouri National Grassland')) %>%
  st_intersection(allots)
  mutate(area = st_area(.), 
         ha = as.numeric(area) * 0.0001) %>%
  summarize(TotalArea = sum(ha) ) 
  


