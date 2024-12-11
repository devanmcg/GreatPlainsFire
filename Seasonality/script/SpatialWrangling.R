pacman::p_load(tidyverse, sf)

st_layers('./gis/GreatPlains.gpkg')
gp <- read_sf('./gis/GreatPlains.gpkg', 'l3_state_boundaries') 

padus %>%
  ggplot() + theme_void() +
  geom_sf() 

# BLM's SMA 
  st_layers("S:/DevanMcG/GIS/SpatialData/US/BLM/SMA_WM.gdb")
  sma_us <- read_sf("S:/DevanMcG/GIS/SpatialData/US/BLM/SMA_WM.gdb", 'SurfaceManagementAgency')

# USGS PADUS 4.0
  st_layers("S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/PADUS4_0_Geodatabase.gdb")
  padus <- read_sf("S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/PADUS4_0_Geodatabase.gdb", 
                   'PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement') %>%
    st_transform(st_crs(gp)) %>%
    st_intersection(gp) 


sma_us %>%
  ggplot() + theme_void() +
  geom_sf() 
sma_us %>%
  st_transform(st_crs(gp)) %>%
  
  st_intersection(gp) 


unique(sma_us$ADMIN_ST)

unique(sma_us$ADMIN_AGENCY_CODE)
