pacman::p_load(tidyverse, sf)

st_layers('./Seasonality/data/spatial/seasonality.gpkg')
gp_l3 <- read_sf('./Seasonality/data/spatial/seasonality.gpkg', 'l3_gp_state_boundaries') 
gp <- read_sf('./Seasonality/data/spatial/seasonality.gpkg', 'StudyRegion') 
gp %>%
  ggplot() + theme_void() +
  geom_sf()

# BLM's SMA 
  # Load as SpatVector
    sma_gp <-     
      terra::vect('S:/DevanMcG/GIS/SpatialData/US/BLM/SMA_WM.gdb', 
                  'SurfaceManagementAgency') 
  # Convert to sf
    sma_sf <- sma_gp %>% st_as_sf() 
  # Create gp object in SMA CRS
    gp_3857 <- gp %>% 
      st_transform(st_crs(sma_sf)) 
  # crop SMA to GP 
    sma_sf %<>% select(ADMIN_ST, ADMIN_AGENCY_CODE) %>% st_intersection(gp_3857)
    
    sma_st <- sma_sf %>%
      filter(ADMIN_AGENCY_CODE == "ST") %>%
      st_transform(st_crs(gp))
    
    sma_st %>%
      ggplot() + theme_void() +
      geom_sf() +
      geom_sf(data = gp, fill = NA, color = 'blue')
  
  sma_ac <- 
    sma_st  %>%
      mutate(acres = st_area(.)*0.0002471054, 
             acres = as.numeric(acres)) %>%
      select(ADMIN_ST, acres)

# USGS PADUS 4.0
  st_layers("S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/PADUS4_0_Geodatabase.gdb")
  padus <- terra::vect("S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/PADUS4_0_Geodatabase.gdb", 
                   'PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement') 
  # Convert to sf
    pad_sf <- padus %>% 
                st_as_sf() %>% 
                st_transform(gp)
  # crop SMA to GP 
    pad_sf %<>% select(ADMIN_ST, ADMIN_AGENCY_CODE) %>% st_intersection(gp)
  %>%
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


-12308929.975 3749115.2695000023.