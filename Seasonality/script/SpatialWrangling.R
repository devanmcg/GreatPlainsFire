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
    padus_sf <- padus %>% st_as_sf() 
  # Resolve invalid geometries
    padus_sfv <- padus_sf %>% st_make_valid() 
  # Might as well save that sucker 
    padus_sfv %>%
      st_write("S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/PADUS4_0_GeoPackage.gpkg", 
               'PADUS4_0Combined_Proclamation_Marine_Fee_Designation_Easement') 
  # Crop to study area 
    padus_gp <- padus_sfv %>% 
                  select(Mang_Type, Mang_Name, Des_Tp, Loc_Ds, Unit_Nm ) %>%
                  st_intersection(gp)
    
    lands <- c("State Land Board Stewardship Trust Lands", "State Lands - General", "School Lands",
               "Other State Land", "State Natural Area", "State Park" , "State Parks" , 'NF',
               "Military Reserve", "Natural Area" , "State Wildlife Area" , "State Habitat Area", 
               "National Game Refuge", "Wildlife Habitat Management Area", "Wildlife Management Area",                                        
               "Wildlife/Recreation Management Area", "County Natural Area" , "Wildlife Habitat Area" ,
               "BLM_NM", "WPA",  "County Parks"  , "State Wildlife Areas", "State Land Board Public Access Program",                          
               "State Land Board", "State Watchable Wildlife Area", "National Land Trust Lands",
               "NWR", "WMA", 'WPA', "National Forest", "National Grassland", "National Monument", 
               "National Park", "National Preserve", "National Wildlife Refuge",
               "Bureau of Land Management CO", "Bureau of Land Management MT", 
               "Bureau of Land Management NM", "Bureau of Land Management WY" , 
               "State Land Board Public Access Program - Seasonal Access", "State Wildlife Management Area",                                  
               "State OHV Area" , "State Recreation Area", "State Preserve", "Wildlife Area", 
               "Department of Defense (DOD)" , "State Land Board Stewardship Trust Land",
               "US Fish and Wildlife Service - General", "Wildlife Habitat Protection Area",
               "Preserve" ,  "Game Refuge", "Wildlife Refuge", 'Audubon Society Preserve or Sanctuary', 
               'Recreation or Education')
    
    padus_gp %>%
      filter(Loc_Ds %in% lands)
    
    padus_gp %>%
      filter(Mang_Name %in% c('BLM', 'DOD', 'ARS', 'USBR', 'NPS') |
             Loc_Ds %in% lands) %>%
      st_write('./Seasonality/data/spatial/seasonality.gpkg', 'padus_gp', append = F)
    
    
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