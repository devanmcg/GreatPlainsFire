pacman::p_load(tidyverse, sf)

st_layers('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg')
gp_l3 <- read_sf('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg', 'l3_gp_state_boundaries') 
gp <- read_sf('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg', 'StudyRegion') 

# Create the state lands layer 
  state_land <- read_sf('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg', 'state_lands') 

  state_land <- 
    state_land %>%
    bind_rows(
    padus_sf %>%
      filter(Loc_Own == 'State of Wyoming') )  
    
  state_polygons <-
    state_land %>%
      st_make_valid() %>%
      st_intersection(gp_states) %>%
      select(ecoregion, state) %>%
    st_cast('MULTIPOLYGON') %>%
    st_cast('POLYGON')
  
 state_polygons  %>%
   st_write('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg', 'gp_state_polygons')
 
 state_sections <-
   state_polygons %>%
   mutate(acres = st_area(.)*0.0002471054, 
          acres = as.numeric(acres) ) %>%
     filter(between(acres, 500, 700)) 
 
 state_sections %>%
   st_write('S:/DevanMcG/Projects/FuelWeatherSeasonality/seasonality.gpkg', 'gp_state_sections')
  
 ggplot() + theme_void() +
      geom_sf(fill = 'gray') +
      geom_sf(data = gp_states, color = 'blue', fill = NA)
 
 
 # Rangeland 
   reeves_rast <- terra::rast('S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/InteriorWestRangelands.tif')
    gp_sp <- state_polygons %>%
               st_union() %>%
               st_transform(5070) %>%
               as_Spatial()
   gp_reeves_rast <-
     reeves_rast %>%
       terra::crop(gp_sp ) 
   
   terra::plot(gp_reeves_rast)
   
   terra::writeRaster(gp_reeves_rast, 'S:/DevanMcG/Projects/FuelWeatherSeasonality/StudyRegionReeves.tif')
   
   gp_reeves_r <- raster::raster(gp_reeves_rast)
   
   reeves_SPDF <- as(gp_reeves_r, "SpatialPointsDataFrame")
   reeves_SPDF %>%
     st_as_sf() %>%
     st_intersection(
       st_transform(
         state_sections, raster::crs(c_wui_SPDF)), 
       .) 

    
