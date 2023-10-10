pacman::p_load(tidyverse, sf)

fp = './gis/boundaries'

# WGS bbox for EarthExplorer
  read_sf('S:/DevanMcG/GIS/SpatialData/NorthDakota/raw/boundaries/AllPastureBoundaries', 
                           'AllPastureBoundaries') %>%
    st_union() %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(4326) %>%
    write_sf(paste0(fp, '/CGREC_bbox_4326.shp'), 
             append = FALSE)
# Get CGREC pastures and patches
  read_sf('S:/DevanMcG/GIS/SpatialData/PBGunits/CGREC/patches', 'patches') %>%
    filter(Unit %in% c('Bob', 'Barker')) %>%
    write_sf(paste0(fp, '/CGREC_pbg_26913.shp'), 
             append = FALSE)
  
  # Fire mapping 

  cgrec_gpkg = './gis/boundaries/CGREC_PBG_26914.gpkg'
  st_layers(cgrec_gpkg ) 
  
  pastures <- st_read(cgrec_gpkg, 'Pastures') 
  patches <- st_read(cgrec_gpkg, 'PasturePatches') 

  fires <- st_read(cgrec_gpkg, 'FirePerimeters') %>%
              mutate(Year = as.factor(Year))

    ggplot() + theme_void() +
      geom_sf(data = filter(fires, unit == 'Barker'), 
      aes(fill = Year, 
          alpha = Season)) +
      geom_sf(data = filter(patches, Unit == 'Barker'), 
              fill = NA, 
              color = 'black', 
              lty = 2, 
              size = 1) + 
      geom_sf(data = filter(pastures, Unit == 'Barker'), 
              fill = NA, 
              color = 'black', 
              lty = 1, 
              size = 2) +
      geom_sf_text(data = filter(fires, unit == 'Barker'), 
                   aes(label = Year), 
                   nudge_y = 50) +
      geom_sf_text(data = filter(fires, unit == 'Barker'), 
                   aes(label = substr(Season, 1, 3)), 
                   nudge_y = -50) +
      scale_alpha_manual(values = c(0.25, 0.8)) + 
      theme(legend.position = 'none')
