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
