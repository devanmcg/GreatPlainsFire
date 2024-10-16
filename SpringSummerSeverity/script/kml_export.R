pacman::p_load(tidyverse, sf)

# Writing .kml files for Copernicus

# CGREC 
  read_sf('S:/DevanMcG/GIS/SpatialData/NorthDakota/raw/boundaries/AllPastureBoundaries', 
          'AllPastureBoundaries') %>%
    st_buffer(500) %>%
    st_union() %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(4326) %>%
    st_write('./data/AOI/CGREC.kml')
