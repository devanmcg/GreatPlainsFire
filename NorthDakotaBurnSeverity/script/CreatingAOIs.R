pacman::p_load(tidyverse, sf)

##
## Prescribed burns 
##

# CGREC
  # Load pasture polygons
    cgrec_gpkg = '../SpringSummerSeverity/gis/boundaries/CGREC_PBG_26914.gpkg'
    pastures <- st_read(cgrec_gpkg, 'Pastures')  
  # Set CGREC AOI 
    pastures %>%
    st_union() %>%
    st_buffer(10000) %>%   # added to ensure capturing PIFs
    
    st_bbox() %>%
    st_as_sfc() %>%
    st_write('S:/DevanMcG/FireScience/Sentinel/SeverityComparison/AOIsBuffered/CGREC.kml', append = FALSE )

# Dayton 
    dayton <- read_sf('./data/ThermocouplePlacements.gpkg', 'DaytonTrees')
    dayton %>% 
      st_buffer(10000) %>% 
      st_bbox() %>%
      st_as_sfc() %>% 
      st_transform(4326) %>%
      st_write('S:/DevanMcG/FireScience/Sentinel/SeverityComparison/AOIsBuffered/Dayton.kml', append = FALSE )
    
# HREC 
    hrec <- read_sf('./data/ThermocouplePlacements.gpkg', 'HREC') 
    hrec %>% 
      st_buffer(8000) %>% 
      st_bbox() %>%
      st_as_sfc() %>% 
      st_transform(4326) %>%
      st_write('S:/DevanMcG/FireScience/Sentinel/SeverityComparison/AOIsBuffered/HREC.kml', append = FALSE )

##
## Wildfires 
##

# Load wf perimeters after hand-editing in QGIS
  wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF')
  
# Loop through perimeters and create buffered AOIs for Copernicus
  for(i in 1:length(unique(wf_sf$FireCode))) {
    wd = "S:/DevanMcG/FireScience/Sentinel/SeverityComparison/AOIsBuffered"
    fire = unique(wf_sf$FireCode)[i] 
    wf_sf %>%
      filter(FireCode == fire) %>% 
      st_make_valid() %>% 
      st_buffer(10000) %>%   # added to ensure capturing PIFs
      st_transform(4326) %>%
      st_bbox() %>%
      st_as_sfc() %>% 
      st_write(., paste0(wd, '/', fire, '.kml'), append = FALSE )}
  
# For the PIF corrections, re-doing previous downloads. 
  # Get start and end dates from first round of downloads: 
    precip_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison/CompWildfires/precip'
    
    precip_scenes <- list.files(precip_dir, '*.tiff')
  
  # Create table of Before/After dates for images
    image_dates <- 
      precip_scenes %>%
        as_tibble() %>%
        rename(file = value) %>%
        mutate(file = str_remove(file, '.tiff')) %>%
        separate(file, into = c('FireCode', 'Period', 'Date'), sep = '_') %>%
        mutate(Period = recode_values(Period, 'A' ~ 'After', 'B' ~ 'Before' ), 
               Period = factor(Period, levels = c('Before', 'After'))) %>%
        pivot_wider(names_from = Period, 
                    values_from = Date, 
                    names_sort = TRUE) 
  
  # Compare to perimeters used for AOIs
    tibble(A= wf_sf$FireCode %>% sort(), 
           B = image_dates$FireCode %>% sort() ) %>% View() 
  
  # write table
    image_dates %>%
      write_csv("S:/DevanMcG/FireScience/Sentinel/SeverityComparison/ImageDatesPrecip.csv")
    