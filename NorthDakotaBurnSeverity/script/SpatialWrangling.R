pacman::p_load(tidyverse, sf)

load('../albersEAC.Rdata')

# Rx burn locations for mapping 
  gpkg = './data/OriginalRxBurnPerims.gpkg'

  purrr::map(1:length(st_layers(gpkg)$name),
             function(i) {
               lyr = st_layers(gpkg)$name[i]
               read_sf(gpkg, lyr) %>%
                 st_transform(albersEAC) %>%
                 st_union() %>%
                 st_centroid() %>%
                 st_as_sf() %>%
                 mutate(location = lyr) 
             } ) %>%
    do.call(rbind, .) %>%
    st_write(gpkg, 'RxLocations')
    
 # put some map stuff together
  ngp <- read_sf('../data/SeverityComparison.gpkg', 'StudyRegion')
  ngp %>% 
    st_write('./data/MapStuff.gpkg', 'ngp')
    
  ngp_counties <- tigris::counties(state = c('MT', "ND", 'SD'), cb = T)  
  
  ngp_counties %>%
    st_transform(st_crs(ngp)) %>% 
    st_intersection(ngp %>% 
                      st_union()) %>% 
    st_write('./data/MapStuff.gpkg', 'ngp_counties')
  load("C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data/gp.Rdata")
   load("C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data/usa.Rdata")

   st_write(usa, './data/MapStuff.gpkg', 'usa')
   st_write(gp, './data/MapStuff.gpkg', 'gp')
# Fetching original thermocouple sampling locations
  # Hettinger
    h_dr = 'D:/NDSU/GoogleDrive/QGIS/HREC/sampling/FireBehavior'
    read_sf(h_dr, 'FireTowerPlacements') %>%
      st_transform(albersEAC) %>%
      select(PastureLoc, PastureRep, Livestock, Patch, SubPlot, FireTower) %>%
      st_write('./data/ThermocouplePlacements.gpkg', 'HREC', append = FALSE)
  # Dayton Tract
    read_sf('S:/DevanMcG/Projects/SoilHeating/SpatialData/DaytonSoilSamplingPoints', 
             'Dayton_soil', crs = 26913) %>%
      filter(Burned == 1) %>%
      select(-ID, -COMMENT)  %>%
      st_transform(albersEAC)  %>%
      st_write('./data/ThermocouplePlacements.gpkg', 'DaytonSamplePoints', append = FALSE)
  # CGREC 
    c_patches <- read_sf('D:/NDSU/GoogleDrive/QGIS/CGREC/boundaries/AllSubPatches', 'AllSubPatches') %>%
                  filter(! Unit %in% c("Wagon Wheel","Refuge"))
    read_sf("D:/NDSU/GoogleDrive/QGIS/CGREC/triangles", "FireTowerPoints") %>%
      st_transform(st_crs(c_patches)) %>%
      st_intersection(c_patches %>% select(Unit:Subpatch))  %>%
      st_transform(albersEAC) %>%
      st_write('./data/ThermocouplePlacements.gpkg', 'CGREC', append = FALSE)

    st_layers('./data/ThermocouplePlacements.gpkg')
    
# Pasture boundaries for Copernicus
  # Southwest ND (Hettinger and Dayton)
   all_sw <-  read_sf("D:/NDSU/GoogleDrive/QGIS/HREC/boundaries/AllPastures2019", 
                      "AllPastureBoundaries") %>%
              st_transform(4326)
   all_sw %>%
     filter(location == 'fitch') %>%
     st_write('./data/fitch.kml', append = FALSE)
   all_sw %>%
     filter(location == 'clement') %>%
     st_write('./data/clement.kml', append = FALSE)
   all_sw %>%
     filter(classifica == 'Cooperator') %>%
     st_write('./data/dayton.kml', append = FALSE)
   
# Connect sensor placements with fire events
   # CGREC
     plots_cgrec <- 
       read_sf('./data/ThermocouplePlacements.gpkg', 'CGREC') %>%
         mutate(loc = 'CG', 
                Unit = recode(Unit, 'Barker' = 'Bar')) %>%
         unite("PlotCode", c(loc,Unit,Pasture,Patch,Subpatch2,SubPlot), sep=".") %>%
         mutate(PlotCode = str_to_upper(PlotCode) ) %>%
        select(PlotCode) %>%
         add_column(location = 'CGREC', .before = 1) %>%
         st_buffer(10)  
  # HREC
    plots_hrec <- 
      read_sf('./data/ThermocouplePlacements.gpkg', 'HREC') %>%
       mutate(loc = 'H', 
              P = str_sub(PastureLoc,1,3)) %>%
       unite("PlotCode", c(loc,P,NewPasture,Patch,SubPlot), sep=".") %>%
        mutate(PlotCode = str_to_upper(PlotCode) ) %>%
        group_by(PlotCode) %>%
        summarize(geometry = st_union(geom)) %>% 
        st_centroid() %>%
        add_column(location = 'HREC', .before = 1) %>%
       st_buffer(10) 
  # Dayton
    plots_dayton_1 <-
    read_sf('./data/ThermocouplePlacements.gpkg', 'DaytonTrees') %>%
      rename(plot = SubPlot) %>%
      group_by(plot) %>%
      summarize(geometry = st_union(geom)) %>% 
      st_centroid()  %>%
      add_column(FireCode = 'DAY.1.19', .before = 1) %>%
      st_buffer(10) 
      
    read_sf('./data/ThermocouplePlacements.gpkg', 'DaytonSamplePoints')
    
# Boundaries for severity comparison 
  # Build GeoPackage with original data on fire perimeters to tweak & combine in QGIS
    # Dayton from Initial attempt from MT Rx fire council meeting (+ Highway 14 fire)
      read_sf('C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data/SeverityComparison.gpkg', 
             'perimeters') %>%
       filter(FireType == 'Rx_NoGraze') %>%
       st_cast('POLYGON') %>%
      mutate(incident = str_remove_all(incident, '[0-9]')) %>%
      rename(location = incident) %>%
      st_write('./data/OriginalRxBurnPerims.gpkg', 'dayton')
   
    BurnDates <- read_csv('./data/BurnDates.csv')
    AllBurnData <- read_sf('./data/ThermocouplePlacements.gpkg', 'AllBurnData') 
    # CGREC burn perimeters (pretty complete)
      read_sf('../SpringSummerSeverity/gis/boundaries/CGREC_PBG_26914.gpkg', 
              'FirePerimeters') %>%
        filter(Season == 'Spring') %>%
        select(Year, PreBurn:BurnDate) %>%
        rename(FireYear = Year) %>%
        add_column(location = 'CGREC', .before = 'FireYear') %>%
        add_column(FireType = 'Rx_Graze', .after = 'FireYear') %>%
      st_write('./data/OriginalRxBurnPerims.gpkg', 'cgrec')
    # HREC 
      read_sf('C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data/SeverityComparison.gpkg', 
              'perimeters') %>%
        filter(FireType == 'Rx_Graze') %>%
        st_cast('POLYGON') %>%
        mutate(incident = str_remove_all(incident, '[0-9]')) %>%
        rename(location = incident) %>%
        st_write('./data/OriginalRxBurnPerims.gpkg', 'hrec')
    # Dayton Tract 
      read_sf('S:/DevanMcG/Projects/SoilHeating/SpatialData/DaytonSoilSamplingPoints', 
              'Dayton_soil', crs = 26913) 
      
      AllBurnData %>%
        filter(location == 'Dayton', ! is.na(SoilMaxC)) 
  # Escapes 
      read_sf('C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data/SeverityComparison.gpkg', 
              'perimeters') %>%
        filter(FireType == 'Escape') %>%
        st_transform(4326) %>%
        mutate(L3 = case_when(
          incident == 'Lemonade' ~ 'Northwestern Great Plains', 
          incident == 'Highway14' ~ 'Northwestern Glaciated Plains'
        )) %>% 
        st_write('./data/SeverityComparison.gpkg', 'EscapePerimeters')
        
# Getting wildfire perimeters 
  # Define a study region (zoom in a bit on study areas )
    load('../albersEAC.Rdata')
    region_box <- 
      tibble(feature = 'region', 
             Easting = c(-900000, -215000, -215000, -900000), 
             Northing = c(2470000, 2950000, 2470000, 2950000) ) %>%
      st_as_sf(coords = c("Easting", "Northing"), crs = 'albersEAC') %>%
      group_by(feature) %>%
      summarize(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON") %>%
      st_bbox() %>%
      st_as_sfc(st_bbox(.))
  # Get EPA L3 for study region
    l3 <- 
    read_sf("S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3", 
            "us_eco_l3_state_boundaries") %>%
      filter(NA_L3NAME %in% c('Northwestern Glaciated Plains', 
                              'Northwestern Great Plains')) %>%
      select(NA_L3NAME, STATE_NAME) %>%
      rename(L3 = NA_L3NAME, state = STATE_NAME) %>%
     st_crop(region_box)
  # Get fire perimeters for the study region by L3
    # From NIFC
    region_perims2 <-
      read_sf("S:/DevanMcG/GIS/SpatialData/US/NIFC", 
              "InterAgencyFirePerimeterHistory_All_Years_View") %>%
        select(FIRE_YEAR, INCIDENT, FEATURE_CA) %>%
        st_transform(st_crs(l3)) %>%
        st_make_valid(.) %>%
        st_intersection(l3) %>%
        rename(FireYear = FIRE_YEAR, FireName = INCIDENT, FireType = FEATURE_CA)
    
    region_perims <- 
     region_perims2 %>%
        filter(between(as.numeric(FireYear), 2017,9998 ), 
               state != "Wyoming") %>%
        mutate(FireName = case_when(
          FireName == 'nd-crr-fy22-wf-knutsen' ~ 'Knutsen', 
          TRUE ~ FireName )) %>%
        mutate(FireCode = str_remove_all(FireName, ' Fire'), 
               FireCode = gsub(pattern     = "[^a-z,A-Z,0-9]", 
                               replacement = "", 
                               FireCode), 
               FireCode = paste0(FireCode, '_', FireYear), 
               Ha = st_area(.), 
               Ha = as.numeric(Ha) * 0.0001) %>%
        group_by(FireCode, FireYear, FireName, FireType, L3, state) %>%
        summarize(AreaHa = sum(Ha), 
                  .groups ='drop') %>%
        mutate(FireType = case_when(
          str_sub(FireType, 1,4)=='Wild' ~ 'Wildfire', 
          TRUE ~ 'RxFire' ))  %>%
        group_by(FireCode) %>%
        arrange(desc(AreaHa)) %>%
        slice(1) %>% # in case fire crosses ecoregion/state lines 
        ungroup() 
        
 
  # Get federal land for the study region 
    PADUS <- read_sf("S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/PADUS4_0_GeoPackage.gpkg")%>% 
              st_crop(region_box) %>%
              select(Mang_Type, Mang_Name)
    
   fed_fires  <-
    region_perims %>% 
      st_intersection(PADUS %>% 
                        select(Mang_Type) %>%
                        mutate(Mang_Type = ifelse(Mang_Type == 'FED', 'Fed', 'NotFed'))) %>%
      mutate(Ha = st_area(.), 
             Ha = as.numeric(Ha) * 0.0001) %>% 
      group_by(FireCode, FireType, L3, state, AreaHa, Mang_Type) %>%
      summarize(Ha = sum(Ha), 
                .groups ='drop') %>%
      as_tibble() %>%
      pivot_wider(names_from = Mang_Type, 
                  values_from = AreaHa, 
                  values_fill = 0, 
                  id_cols = -geometry)  %>%
     filter(Fed > 0) 

  # Get rangeland classifications 
    rr_tr <- terra::rast("S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/USrangelands.tif") %>%
                terra::crop(region_perims %>% st_transform(5070))
    # Find the proportion rangeland for each fire
      range_fires <- tibble() 
      for(i in 1:length(unique(region_perims$FireCode))){      
        fire = unique(region_perims$FireCode)[i]
        region_perims %>%
          filter(FireCode == fire )%>% 
          st_transform(st_crs(rr_tr)) %>%
          terra::extract(rr_tr, .) %>%
          group_by(LABEL) %>%
          summarize(pixels = n() ) %>%
          mutate(prop = pixels / sum(pixels)) %>%
          add_column(FireCode = fire, .before = 1) %>%
          filter(LABEL == 'Rangeland') %>%
          bind_rows(range_fires) -> range_fires }
      
      range_fires %<>%
        mutate(RangeHa = (pixels * 900)* 0.0001) %>%
        select(-LABEL, -pixels)  %>%
        left_join(by = 'FireCode', 
                   fed_fires %>% select(FireCode, Ha:NotFed)) %>%
        rename(PropRange = prop, TotalHa = Ha, FedHa = Fed, NotFedHa = NotFed)

    # Find the highest-proportion rangeland wildifre perimeters for each ecoregion
      EarlyPerims <-
      read_sf("C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data/SeverityComparison.gpkg", 
              'perimeters')
      reg_buff <- 
        EarlyPerims %>%
        filter(incident != 'Highway14') %>%
        st_transform(st_crs(region_perims)) %>%
        st_buffer(20000) %>%
        st_union() 
      
      comp_wf <- 
        bind_rows( 
        # coteau fires (NW Glaciated Plains)
        region_perims %>% 
          ungroup() %>%
            right_join(by = 'FireCode', 
                    range_fires) %>% 
          filter(L3 == 'Northwestern Glaciated Plains', 
                 FireType == 'Wildfire') %>%
           arrange(desc(PropRange)) %>%
          filter(TotalHa >= 10, 
                 FedHa > 10,
                 PropRange > 0.4) %>%
          arrange(desc(state), desc(FedHa)) %>% 
          slice(1:15) , 
        # Plains fires (NW Great Plains) 
        region_perims %>%
          right_join(by = 'FireCode', 
                     range_fires) %>%  
            st_crop( reg_buff )  %>% 
             filter(FedHa > 30,
                    PropRange > 0.6,
                   FireType == 'Wildfire', 
                   ! FireName %in% c('Paddy Fay Creek')) %>% 
          arrange(desc(state), desc(FedHa)) %>%
            slice(1:15)  ) 
      
   
      
      comp_wf %>%
        st_transform(4326) %>%
        st_write('./data/SeverityComparison.gpkg', 'CompPerimeters4326', append = FALSE)
      
  # Add in other perimeters from before
      st_layers('./data/SeverityComparison.gpkg')
      
      comp_wf <- 
        read_sf('./data/SeverityComparison.gpkg', 'CompPerimeters4326')
      
      read_sf('./data/SeverityComparison.gpkg', 'ComparisonPerimeters') %>%
        filter(FireName %in% c('Dorothy Draw', 'Rest Area')) %>%
        st_transform(4326) %>%
        bind_rows(comp_wf) %>%
        st_write('./data/SeverityComparison.gpkg', 'CompPerimeters4326', append = FALSE)
      
      
      
      comp_wf %>%
        filter(FireCode == 'Windy_2021') %>%
        ggplot() + theme_bw() +
        geom_sf()
      
    # Create AOIs for each of these fires for input into Copernicus 
      
      for(i in 1:length(unique(comp_wf$FireCode))) {
        wd = "S:/DevanMcG/FireScience/Sentinel/SeverityComparison/AOIs"
        fire = unique(comp_wf$FireCode)[i] 
        comp_wf %>%
          filter(FireCode == fire) %>%
           st_bbox() %>%
          st_as_sfc() %>% 
          st_write(., paste0(wd, '/', fire, '.kml'), append = FALSE )}
    
        # AOIs for escapes
          esc_sf <- read_sf('./data/SeverityComparison.gpkg', 'EscapePerimeters')
          for(i in 1:length(unique(esc_sf$incident))) {
            wd = "S:/DevanMcG/FireScience/Sentinel/SeverityComparison/AOIs"
            fire = unique(esc_sf$incident)[i] 
            esc_sf %>%
              filter(incident == fire) %>%
              st_bbox() %>%
              st_as_sfc() %>% 
              st_write(., paste0(wd, '/', fire, '.kml'), append = FALSE )}

      l3 %>%
        st_write('./data/SeverityComparison.gpkg', 'StudyRegion')
      
      reg_wf %>% filter(FireCode == '744_2021') %>% 
        st_cast('POLYGON') %>% 
        # st_centroid() %>%
        # st_buffer(500) %>%
      # rowid_to_column() %>%  ggplot() + geom_sf() + facet_wrap(~rowid)
        slice(7) %>% 
        st_write(., paste0(wd, '/SagebrushDraw_2017.kml'), append = FALSE)
 
    

        ggplot() + theme_void() + 
        geom_sf(data = l3 %>% st_crop(region_box)) +
        geom_sf(data = region_box, fill = NA, color = 'darkred') +
        geom_sf() +
        geom_sf(data = perims %>%
                  st_transform(st_crs(region_wf)) %>%
                  filter(incident != 'Highway14') %>%
                  st_cast('POLYGON') %>%
                  #st_cast('MULTIPOLYGON') %>%
                  st_union()  %>%
                  st_buffer(40000), 
                fill = NA )
      
     


    
  
  
    
   
    