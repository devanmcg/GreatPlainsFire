
# study area 
  ngp <- read_sf('./data/SeverityComparison.gpkg', 'StudyRegion')
  ngp_counties <- tigris::counties(state = c('MT', "ND", 'SD'), cb = T)

# How many fires by state? 
  NGP <- read_sf('S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3/us_eco_l3.shp') %>%
    filter(NA_L3NAME %in% c("Northwestern Great Plains", 'Northwestern Glaciated Plains')) %>%
   st_intersection(  ngp_counties %>%
        group_by(STATE_NAME) %>%
        summarize(area = sum(ALAND)) %>%
        st_transform(st_crs(NGP))) 
  
  st_write(NGP, './data/MapStuff.gpkg', 'NGP_states')
  
  read_sf('./data/MapStuff.gpkg', 'NGP') %>%
    ggplot() + geom_sf() 
  cities <- tigris::urban_areas(filter_by= st_transform(NGP, st_crs(ngp_counties)), 
                                 'ftp', cb = T)
  
  
  AllFires <-  read_sf("S:/DevanMcG/GIS/SpatialData/US/NIFC", 
                       "InterAgencyFirePerimeterHistory_All_Years_View") %>%
    select(FIRE_YEAR, INCIDENT, FEATURE_CA) %>%
    st_transform(st_crs(NGP)) %>%
    st_centroid() %>%
    # st_make_valid(.) %>%
    st_intersection(NGP) %>%
    rename(FireYear = FIRE_YEAR, FireName = INCIDENT, FireType = FEATURE_CA ) 
  
  AllFires %>%
  mutate(FireType = case_when(
    str_sub(FireType, 1,4)=='Wild' ~ 'Wildfire', 
    TRUE ~ 'RxFire' )) %>%
    filter(FireType == 'Wildfire', 
           between(as.numeric(FireYear), 2017, 2024), 
           STATE_NAME != "Wyoming") %>%
    # st_write('./data/MapStuff.gpkg', 'NGP_fires')
    as_tibble() %>%
    group_by(STATE_NAME,  NA_L3NAME) %>%
    summarize(fires = n(), 
              .groups = 'drop') %>%
    pivot_wider(names_from =  NA_L3NAME, 
                values_from = fires) %>%
    mutate(`Total fires` = `Northwestern Glaciated Plains` + `Northwestern Great Plains`) %>%
    arrange(desc(`Total fires`)) %>%
    save(file = './data/FireCountNGP.Rdata')
  
  
  
# Chugged precipitation data
  load( './data/ppt_dat.Rdata')

region_perims2 <-
  read_sf("S:/DevanMcG/GIS/SpatialData/US/NIFC", 
          "InterAgencyFirePerimeterHistory_All_Years_View") %>%
  select(FIRE_YEAR, INCIDENT, FEATURE_CA) %>%
  st_transform(st_crs(ngp)) %>%
  st_make_valid(.) %>%
  st_intersection(ngp) %>%
  rename(FireYear = FIRE_YEAR, FireName = INCIDENT, FireType = FEATURE_CA )

region_perims <- 
  region_perims2  %>%
  mutate(FireType = case_when(
    str_sub(FireType, 1,4)=='Wild' ~ 'Wildfire', 
    TRUE ~ 'RxFire' )) %>%
  filter(FireType == 'Wildfire', 
         between(as.numeric(FireYear), 2017, 2024), 
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
            .groups ='drop')  %>%
  group_by(FireCode) %>%
  arrange(desc(AreaHa)) %>%
  slice(1) %>% # in case fire crosses ecoregion/state lines 
  ungroup() 

region_perims %>%
  as_tibble() %>%
  group_by(state) %>%
  summarize(Fires = n())

# Identify wildfires within the precipitation anomaly
  win_fires <-
    region_perims %>%
      st_centroid() %>%
      st_intersection(ppt_dat$anomaly) %>%
      filter(anom_cat == 'within', 
             AreaHa > 10) %>%
    as_tibble() %>%
    select(FireCode, L3, state, anomaly)
  
  precip_perims <-
    region_perims %>%
    filter(FireCode %in% win_fires$FireCode)

# Get rangeland classifications 
rr_tr <- terra::rast("S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/USrangelands.tif") %>%
  terra::crop(region_perims %>% st_transform(5070))
# Find the proportion rangeland for each fire
range_fires <- tibble() 
for(i in 1:length(unique(precip_perims$FireCode))){      
  fire = unique(precip_perims$FireCode)[i]
  precip_perims %>%
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
  rename(PropRange = prop) 


# coteau fires (NW Glaciated Plains)
  precip_perims %>% 
    right_join(by = 'FireCode', 
               range_fires) %>% 
    filter(L3 == 'Northwestern Glaciated Plains') %>%
    arrange(desc(PropRange)) %>%
    filter(PropRange > 0.2)
  
# NW Great Plains 
  gp_perims <- 
  precip_perims %>% 
    right_join(by = 'FireCode', 
               range_fires) %>% 
    #st_intersection( ngp_counties %>% select(STUSPS, NAME)) %>%
    merge(by ='FireCode', 
          win_fires %>% select(FireCode, anomaly)) %>% 
    filter(L3 == 'Northwestern Great Plains' &
           RangeHa > 10) %>%
    arrange((abs(anomaly)), desc(PropRange)) %>%
    slice(1:20) %>% 
    
    precip_perims %>% 
    filter(FireName == 'Dorothy Draw')
  
  gp_perims %>%
    st_transform(4326) %>%
    st_write('./data/SeverityComparison.gpkg', 'GreatPlainsWF', append = FALSE)

  for(i in 1:length(unique(gp_perims$FireCode))) {
    wd = "S:/DevanMcG/FireScience/Sentinel/SeverityComparison/AOIs"
    fire = unique(gp_perims$FireCode)[i] 
    gp_perims %>%
      filter(FireCode == fire) %>%
      st_transform(4326) %>%
      st_bbox() %>%
      st_as_sfc() %>% 
      st_write(., paste0(wd, '/', fire, '.kml'), append = FALSE )}
  
  gp_perims %>% 
    filter(FireCode == 'Coyote_2017') %>%
    ggplot() + theme_bw() +
    geom_sf() 
  
  
  ppt_grd %>%
    st_intersection(ngp, .) %>%
    filter(st_geometry_type(., by_geometry = TRUE) %in% c('MULTIPOLYGON', 'POLYGON'))  %>%
    mutate(anomaly = case_when(
      L3 == "Northwestern Glaciated Plains" ~ ppt - 447,  
      L3 == "Northwestern Great Plains" ~ ppt - 431 ), 
      anom_cat = case_when(
        L3 == "Northwestern Glaciated Plains" &
          abs(anomaly) > 12 * SDs ~ 'beyond',
        L3 == "Northwestern Glaciated Plains" & 
          abs(anomaly) < 12 * SDs ~ 'within',
        L3 == "Northwestern Great Plains" & 
          abs(anomaly) > 19 * SDs ~ 'beyond',
        L3 == "Northwestern Great Plains" & 
          abs(anomaly) < 19 * SDs ~ 'within') ) %>%
    ggplot() + theme_void() +
    geom_sf(aes(fill = anomaly), color = 'NA') +
    geom_sf_pattern(data = anom_cat %>% filter(anom_cat == 'beyond'), 
                    aes(pattern = anom_cat), 
                    pattern_color = NA,
                    pattern_fill = "grey80",
                    pattern_angle = 45,
                    pattern_alpha = 0.5,
                    pattern_density = 0.5,
                    pattern_spacing = 0.025,
                    pattern_key_scale_factor = 1) +
    geom_sf(data = ngp_counties, fill = NA, color = 'grey50') + 
    geom_sf(data = ngp, fill = NA, color = 'black') + 
    scale_fill_gradient2("Precipitation anomaly (mm)\n", 
                         low = wes_palette("Zissou1")[4],
                         mid = 'white',
                         high = wes_palette("Zissou1")[1], 
                         na.value = "grey70") +
    scale_pattern_manual(values = c(beyond = "circle", within = "none")) + 
    guides(pattern = guide_legend(override.aes = list(fill = "white"))) 
