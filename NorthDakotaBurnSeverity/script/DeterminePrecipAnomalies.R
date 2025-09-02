pacman::p_load(tidyverse, magrittr, climateR, foreach, doSNOW)

# study area 
  ngp <- read_sf('./data/SeverityComparison.gpkg', 'StudyRegion')
# Census tracts
  ngp_counties <- tigris::counties(state = c('MT', "ND", 'SD'), cb = T)
  
  ngp_counties%<>% st_transform(st_crs(ngp)) %>% st_intersection(ngp)
ngpFIPS <-
  read_sf('S:/DevanMcG/GIS/SpatialData/US/census/tracts2020', 
          'ConterminousTracts2020') %>%
  select(-state) %>%
  st_transform(st_crs(ngp)) %>%
  st_intersection(ngp)  

ngp_grid <- 
ngp %>%
  st_make_grid(cellsize = c(10000, 10000)) %>%
  st_intersection(ngp)  %>% 
  st_as_sf() %>%
  rowid_to_column('cell')
ggplot() + geom_sf( )  + 
  geom_sf(data = ngp, fill = NA, color = 'blue')

setdiff(ngpFIPS$fips, ngpFIPS2$fips)

length(unique(ngpFIPS2$fips))


{
  cores= parallel::detectCores()
  cl <- makeCluster(cores) 
  registerDoSNOW(cl )
  
  begin = Sys.time()
  ngp_ppt <- 
    foreach(i=1:length(ngp_grid$cell), 
            .combine = bind_rows, 
            .errorhandling = 'remove', 
            .inorder = FALSE, 
            .packages = c('tidyverse', 'sf', 'climateR')) %dopar% {
              getTerraClim(
                AOI = ngp_grid %>%
                      slice(i) %>%
                       st_centroid() , 
                varname = 'ppt',
                startDate = '2017-01-01', 
                endDate = '2024-12-31'  ) %>% 
                as_tibble() %>%
                separate(date, into = c('year', 'month', 'day'), sep = "-") %>%
                 group_by(year) %>%
                summarize(ppt = sum(ppt_total, na.rm = TRUE)) %>%
                summarize(ppt = mean(ppt, na.rm = TRUE)) %>%
                add_column(cell = i)
            }
  stopCluster(cl)
  Sys.time() - begin 
}

ppt_dat <- lst() 
ppt_grd <- 
  ngp_grid  %>%
    merge(by = 'cell', 
              ngp_ppt) 
ppt_dat$ppt_grd <- ppt_grd

ppt_dat$REC_PPT <-
  ppt_grd %>%
    st_intersection(ngp_counties %>%
                      filter(STUSPS == 'ND', 
                             NAME %in% c('Stutsman', 'Kidder', 'Adams')) %>%
                      select(L3) ) %>%
  filter(st_geometry_type(., by_geometry = TRUE) %in% c('MULTIPOLYGON', 'POLYGON'))  %>%
    as_tibble() %>%
    group_by(L3) %>%
    summarize(PPT_Mean = mean(ppt), 
              PPT_SD = sd(ppt))

# Precipitation map
  ppt_grd %>%
    ggplot() + theme_void() +
    geom_sf(aes(fill = ppt), color =NA ) +
    geom_sf(data = ngp_counties, fill = NA, color = 'grey50') + 
    geom_sf(data = ngp, fill = NA, color = 'black') + 
    scale_fill_gradientn("Mean annual\nprecipitation (mm)\n", 
                         colours = rev(wes_palette("Zissou1", 10, type = "continuous")), 
                                       na.value = "grey70") 
# Precipitation anomaly map
  ppt_grd %>%
    st_intersection(ngp, .) %>%
    mutate(anomaly = case_when(
      L3 == "Northwestern Glaciated Plains" ~ ppt - 447,  
      L3 == "Northwestern Great Plains" ~ ppt - 429
    )) %>%
    ggplot() + theme_void() +
    geom_sf(aes(fill = anomaly), color = 'NA') +
    geom_sf(data = ngp_counties, fill = NA, color = 'grey50') + 
    geom_sf(data = ngp, fill = NA, color = 'black') + 
    scale_fill_gradient2("Precipitation anomaly (mm)\n", 
                         low = wes_palette("Zissou1")[4],
                         mid = 'white',
                         high = wes_palette("Zissou1")[1], 
                         na.value = "grey70") 
  
# Precipitation anomaly map
  
  rx_sf <- read_sf( './data/OriginalRxBurnPerims.gpkg', 'RxLocations')
 locations <- 
   bind_rows( 
    wf_sf %>% 
      st_transform(albersEAC) %>% 
      st_centroid() %>%
      mutate(type = 'Wildfire',
             L3 = case_when(
               FireCode %in% c('Swather_2021','Hwy31101STWest_2022', 'CB00121_2021', 
                               'CoalSeamWest_2021', '1806HunkpapaCreek_2021') ~
                 'Northwestern Glaciated Plains', 
               TRUE ~ L3), 
      zone = ifelse(L3 == "Northwestern Great Plains", 'Western', 'Eastern'), 
      zone = fct_rev(zone)) %>%
      select(type, zone), 
    rx_sf %>%
      mutate(type = 'Prescribed fire', 
             zone = ifelse(location == "cgrec", 'Eastern', 'Western')) %>%
      select(type, zone) )
  
 states <-  
   ngp %>%
   mutate(Area = st_area(.)) %>%
   group_by(state) %>%
   summarize(Area = sum(Area))  

  ppt_grd %>%
    st_intersection(ngp, .) %>%
    filter(st_geometry_type(., by_geometry = TRUE) %in% c('MULTIPOLYGON', 'POLYGON'))  %>%
    mutate(anomaly = case_when(
      L3 == "Northwestern Glaciated Plains" ~ ppt - 447,  
      L3 == "Northwestern Great Plains" ~ ppt - 431 )) %>%
    ggplot() + theme_void() +
    geom_sf(aes(fill = anomaly), color = 'NA') +
    geom_sf_pattern(data =  ppt_dat$anom_map %>% filter(anom_cat == 'beyond'), 
                    aes(pattern = anom_cat), 
                    pattern_color = NA,
                    pattern_fill = "grey80",
                    pattern_angle = 45,
                    pattern_alpha = 0.5,
                    pattern_density = 0.5,
                    pattern_spacing = 0.025,
                    pattern_key_scale_factor = 1,, 
                    show.legend = F) +
    geom_sf(data = ngp_counties, fill = NA, color = 'grey70') + 
    geom_sf(data = states, fill = NA, color = 'black') + 
    geom_sf_text(data = states %>%
                   st_centroid(), 
                 aes(label = state),
                 fontface = 'bold',
                  color = 'black') + 
    geom_sf_label(data = locations %>% filter(type != 'Wildfire'), 
            aes(color = zone),
            label = expression(R[X]), 
            label.padding = unit(0.1, "lines"),
            label.r = unit(0.5, "lines"),
            label.size = 0.15,
            size = 5) +
    geom_sf(data = locations %>% filter(type == 'Wildfire'), 
            aes(shape = zone, color = zone), 
             stroke = 2, size = 3) +
    geom_sf(data =   precip_perims %>% 
              filter(FireName == 'Dorothy Draw'), 
            pch = 8, size = 10) + 
    # geom_sf(data = locations, aes(shape = type, color = type), 
    #         fill = 'lightgreen', 
    #         stroke = 2, size = 3) +
    scale_fill_gradient2("Precipitation anomaly (mm)\n", 
                         low = wes_palette("Zissou1")[4],
                         mid = 'white',
                         high = wes_palette("Zissou1")[1], 
                         na.value = "grey70") +
    scale_pattern_manual(values = c(beyond = "circle", within = "none")) + 
    scale_shape_manual('Zone', values = c(4,3)) + 
    scale_color_manual('Fire type', values = c('darkgreen', 'darkred'), guide = 'none')

  
  
  ppt_dat$anomaly <- 
    #   SDs = 3.5
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
          abs(anomaly) < 19 * SDs ~ 'within') ) 
  
  ppt_dat$anom_map <-
    ppt_dat$anomaly %>%
      select(anom_cat) %>%
      mutate(area = st_area(.)) %>%
      group_by(anom_cat) %>%
      summarize(Area = sum(area)) 
  
  # save(ppt_dat, file = './data/ppt_dat.Rdata')


