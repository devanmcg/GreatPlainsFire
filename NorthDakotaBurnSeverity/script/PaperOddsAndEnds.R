pacman::p_load(tidyverse, ggpattern, magrittr, sf, stars, wesanderson)
load('../albersEAC.Rdata')

# Create objects for data sharing 
  # Space-based vs Rx comparison
  load('./data/PlotBurnIndices.Rdata')
  PlotBurnIndices %>%
    select(location, burn, plot:dNBR)  %>%
    mutate(across(c(MaxC, SoilMaxC), ~ round(., 1)),
           ros = round(ros, 2),
           dNBR = round(dNBR, 3)) %>%
    write_csv('./data/PlotBurnIndices.csv')

  # Wildfire vs Rx comparison
    load('./data/BurnSeverityData.Rdata')
    BurnSeverityData %>%
      select(type:dNBR_Mean) %>%
      mutate(dNBR_Mean = round(dNBR_Mean, 3)) %>%
      write_csv('./data/BurnSeverityData.csv')

  # spatial data 
    read_sf('./data/SeverityComparison.gpkg', 'StudyRegion') %>%
      st_write('./data/SpatialData.gpkg', 'StudyRegion')
    read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF') %>%
      select(-STUSPS, -FireType, -NAME) %>%
      st_write('./data/SpatialData.gpkg', 'GreatPlainsModifiedWF', append = F)
    
    st_layers('./data/SpatialData.gpkg')
    
    read_sf('./data/SeverityComparison.gpkg', 'StudyRegion') %>%
      st_union() %>%
      st_as_sf() %>%
      st_simplify(dTolerance = 1000) %>% 
      geojsonsf::sf_geojson( simplify = TRUE,
                             digits = 3) %>%
      write.table("clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
# Get burn dates 
# Regional comparison wildfires 

st_layers('./data/SeverityComparison.gpkg')

wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF') %>%
  mutate(zone = case_when(
    FireCode %in% c('Swather_2021','Hwy31101STWest_2022', 'CB00121_2021', 
                    'CoalSeamWest_2021', '1806HunkpapaCreek_2021') ~
      'east', 
    L3 == 'Northwestern Great Plains' ~ 'west',
    L3 == 'Northwestern Glaciated Plains' ~ 'east'))

wf_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison/CompWildfires/precip'

wf_images <- 
  tibble(file = list.files(wf_dir, pattern = "\\.tiff$", ignore.case = TRUE) ) %>% 
  mutate(info = str_remove(file, '.tiff')) %>%
  separate(info, into = c('FireCode', 'period', 'ImageDate'), sep = '_') %>%
  mutate(FireCode = str_replace(FireCode, '-', '_'))

wf_images %>%
  mutate(ImageDate = as.Date(ImageDate)) %>%
  group_by(FireCode) %>%
  summarise(mid = mean(ImageDate), 
            .groups = 'drop') %>%
  full_join(by = "FireCode", 
wf_sf %>% 
  filter(FireCode %in% unique(wf_images$FireCode) ) %>%
  as_tibble() %>%
  select(FireCode, FireName, state, zone) ) %>%
  mutate(month = month(mid, label = TRUE)) %>%
  group_by(zone, month) %>%
  summarise(count = n(), 
            .groups = 'drop') %>%
  pivot_wider(names_from = month, 
              values_from = count) %>%
  xtable::xtable() 

# Make thumbnail for Ag Data Commons? 

rx_sf <- read_sf( './data/OriginalRxBurnPerims.gpkg', 'RxLocations')
wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF')
ngp <- read_sf('./data/MapStuff.gpkg', 'ngp')
ngp_counties <- read_sf('./data/MapStuff.gpkg', 'ngp_counties')
load('./data/ppt_dat.Rdata')

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

  ppt_dat$ppt_grd %>%
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
  geom_sf(data = ngp_counties %>% 
            st_intersection(states %>% st_transform(st_crs(ngp_counties))), 
          fill = NA, color = 'grey70') + 
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
                size = 3) +
  geom_sf(data = locations %>% filter(type == 'Wildfire'), 
          aes(shape = zone, color = zone), 
          stroke = 2, size = 1) +
  # geom_sf(data = locations, aes(shape = type, color = type), 
  #         fill = 'lightgreen', 
  #         stroke = 2, size = 3) +
  scale_fill_gradient2("Precipitation\nanomaly (mm)", 
                       low = wes_palette("Zissou1")[4],
                       mid = 'white',
                       high = wes_palette("Zissou1")[1], 
                       na.value = "grey70") +
  scale_pattern_manual(values = c(beyond = "circle", within = "none")) + 
  scale_shape_manual('Zone', values = c(4,3)) + 
  scale_color_manual('Fire type', values = c('darkgreen', 'darkred'), guide = 'none') +
    theme(legend.position = 'none')


# Make demo NBR figure 

perim <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF') %>%
          filter(FireCode == 'Hwy31101STWest_2022') 

after <- terra::rast("S:/DevanMcG/FireScience/Sentinel/SeverityComparison/CompWildfires/precip/Hwy31101STWest-2022_A_2022-08-31.tiff")
before  <- terra::rast("S:/DevanMcG/FireScience/Sentinel/SeverityComparison/CompWildfires/precip/Hwy31101STWest-2022_B_2022-08-16.tiff")

before = (before[[1]]-10000)/10000
after = (after[[1]]-10000)/10000 

example <- before 
names(example) <- 'Before'
example$After <- after
example$dNBR <- before - after
example %<>% terra::crop(., perim %>%st_buffer(500)) 

nbr_star <- example %>% st_as_stars()
d_star <- example$dNBR %>% terra::mask(perim) %>% st_as_stars() 

nbr_gg <- 
  ggplot() + theme_void() +
  coord_sf() + 
  geom_stars(data = nbr_star %>% slice(band, 1:2) ) + 
    facet_wrap(~band) +
    scale_fill_gradient('Normalized Burn Ratio', 
                        high = wes_palette("Darjeeling2")[3], 
                        low = wes_palette("Darjeeling2")[5]) +
    theme(#plot.margin = margin(0,-30,0,-30, "lines"),
      strip.text = element_text(size = 12),
      legend.position = 'top',
      legend.margin = margin(1,0,0,0, 'lines'), 
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.title.position = 'top',
      legend.direction = 'horizontal')

png('./paper/nbr_gg.png')
nbr_gg ; dev.off()

dnbr_gg <-
  ggplot() + theme_void() +
  coord_sf() + 
  geom_stars(data = d_star ) + 
  labs(title = 'Burn severity') + 
  scale_fill_gradientn("Change (ΔNBR)", 
                       colors = wes_palette(name = "Zissou1", type = "continuous"), 
                       na.value = 'grey90' )  +
  theme(#plot.margin = margin(0,-20,0,0, "lines"), 
    strip.text = element_text(size = 12),
    legend.position = 'top',
    legend.title = element_text(size = 14, hjust = 0.5),
    legend.margin = margin(0,0,1,0, 'lines'), 
    legend.title.position = 'top',
    legend.direction = 'horizontal', 
    plot.title = element_text(hjust = 0.5, vjust = -35, size = 12))  

png('./paper/dnbr_gg.png')
dnbr_gg ; dev.off()


wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF') %>%
         st_transform(albersEAC)

PADUS <- read_sf("S:/DevanMcG/GIS/SpatialData/US/USGS/PADUS/PADUS4_0_GeoPackage.gpkg")%>% 
          st_crop(wf_sf) %>%
          select(Mang_Type, Mang_Name)

PADUS %>%
  st_buffer(0) %>%
  st_make_valid(.)

wf_sf %>%
  slice(1:9) %>% 
  st_intersection(PADUS %>%
                    st_buffer(0) %>%
                    st_make_valid(.) ) 


  ggplot()+
  geom_sf(data = wf_sf %>%
            slice(1) ) +
  geom_sf( fill = 'red')
