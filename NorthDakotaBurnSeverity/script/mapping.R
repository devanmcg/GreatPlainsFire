pacman::p_load(tidyverse, sf)

# Data loading 
load('../albersEAC.Rdata')
rx_sf <- read_sf( './data/OriginalRxBurnPerims.gpkg', 'RxLocations')
wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF')
ngp <- read_sf('./data/MapStuff.gpkg', 'ngp')
ngp_counties <- read_sf('./data/MapStuff.gpkg', 'ngp_counties')
usa <- read_sf('./data/MapStuff.gpkg', 'usa')
gp <- read_sf('./data/MapStuff.gpkg', 'gp')
NGP <- read_sf('./data/MapStuff.gpkg', 'NGP_states') # L3 for MT, ND, SD
NGP_fires <- read_sf('./data/MapStuff.gpkg', 'NGP_fires')
load('./data/ppt_dat.Rdata')

# Region map 
reg_gg <-
ppt_dat$ngp_ppt %>%
  mutate(NA_L3NAME = str_replace(NA_L3NAME, ' ', '\n')) %>%
ggplot() + theme_void() +
  geom_sf(aes(fill = NA_L3NAME, alpha = ppt), 
          color = 'NA') +
  geom_sf(data = NGP, 
          fill = NA) + 
  geom_sf_label(data = locations %>% 
                  filter(type != 'Wildfire'), 
                label = expression(R[X]), 
                label.padding = unit(0.1, "lines"),
                label.r = unit(0.5, "lines"),
                label.size = 0.15,
                size = 4) +
  geom_sf(data = NGP_fires, 
          pch = 1, alpha = 0.5) + 
  geom_sf_label(data = NGP %>%
                  group_by(STATE_NAME) %>%
                  summarize(area = sum(Shape_Area)) , 
                aes(label = state.abb[match(STATE_NAME,state.name)])) + 
  scale_fill_manual('EPA Level III\necoregion', 
                    values = c("#74A089","#E1BD6D")) +
  scale_alpha_continuous("Precipitation\nmean annual (mm)", 
                         range = c(0.1, 1)) + 
  theme(legend.position = 'right', 
        legend.key.height = unit(1, 'cm')) + 
  guides(alpha = guide_legend(order = 2), fill = guide_legend(order = 1) ) 


us_gg <- 
  ggplot() + theme_void() + 
  geom_sf(data = usa %>% st_buffer(10000), color = NA, fill = 'white')  + 
  geom_sf(data = usa, color = 'white', fill = 'grey60')  + 
  geom_sf(data = gp, fill = 'white', 
          color =  wes_palette('Zissou1')[5], 
          alpha = 0.35, size = 2) +
  geom_sf(data = NGP, 
          aes(fill = NA_L3NAME), 
          color = NA, 
          show.legend = F) +
  scale_fill_manual('EPA Level III\necoregion', 
                    values = c("#74A089","#E1BD6D")) 


grid.newpage() 
main <- viewport(width = 1, height = 0.95, x = 0.5, y = 0.5) 
inset <- viewport(width = 0.3, height = 0.4, x = 0.25, y = 0.25)
print(reg_gg, vp = main)
print(us_gg, vp = inset)

# Study locations

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

reg_gg <- 
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
                size = 5) +
  geom_sf(data = locations %>% filter(type == 'Wildfire'), 
          aes(shape = zone, color = zone), 
          stroke = 2, size = 3) +
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
  scale_color_manual('Fire type', values = c('darkgreen', 'darkred'), guide = 'none')

us_gg <- 
  ggplot() + theme_void() + 
  geom_sf(data = usa %>% st_buffer(10000), color = NA, fill = 'white')  + 
  geom_sf(data = usa, color = 'white', fill = 'grey60')  + 
  geom_sf(data = gp, fill = 'white', 
          color =  'black', 
          alpha = 0.5, size = 2) +
  geom_sf(data = NGP, 
          fill =  wes_palette('Zissou1')[5],  
          color = NA, 
          show.legend = F) +
  scale_fill_manual('EPA Level III\necoregion', 
                    values = c("#74A089","#E1BD6D")) 


grid.newpage() 
main <- viewport(width = 1, height = 0.95, x = 0.5, y = 0.5) 
inset <- viewport(width = 0.3, height = 0.4, x = 0.8, y = 0.25)
print(reg_gg, vp = main)
print(us_gg, vp = inset)  

