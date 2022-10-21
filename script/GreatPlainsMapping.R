pacman::p_load(tidyverse, sf)
source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')
load('./gis/Robjects/mapping.Rdata') 

epa <- read_sf('S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3', 
               'us_eco_l3_state_boundaries')
aea <- st_crs(epa)

load('C:/Users/devan.mcgranahan/GithubProjects/GreatPlainsFire/gis/Robjects/us_sf.Rdata')
us_sf <- us_sf %>%
          st_transform(aea) %>%
            st_simplify()

gp_states <- 
  epa %>%
  as_tibble() %>%
  select(NA_L1NAME, STATE_NAME) %>%
  filter(NA_L1NAME == "GREAT PLAINS") %>%
  filter(STATE_NAME != "Wisconsin")

gp_sf <- epa %>%
          filter(STATE_NAME %in% gp_states$STATE_NAME) %>%
          select(STATE_NAME) %>%
          mutate(area = st_area(.)) %>%
          group_by(STATE_NAME) %>%
          summarize(area = sum(area)) %>%
          ungroup() %>%
          select(-area) 

gp_l2 <- epa %>% 
          filter(NA_L1NAME == "GREAT PLAINS") %>%
            mutate(area = st_area(.)) %>%
            group_by(NA_L2NAME) %>%
            summarize(area = sum(area)) %>%
            select(-area) %>%
            rename(L2 = NA_L2NAME) %>%
            mutate(L2 = str_to_title(L2))

gp_l3 <- epa %>% 
          filter(NA_L1NAME == "GREAT PLAINS") %>%
          select(NA_L2NAME, US_L3NAME) %>%
          rename(L2 = NA_L2NAME, 
                 L3 = US_L3NAME) %>%
          mutate(L2 = str_to_title(L2))

gp_l2_5 <- epa %>% 
  filter(NA_L1NAME == "GREAT PLAINS") %>%
  select(NA_L2NAME, US_L3NAME) %>%
  rename(L2 = NA_L2NAME, 
         L3 = US_L3NAME) %>%
  mutate(L2 = str_to_title(L2)) %>%
  mutate(L2 = case_when(
    L2 %in% c("Tamaulipas-Texas Semiarid Plain", 
              'Texas-Louisiana Coastal Plain') ~ "Texas Southern Plains", 
    TRUE ~ L2) ) %>%
  mutate(L2_5 = case_when(
    L3 %in% c('Lake Agassiz Plain', 
              'Northern Glaciated Plains',
             "Northwestern Glaciated Plains", 
             'Northwestern Great Plains') ~ "Northern",
    L3 %in% c("Central Irregular Plains", 
              "Flint Hills", 
              'Western Corn Belt Plains') ~ "Eastern",
    L3 %in% c("Central Great Plains", 
              'Nebraska Sand Hills',
              'Cross Timbers') ~ "Central",
    L3 %in% c("High Plains", 
              "Southwestern Tablelands") ~ 'Western',
    L3 %in% c("Texas Blackland Prairies", 
              "Southern Texas Plains", 
              'Edwards Plateau',
              "Western Gulf Coastal Plain") ~ 'Southern',
    TRUE ~ L3
  ))

mapping <- lst(us = us_sf, 
               gp_st = gp_sf, 
               gp_l2 = gp_l2, 
               gp_l3 = gp_l3, 
               gp_l2_5 = gp_l2_5)
# save(mapping, file = './gis/Robjects/mapping.Rdata')

ggplot() + theme_map(14) +
  geom_sf(data = mapping$us, 
          fill = "grey90") + 
  geom_sf(data = mapping$gp_l2, 
          aes(fill = L2))  + 
  geom_sf(data = mapping$gp_st, 
          fill = NA, 
          color = 'grey50') +
  scale_fill_manual(name = 'Level 2 ecoregion',
                    values = wes_palette("Zissou1")) 

# Sub-sampling Great Plains L3 regions

pal <- wes_palette("Zissou1", 40, type = "continuous") 

ggplot() + theme_map(14) +
  geom_sf(data = gp_l2, fill = 'white')  + 
  geom_sf(data = gp_l2 %>%
  filter(L2 %in% c("Tamaulipas-Texas Semiarid Plain", 
                   'Texas-Louisiana Coastal Plain')), 
  aes(fill = L2))  + 
  scale_fill_viridis_d(name = 'Level 2.5 ecoregion') 

ggplot( ) + theme_map(14) +
  geom_sf(data = gp_l2_5, 
          aes(fill = L2))  + 
  scale_fill_manual(name = 'Level 2 ecoregion', 
                    values = pal[c(5, 15, 25, 35)])  

GP_Wx_SamplePoints <-
  st_union(gp_l2) %>%
    st_buffer(-8000) %>%
    st_sample(100, 
              type = "regular", 
              exact = TRUE, 
              by_polygon = TRUE) %>%
    st_sf()  %>% 
  st_join(mapping$gp_l3)

# save(GP_Wx_SamplePoints, file = './gis/Robjects/GP_Wx_SamplePoints.Rdata')

GP_Wx_SamplePoints %>%
  ggplot() + theme_map() +
  geom_sf(data = mapping$gp_l2, 
                         aes(fill = L2), 
                         alpha = 0.25) +
  geom_sf() 
          

  

