pacman::p_load(tidyverse, sf, geonames, 
               grid, gridExtra)
source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')

load('C:/Users/devan.mcgranahan/GithubProjects/SpatialFireBehavior/paper/figures/mapping.Rdata')
# Make maps

load('./paper/figures/mapping.Rdata')

region_map <-
  ggplot() + theme_map(14)  +
  geom_sf(data = mapping$us, 
          fill = 'lightgrey', 
          color = NA)+ 
    geom_sf(data = mapping$gp_l1, 
            fill = 'lightblue') +
    geom_sf(data = mapping$nd,
            fill = 'darkblue', 
            color = "darkgrey") +
    geom_sf(data = mapping$us, 
            fill = NA, 
            color = "white")

  
state_map <-
  ggplot() + theme_map(14)  +
    geom_sf(data = mapping$nd,
            fill = 'NA', 
            color = "darkgrey") + 
    geom_sf(data = mapping$nd_l3,
            aes(fill = L3), 
            show.legend = FALSE) +
    geom_sf_label(data = mapping$nd_l3,
                  aes(label = L3)) +
    geom_sf(data = mapping$pts, 
            aes(shape = feature), 
            size = 4,
            show.legend = FALSE) +
    scale_shape_manual(values = c(16, 16, 17)) +
    geom_sf_text(data = mapping$pts,
                 aes(label = name), 
                 fontface = c("bold", 'bold', 'italic', 'italic'),
                 nudge_y = 20000,
                 nudge_x = c(-35000, 0, 0, -15000),
            show.legend = FALSE)

v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.45, height = 0.45, x = 0.71, y = 0.75) #plot area for the inset map
print(state_map,vp=v1) 
print(region_map,vp=v2)
