pacman::p_load(tidyverse, sf, wesanderson) 

# Sentinel imagery
  sev_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison'
  images = list.files(sev_dir, include.dirs = FALSE)
  
  ImgTab <- 
    images %>%
      as_tibble() %>%
      separate(value, into = c('incident', 'period', 'date', 'script'), sep = '_') %>%
      rowid_to_column('image')
# Fire perimeters and sample points 
  perims <- 'C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data' %>%
    paste0(., '/SeverityComparison.gpkg') %>%
    read_sf('perimeters')
  points <- 'C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data' %>%
    paste0(., '/SeverityComparison.gpkg') %>%
    read_sf('SamplePoints')

SeveritySamples <- tibble() 

for(i in 1:length(unique(ImgTab$incident))){
  inc = unique(ImgTab$incident)[i]
  # Get pre-burn image
    b_num <- ImgTab %>% filter(incident == inc, period == 'B') %>% select(image)
    b_fn = images[as.vector(b_num$image)]
    b_ras <- paste0(sev_dir, '/', b_fn) %>% terra::rast() 
    names(b_ras) <- c('nbr', 'ndvi') 
    b_fl = (b_ras-10000)/10000
  # Get post-burn image
    a_num <- ImgTab %>% filter(incident == inc, period == 'A') %>% select(image)
    a_fn = images[as.vector(a_num$image)]
    a_ras <- paste0(sev_dir, '/', a_fn) %>% terra::rast() 
    names(a_ras) <- c('nbr', 'ndvi') 
    a_ras <- a_ras[['nbr']]
    a_fl = (a_ras-10000)/10000
  # differencing NBR
    dNBR = b_fl[['nbr']] - a_fl
    names(dNBR) <- 'dNBR'
    #terra::plot(dNBR)
  # Subsetting vectors 
    perim <- filter(perims, incident == inc)
    pts <- st_intersection(perim, points)
  # Sample rasters & export data as tibble
    full_join(by = c('incident','FireYear','FireType','id'), 
      # get dNBR for sample points
      terra::extract( dNBR,
                      pts %>% terra::vect() , 
                      FUN = mean, 
                      bind = TRUE)  %>%
        st_as_sf() %>%
        as_tibble() %>%
        select(-geometry), 
      # get pre-burn NDVI for sample points
      terra::extract( b_fl[['ndvi']],
                      pts %>% terra::vect() , 
                      FUN = mean, 
                      bind = TRUE)  %>%
        st_as_sf() %>%
        as_tibble() %>%
        select(-geometry)
    ) %>%
      bind_rows(SeveritySamples) -> SeveritySamples
    }

# save(SeveritySamples, file = 'C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Fire x herbivory/Lemonade Fire/talks/data/SeveritySamples.Rdata')

SeveritySamples %>%
  filter(dNBR > 0) %>%
  mutate(FireType = fct_relevel(FireType, 'Rx_Graze', 'Rx_NoGraze', 'Escape','Wildfire' ), 
         FireType = recode(FireType, 'Rx_NoGraze' = "Rx patch burn,\nno grazing",
                                     'Rx_Graze' = "Rx patch burn,\nmoderate grazing")) %>%
  ggplot() + theme_bw(20) + 
  geom_boxplot(aes(x = dNBR, y = FireType, fill = FireType), 
               staplewidth = 0.25, size = 2, 
               outlier.shape = 4, outlier.stroke = 2, 
               outlier.alpha = 0.6, outlier.size = 3) +
  labs(x = 'Severity (dNBR)', y = '') + 
  scale_fill_manual(values = wes_palette("Zissou1")[c(1,2,4,5)], guide = 'none') +
  theme(axis.text.y = element_text(color = 'black', size = 20, hjust = 0.5))
