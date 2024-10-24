pacman::p_load(tidyverse, sf, stars)

cgrec_gpkg = './gis/boundaries/CGREC_PBG_26914.gpkg'
st_layers(cgrec_gpkg ) 

pastures <- st_read(cgrec_gpkg, 'Pastures') 
patches <- st_read(cgrec_gpkg, 'PasturePatches') 

NoFirePts <- st_read(cgrec_gpkg, 'SamplePoints') %>%
              filter(location == 'Refuge')

# Get NDVI data for unburned pastures
  ndvi_dir = 'S:/DevanMcG/Projects/SpringSummerSeverity/sentinel/NDVI'
  ndvi_images <- list.files(ndvi_dir)
  
  { 
    begin = Sys.time() 
    pacman::p_load(foreach, doSNOW)
    cores = parallel::detectCores()
    cl <- makeCluster(cores -1 , methods = F, useXDR = F)
    registerDoSNOW(cl)
    NoFireNDVI <-
      foreach(i=1:length(ndvi_images), 
              .combine = 'bind_rows',
              .errorhandling = 'remove', 
              .packages=c('tidyverse', 'sf')) %dopar% {
        image = ndvi_images[i]
        image_path = paste0(ndvi_dir, '/', image)
        ras <- terra::rast(image_path)
        names(ras) <- 'ndvi'
        ndvi_float = (ras-10000)/10000
        terra::extract( ndvi_float,
          NoFirePts %>%
            select(pasture, sample) %>%
            terra::vect() , 
          FUN = mean, 
          bind = TRUE)  %>%
        st_as_sf() %>%
        mutate(ImageDate = substr(image, 1, 10)) %>%
        as_tibble() %>%
        select(ImageDate, pasture, sample, ndvi) 
            }
    stopCluster(cl)
  Sys.time() - begin 
  }

NoFireNDVI %>%
  group_by(ImageDate, pasture) %>%
    summarise(Mean = mean(ndvi), 
              SEM = sd(ndvi)/sqrt(n()), 
              .groups = 'drop') %>%
  mutate(ImageDate = as.Date(ImageDate, '%Y-%m-%d'), 
         Year = lubridate::year(ImageDate)) %>%
  filter(Year != 2016) %>%
ggplot(aes(x = ImageDate)) + theme_bw() +
  geom_smooth(aes(y = Mean, 
                  group = 1) , 
              color = 'darkgreen', 
              fill = 'lightgreen', 
              level = 0.99) +
  geom_errorbar(aes(ymin = Mean - SEM, 
                    ymax = Mean + SEM), 
                color = 'darkgreen', alpha = 0.5) +
  geom_line(aes(y = Mean, 
                group = pasture), 
            color = 'darkgreen', alpha = 0.5) +
  labs(x = 'Image date', y = 'NDVI') + 
  facet_wrap(~Year, scale = 'free_x') +
  scale_x_date( )

# Get fuel greenness and dNBR for completed burns

  fires <- st_read(cgrec_gpkg, 'FirePerimeters') %>%
              mutate(Year = as.factor(Year)) %>%
              filter(status == 'Completed') %>%
              unite('fire', c(unit, Pasture, Patch), sep = "-") %>%
              select(fire, Year, Season, PreBurn, PostBurn )
  # Getting imagery dates
  fires %>%
    as_tibble() %>%
    select(PreBurn, PostBurn) %>% 
    pivot_longer(c(PreBurn, PostBurn), 
                 values_to = 'ImageryDate') %>%
    select(ImageryDate) %>%
    distinct()%>%
    arrange(ImageryDate) %>%
    View()
  
  SamplePts <- 
    st_read(cgrec_gpkg, 'SamplePointsRegular')
  
  ndvi_dir = 'S:/DevanMcG/Projects/SpringSummerSeverity/sentinel/NDVI'
  nbr_dir = 'S:/DevanMcG/Projects/SpringSummerSeverity/sentinel/NBR'
  ndvi_images <- list.files(ndvi_dir)
  nbr_images <- list.files(nbr_dir)

{ 
  begin = Sys.time() 
  pacman::p_load(foreach, doSNOW)
  cores = parallel::detectCores()
  cl <- makeCluster(cores -1 , methods = F, useXDR = F)
  registerDoSNOW(cl)
  dNBR <-
    foreach(i=1:length(fires$fire), 
            .combine = 'bind_rows',
            .errorhandling = 'remove', 
            .packages=c('tidyverse', 'sf')) %dopar% {
              # Get fire
                fire = slice(fires, i)
              # Get sample points
                pts <-
                  fire %>%
                    st_intersection(SamplePts)
              # Get dates
                pre_date = fire$PreBurn
                post_date = fire$PostBurn
              #
              # get NBR
              #
              # pre-fire
                pre_file = nbr_images[substr(nbr_images, 1, 10) == pre_date]
                pre_path = paste0(nbr_dir, '/', pre_file)
                pre_ras <- terra::rast(pre_path)[[1]] %>%
                            terra::crop(terra::vect(fire))
                pre_ras2 = (pre_ras-10000)/10000
              # post-fire
                post_file = nbr_images[substr(nbr_images, 1, 10) == post_date]
                post_path = paste0(nbr_dir, '/', post_file)
                post_ras <- terra::rast(post_path)[[1]] %>%
                              terra::crop(terra::vect(fire))
                post_ras2 = (post_ras-10000)/10000
              # Calculate dNBR
                d_ras = pre_ras2 - post_ras2
                names(d_ras) <- 'dNBR'
              # Get NDVI
                ndvi_file = ndvi_images[substr(ndvi_images, 1, 10) == pre_date]
                ndvi_path = paste0(ndvi_dir, '/', ndvi_file)
                ndvi_ras <- terra::rast(ndvi_path) %>%
                  terra::crop(terra::vect(fire))
                names(ndvi_ras) <- 'ndvi'
                ndvi_ras2 = (ndvi_ras-10000)/10000
              # Sample rasters
                full_join(by = join_by(fire, Year, Season, PreBurn, id), 
                   terra::extract( ndvi_ras2,
                              pts %>%
                                select(-PostBurn) %>%
                                terra::vect() , 
                              FUN = mean, 
                              bind = TRUE)  %>%
                  st_as_sf() %>%
                    as_tibble() %>%
                    select(-geometry) , 
                terra::extract( d_ras,
                                pts %>%
                                  select(-PostBurn) %>%
                                  terra::vect() , 
                                FUN = mean, 
                                bind = TRUE)  %>%
                  st_as_sf() %>%
                  as_tibble()  %>%
                  select(-geometry) ) 
            }
  stopCluster(cl)
  Sys.time() - begin 
}

  dNBR  %>%
    group_by(Year, Season, fire) %>%
    summarize(SeverityMean = mean(dNBR), 
              SeveritySEM = sd(dNBR)/sqrt(n()), 
              GreenMean = mean(ndvi), 
              GreenSEM = sd(ndvi)/sqrt(n())) %>%
    ggplot(aes(x = GreenMean, y = SeverityMean, color = Year)) + theme_bw() +
      geom_errorbar(aes(ymin = SeverityMean - SeveritySEM, 
                        ymax = SeverityMean + SeveritySEM)) +
    geom_errorbarh(aes(xmin = GreenMean - GreenSEM, 
                       xmax = GreenMean + GreenSEM)) +
    geom_point() + 
    geom_smooth(method = 'lm') +
    facet_wrap(~Season)
  
  NegSev <-
  dNBR  %>%
    group_by(Year, Season, fire) %>%
    summarize(SeverityMean = mean(dNBR), 
              SeveritySEM = sd(dNBR)/sqrt(n()), 
              GreenMean = mean(ndvi), 
              GreenSEM = sd(ndvi)/sqrt(n())) %>%
    filter(SeverityMean < 0) %>%
    select(fire)
  
  fires %>% filter(fire %in% NegSev$fire)
