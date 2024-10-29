pacman::p_load(tidyverse, sf, stars)

cgrec_gpkg = './gis/boundaries/CGREC_PBG_26914.gpkg'
st_layers(cgrec_gpkg ) 

pastures <- st_read(cgrec_gpkg, 'Pastures') 
patches <- st_read(cgrec_gpkg, 'PasturePatches') 

NoFirePts <- st_read(cgrec_gpkg, 'SamplePoints') %>%
              filter(location == 'Refuge')

# Get veg data for unburned pastures
  imagery_dir = 'S:/DevanMcG/Projects/SpringSummerSeverity/sentinel/MultiBand'
  images <- list.files(imagery_dir)
  
  { 
    begin = Sys.time() 
    pacman::p_load(foreach, doSNOW)
    cores = parallel::detectCores()
    cl <- makeCluster(cores, methods = F, useXDR = F)
    registerDoSNOW(cl)
    NoFireIndices <-
      foreach(i=1:length(images), 
              .combine = 'bind_rows',
              .errorhandling = 'remove', 
              .packages=c('tidyverse', 'sf')) %dopar% {
        image = images[i]
        image_path = paste0(imagery_dir, '/', image)
        ras <- terra::rast(image_path)
        names(ras) <- c('nbr', 'ndvi', 'msavi', 'msi', 'ndmi') 
        ras <- ras[[c('ndvi', 'msi','ndmi')]]
        float = (ras-10000)/10000
        terra::extract(float,
          NoFirePts %>%
            select(pasture, sample) %>%
            terra::vect() , 
          FUN = mean, 
          bind = TRUE)  %>%
        st_as_sf() %>%
          as_tibble() %>%
        mutate(ImageDate = substr(image, 1, 10)) %>%
        select(ImageDate, pasture, sample, ndvi:ndmi) 
            }
    stopCluster(cl)
  Sys.time() - begin 
  }
# save(NoFireIndices, file = './data/NoFireIndices.Rdata')

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
  # Get burn seasons
    fires %>%
      as_tibble() %>%
      select(Season, PreBurn, PostBurn) %>%
      pivot_longer(names_to='type', 
                   values_to = 'date', 
                   cols = PreBurn:PostBurn) %>%
      mutate(date = as.Date(date, '%Y-%m-%d')) %>%
      filter(lubridate::year(date) != '2016') %>%
      mutate(date = format(date, '%m-%d'), 
             date = as.Date(date, '%m-%d')) %>%
      group_by(Season) %>%
      summarize(start = min(date), 
                end = max(date)) 
  
  SamplePts <- 
    st_read(cgrec_gpkg, 'SamplePointsRegular')
  
  imagery_dir = 'S:/DevanMcG/Projects/SpringSummerSeverity/sentinel/MultiBand'
  images <- list.files(imagery_dir)

{ 
  begin = Sys.time() 
  pacman::p_load(foreach, doSNOW)
  cores = parallel::detectCores()
  cl <- makeCluster(cores , methods = F, useXDR = F)
  registerDoSNOW(cl)
  BurnIndices <-
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
              # Fetch & process multi-band rasters
                # pre image
                  pre_image = images[substr(images, 1, 10) == pre_date]
                  pre_path = paste0(imagery_dir, '/', pre_image)
                  pre_ras <- terra::rast(pre_path) %>%
                                terra::crop(terra::vect(fire))
                  names(pre_ras) <- c('nbr', 'ndvi', 'msavi', 'msi', 'ndmi') 
                  pre_ras <-  (pre_ras-10000)/10000
                # post-fire
                  post_image = images[substr(images, 1, 10) == post_date]
                  post_path = paste0(imagery_dir, '/', post_image)
                  post_ras <- terra::rast(post_path)[[1]] %>%
                                terra::crop(terra::vect(fire))
                  post_ras = (post_ras-10000)/10000
                # Calculate dNBR & replace in pre-fire raster
                  d_ras = pre_ras['nbr'] - post_ras
                  names(d_ras) <- 'dNBR'
                  pre_ras[[1]] <- d_ras
              # Sample rasters
                 terra::extract( pre_ras,
                          pts %>%
                            select(-PostBurn) %>%
                            terra::vect() , 
                          FUN = mean, 
                          bind = TRUE)  %>%
                  st_as_sf() %>%
                    as_tibble() %>%
                    select(-geometry) 
            }
  stopCluster(cl)
  Sys.time() - begin 
}

  # save(BurnIndices, file = './data/BurnIndices.Rdata')

BI_sum <-
  BurnIndices  %>%
    group_by(Year, Season, fire) %>%
    summarise_at(.vars = vars(dNBR:ndmi),
                 .funs = c(Mean="mean", 
                           SEM = function(x) {sd(x)/sqrt(n()) } )) %>%
    ungroup() 

BI_sum %>%
    filter(Season == "Spring") %>%
    select(Year, dNBR_Mean:ndmi_Mean) %>%
    GGally::ggpairs(aes(color = Year)) 

BI_sum %>%
  filter(Season == "Summer")

fires %>%
  filter(Season == 'Summer')


  

# Severity vs greenness
  BI_sum %>%
      ggplot(aes(x = ndvi_Mean, y = dNBR_Mean, color = Year)) + theme_bw() +
        geom_errorbar(aes(ymin = dNBR_Mean - dNBR_SEM, 
                          ymax = dNBR_Mean + dNBR_SEM)) +
      geom_errorbarh(aes(xmin = ndvi_Mean - ndvi_SEM, 
                         xmax = ndvi_Mean + ndvi_SEM)) +
      geom_point() + 
      geom_smooth(method = 'lm', se = F) +
      facet_wrap(~Season)
# Severity vs. moisture content
  BI_sum %>%
    ggplot(aes(x = ndmi_Mean, y = dNBR_Mean, color = Year)) + theme_bw() +
    geom_errorbar(aes(ymin = dNBR_Mean - dNBR_SEM, 
                      ymax = dNBR_Mean + dNBR_SEM)) +
    geom_errorbarh(aes(xmin = ndmi_Mean - ndmi_SEM, 
                       xmax = ndmi_Mean + ndmi_SEM)) +
    geom_point() + 
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(~Season)
# Severity vs. moisture stress
  BI_sum %>%
    ggplot(aes(x = msi_Mean, y = dNBR_Mean, color = Year)) + theme_bw() +
    geom_errorbar(aes(ymin = dNBR_Mean - dNBR_SEM, 
                      ymax = dNBR_Mean + dNBR_SEM)) +
    geom_errorbarh(aes(xmin = msi_Mean - msi_SEM, 
                       xmax = msi_Mean + msi_SEM)) +
    geom_point() + 
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(~Season)


  
  NegSev <-
    BurnIndices  %>%
    group_by(Year, Season, fire) %>%
    summarize(SeverityMean = mean(dNBR) ) %>%
    filter(SeverityMean < 0) %>%
    select(fire)
  
  fires %>% filter(fire %in% NegSev$fire)
