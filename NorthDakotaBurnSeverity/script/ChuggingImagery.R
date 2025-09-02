pacman::p_load(tidyverse, magrittr, sf, stars)

load('../albersEAC.Rdata')

##
##  T H E R M O C O U P L E   C O M P A R I S O N S
##

# This will not run now, 
# but the output remains valid. 
# object 'BurnIndices' has plot-level data for 26 Rx burns 
# renamed as-is to 'PlotBurnIndices' 8/8/2025 to avoid conflict with 
# broader-scale data from CGREC from previous project with same name :(

  # Get fuel greenness and dNBR for completed burns
  fires <- st_read('./data/ThermocouplePlacements.gpkg', 'AllBurnData') 
  
  dates <- read_csv('./data/BurnDates.csv') %>%
            mutate(across(c(BurnDate:PostBurn), ~ as.Date(.x, '%m/%d/%Y')))
  
  # locations of imagery on S
  {
    cgrec_imagery = 'S:/DevanMcG/Projects/SpringSummerSeverity/sentinel/MultiBand'
    hrec_imagery = 'S:/DevanMcG/Projects/NorthDakotaBurnSeverity/sentinel/HREC'
    day_imagery = 'S:/DevanMcG/Projects/NorthDakotaBurnSeverity/sentinel/dayton'
    }

    BurnIndices <- tibble() 
    for(i in 1:length(unique(fires$burn))) {
                # Get fire & extract spatial, temporal info
                  fire = fires %>% filter(burn == unique(fires$burn)[i]) 
                  BurnDay = unique(fire$date)
                  loc = unique(fire$location)
                # Get dates
                  pre_date = dates %>% filter(BurnDate == BurnDay) %>% select(PreBurn)
                  post_date = dates %>% filter(BurnDate == BurnDay) %>% select(PostBurn)
                # Map to location's imagery
                  imagery_dir <- case_when(
                      loc == 'CGREC' ~ cgrec_imagery, 
                      loc == 'HREC' ~ hrec_imagery,
                      loc == 'Dayton' ~ day_imagery
                    )
                # Fetch images in location's folder
                  images <- list.files(imagery_dir, pattern = "\\.tiff$", ignore.case = TRUE)
                # Set location CRS to match rasters
                 fire %<>% st_transform(case_when(
                                        loc == 'CGREC' ~ 32614, 
                                        TRUE ~ 32613
                                      ) ) 
                # Fetch & process multi-band rasters
                # pre image
                  pre_image = images[substr(images, 1, 10) == pre_date[[1]]]
                  pre_path = paste0(imagery_dir, '/', pre_image)
                # Some HREC dates have 1 file for each of 2 pasture blocks
                # this checks whether there are 2, and which one to use.
                  if(length(pre_path) == 2) {
                     r1 <- terra::rast(pre_path[1])
                     r2 <- terra::rast(pre_path[2])  
                     N = ifelse(terra::is.related(terra::vect(fire), 
                            terra::rast(r1), 
                            'intersects')[1] == TRUE, 1, 2)
                    } else {N = 1}
                  pre_ras <- terra::rast(pre_path[N]) %>%
                             terra::crop(terra::vect(fire))
                  # Sometimes Copernicus downloads get cut off at 3 bands...
                  if(length( names(pre_ras)) == 3) {
                  names(pre_ras) <- c('nbr', 'ndvi', 'msavi') 
                  } else {
                    names(pre_ras) <- c('nbr', 'ndvi', 'msavi', 'msi', 'ndmi') 
                  }
                  pre_ras <-  (pre_ras-10000)/10000
                  # post-fire
                  post_image = images[substr(images, 1, 10) == post_date[[1]]]
                  post_path = paste0(imagery_dir, '/', post_image)
                  post_ras <- terra::rast(post_path[N])[[1]] %>%
                                terra::crop(terra::vect(fire))
                  post_ras = (post_ras-10000)/10000
                  # Calculate dNBR & replace in pre-fire raster
                  d_ras = pre_ras['nbr'] - post_ras
                  names(d_ras) <- 'dNBR'
                  pre_ras[[1]] <- d_ras
                  # Sample rasters
                  terra::extract( pre_ras,
                                  fire %>%
                                    terra::vect() , 
                                  fun = mean, 
                                  method = 'bilinear',
                                  bind = TRUE)  %>%
                    st_as_sf() %>%
                    as_tibble() %>%
                    select(location:ndvi) %>%
                    bind_rows(BurnIndices, .) -> BurnIndices
    }
    
  #  save(BurnIndices, file = './data/BurnIndices.Rdata')
  #  PlotBurnIndices <- BurnIndices
  #  save(PlotBurnIndices, file = './data/PlotBurnIndices.Rdata')
    
    
##
## S E V E R I T Y   C O M P A R I S O N S 
##
  # SW ND Rx fires 
  rx_perims <-
    bind_rows(
      read_sf('./data/OriginalRxBurnPerims.gpkg', 'hrec') %>%
        mutate(burn = paste0(location, '_', row_number())) %>%
        select(location, burn, PreBurn, PostBurn), 
      read_sf('./data/OriginalRxBurnPerims.gpkg', 'dayton')  %>%
        mutate(burn = paste0(location, '_', row_number())) %>%
        select(location, burn, PreBurn, PostBurn) )
    
    
    rx_dir = 'S:/DevanMcG/Projects/NorthDakotaBurnSeverity/sentinel'
    rx_images <- 
      tibble(file = list.files(rx_dir, pattern = "\\.tiff$", ignore.case = TRUE) ) %>%
      mutate(info = str_remove(file, '.tiff')) %>%
        separate(info, into = c('location', 'date'), sep = '_')
    
    rx_perims  %>%
      filter(burn %in% c('Dayton_6', 'Dayton_7'))
        
      
    RxBurnIndices <- tibble() 
    for(i in 1:length(unique(rx_perims$burn))) {
      # Get fire & extract spatial, temporal info
      fire = rx_perims %>% filter(burn == unique(rx_perims$burn)[i]) 
      loc = unique(fire$location)
      # create gridded sample points
        buff <- fire %>% st_buffer(-10)
        pts <-  buff %>%
                st_make_grid(cellsize = c(diff(st_bbox(.)[c(1, 3)]), 
                                          diff(st_bbox(.)[c(2, 4)]))/ 10) %>% 
                st_centroid() %>%
                st_intersection(buff)
      # Get dates
        pre_date = fire$PreBurn
        post_date = fire$PostBurn
      # Fetch & process multi-band rasters
      # pre image
        pre_image = filter(rx_images, location == loc, date == pre_date)$file
        pre_path = paste0(rx_dir, '/', pre_image)
        pre_ras <- terra::rast(pre_path) %>%
                   terra::crop(terra::vect(fire))
      # Sometimes Copernicus downloads get cut off at 3 bands...
      if(length(names(pre_ras)) == 3) {
        names(pre_ras) <- c('nbr', 'ndvi', 'msavi') 
      } else {
        names(pre_ras) <- c('nbr', 'ndvi', 'msavi', 'msi', 'ndmi') 
      }
      pre_ras <-  (pre_ras-10000)/10000
      # post-fire
      post_image = filter(rx_images, location == loc, date == post_date)$file
      post_path = paste0(rx_dir, '/', post_image)
      post_ras <- terra::rast(post_path) %>%
                  terra::crop(terra::vect(fire))
      post_ras = (post_ras-10000)/10000
      # Calculate dNBR & replace in pre-fire raster
      d_ras = pre_ras['nbr'] - post_ras[[1]]
      names(d_ras) <- 'dNBR'
      pre_ras[[1]] <- d_ras
      # Sample rasters
      terra::extract(pre_ras[[1:2]],
                     pts %>% 
                        terra::vect(), 
                      fun = mean, 
                      method = 'bilinear',
                      bind = TRUE)  %>%
        st_as_sf() %>%
        as_tibble() %>%
        mutate(location = ifelse(loc == 'Dayton', loc, 'HREC'), 
               burn = fire$burn) %>%
        select(location, burn, dNBR, ndvi) %>%
        bind_rows(RxBurnIndices, .) -> RxBurnIndices
    }
    
    #  save(RxBurnIndices, file = './data/RxBurnIndices.Rdata')
    
    # Regional rangeland raster
    # rr <- terra::rast("S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/USrangelands.tif")
    # region <- read_sf('./data/SeverityComparison.gpkg', 'StudyRegion') %>%
    #             st_transform(st_crs(rr))
    # 
    # r_rr <- rr %>% terra::crop(region)
    # terra::writeRaster(r_rr, "S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/NoPlainsRange.tif")
    
    r_rr <- terra::rast("S:/DevanMcG/GIS/SpatialData/US/USFS/ReevesRangelands/NoPlainsRange.tif")

# Regional comparison wildfires 
    
    st_layers('./data/SeverityComparison.gpkg')

    wf_sf <- read_sf('./data/SeverityComparison.gpkg', 'GreatPlainsModifiedWF')
    
    wf_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison/CompWildfires/precip'
    
    wf_images <- 
      tibble(file = list.files(wf_dir, pattern = "\\.tiff$", ignore.case = TRUE) ) %>% 
      mutate(info = str_remove(file, '.tiff')) %>%
      separate(info, into = c('FireCode', 'period', 'ImageDate'), sep = '_') %>%
      mutate(FireCode = str_replace(FireCode, '-', '_'))
    
    wildfires <- wf_sf %>% 
                filter(FireCode %in% unique(wf_images$FireCode) ) %>%
                st_transform(albersEAC) %>%
                mutate(AreaHa = st_area(.), 
                       AreaHa = as.numeric(AreaHa) * 0.0001) 
    
    unique(wf_images$FireCode)
    unique(wf_images$file)
    unique(wildfires$FireCode)
    
    WfBurnIndices <- tibble() 
{
  begin = Sys.time()
    for(i in 1:length(wildfires$FireCode)) {
      # Get fire
        fire <-  wildfires %>% 
                  filter(FireCode == unique(wildfires$FireCode)[i]) %>%
                  st_transform(st_crs(r_rr))
      # create gridded sample points
        buff <- fire  %>% st_buffer(-20)
        # cell size factor scaled to total area
          cs = case_when(
            fire$AreaHa < 10 ~ 25, 
            between(fire$AreaHa, 10, 50) ~ 36,
            between(fire$AreaHa, 50, 100) ~ 50, 
            between(fire$AreaHa, 100, 200) ~ 100, 
            between(fire$AreaHa, 200, 300) ~ 125, 
            between(fire$AreaHa, 300, 500) ~ 150, 
            between(fire$AreaHa, 500, 700) ~ 200, 
            between(fire$AreaHa, 700, 3000) ~ 300, 
            between(fire$AreaHa, 3000, 5000) ~ 500, 
            between(fire$AreaHa, 5000, 7000) ~ 700,
            fire$AreaHa > 7000 ~ 1000 )
        
        pts <- buff %>%
          st_make_grid(cellsize = cs, 
                       square = FALSE) %>% 
          st_centroid() %>%
          st_intersection(buff) %>%
          st_as_sf() %>%
          rowid_to_column('ID')
       # Filter sample points to rangeland cells
          r_pts <-  
            terra::extract(r_rr, 
                          terra::vect(pts), 
                          df = TRUE) %>%
            filter(LABEL %in% c('Rangeland', 
                                'Transitional Rangeland', 
                                'Afforested CO') ) 
          pts %<>% filter(ID %in% r_pts$ID) %>%
                    st_transform(4326)

    # dNBR & NDVI
      # Fetch & process multi-band rasters
      # pre image
      pre_image = filter(wf_images, FireCode == fire$FireCode, , 
                         period == 'B')$file
      pre_path = paste0(wf_dir, '/', pre_image)
      pre_ras <- terra::rast(pre_path) %>%
                  terra::crop(terra::vect(fire %>% st_transform(4326)))
      pre_ras <-  (pre_ras-10000)/10000
      # post-fire
      post_image = filter(wf_images, FireCode == fire$FireCode, , 
                          period == 'A')$file
      post_path = paste0(wf_dir, '/', post_image)
      post_ras <- terra::rast(post_path) %>%
                  terra::crop(terra::vect(fire %>% st_transform(4326)))
      post_ras = (post_ras-10000)/10000
      # Calculate dNBR & replace in pre-fire raster
      d_ras = pre_ras[[1]]- post_ras[[1]]
      pre_ras[[1]] <- d_ras
      names(pre_ras) <- c('dNBR', 'ndvi')
      # Sample rasters
      sat_dat <- 
        terra::extract(pre_ras[[1:2]],
                       pts %>%
                         terra::vect(), 
                       fun = mean, 
                       method = 'bilinear',
                       bind = TRUE)  %>% 
        as_tibble()
      
    # RAP 
      product = c('vc', 'ab')
      year = as.numeric(fire$FireYear) - 1
      rap_dat <- tibble(ID = pts$ID) 
      for(p in 1:length(product)) {
         # URL for RAP type + year 
      index_url <- 
        ifelse(product[p] == 'vc', 
               'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v3/vegetation-cover-v3-' ,
               'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-biomass/v3/vegetation-biomass-v3-' )
      rap_url = paste0('/vsicurl/', index_url, year, '.tif')
      
      # Extract RAP data from URL by feature subset 
      rd <-
      terra::extract(terra::rast(rap_url), 
                     terra::vect(pts), 
                     fun = 'mean', 
                     method = 'bilinear',
                     list = FALSE,
                     na.rm = TRUE, 
                     df = TRUE) %>%
        as_tibble()  %>%
        select(-ID)
      if(product[p] == 'vc') { 
             names(rd) <- c('AFGC', 'BG', 'LTR', 'PFGC', 'SHR', 'TREE') } 
        else { names(rd) <- c('AnnProd', 'PerProd')   } 
      rd %>%
        bind_cols(rap_dat, .) -> rap_dat
      }

     full_join(sat_dat, 
               rap_dat, 
               by = 'ID') %>%
     add_column(FireCode=fire$FireCode, .before = 1 ) %>%
     bind_rows(., WfBurnIndices) -> WfBurnIndices
    }
  Sys.time() - begin 
}
    
    #  save(WfBurnIndices, file = './data/WfBurnIndices.Rdata')

    WfBurnIndices %>%
      group_by(FireCode) %>%
      summarize(pts = n() ) %>%
      arrange(desc(pts))
    
    wf_sums <- 
      WfBurnIndices %>%
        mutate(WDY = TREE + SHR, 
               HERB = AFGC + PFGC, 
               PROD = AnnProd + PerProd) %>%
        group_by(FireCode) %>%
        summarize(across(c(dNBR, ndvi, BG, WDY, HERB, PROD), 
                         list('Mean' = mean, 
                              'SEM' = function(x){sd(x)/sqrt(n( ))} ))) 
    # All points dNBR v NDVI
    WfBurnIndices %>%
      left_join(by = 'FireCode', 
                wf_sf %>% 
                  as_tibble() %>%
                  select(FireCode, L3)) %>%
      filter(dNBR > 0 & ndvi > 0) %>%
    ggplot(aes(x = ndvi, y = dNBR)) + theme_bw() +
      geom_point(pch = 1, alpha = 0.4) +
      geom_smooth(aes(group = FireCode, color = L3), 
                  method = 'lm') +
      labs(title = 'wildfires only')
   
  # Severity requires fuel 
    wf_sums %>%
      ggplot(aes(x = BG_Mean, y = dNBR_Mean)) + theme_bw() +
      geom_errorbar(aes(ymin = dNBR_Mean - dNBR_SEM, 
                        ymax = dNBR_Mean + dNBR_SEM)) +
      geom_errorbarh(aes(xmin = BG_Mean - BG_SEM, 
                         xmax = BG_Mean + BG_SEM)) +
      geom_point() +
      geom_smooth(method = 'lm') 
    
    wf_sums %>%
      filter(BG_Mean < 10) %>%
      ggplot(aes(x = BG_Mean, y = ndvi_Mean)) + theme_bw() +
      geom_errorbar(aes(ymin = ndvi_Mean - ndvi_SEM, 
                        ymax = ndvi_Mean + ndvi_SEM)) +
      geom_errorbarh(aes(xmin = BG_Mean - BG_SEM, 
                         xmax = BG_Mean + BG_SEM)) +
      geom_point() +
      geom_smooth(method = 'lm') 
    
    wf_sums %>%
      filter(PROD_Mean < 1900) %>%
      ggplot(aes(x = PROD_Mean, y = dNBR_Mean)) + theme_bw() +
      geom_errorbar(aes(ymin = dNBR_Mean - dNBR_SEM, 
                        ymax = dNBR_Mean + dNBR_SEM)) +
      geom_errorbarh(aes(xmin = PROD_Mean - PROD_SEM, 
                         xmax = PROD_Mean + PROD_SEM)) +
      geom_point() +
      geom_smooth(method = 'lm') 
    
    
# Escapes 
    esc_sf <- read_sf('./data/SeverityComparison.gpkg', 'EscapePerimeters') %>%
      mutate(Ha = st_area(.), 
             Ha = as.numeric(Ha) * 0.0001) 
    
    esc_dir = 'S:/DevanMcG/FireScience/Sentinel/SeverityComparison/escapes'
    
    esc_images <- 
      tibble(file = list.files(esc_dir, pattern = "\\.tiff$", ignore.case = TRUE) ) %>% 
      mutate(info = str_remove(file, '.tiff')) %>%
      separate(info, into = c('incident', 'period', 'ImageDate'), sep = '_') 
    EscapeBurnIndices <- tibble() 
    for(i in 1:length(esc$incident)) {
      # Get fire
      fire <-  esc_sf %>% 
        filter(incident == unique(esc_sf$incident)[i]) %>%
        st_transform(st_crs(r_rr))
      # create gridded sample points
      buff <- fire  %>% st_buffer(-20)
      # cell size factor scaled to total area
      cs = case_when(
        fire$Ha < 10 ~ 25, 
        between(fire$Ha, 10, 100) ~ 50, 
        between(fire$Ha, 100, 200) ~ 100, 
        between(fire$Ha, 200, 300) ~ 125, 
        between(fire$Ha, 300, 500) ~ 150, 
        between(fire$Ha, 500, 700) ~ 200, 
        between(fire$Ha, 700, 3000) ~ 300, 
        between(fire$Ha, 3000, 5000) ~ 500, 
        between(fire$Ha, 5000, 7000) ~ 700,
        fire$Ha > 7000 ~ 1000 )
      
      pts <- buff %>%
        st_make_grid(cellsize = cs, 
                     square = FALSE) %>% 
        st_centroid() %>%
        st_intersection(buff) %>%
        st_as_sf() %>%
        rowid_to_column('ID')
      # Filter sample points to rangeland cells
      r_pts <-  
        terra::extract(r_rr, 
                       terra::vect(pts), 
                       df = TRUE) %>%
        filter(LABEL %in% c('Rangeland', 
                            'Transitional Rangeland', 
                            'Afforested CO') ) 
      pts %<>% filter(ID %in% r_pts$ID) %>%
        st_transform(4326)
      
      # dNBR & NDVI
      # Fetch & process multi-band rasters
      # pre image
      pre_image = filter(esc_images, incident == fire$incident, 
                         period == 'B')$file
      pre_path = paste0(esc_dir, '/', pre_image)
      pre_ras <- terra::rast(pre_path) %>%
        terra::crop(terra::vect(fire %>% st_transform(4326)))
      pre_ras <-  (pre_ras-10000)/10000
      # post-fire
      post_image = filter(esc_images, incident == fire$incident, 
                          period == 'A')$file
      post_path = paste0(esc_dir, '/', post_image)
      post_ras <- terra::rast(post_path) %>%
        terra::crop(terra::vect(fire %>% st_transform(4326)))
      post_ras = (post_ras-10000)/10000
      # Calculate dNBR & replace in pre-fire raster
      d_ras = pre_ras[[1]]- post_ras[[1]]
      pre_ras[[1]] <- d_ras
      names(pre_ras) <- c('dNBR', 'ndvi')
      # Sample rasters
      terra::extract(pre_ras[[1:2]],
                     pts %>%
                       terra::vect(), 
                     fun = mean, 
                     method = 'bilinear',
                     bind = TRUE)  %>% 
        as_tibble() %>%
        add_column(incident=fire$incident, .before = 1 ) %>%
        bind_rows(., EscapeBurnIndices) -> EscapeBurnIndices
    }
    #  save(EscapeBurnIndices, file = './data/EscapeBurnIndices.Rdata')

