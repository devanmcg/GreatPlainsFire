pacman::p_load(tidyverse, sf, rgee, rgeeExtra)
#ee_Initialize(drive = TRUE)
ee$Initialize(project='ee-devanmcg')
rgeeExtra::extra_Initialize()

# CGREC scene days to fetch from GEE

  passes <- 
    readxl::read_xlsx('./data/BurnData.xlsx', 'SentinelDays') %>%
    unite('PassDate', sep = '-') %>%
    mutate(PassDate = as.Date(PassDate, '%Y-%m-%d'), 
           PassDate = as.character(PassDate)) 
  
  passes <- as.vector(unique(passes))[[1]]

# CGREC boundaries 
  cgrec_gpkg = './gis/boundaries/CGREC_PBG_26914.gpkg'
  st_layers(cgrec_gpkg ) 
  
  pastures <- st_read(cgrec_gpkg, 'Pastures') %>%
                mutate(Unit = recode(Unit, 'Bob' = 'North', 'Barker' = 'South'))
  patches <- st_read(cgrec_gpkg, 'PasturePatches') %>%
                mutate(Unit = recode(Unit, 'Bob' = 'North', 'Barker' = 'South'))
  fires <- st_read(cgrec_gpkg, 'FirePerimeters') %>%
            mutate(Year = as.factor(Year), 
                   unit = recode(unit, 'Bob' = 'North', 'Barker' = 'South'))
  # Create GEE geometries
    CGREC_PBG <-
      pastures %>%
        st_union() %>%
        st_transform(4326) %>% 
        sf_as_ee()
    CGREC_NorthPBG <-
      pastures %>%
        filter(Unit == 'North') %>%
        st_union() %>%
        st_transform(4326) %>% 
        sf_as_ee()
    CGREC_SouthPBG <-
      pastures %>%
        filter(Unit == 'South') %>%
        st_union() %>%
        st_transform(4326) %>%
        sf_as_ee()
    refuge_ee <- st_read(cgrec_gpkg, 'SamplePoints') %>%
      filter(location == 'Refuge')  %>%
      st_transform(4326) %>%
      st_union() %>%
      st_bbox() %>%
      st_as_sfc(., crs=4326) %>%
      sf_as_ee()

# Turn on GEE connection
  #ee_Initialize(drive = TRUE)
  ee$Initialize(project='ee-devanmcg')
  extra_Initialize()
  
  years = 1990:2023
  seasons = tibble(season = c('spring', 'summer'), 
                   start = c('04-20', '07-20'), 
                   end = c('06-10', '09-10') ) 
  collections = tibble(start = c(1990, 1999, 2013), 
                       end = c(1998, 2012, 2023), 
                       collection = c('LS5', 'LS7', 'LS8'), 
                       ee_tag = c("LANDSAT/LT05/C02/T1_L2","LANDSAT/LE07/C02/T1_L2","LANDSAT/LC08/C02/T1_L2") )
  
  for(y in 1:length(years)) {
  for(s in 1:2) {
    StartDate = paste0(years[y], '-', slice(seasons, s)$start )
    EndDate = paste0(years[y], '-', slice(seasons, s)$end )
    collection <- case_when(
      years[y] %>% between(collections$start[1], collections$end[1]) ~ collections$ee_tag[1], 
      years[y] %>% between(collections$start[2], collections$end[2]) ~ collections$ee_tag[2],
      years[y] %>% between(collections$start[3], collections$end[3]) ~ collections$ee_tag[3]
    )
  spectra_ee_image <- 
    ee$ImageCollection(collection)$
    filterBounds(refuge_ee) %>%
    ee$ImageCollection$filterDate(StartDate, EndDate) %>%
    ee$ImageCollection$Extra_preprocess() %>%
    ee$Image$Extra_maskClouds(prob = 75,buffer = 300,cdi = -0.5) %>%
    ee$Image$Extra_spectralIndex(c("NDVI",'NDMI',"MSI")) %>%
    ee$ImageCollection$toBands() 
    ImageDate <- spectra_ee_image$bandNames()$getInfo()[1] %>% str_sub(13,20)
    FileDate = paste(str_sub(ImageDate, 1,4), str_sub(ImageDate, 5,6), str_sub(ImageDate, 7,8), sep = '_')
    names(spectra_ee_image) <- c("NDVI",'NDMI',"MSI")

   ee_as_rast(
      image = spectra_ee_image,
      region = refuge_ee, 
      dsn = paste0('./landsat/', paste0('refuge', FileDate)),
      timePrefix = FALSE,
      via = "drive", 
      scale = 30 )
    ee_as_rast(
      image = spectra_ee_image,
      region = CGREC_NorthPBG, 
      dsn = paste0('./landsat/', paste0('CGREC_North_', FileDate)),
                   timePrefix = FALSE,
                   via = "drive", 
                   scale = 30 )
      
  
  
# Download Image Collections as .tif
  pacman::p_load(foreach, doSNOW) 
  ee$Initialize(project='ee-devanmcg')
  extra_Initialize()
    
 LoopCounter = 0 
  for(i in 91:length(passes)) {
  spectra_ee_ic <- 
    ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
    filterBounds(CGREC_PBG) %>%
    ee$ImageCollection$Extra_closest(passes[i],  7, "day") %>% 
    #ee$ImageCollection$filterDate("2020-10-02") %>% 
    ee$ImageCollection$Extra_preprocess() %>%
    ee$Image$Extra_maskClouds(prob = 75,buffer = 300,cdi = -0.5) %>%
    ee$Image$Extra_spectralIndex(c("NBR", 'CSI', "NDVI", 'NDMI', 'MSAVI', 'SAVI'))
    ImageDate = spectra_ee_ic$getInfo()$features[[1]]$properties$DATE_ACQUIRED
  
    ee_imagecollection_to_local(spectra_ee_ic, 
                                region = CGREC_NorthPBG, 
                                dsn = paste0('./landsat/CGREC_North_', ImageDate),
                                timePrefix = FALSE,
                                via = "drive", 
                                scale = 30)
    ee_imagecollection_to_local(spectra_ee_ic, 
                                region = CGREC_SouthPBG, 
                                dsn = paste0('./landsat/CGREC_South_', ImageDate),
                                timePrefix = FALSE,
                                via = "drive", 
                                scale = 30)
    LoopCounter = LoopCounter + 1
    if (LoopCounter %% 10 == 0) {
      Sys.sleep(10)
      } else {  
      Sys.sleep(0)   
      }
          }

  
  if (counter %% 10 == 0) {
      Sys.sleep(10)
  } else {  Sys.sleep(0)   }
  vis_ee <- 
    ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
    filterBounds(CGREC_NorthPBG) %>%
    ee$ImageCollection$Extra_closest(passes[i],  7, "day") %>% 
    #ee$ImageCollection$filterDate("2020-10-02") %>% 
    ee$ImageCollection$Extra_preprocess() %>%
    ee$Image$Extra_maskClouds(prob = 75,buffer = 300,cdi = -0.5) %>%
    ee$ImageCollection$map(function(x) x$select(c('SR_B2', 'SR_B3', 'SR_B4'))) %>% 
    ee$ImageCollection$toBands() 
  ImageDate <- vis_ee$bandNames()$getInfo()[1] %>% str_sub(13,20)
  FileName = paste('CGREC_NorthPBG', str_sub(ImageDate, 1,4), str_sub(ImageDate, 5,6), str_sub(ImageDate, 7,8), sep = '_')
  names(vis_ee) <- c('Blue', 'Green', 'Red')
  
  vis_rast <- ee_as_rast(
    image = vis_ee,
    region = CGREC_NorthPBG, 
    dsn = FileName,
    timePrefix = FALSE,
    via = "drive", 
    scale = 10 )
  terra::plot(vis_rast)
  
  ee_imagecollection_to_local(burn_ee, 
                              region = CGREC_NorthPBG, 
                              dsn = FileName,
                              timePrefix = FALSE,
                              via = "drive", 
                              scale = 10)
  
  

    ggplot() + theme_void() + 
      geom_stars(data = burn_rast %>% st_as_stars() ) 
    
  