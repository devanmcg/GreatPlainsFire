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
    
    st_read(cgrec_gpkg, 'SamplePoints') %>%
      filter(location == 'Refuge')  %>%
      st_transform(4326) %>%
      st_union() %>%
      st_bbox() %>%
      st_as_sfc(., crs=4326) %>%
      write_sf('./data/AOI/refuge.shp')

# Turn on GEE connection
  #ee_Initialize(drive = TRUE)
  ee$Initialize(project='ee-devanmcg')
  extra_Initialize()
  
  years = 2019:2023
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
    ImageCol <- case_when(
      years[y] %>% between(collections$start[1], collections$end[1]) ~ collections$ee_tag[1], 
      years[y] %>% between(collections$start[2], collections$end[2]) ~ collections$ee_tag[2],
      years[y] %>% between(collections$start[3], collections$end[3]) ~ collections$ee_tag[3]
    )
    
    cloudMaskL457 <- function(image) {
                            qa <- image$select("pixel_qa")
                                  cloud <- qa$bitwiseAnd(32L)$
                                            And(qa$bitwiseAnd(128L))$
                                            Or(qa$bitwiseAnd(8L))
                mask2 <- image$mask()$reduce(ee$Reducer$min())
                image <- image$updateMask(cloud$Not())$updateMask(mask2)
                image$normalizedDifference(list("B4", "B3"))
    }
    
    sd = ee$Date(StartDate) 
    sd$get(value) $value()
    
    sd$format('yyyy-MM-dd')$getInfo()
    
    # Landsat 4-7: NDVI = (Band 4 – Band 3) / (Band 4 + Band 3)
    # Landsat 8-9: NDVI = (Band 5 – Band 4) / (Band 5 + Band 4)
    
    # Grab collection and filter clouds
     comp <- 
        ee$ImageCollection(ImageCol)$
        filterBounds(refuge_ee) %>%
        ee$ImageCollection$filterDate(StartDate, EndDate) %>%
        ee$Image$Extra_maskClouds(prob = 75,buffer = 300,cdi = -0.5) %>%
        ee$ImageCollection$mean() %>%
        ee$Image$select(c("SR_B5", "SR_B4")) 
   
      comp_sc <- (comp * 0.0000275) - 0.2
      ndvi = comp_sc$normalizedDifference(c("SR_B5", "SR_B4"))
   
   ee_as_stars(
     image = ndvi,
     region = refuge_ee, 
     dsn = paste0('./landsat/', paste0('refuge_', years[y], '_', seasons$season[s])),
     timePrefix = FALSE,
     via = "drive", 
     scale = 30 )
  
   # Visualize squared NDVI on map
   Map$centerObject(ndvi)
   Map$addLayer(
     eeObject = ndvi, 
     visParams = list(
       min = -1, 
       max = 1, 
       palette = c("brown", "yellow", "green")
     ),
     name = "NDVI"
   )
  
  task_img <- ee_image_to_drive(
    image = ndvi,
    fileFormat = "GEO_TIFF",
    region = refuge_ee,
    fileNamePrefix = paste0('refuge_', years[y], '_', seasons$season[s])
  )
  
  task_img$start()
  ee_monitoring(task_img)
   
   names(img)
      
     img_sc <- (img * 0.0000275) - 0.2
     
     ndvi <- ((img_sc[["SR_B5"]] - img_sc[["SR_B4"]]) / (img_sc[["SR_B5"]] + img_sc[["SR_B4"]]))
     names(ndvi) <- "NDVI"
     
     img$bandNames()$getInfo()
    
    ndvi$bandNames()$getInfo()
    

    tif = "C:/Users/devan.mcgranahan/Downloads/refuge_2019_spring_2024_10_28_18_04_37.tif"
    
    tif = "C:/Users/devan.mcgranahan/Downloads/refuge_2019_comp.tif"
    ras <- terra::rast(tif)
    names(ras) <- c('B5', 'B4')
    ras <-(ras * 0.0000275) - 0.2
    
    terra::plot(ras)
    
    ndvi <- ((ras[["B5"]] - ras[["B4"]]) / (ras[["B5"]] + ras[["B4"]]))
    names(ndvi) <- "NDVI"
    
    
    terra::plot(ndvi)
    
   

  
  
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
    
    dates <- vector() 
    
    for(i in 1:31) {
    '1991-04-20' %>%
      as.Date('%Y-%m-%d') %>%
      + years(i) %>%
      c(dates) -> dates }
    
    for(i in 1:31) {
      '1991-07-20' %>%
        as.Date('%Y-%m-%d') %>%
        + years(i) %>%
        c(dates) -> dates }
    
    arrange(desc(dates))
    
    sort(dates)
  