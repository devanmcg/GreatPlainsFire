pacman::p_load(tidyverse, sf, rgee, rgeeExtra)

# CGREC scene days to fetch from GEE

  passes <- 
    readxl::read_xlsx('./data/BurnData.xlsx', 'SentinelDays') %>%
    unite('PassDate', sep = '-') %>%
    mutate(PassDate = as.Date(PassDate, '%Y-%M-%d'), 
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

# Turn on GEE connection
  #ee_Initialize(drive = TRUE)
  ee$Initialize(project='ee-devanmcg')
  extra_Initialize()


  burn_ee <- 
    ee$ImageCollection("COPERNICUS/S2_SR")$
    filterBounds(CGREC_NorthPBG) %>%
    ee$ImageCollection$Extra_closest("2020-10-04",  5, "day") %>% 
    #ee$ImageCollection$filterDate("2020-10-02") %>% 
    ee$ImageCollection$Extra_preprocess() %>%
    ee$Image$Extra_maskClouds(prob = 75,buffer = 300,cdi = -0.5) %>%
    ee$Image$Extra_spectralIndex(c("NBR", 'CSI', "NDVI", "NDREI", 'NDMI', 'MSAVI', 'SAVI')) %>%
    #ee$ImageCollection$map(function(x) x$select("B1", "B2")) %>% 
    ee$ImageCollection$toBands() 
    ImageDate <- burn_ee$bandNames()$getInfo()[1] %>% str_sub(1,8)
    FileName = paste('CGREC_NorthPBG', str_sub(ImageDate, 1,4), str_sub(ImageDate, 5,6), str_sub(ImageDate, 7,8), sep = '_')
    names(burn_ee) <- c("NBR", 'CSI', "NDVI", "NDREI", 'NDMI', 'MSAVI', 'SAVI')
  
  burn_rast <- ee_as_rast(
    image = burn_ee,
    region = CGREC_NorthPBG, 
    dsn = FileName,
    timePrefix = FALSE,
    via = "drive", 
    scale = 10 )
  terra::plot(burn_rast)
  
  ee_imagecollection_to_local(burn_ee, 
                              region = CGREC_NorthPBG, 
                              dsn = FileName,
                              timePrefix = FALSE,
                              via = "drive", 
                              scale = 10)
  
  

    ggplot() + theme_void() + 
      geom_stars(data = burn_rast %>% st_as_stars() ) 
    
  