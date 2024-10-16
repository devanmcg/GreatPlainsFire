pacman::p_load(tidyverse, sf, rgee)

lem <- read_sf('C:/Users/devan.mcgranahan/USDA/Rangeland responses to fire - Patch burning/Lemonade Fire/mapping/SpatialData/LemonadePerimeter', 
               'LemonadePerimeter') %>%
                st_transform(4326)
lem_box <- st_as_sfc(st_bbox(lem), crs=4326)

lem_box_ee <- lem_box %>% sf_as_ee()

lem_geom <- ee$Geometry$Rectangle(
                coords = st_bbox(lem),
                proj = "EPSG:4326",
                geodesic = FALSE)

lem_ee <- ee$ImageCollection('COPERNICUS/S2_SR') %>% 
            ee$ImageCollection$filterDate('2022-09-01', '2022-09-30') %>% 
             #ee$ImageCollection$map(function(x) x$select('B1')) %>% 
            ee$ImageCollection$toBands() 

# Recovery indices 
ee$Image$Extra_spectralIndex(c("NDVI", "NDREI", 'NDMI', 'MSAVI', 'SAVI')) 
  
  burns_ee <- ee$ImageCollection("COPERNICUS/S2_SR")$
                filterBounds(lem_box_ee) %>%
                ee$ImageCollection$filterDate('2022-09-01', '2022-09-30') %>% 
                ee$ImageCollection$Extra_preprocess() %>%
                ee$Image$Extra_spectralIndex(c("NBR", "NBR2", 'CSI')) %>% 
                ee$ImageCollection$toBands() 
names(burns_ee) <- c("NBR", "NBR2", 'CSI')
burns_ee$bandNames()$getInfo()
burn_rast <- ee_as_rast(
  image = burn_ee,
  region = lem_box_ee, 
  via = "drive" )
terra::plot(burn_rast)
  
#
# This works
  burn_ee <- ee$ImageCollection("COPERNICUS/S2_SR")$
                filterBounds(lem_box_ee) %>%
                ee$ImageCollection$Extra_closest("2022-09-22",  2, "week") %>% 
                ee$ImageCollection$Extra_preprocess() %>%
                ee$Image$Extra_spectralIndex(c("NBR", "NBR2", 'CSI')) %>% 
                ee$ImageCollection$toBands() 
  names(burn_ee) <- c("NBR", "NBR2", 'CSI')
  burn_ee$bandNames()$getInfo()
  burn_rast <- ee_as_rast(
                image = burn_ee,
                region = lem_box_ee, 
                dsn = '20220901',
                timePrefix = FALSE,
                via = "drive" )
  terra::plot(burn_rast)
#


#
# This works
  closest_ee <- ee$ImageCollection("COPERNICUS/S2_SR")$
                filterBounds(lem_box_ee) %>%
                ee$ImageCollection$Extra_closest("2022-09-22",  2, "week") %>% 
                ee$ImageCollection$map(function(x) x$select(c("B1",'B2'))) %>% 
                ee$ImageCollection$toBands() 
  
  closest_ee$bandNames()$getInfo() 
  
  closest_rast <- ee_as_rast(
                    image = closest_ee,
                    region = lem_box_ee, 
                    via = "drive" )
  terra::plot(closest_rast)
#
#
# This works
  db <- 'COPERNICUS/S2_SR/20220921T180039_20220921T180538_T13TDM'
  image <- ee$Image(db)
  image$bandNames()$getInfo()
  
  image_B1_B2 <- image$select(c("B1",'B2'))
  
  B1_rast <- ee_as_rast(
              image = image_B1_B2,
              region = lem_box_ee, 
              via = "drive" )
  
  terra::plot(B1_rast)
#

lem_ee2 <- lem_ee[[1:12]]

lem_dat <- ee_extract(x = lem_ee, 
                      y = lem['section'], 
                      sf = TRUE)


pre_ee <- ee$ImageCollection("COPERNICUS/S2_SR")$
          filterBounds(lem_box_ee) %>%
  ee$ImageCollection$Extra_closest("2022-09-22",  2, "week") %>% 
          ee$ImageCollection$toBands() 

pre_ee$bandNames()$getInfo() 

ee_s2$size()$getInfo() # 126

# Get the first 5 elements
ee_get(ee_s2, index = 0:5)$size()$getInfo() # 5

pre_ee %>% ee_extract(y = lem_box_ee, sf = TRUE) %>% ggplot() + geom_sf() 
# Examples 

pacman::p_load(tidyverse, sf, rgee, exploreRGEE)
ee_Initialize()

lem_NBR <- 
get_sent2(
  lem_box, 
  method = 'S2_2A',
  param = 'NBR',
  stat = "max",
  cloud_mask = TRUE,
  startDate = '2022-09-01',
  endDate = '2022-09-30',
  mask = FALSE,
  m.low = NULL,
  m.high = NULL,
  c.low = 1,
  c.high = 12
)

lem_NBR %>%  ee$ImageCollection$toBands() ee_viz(band = 'NBR_max')
lem_NBR %>% ee_extract(band = 'NBR_max')

huc <- exploreRGEE::huc

ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
                   endDate = '2018-12-31', c.low = 6, c.high = 11)

ld8 %>% ee_viz(scale = 30, band = 'NDVI', palette = 'RdYlGn')

custer_county <- AOI::aoi_get(state = "Montana", county = "custer")

npp <- get_npp(custer_county, method = 'terra', param = 'annualNPP', stat = 'median', startDate = '1986-01-01', endDate = '2016-01-01')
npp %>% ee_viz(scale = 250)

pacman::p_load(tidyverse, sf, rgee, magick)
pacman::p_load_gh("r-earthengine/rgeeExtra") 

ee_Initialize()
extra_Initialize()

mask <- lem %>% sf_as_ee()
region <- mask$geometry()$bounds()

col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')

col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})
distinctDOY <- col$filterDate('2023-04-01', '2023-10-31')
filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')
join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))

comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})

visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})

gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)

dates_modis_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::month() %>% # Get the month component of the datetime
  '['(month.abb, .)

animation <- ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")
animation %>%
  ee_utils_gif_annotate(
    text = "NDVI: MODIS/006/MOD13A2",
    size = 15, color = "white",
    location = "+10+10"
  ) %>%
  ee_utils_gif_annotate(
    text = dates_modis_mabbr,
    size = 30,
    location = "+290+350",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  )


s2_indices <- ee$ImageCollection("COPERNICUS/S2_SR") %>%
  ee$ImageCollection$first() %>%
  ee$Image$Extra_preprocess() %>%
  ee$Image$Extra_spectralIndex(c("NDVI", "SAVI"))

names(s2_indices)

s2_indices %>% ee_extract(y = lem['section'], sf = TRUE)  %>% ee_viz( band = 'NDVI')

s2_indices
