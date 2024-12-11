pacman::p_load(tidyverse, sf)

# Creating

  # set a file path to where your spatial data live
    fp = 'C:/Path/To/Spatial/Data'
  
  # import a shapefile
    shp <-
      paste0(fp, '/ShapefileName') %>%  # shapefile name
        read_sf('Layer') # specific layer
    
  # convert layer to GeoPackage
    shp %>%
      st_write(paste0(fp, '/GeoPackageName.gpkg', 'LayerName')
               
# Loading
  # specify the GeoPackage you want
      gpkg = paste0(fp, '/GeoPackageName.gpkg')
  # View available layers
      st_layers(gpkg ) 
  # load a layer
      layer <- st_read(gpkg, 'LayerName') 
      
      
shps <- list.files(fp)

for(i in 1:length(shps)){
  f = shps[i]
  shp <-
    paste0(fp, '/', f) %>%
    st_write(paste0(fp, '/GeoPackageName.gpkg', f)
  
  
}


fp = 'C:/Path/To/Spatial/Data'
shps <- list.files(fp, pattern = "\\.shp$") # should return unique list of shapefile names??

for(i in 1:length(shps)){
  f = shps[i]
  lyrs = st_layers(f)
  for(l in 1:length(lyers)){
    lyr = lyrs[l]
  paste0(fp, '/', f) %>%
    read_sf(lyr) %>%
    st_write(paste0(fp, '/GeoPackageName.gpkg', f))
  }
}