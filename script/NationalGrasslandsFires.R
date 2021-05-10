pacman::p_load(tidyverse, sf)

source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')

gis_fp = 'C:/Users/devan.mcgranahan/GithubProjects/GreatPlainsFire/gis'

load(file=paste0(gis_fp, '/Robjects/GP_WF.Rdata'))

gp_fires <-
  GP_fires %>%
    st_transform(26915) 

gp_fires_2k <-
  GP_fires %>% 
    st_buffer(2000) 

  # st_write(gp_fires, paste0(gis_fp, '/BufferedPerimeters/GP_UTM15.shp'), append = FALSE)
  # st_write(gp_fires_2k, paste0(gis_fp, '/BufferedPerimeters/BufferedGP_UTM15.shp'), append = FALSE)


ggplot() + theme_map() + 
  geom_sf(data = gp_wf_2k %>%
                  slice(1:5),
          aes(fill = Incid_Type),
          fill = 'darkblue') %>%
  geom_sf(data = GP_WF %>%
            slice(1:5) %>%
            st_transform(26915),
          aes(fill = Incid_Type),
          fill = 'lightblue')

NG_fires <-
  read_sf('S:/DevanMcG/GIS/SpatialData/US/USFS/NationalGrasslands', 
          'S_USA.NationalGrassland') %>% 
  st_transform(26915) %>%
  st_intersection(gp_fires)

# st_write(NG_fires, paste0(gis_fp, '/BufferedPerimeters/NGfires_UTM15.shp'), append = FALSE)

# While this starts the buffer process, 
# a better (dissolved) buffer was created in QGIS: 
# S:/DevanMcG/GIS/SpatialData/GreatPlains/NGfireBuffer_UTM15.shp
NG_fires_2k <-
  read_sf('S:/DevanMcG/GIS/SpatialData/US/USFS/NationalGrasslands', 
          'S_USA.NationalGrassland') %>% 
    st_transform(26915) %>%
    st_intersection(NG_fires) %>% 
  st_buffer(2000) 

NG_fires_2k %>% 
  st_transform(4326) %>%
  select(GRASSLANDN, Incid_Name, Ig_Date) %>%
  st_write('S:/DevanMcG/GIS/SpatialData/GreatPlains/NGfireBuffs_LL.shp', append = FALSE)

# st_write(NG_fires_2k, paste0(gis_fp, '/BufferedPerimeters/BufferedNGfires_UTM15.shp'), append = FALSE)

NG_fires_2k <-
read_sf('S:/DevanMcG/GIS/SpatialData/GreatPlains', 
        'NGfiresBuffered_UTM15') %>% 
  st_transform(4326) %>%
  st_write('S:/DevanMcG/GIS/SpatialData/GreatPlains/NGfireBuffer_LL.shp')
  

gp_fires %>%
    st_intersection(NG_wf_2k) %>%
      st_write(paste0(gis_fp, '/BufferedPerimeters/NG_Fires_UTM15.shp'))

ggplot(NG_wf_2k) + theme_bw() +
  geom_sf() 
  
