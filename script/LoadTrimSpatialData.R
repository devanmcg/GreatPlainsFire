pacman::p_load(tidyverse, sf)

gis_fp = 'C:/Users/devan.mcgranahan/GithubProjects/GreatPlainsFire/gis'

GP_L3 <- 
  read_sf('S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3', 
          'us_eco_l3_state_boundaries') %>%
    filter(NA_L1NAME == 'GREAT PLAINS')
# save(GP_L3, file=paste0(gis_fp, '/Robjects/GP_L3.Rdata'))

albersEAC = st_crs(GP_L3)

GP_fires <- 
  read_sf('S:/DevanMcG/GIS/SpatialData/US/mtbs', 
          'mtbs_perims_DD') %>%
      st_transform(albersEAC) %>%
        st_intersection(GP_L3) %>%
          filter(Incid_Type %in% c('Wildfire', 'Prescribed Fire', 'Complex'))

# save(GP_WF, file=paste0(gis_fp, '/Robjects/GP_WF.Rdata'))

ggplot() + theme_bw() +
  geom_sf(data = GP_L3) +
  geom_sf(data = GP_MTBS, fill = "lightgrey")
