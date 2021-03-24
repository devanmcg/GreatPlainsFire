pacman::p_load(tidyverse, sf)

gis_fp = 'C:/Users/devan.mcgranahan/GithubProjects/GreatPlainsFire/gis'

GP_L3 <- 
  read_sf(paste0(gis_fp, '/L3ecoregions'), 'us_eco_l3') %>%
    filter(NA_L1NAME == 'GREAT PLAINS')
# save(GP_L3, file=paste0(gis_fp, '/Robjects/GP_L3.Rdata'))

albersEAC = st_crs(GP_L3)

GP_WF <- 
  read_sf(paste0(gis_fp, '/mtbs'), 'mtbs_perims_DD') %>%
      st_transform(albersEAC) %>%
        st_intersection(GP_L3) %>%
          filter(Incid_Type == "Wildfire")

# save(GP_WF, file=paste0(gis_fp, '/Robjects/GP_WF.Rdata'))

ggplot() + theme_bw() +
  geom_sf(data = GP_L3) +
  geom_sf(data = GP_MTBS, fill = "lightgrey")
