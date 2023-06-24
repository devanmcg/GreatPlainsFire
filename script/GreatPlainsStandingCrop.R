pacman::p_load(tidyverse, sf)
source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')

tmp_dir = tempdir()
tmp = tempfile(tmpdir = tmp_dir, fileext = ".zip")

# Get a boundary for the Great Plains (EPA L3)
  GP_L3 <- 
    read_sf('S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3', 
            'us_eco_l3_state_boundaries') %>%
    filter(NA_L1NAME == 'GREAT PLAINS') %>%
    st_transform(4269)
  GP <- st_union(GP_L3)

# Get National Grasslands
  ng_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.NationalGrassland.zip'
  
  download.file(ng_url, tmp)  
  unzip(tmp, exdir = tmp_dir)
  ng <- read_sf(tmp_dir, "S_USA.NationalGrassland")
  
# Get USFS fires 
  fp_url = 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.FinalFirePerimeter.zip'
  download.file(fp_url, tmp)  
  unzip(tmp, exdir = tmp_dir)
  fp <- read_sf(tmp_dir, "S_USA.FinalFirePerimeter") 
 
# Crop fires to NG 
  ng %>%
    st_intersection(fp)
  

  

  gp_ng <- read_sf(paste0(s_gis, '/NationalGrasslands'), 
                    'S_USA.NationalGrassland') %>%
                st_intersection(GP)
  # Filter grazing allotments to Great Plains National Grasslands
  gpng_allots <- read_sf(paste0(s_gis, '/RangeAllotments'), 
                         'S_USA.Allotment')  %>%
                  st_intersection(gp_ng %>% select(GRASSLANDN)) %>%
                  select(GRASSLANDN, ALLOTMENT_:ALLOTMEN_2)%>%
                  rename(unit = 'GRASSLANDN', 
                         allot_name = 'ALLOTMENT_',
                         allot_id = 'ALLOTMENT1',
                         allot_cn = 'ALLOTMEN_1', 
                         status = 'ALLOTMEN_2') %>%
                    mutate(unit = str_to_title(unit)) 
 # save(gpng_allots, file = './gis/Robjects/gpng_allots.Rdata')
  gpng_allots %>% as_Spatial()
# USFS fire perimeters within the Great Plains National Grasslands
  gpng_perims <- 
    read_sf(paste0(s_gis, '/FinalFirePerimeter'), 
           'S_USA.FinalFirePerimeter') %>%
      filter(between(FIREYEAR, 2006, 2017 )) %>% # RAP starts 1985, grazing data 2005
      mutate(valid = st_is_valid(.)) %>%
      filter(valid != FALSE) %>%
      select(-valid) %>%
        st_intersection(gp_ng %>% select(GRASSLANDN)) %>%
        filter(TOTALACRES >= 247.12) %>% # > 100 ha
        select(GRASSLANDN, FIRENAME, DISCOVERYD)  %>%
      rename(unit = 'GRASSLANDN', 
             fire = 'FIRENAME', 
             date = 'DISCOVERYD') %>%
      mutate(InFire = 'Y')
  
  # save(gpng_perims, file = './gis/Robjects/gpng_perims.Rdata')
  # Assign allotments to fires 
    burned_allots <- 
      gpng_perims %>%
        mutate(unit = str_to_title(unit))  %>%
        st_intersection(select(gpng_allots, -unit, -status) ) %>%
      select(unit, allot_name, allot_id, allot_cn, fire, date)
    
# Establish a 6-mile unburned buffer around each GP NG fire perimeter
# Clip out fire perimeter from buffer
# Combine unburned buffer and fire perimeter 
# Determine span of grazing 
  BurnBuffAllots <- 
      gpng_perims %>%
        select(unit, fire, date) %>%
        st_transform(26915) %>%
          st_buffer(10000) %>%
        st_transform(4269)  %>%
      mutate(unit = str_to_title(unit))  %>%
      st_difference(st_union(gpng_perims)) %>%
      mutate(InFire = "N")  %>%
      bind_rows(gpng_perims) %>%
      st_intersection(select(gpng_allots, -unit, -status) ) %>%
      mutate(BurnedAllotment = ifelse(allot_id %in% burned_allots$allot_id, 
                                      'Y', 'N')) %>%
      select(unit, allot_name, allot_id, allot_cn, 
             fire, date, BurnedAllotment, InFire)  %>%
      # mutate(GrY0 = as.Date(date, format = '%Y') - lubridate::years(1), 
      #        GrY6 = GrY0 + lubridate::years(6)) %>% 
      # mutate(across(GrY0:GrY6, ~format(., '%Y'))) %>%
      filter(! st_geometry_type(.) %in% c("LINESTRING","MULTILINESTRING"))  %>%
    as_Spatial() 
    select(geometry)
    as_tibble() %>%
    mutate(across(c(GrY0, GrY6), ~as.numeric(.))) %>%
    mutate(GrY1 = GrY0 + 1, 
           GrY2 = GrY1 + 1, 
           GrY3 = GrY2 + 1, 
           GrY4 = GrY3 + 1, 
           GrY5 = GrY4 + 1) %>%
    pivot_longer(names_to = 'GrY', 
                 values_to = 'GrazeYear', 
                 cols = c(GrY0,GrY1, GrY2, GrY3, GrY4, GrY5, GrY6)) %>% 
    mutate(InFire = "N") %>%
    group_by(fire, allot_id) %>%
    arrange(GrazeYear) %>%
    ungroup() %>%
    select(-GrY, -allot_cn) %>%
    st_as_sf(crs = st_crs(buffer_allots))
  
  # Load grazing data from USFS
  grazing <- 
    readxl::read_xlsx('./data/GrazingRecordsNG.xlsx', 
                      'RawData') %>%
    rename(district = 'MANAGING_ORG_NAME', 
           unit = 'PROC_UNIT_NAME', 
           allot_id = 'RMU_ID',
           allot_name = 'RMU_NAME', 
           allot_cn = 'RMU_CN', 
           GrazeYear = 'GRZ_YEAR')  %>%
    mutate(across(c(district, unit), ~str_to_title(.)) ) %>%
    select(-MANAGING_ORG, -PERMIT_CN, -PERMIT_ID) %>%
    filter(complete.cases(.)) %>%
    group_by(unit, allot_name, allot_id, GrazeYear) %>%
    summarize(grazed = ifelse(n() > 0, "Y", "N"), 
              .groups = 'drop') 
  
  # Identify whether allotments were grazed, by year
  BurnAllotsGrazing <-
    BurnBuffAllots %>%
    left_join(grazing, 
              by = c("unit", 'allot_name', "allot_id", "GrazeYear")) %>%
    mutate(grazed = replace_na(grazed, 'N')) 
  
  # save(BurnAllotsGrazing, file = './gis/Robjects/BurnAllotsGrazing.Rdata')
  
  
  # A little example with Brown Fire from LMNG
  brown_fire <- gpng_perims %>% filter(fire == "Brown") %>%
    mutate(InFire = "Y") %>%
    select(InFire)
  
  BurnAllotsGrazing %>%
    filter(fire == "Brown") %>%
    ggplot() + theme_map() +
    geom_sf(aes(fill = grazed, 
                color = BurnedAllotment)) +
    geom_sf(data = brown_fire, color = "blue", fill = NA) +
    facet_wrap(~GrazeYear)
  

