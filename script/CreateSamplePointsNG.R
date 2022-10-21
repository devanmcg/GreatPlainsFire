pacman::p_load(tidyverse, sf, multidplyr)

load('./CommonFiles/albersEAC.Rdata')

# Shapefile of GP NPS units
gp_nps_sf <- read_sf('./NPS/SpatialData/GreatPlainsNPS/NPSunitsForAnalysis', 
                     'gp_nps_LL') %>%
  st_transform(albersEAC)

# Shapefile of soils & eco site data for all GP NPS units
AllUnits <- read_sf('./NPS/SpatialData/GreatPlainsNPS/SoilTextureEcoSite', 
                    'nps_soils') %>% 
  st_transform(albersEAC) %>%
  st_intersection(gp_nps_sf) 

{ 
  begin = Sys.time()
  cl <- new_cluster(4)
  cluster_library(cl, c("tidyverse", "sf"))
  # Find dominant ecological sites (summing to at least 75% of allotment's area)
  DomES <-
    AllotmentSites %>%
    filter(!is.na(site), site != "Non-site") %>% 
    mutate(area = st_area(.)) %>%
    group_by(unit, allot_name, site) %>%
    partition(cl) %>%
    summarize(TotalArea = sum(area) ) %>%
    arrange(desc(TotalArea)) %>%
    mutate(PropArea = TotalArea / sum(TotalArea), 
           c = accumulate(as.numeric(PropArea), ~if_else(.x>=0.75, .y, .x+.y)) )  %>% 
    filter(c >= c[1]) %>%
    select(-c, -PropArea) %>%
    ungroup() %>%
    collect() 
  Sys.time() - begin
  }

# Determine the number of points necessary to sample ~ 50% of dominant ES area

points <- 
  DomES %>%
  select(-TotalArea) %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  filter(as.numeric(area) >= 20000) %>%
  group_by(unit) %>%
  summarize(area = sum(area)) %>%
  mutate(NumPoints = (area * 0.5) / 7854, 
         NumPoints = (round(as.numeric(NumPoints), 0))) %>%
  as_tibble() %>%
  select(unit, NumPoints)

# Assign random points and create sample circles
pacman::p_load(foreach, doParallel)

DomES <- DomES %>% st_sf() 

{ 
  begin = Sys.time()
  cl <- makeCluster(length(unique(DomES$unit))) 
  registerDoParallel(cl)
  
  NPSSamplePoints <- 
    foreach(u=1:length(unique(DomES$unit)), 
            .combine='rbind', 
            .packages = c('tidyverse', 'sf'), 
            .errorhandling = 'remove') %dopar% {
              un <- filter(DomES, unit == unique(unit)[u])
              un %>%
                st_cast("MULTIPOLYGON") %>%
                st_cast("POLYGON") %>%
                st_buffer(-100) %>%
                filter(!is.na(st_dimension(.))) %>%
                st_sample(filter(points, unit == unique(unit)[u])$NumPoints, 
                          type = "regular", 
                          exact = TRUE, 
                          by_polygon = TRUE) %>%
                st_sf() %>% 
                # st_buffer(50) %>%
                mutate(unit = unique(un$unit))
            }
  stopCluster(cl)
  Sys.time() - begin 
}

SitesSamplePoints <- 
  NPSSamplePoints %>%
  st_intersection(DomES) %>%
  select(-unit.1, -TotalArea) %>%
  rowid_to_column("point") %>%
  st_transform(4326) 

SitesSamplePoints %>%
  st_write('./NPS/SpatialData/SamplePoints/NPSSamplePoints.shp', append = F)
