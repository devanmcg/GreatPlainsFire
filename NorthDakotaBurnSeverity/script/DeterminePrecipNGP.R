pacman::p_load(tidyverse, magrittr, climateR, foreach, doSNOW)

# study area 
NGP <- read_sf('./data/MapStuff.gpkg', 'NGP_states')

NGP_grid <- 
NGP %>%
  st_make_grid(cellsize = c(20000, 20000)) %>%
  st_intersection(NGP)  %>% 
  st_as_sf() %>%
  rowid_to_column('cell') 
ggplot() + geom_sf( )  + 
  geom_sf(data = NGP, fill = NA, color = 'blue')


{
  cores= parallel::detectCores()
  cl <- makeCluster(cores) 
  registerDoSNOW(cl )
  
  begin = Sys.time()
  NGP_ppt <- 
    foreach(i=1:length(NGP_grid$cell), 
            .combine = bind_rows, 
            .errorhandling = 'remove', 
            .inorder = FALSE, 
            .packages = c('tidyverse', 'sf', 'climateR')) %dopar% {
              getTerraClim(
                AOI = NGP_grid %>%
                      slice(i) %>%
                       st_centroid() , 
                varname = 'ppt',
                startDate = '2017-01-01', 
                endDate = '2024-12-31'  ) %>% 
                as_tibble() %>%
                separate(date, into = c('year', 'month', 'day'), sep = "-") %>%
                 group_by(year) %>%
                summarize(ppt = sum(ppt_total, na.rm = TRUE)) %>%
                summarize(ppt = mean(ppt, na.rm = TRUE)) %>%
                add_column(cell = i)
            }
  stopCluster(cl)
  Sys.time() - begin 
  }

ppt_dat$ngp_ppt <-  
NGP_grid %>%
  st_intersection(NGP %>% 
                    select(NA_L3NAME)) %>%
  mutate(Area = st_area(.)) %>%
  group_by(cell, NA_L3NAME) %>%
  summarize(Area = sum(Area)) %>%
  arrange(desc(Area)) %>%
  slice(1) %>%
  select(-Area) %>%
  merge(by = 'cell', 
        NGP_ppt) 
  

# save(ppt_dat, file = './data/ppt_dat.Rdata')
