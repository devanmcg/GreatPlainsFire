pacman::p_load(tidyverse, lubridate, magrittr, readxl,  sf)

# Combine burn dates with non-imputed fire behavior data 
  # CGREC
    BurnData_CGREC  <- 
    left_join(by = 'FireCode', 
      # Medium-rare version of the fire behavior data with burn dates
        read_csv("S:/DevanMcG/Projects/SpatialFireBehavior/data/fromMZ/CompiledData2.csv") %>%
        filter(location == "CGREC") %>%
        mutate(date = as.Date(date, format = "%m/%d/%Y"),
               L = str_remove(location, "REC"), 
               B = str_sub(block, 1,3), 
               Ps = str_replace(pasture, "[.]", ""), 
               Ps = str_sub(Ps, 1,2), 
               patch = str_replace(patch, "[.]", ""),
               sp = str_sub(patch, 3,3),
               sp = ifelse(sp == '', NA, sp), 
               patch = str_sub(patch, 1,2),
               y = format(date, "%y")) %>% 
        unite("FireCode", c(L,B,Ps,patch,sp,y), sep=".") %>%
        mutate(FireCode = str_to_upper(FireCode) ) %>%
        select(FireCode, date) %>%
        group_by(FireCode) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(date) %>%
        mutate(burn = paste0('CGREC_', row_number()) ) , 
      # Medium-done fire behavior data prior to imputation
        read_csv("S:/DevanMcG/Projects/SpatialFireBehavior/data/AnalysisDataKY.csv") %>%
        filter(location == "CG") %>%
        mutate(patch = str_replace(patch, "[.]", ""),
               sp = str_sub(patch, 3,3),
               sp = ifelse(sp == '', NA, sp),
               patch = str_sub(patch, 1,2)) %>%
        unite("FireCode", c(location,block,pasture,patch,sp,year), sep=".") %>%
        select(FireCode, plot, array, MaxC, SoilMaxC, ros) ) %>%
          group_by(FireCode, burn, date, plot) %>%
          summarise(across(c(MaxC:ros), ~mean(.x, na.rm = TRUE)), .groups = 'drop' )
    
    cgrec_bd <- 
      BurnData_CGREC %>%
      arrange(date) %>%
      mutate(PlotCode = str_remove_all(FireCode, '[0-9]'), 
             PlotCode = paste0(PlotCode, plot), 
             plot = paste0('CGREC_', row_number())) 
    
    plots_cgrec %>%
      merge(cgrec_bd) %>%
     select(location, burn, date, plot, MaxC:ros) %>%
      st_write('./data/ThermocouplePlacements.gpkg', 'plots_CGREC')

  # HREC
    BurnData_HREC  <- 
    left_join(by = 'FireCode', 
    # Medium-rare version of the fire behavior data with burn dates
      read_csv("S:/DevanMcG/Projects/SpatialFireBehavior/data/fromMZ/CompiledData3.csv") %>%
        filter(location == "HREC", block != 'Dayton') %>%
        mutate(date = as.Date(date, format = "%m/%d/%Y"),
               L = str_remove(location, "REC"), 
               B = str_sub(block, 1,3), 
               y = format(date, "%y")) %>% 
        unite("FireCode", c(L,B,pasture,patch,y), sep=".") %>%
        mutate(FireCode = str_to_upper(FireCode) ) %>%
        select(FireCode, date) %>%
        group_by(FireCode) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(date) %>%
        mutate(burn = paste0('HREC_', row_number()) ) , 
    # Medium-done fire behavior data prior to imputation
      read_csv("S:/DevanMcG/Projects/SpatialFireBehavior/data/AnalysisDataKY.csv") %>%
        filter(location == "H", block != 'Day') %>%
        unite("FireCode", c(location, block,NewPasture,patch,year), sep=".") %>%
        mutate(FireCode = str_to_upper(FireCode) ) %>%
        select(FireCode, plot, array, MaxC, SoilMaxC, ros) ) %>%
      group_by(FireCode, burn, date, plot) %>%
      summarise(across(c(MaxC:ros), ~mean(.x, na.rm = TRUE)), .groups = 'drop' )
    
    hrec_bd <- 
      BurnData_HREC %>%
      arrange(date) %>%
      mutate(PlotCode = str_remove_all(FireCode, '[0-9]'), 
             PlotCode = paste0(PlotCode, plot), 
             plot = paste0('HREC_', row_number())) 
    
  # Dayton tract
    
    # 2019 burn
      read_csv("S:/DevanMcG/Projects/SpatialFireBehavior/data/fromMZ/CompiledData3.csv") %>%
        filter( block == 'Dayton', pasture == 'NW.SE') %>%
        slice(1) %>%
        select(date)  
      day_19 <- 
      read_csv("S:/DevanMcG/Projects/SpatialFireBehavior/data/AnalysisDataKY.csv")  %>%
      filter( block == 'Day') %>%
        mutate(FireCode = 'DAY.1.19' ) %>%
        select(FireCode, plot, array, MaxC, SoilMaxC, ros) %>%
        group_by(FireCode, plot) %>%
        summarise(across(c(MaxC:ros), ~mean(.x, na.rm = TRUE)), .groups = 'drop' ) %>%
        merge(plots_dayton_1, .) %>%
        rename(burn = FireCode) %>%
        mutate(plot = paste0('DAY_', plot)) %>% 
        add_column(date = as.Date('2019-10-07', '%Y-%m-%d'), .after = 'burn') %>%
        add_column(location = 'Dayton', .before = 'burn') 
    
    # 2020 burns  
      load("S:/DevanMcG/Projects/SoilHeating/DaytonTract/Robjects/DaytonSoilMaxC.Rdata")
      load("S:/DevanMcG/Projects/SoilHeating/DaytonTract/Robjects/SoilSurfaceCurves.Rdata")
      day_pts <- read_sf('./data/ThermocouplePlacements.gpkg', 'DaytonSamplePoints')
      
      SoilSurfaceCurves %>%
        mutate(date = format(timestamp, '%Y-%m-%d')) %>%
        group_by(burn) %>%
        slice(1)
      
      day_20 <- 
      day_pts %>%
        select(NAME, BurnOrder) %>%
        merge(by = 'NAME', 
                MaxC ) %>%
        select(NAME, BurnOrder, LoggerLevel, MaxC) %>% 
        pivot_wider(names_from = LoggerLevel, 
                    values_from = MaxC)  %>%
        add_column(burn = paste0('DAY.', .$BurnOrder,'.20'), .before = 'NAME') %>%
        add_column(date = as.Date('2020-10-02', '%Y-%m-%d'), .after = 'burn') %>%
        mutate(plot = paste0('DAY_', NAME), 
               MaxC = canopy, 
               SoilMaxC = surface) %>%
        select(burn, date, plot, MaxC, SoilMaxC)  %>%
        add_column(location = 'Dayton', .before = 'burn') 
        
    
# Combine location data into plot layer
    bind_rows(
      # CGREC 
        plots_cgrec %>%
          merge(cgrec_bd) %>%
          select(location, burn, date, plot, MaxC:ros),
      # HREC
        plots_hrec %>%
          merge(hrec_bd) %>%
          select(location, burn, date, plot, MaxC:ros), 
      # Dayton data
        day_19 %>% st_buffer(10) , 
        day_20  %>% st_buffer(10)
      ) %>% 
      st_write('./data/ThermocouplePlacements.gpkg', 'AllBurnData')
    
    read_sf('./data/ThermocouplePlacements.gpkg', 'AllBurnData') %>%
      group_by(date, location) %>%
      slice(1) %>%
      as_tibble() %>%
      select(location, date) %>% 
      rename(BurnDate = date) %>%
      arrange(location, BurnDate) %>%
      write_csv('./data/BurnDates.csv', append = FALSE)
    
    st_layers('./data/ThermocouplePlacements.gpkg')
    