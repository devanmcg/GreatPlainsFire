pacman::p_load(tidyverse, magrittr, FSA)


  FilePath = "./A_cana/Data/ThermocoupleData/FA21/"
  MinTemp = 15
  ExcludeColumns = c("logger", "timestamp")

# list.dirs(FilePath)
tc_files <- list.files(FilePath, pattern = "\\.TXT$")

f = 6 # edit manually to go through the files one by one
l = tools::file_path_sans_ext(tc_files[f])

{
  rd1 <- 
    paste0(FilePath, tc_files[f]) %>% 
    read_csv(col_names = FALSE, show_col_types = FALSE) %>% 
    mutate(X1 = l)

  # Prepare data for pivoting
  tcs <- dim(rd1)[2] - length(ExcludeColumns)
  colnames(rd1) <- c(ExcludeColumns, seq(1, tcs, 1))
  # Pivot logger data into long format
  rd <-
    rd1 %>%
    pivot_longer(values_to = 'degC', 
                 names_to = 'TC', 
                 -all_of(ExcludeColumns) ) %>%
    group_by(TC) %>%
    mutate(obs = seq(1, n(), 1)) %>%
    ungroup() 
}
  # check with plots
    rd %>%
    group_by(TC) %>%
     slice( which(row_number() %% 10 == 1)) %>%
    ggplot() + theme_bw(14) + 
    geom_line(aes(x = obs, 
                  y = degC)) + 
    labs(title = paste0("Logger ", unique(rd$logger))) + 
    facet_wrap(~TC, scales = "free_y")
    
    #HeatingCurves <- tibble() 
{   
    # Prompt channel selection 
    channels <- readline(prompt = message("Select channels to import (see graphics device) by entering numbers of desired channels separated by commas, or...\n Enter: select all channels\n 0: None (and skip to next file)\n Q: quit (then select Cancel to avoid closing R altogether)")) # \nTo *exclude* certain channels, precede channel number with negative sign (-)."))
    
    if(channels == "Q") { 
      quit(save = 'ask') 
    } else {
      if(channels == "") { 
        selections <- unique(rd$TC) 
      } else {
        selections <- ifelse(channels %in% c('0', 'O'), 'None', channels) 
        selections <- unlist(strsplit(selections, ",")) %>% 
          trimws()  
      } 
    }

    for(j in 1:length(selections)) { # loop through multi-channel data on logger file
      tc = as.numeric(selections[j] ) 
      rd_j <- filter(rd, TC == unique(rd$TC)[tc]) 
      # User interaction section. 
      # Open external window
      x11(width=16, height=6)
      plot(degC ~ obs, 
           data = rd_j %>% filter(degC < 1000) %>% slice( which(row_number() %% 5 == 1)) ,  
           type = 'l', las = 1, 
           main = paste0('Logger ', unique(rd_j$logger),', sensor ', unique(rd_j$TC)) )  
      pts <- identify(rd_j$obs,
                      rd_j$degC, 
                      labels = "^", 
                      col = "red") ; dev.off() 

      # Proceed with processing logger data 
      # process user-defined events
      # identify beginning and end points of rough windows
      events <- 
        pts %>%
        as_tibble(rownames = "click") %>%
        rename(obs = value) %>%
        mutate( click = as.numeric(click), 
                rough_endpt = ifelse(FSA::is.odd(click), "start", "stop"), 
                logger = unique(rd_j$logger), 
                tc = unique(rd_j$TC)) %>%
        group_by(rough_endpt) %>%
        mutate(event = seq(1:n())) %>%
        ungroup() %>%
        select(-click) 
      
      rough_windows <- tibble()
      for(e in 1:length(unique(events$event))) { # Loop through fire events on channel
        event = filter(events, event == e) 
        filter(rd_j, between(obs, event$obs[1], event$obs[2])) %>%
          mutate(logger = unique(rd_j$logger),
                 event = e) %>%
          select(logger, TC, event,obs, timestamp, degC) %>%
          bind_rows(rough_windows) -> rough_windows
      } # close events loop
  
      # rough_windows <- filter(rough_windows, degC >= MinTemp) 
      
      # rough_windows %>%
      #   ggplot() + theme_bw(14) +
      #   geom_line(aes(x = obs, y = degC)) +
      #   facet_wrap(~event, scales = 'free_x') +
      #   labs(title = paste0('Logger ', unique(rd_j$logger),', sensor ', unique(rd_j$TC)), 
      #        subtitle = "User-identified rough windows of flame front passage")
      
      maxes <- 
        rough_windows %>%
        group_by(event, obs) %>%
        summarize(Max = max(degC), 
                  .groups = 'drop_last') %>%
        slice(which.max(Max)) %>%
        ungroup() 
      hc <- tibble() 
      for(e in 1:max(maxes$event)) { # loop through events, take out max C rows
        m = filter(maxes, event == e)$obs
        rough_windows %>%
          filter(event == e ,  
                 obs <= m) %>%
          #mutate(logger = as.numeric(logger) ) %>% # ???
          bind_rows(hc) -> hc
      } # close max loop
      HeatingCurves <- bind_rows(HeatingCurves, hc)
    } # close multi-channel loop
  # return(HeatingCurves)
}

    unique(HeatingCurves$logger)
    unique(HeatingCurves$TC)
    
    HeatingCurves %<>% mutate(degC = abs(degC))
    
    min(HeatingCurves$degC)

# save(HeatingCurves, file = './A_cana/Data/ThermocoupleData/FA21/HeatingCurves.Rdata') 

    HeatingCurves  %>% 
  ggplot() + theme_bw(14) +
  geom_line(aes(x = obs, 
                y = degC, 
                color = as.factor(event))) +
  facet_grid(logger~TC, scales = 'free_x') 

