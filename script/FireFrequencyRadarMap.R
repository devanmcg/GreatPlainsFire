pacman::p_load(tidyverse, lubridate, wesanderson, grid)
pacman::p_load_current_gh("ricardo-bion/ggradar")

load('./data/HistoricalWeather/DayOfWx.Rdata')

pal40 <- wes_palette("Zissou1", 40, type = "continuous") 

events_count <- 
DayOfWx %>% 
  mutate(month = month(date, label = T), 
         decade = floor_date(date, "10 year"), 
         decade = paste0(format(decade, '%Y'), ' - ', format(decade+years(10), '%Y'))) %>%
  group_by(L2, decade, month) %>%
  summarize(events = n() ) %>%
  ungroup() 
  
sc_rp <-
  events_count %>%
    ungroup() %>%
    filter(L2 == "South Central Semi-Arid Prairies") %>%
    select(-L2) %>% 
    pivot_wider(names_from = 'month', 
                values_from = 'events', 
                values_fill = 0, 
                names_sort = TRUE) %>%
  gg_radar(
    axis.label.size = 4,
    grid.label.size = 3,
    legend.text.size = 8,
    values.radar = c(0, 1250, 2500),
    grid.min = 0, grid.mid = 1250, grid.max = 2500,
    # Polygons
      group.line.width = 0.4, 
      group.point.size = 1.5,
      group.colours = wes_palette("FantasticFox1")[c(3,2,1,5)],
    # Background and grid lines
    background.circle.colour = "white",
    gridline.mid.colour = "grey",
    plot.legend = FALSE, 
    plot.title = 'South Central Semi-Arid Prairies') +
  theme(plot.title = element_text(size = 8, hjust = 0.5))
  
tp_rp <- 
  events_count %>%
    ungroup() %>%
    filter(L2 == "Temperate Prairies") %>%
    select(-L2) %>% 
    pivot_wider(names_from = 'month', 
                values_from = 'events', 
                values_fill = 0, 
                names_sort = TRUE) %>%
    gg_radar(
      axis.label.size = 4,
      grid.label.size = 3,
      legend.text.size = 8,
      values.radar = c(0, 250, 500),
      grid.min = 0, grid.mid = 250, grid.max = 500,
      # Polygons
        group.line.width = 0.4, 
        group.point.size = 1.5,
        group.colours = wes_palette("FantasticFox1")[c(3,2,1,5)],
      # Background and grid lines
      background.circle.colour = "white",
      gridline.mid.colour = "grey",
      plot.legend = FALSE, 
      plot.title = 'Temperate Prairies') +
  theme(plot.title = element_text(size = 8, hjust = 0.5))
  
st_rp <-
  events_count %>%
    ungroup() %>%
    filter(L2 %in% c("Tamaulipas-Texas Semiarid Plain", 
                     'Texas-Louisiana Coastal Plain')) %>%
    group_by(decade, month) %>%
    summarize(events = sum(events)) %>%
    pivot_wider(names_from = 'month', 
                values_from = 'events', 
                values_fill = 0, 
                names_sort = TRUE) %>%
    gg_radar(
      axis.label.size = 4,
      grid.label.size = 3,
      legend.text.size = 8,
      values.radar = c(0, 100, 200),
      grid.min = 0, grid.mid = 100, grid.max = 200,
      # Polygons
        group.line.width = 0.4, 
        group.point.size = 1.5,
        group.colours = wes_palette("FantasticFox1")[c(3,2,1,5)],
      # Background and grid lines
      background.circle.colour = "white",
      gridline.mid.colour = "grey",
      plot.legend = FALSE, 
      plot.title = 'Texas Southern Plains') +
  theme(plot.title = element_text(size = 8, hjust = 0.5))
  
wc_rp <-
  events_count %>%
    ungroup() %>%
    filter(L2 == 'West-Central Semi-Arid Prairies') %>%
    select(-L2) %>% 
    pivot_wider(names_from = 'month', 
                values_from = 'events', 
                values_fill = 0, 
                names_sort = TRUE) %>%
    gg_radar(
      axis.label.size = 4,
      grid.label.size = 3,
      legend.text.size = 8,
      values.radar = c(0, 600, 1200),
      grid.min = 0, grid.mid = 600, grid.max = 1200,
      # Polygons
      group.line.width = 0.4, 
      group.point.size = 1.5,
      group.colours = wes_palette("FantasticFox1")[c(3,2,1,5)],
      # Background and grid lines
      background.circle.colour = "white",
      gridline.mid.colour = "grey",
      legend.position = "bottom",
      legend.direction = "vertical",
      #plot.legend = FALSE,
      plot.title = 'West-Central Semi-Arid Prairies') +
  theme(plot.title = element_text(size = 8, hjust = 0.5))

  MainMap <-
  mapping$gp_l2_5 %>%
    mutate(area = st_area(.)) %>%
    group_by(L2) %>%
    summarize(area = sum(area)) %>%
  ggplot( ) + theme_map(14) +
    geom_sf(aes(fill = L2))  + 
    geom_sf(data = mapping$gp_st, 
            fill = NA, 
            color = 'grey50') +
    scale_fill_manual(name = 'Level 2 ecoregion', 
                      values = pal40[c(5, 15, 25, 35)])  +
    theme(plot.margin = unit(c(0,0,0,12), 'lines'))
  
  grid.newpage() 
  main <- viewport(width = 1, height = 0.95, x = 0.5, y = 0.5) 
  TL <- viewport(width = 0.33, height = 0.5, x = 0.15, y = 0.7)
  BL <- viewport(width = 0.33, height = 0.35, x = 0.15, y = 0.25)
  TR <- viewport(width = 0.33, height = 0.35, x = 0.83, y = 0.8)
  BR <- viewport(width = 0.33, height = 0.35, x = 0.83, y = 0.2)
  print(MainMap, vp = main)
  print(wc_rp, vp = TL)
  print(sc_rp, vp = BL)
  print(tp_rp, vp = TR)
  print(st_rp, vp = BR)
  dev.off() 
  
