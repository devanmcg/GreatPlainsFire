

load('./data/HistWx.Rdata')

HistWx %>%
  separate(date, c('year', 'month', 'day'), sep = "-", remove = F) %>%
  mutate(season = case_when(
    month %in% c('04', '05') ~ 'spring',
    month %in% c('08') ~ 'summer'), 
    day = format(date, '%b-%d')) %>%
  filter(!is.na(season),
         !is.na(value), 
         param %in% c("burn_index", "fmoist_100", "energy_release", "palmer",        
                      "pet_grass", "hdwi" )) %>%
  group_by(param, season, day) %>%
  summarize(mean = mean(value, na.rm = TRUE), 
            SEM = sd(value, na.rm = TRUE) / sqrt(n()), 
            .groups = 'drop') %>%
  ggplot() + theme_bw() + 
    geom_smooth(aes(x = day, 
                  y = mean, 
                  group = 1) ) +
  facet_grid(param ~ season, scales = 'free')
