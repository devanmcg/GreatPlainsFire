pacman::p_load(tidyverse, sf, vegan)

source('https://raw.githubusercontent.com/devanmcg/rangeR/master/R/CustomGGplotThemes.R')

load('./data/HistoricalWeather/DayOfWx.Rdata') 
load('./data/HistoricalWeather/HistWx.Rdata')

# Calculate & ordinate the anomaly 
anomalies <- 
    HistWx %>%
    group_by(event, param) %>%
    summarize(Normal = median(TenDayMean), 
              SD = sd(TenDayMean)) %>%
    ungroup() %>%
    filter(!is.na(Normal)) %>%
    left_join(
      DayOfWx %>% 
        mutate(month = format(date, '%b')) %>%
        select(-date), 
      by = c('event', 'param')) %>%
    mutate(anomaly = (DayOfValue - Normal) / SD ) %>%
      select(-Normal, -SD, -DayOfValue) %>% 
    pivot_wider(names_from = 'param', 
                values_from = 'anomaly', 
                values_fn = mean) %>%
      mutate(burn_index = ifelse(burn_index < 0, NA, burn_index)) %>%
  filter(!is.na(burn_index)) 

anom_d <-  
  anomalies %>%
    select(-event, -L2, -L3, -month, -wind_vel, -prcp, -pet_grass, -srad) 

cor(anom_d)
  
  anom_pca <- rda(anom_d ~ 1 + Condition(anomalies$wind_vel), scale = T)
  summary(eigenvals(anom_pca))
  CumProp <- 
    summary(eigenvals(anom_pca)) %>% 
      as_tibble() %>%
    round(2) %>%
      select(PC2) %>%
      slice(3) 

pca_sites <- 
  anom_pca$CA$u %>%
    as_tibble() %>%
    select(PC1, PC2) %>%
    mutate(event = anomalies$event, 
           L2 = anomalies$L2, 
           L3 = anomalies$L3, 
           month = anomalies$month, 
           season = case_when(
             month %in% c('Mar', 'Apr', 'May') ~ 'Spring', 
             month %in% c('Jun', 'Jul', 'Aug') ~ 'Summer', 
             month %in% c('Sep', 'Oct', 'Nov') ~ 'Autumn', 
             month %in% c('Dec', 'Jan', 'Feb') ~ 'Winter'
           ))

envfit(anom_pca ~ season, pca_sites, strata = pca_sites$L2)

pca_vars <- 
  anom_pca$CA$v %>%
  as_tibble() %>%
  select(PC1, PC2, PC3) %>%
  mutate(parameter = colnames(anom_d))
  
  pca_sites %>%
    ggplot(aes(x = PC1, y = PC2)) + theme_ord(14) +
    geom_hline(yintercept = 0, lty = 3) +
    geom_vline(xintercept = 0, lty = 3) + 
    labs(caption = paste0('Cumulative variance explained: ', CumProp) )  +
    geom_point(pch = 21, 
               fill = "grey40", 
               color = "grey90",
               alpha = 0.5) + 
    geom_text(data = pca_vars, 
              aes(x = PC1 / 10, 
                  y = PC2 / 10, 
                  label = parameter), 
              color = "darkred", 
              fontface = "bold") +
    stat_ellipse(aes(color = season)) 
  geom_path(aes(group = event ), arrow = arrow(), 
            color = "grey40")
  
# Common denominators of anomalies? 
  
  anomalies %>%
    select(-wind_vel, -prcp) %>%
    pivot_longer(names_to = 'param', 
                 values_to = 'anomaly', 
                 cols = (burn_index:vpd)) %>%
    mutate(OneSD = ifelse(abs(anomaly) > 1, T, F), 
           TwoSD = ifelse(abs(anomaly) > 2, T, F)) %>%
    
    mutate(count = rowSums(across(burn_index)))
  
# Stack fire days and non-fire days together 
  comb_d <- 
    HistWx %>%
      group_by(event, param) %>%
      summarize(Normal = median(TenDayMean)) %>%
      ungroup() %>%
      filter(!is.na(Normal)) %>%
      pivot_wider(names_from = 'param', 
                  values_from = 'Normal') %>%
      mutate(fire = "no") %>%
      bind_rows( DayOfWx %>%
                   select(-L2, -L3, -date) %>%
                   pivot_wider(names_from = 'param', 
                               values_from = 'DayOfValue', 
                               values_fn = mean) %>%
                   mutate(burn_index = ifelse(burn_index == 0, NA, burn_index)) %>%
                   filter(!is.na(burn_index))  %>% 
                   mutate(fire = 'yes') )  %>%
      left_join(unique(select(DayOfWx, event, L2)) )

  pca_d <- comb_d %>% 
            select(-fire, -event, -L2, -wind_vel, -prcp)

wx_pca <- rda(pca_d ~ 1 + Condition(comb_d$wind_vel) , scale = T)
summary(eigenvals(wx_pca))
envfit(wx_pca ~ fire + L2, comb_d) 

wx_sites <- 
wx_pca$CA$u %>%
  as_tibble() %>%
  select(PC1, PC2) %>%
  mutate(fire = comb_d$fire, 
         event = comb_d$event, 
         L2 = comb_d$L2) %>%
  left_join(unique(select(DayOfWx, event, L3)))

site_means <-
  wx_sites %>%
    group_by(L2, fire) %>%
    summarise(X = mean(PC1, na.rm = T), 
              Y = mean(PC2, na.rm = T)) %>%
    ungroup() %>%
    pivot_longer(names_to = 'axis', 
                 values_to = 'score', 
                 cols = c('X', 'Y')) %>%
    unite('point', c('axis', 'fire'), sep='') %>%
    pivot_wider(names_from = 'point', 
                values_from = 'score')

wx_vars <- 
  wx_pca$CA$v %>%
  as_tibble() %>%
  select(PC1, PC2) %>%
  mutate(parameter = colnames(pca_d))

  ggplot(wx_sites, aes(x = PC1, y = PC2)) + theme_ord(14) +
  # geom_point(aes(fill = fire), 
  #            pch = c(21), 
  #            alpha = 0.25) + 
    geom_text(data = wx_vars, 
              aes(x = PC1 / 10, 
                  y = PC2 / 10, 
                  label = parameter), 
              color = "darkred") + 
    stat_ellipse(aes(color = L2, 
                     lty = fire), 
                 size = 0.9, 
                 alpha = 0.75, 
                 level = 0.95) + 
  geom_segment(data = site_means, 
    aes(x = Xno, y = Yno, 
        xend = Xyes, yend = Yyes, 
        color = L2), 
    arrow = arrow(angle = 30, 
                  length = unit(5, "mm"), 
                  type = 'closed') , 
    size = 1.5) +
    scale_color_viridis_d() +
    theme(panel.background = element_rect(fill = "grey90")) 
  
# Calulate angle and distance moved 

region_angles <- 
  wx_sites %>% 
    pivot_longer(names_to = "axis", 
                 values_to = "score", 
                 cols = c('PC1', 'PC2')) %>%
    unite('pair', c(axis, fire), sep='') %>%
        filter(!is.na(score)) %>% 
        pivot_wider(names_from = pair, 
                    values_from = score) %>%
        filter(complete.cases(.)) %>%
        mutate(dx = PC1no - PC1yes, 
               dy = PC2no - PC2yes,
          dist = sqrt((dx)^2 + (dy)^2) , 
          angle = ifelse(dist < 1e-07, NA, atan2(dy, dx)), 
          angle = angle * 57.29578) %>%
    group_by(L2) %>%
    summarize(AngleMean = mean(angle), 
              AngleSE = sd(angle) / sqrt(n()), 
              DistMean = mean(dist), 
              DistSE = sd(dist) / sqrt(n())) %>%
  ungroup()  

var_angles <- 
  wx_vars %>% 
  mutate(dx = 0 + PC1, 
         dy = 0 + PC2,
         dist = sqrt((dx)^2 + (dy)^2) , 
         angle = ifelse(dist < 1e-07, NA, atan2(dy, dx)), 
         angle = angle * 57.29578) %>%
  select(parameter, angle) %>%
  mutate(angle = angle -90, 
         angle = ifelse(angle < 0, angle + 360, angle), 
         angle = 360 - angle, 
         angle = ifelse(angle > 180, angle - 180, angle)) %>%
  arrange(angle) %>%
  mutate(parameter = recode(parameter, 
                            shum = 'Sp Hum.', 
                            tmax = 'Air\ntemp', 
                            srad = 'Solar\nradiation', 
                            pet_grass = 'PET', 
                            vpd = 'VPD', 
                            energy_release = 'ERC', 
                            burn_index = 'BI', 
                            rhmin = 'RH')) 
region_angles %>%
    ggplot() + theme_bw() +
  geom_hline(yintercept = var_angles$angle, 
             size = 1, 
             color = "darkgrey") + 
    geom_errorbar(aes(x = L2, 
                      ymin = AngleMean - AngleSE,
                      ymax = AngleMean + AngleSE), 
                  width = 0.25, 
                  size = 1.1, 
                  color = "darkblue") +
    geom_point(aes(x = L2, 
                   y = AngleMean), 
               size = 4, 
               pch = 21, 
               fill = "lightblue", 
               col = "darkblue", 
               stroke = 1.1) +
  coord_flip(xlim = c(1,6), 
             ylim = c(5,150), 
             clip = 'off') +
  labs( x = '', 
        y = 'Mean trend in multivariate anomaly (degrees ± s.e.)') + 
  annotate('label', 
           x = c(6.0, 5.5, 6.25, 5.75, 6, 5.75, 6.25, 6),
           y = var_angles$angle, 
           label = var_angles$parameter ) +
  annotate('text', x = 6.8, y = 75, 
           label = 'Weather variable vectors') + 
  theme(axis.text.y = element_text(color = 'black'), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        plot.margin = unit(c(10,2,5,0), 'mm')) 
 
  

# Fire days alone
    pca_d <-
      DayOfWx %>%
      select(-L2, -L3, -date) %>%
      pivot_wider(names_from = 'param', 
                  values_from = 'DayOfValue', 
                  values_fn = mean) %>%
      select(-event, -pet_grass, -wind_vel) 
    
    wx_pca <- rda(pca_d ~ 1, scale = T)
    wx_pca <- capscale(scale(pca_d) ~ 1, 'euc', metaMDSdist = TRUE)
    car::vif(wx_pca)
    
    cor(pca_d)
    
    plot(wx_pca) 
    biplot(wx_pca, scaling = 'species')
    text(wx_pca, "species")
  

