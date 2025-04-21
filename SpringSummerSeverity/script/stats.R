pacman::p_load(tidyverse) 

# The Z-score for a data point is calculated by subtracting the historical mean from the data point and 
# dividing by the historical standard deviation.
# 1 or greater (or -1 or less) suggests the data point is significantly outside the historical mean


# load data 
load('./data/NoFireIndices.Rdata')
load('./data/BurnIndices.Rdata')
load('./data/NoFireTrends.Rdata')
load('./data/WxData.Rdata')

seasons <- 
  tibble(season = c('Spring', 'Summer'), 
         start = c(as.Date('2024-04-25'), as.Date('2024-08-07')), 
         end = c(as.Date('2024-05-25'), as.Date('2024-08-27'))) 

LandsatSums <-
  NoFireTrends  %>%
  separate(ImageDate, into=c('year', 'month', 'day'), sep = '-') %>%
  mutate(season = ifelse(month=='04', 'Spring', 'Summer')) %>%
  pivot_longer(names_to = 'index', 
               values_to = 'value', 
               cols = c(ndvi:ndmi)) %>%
  group_by(year, season, pasture, index) %>%
  summarize(Value = mean(value), 
            .groups = 'drop') 

#
# linear regression
#

# Check distribution

MASS::fitdistr(reg_dat$dNBR, "normal")
MASS::fitdistr(reg_dat$dNBR, "Gamma")
reg_dat <-
  BurnIndices  %>%
    group_by(Year, Season, fire) %>%
    summarize(dNBR = mean(dNBR), 
              NDVI = mean(ndvi), 
              .groups = 'drop') %>%
    separate(fire, into = c('block', 'pasture', 'patch'), sep = '-')

ggplot(reg_dat, aes(x=dNBR)) + theme_bw(16) + 
  geom_histogram(aes(y=after_stat(density)),      
                 binwidth=0.05,
                 colour="black",
                 fill="lightgreen") + 
  geom_density(alpha=.5, fill="lightblue")  + 
  coord_cartesian(xlim = c(0,0.5)) + 
  stat_function(data= tibble(dNBR=seq(0,0.5, 0.01) ), 
                fun = dnorm, 
                args=list(mean = 0.208,
                          sd = 0.106),
                colour="blue", 
                size=1.1)  + 
  stat_function(data= tibble(dNBR=seq(0,0.5, 0.01) ), 
                fun = dgamma, 
                args=list(shape = 3.49,
                          rate = 16.78),
                colour="darkred", 
                size=1.1)

# Test for difference between seasons
  m0 <- lme4::glmer(dNBR ~ 1 + (1|block) , 
                family = Gamma(link = "identity"), 
                data = reg_dat)
  m1 <- lme4::glmer(dNBR ~ NDVI + (1|block), 
                family = Gamma(link = "identity"), 
                data = reg_dat)
  m2 <- lme4::glmer(dNBR ~ NDVI + Season + (1|block) , 
                    family = Gamma(link = "identity"), 
                    data = reg_dat)
  anova(m0, m1, m2)
  summary(m2)
# Test within Spring 
  sp0 <- lme4::glmer(dNBR ~ 1 + (1|block/Year) , 
                    family = Gamma(link = "identity"), 
                    data = filter(reg_dat, Season == 'Spring'))
  sp1 <- lme4::glmer(dNBR ~ NDVI + (1|block/Year), 
                    family = Gamma(link = "identity"), 
                    data = filter(reg_dat, Season == 'Spring') )
  anova(sp0, sp1)
  summary(sp1)
  # Test within summer 
  su0 <- lme4::glmer(dNBR ~ 1 + (1|block) , 
                     family = Gamma(link = "identity"), 
                     data = filter(reg_dat, Season != 'Spring'))
  su1 <- lme4::glmer(dNBR ~ NDVI + (1|block), 
                     family = Gamma(link = "identity"), 
                     data = filter(reg_dat, Season != 'Spring') )
  anova(su0, su1)
  summary(su1)
  
   glm_mod <- glm(dNBR ~ NDVI,  
                     family = Gamma(link = "identity"), 
                     data = filter(reg_dat, Season == 'Spring') )
   
   glm_mod$deviance/glm_mod$null.deviance 
   lm_mod <- lm(dNBR ~ NDVI,  
                 data = filter(reg_dat, Season == 'Spring') )
   summary(lm_mod) 
#
# z scores 
#

  # Create an object to store z scores for manuscript table
  z_tab <- lst() 
  # NDVI x burn season 
  ndvi_z <-
    LandsatSums %>%
    filter(year %in% c(2017:2020), 
           index == 'ndvi') %>%
    group_by(year, season) %>%
    summarize(Mean = mean(Value), 
              .groups = 'drop') %>%
    left_join(by = 'season', 
              LandsatSums %>%
                filter(index == 'ndvi') %>%
                group_by(season) %>%
                summarize(HistMean = mean(Value, na.rm = T),
                          HistSD = sd(Value, na.rm = T),
                          .groups = 'drop') ) %>%
    mutate(z = (Mean - HistMean)/HistSD, 
           interp = case_when(
             z < -1 ~ "Below", 
             z > 1 ~ "Above", 
             TRUE ~ 'Within'
           ), 
           z = round(z, 2) , 
           Anomaly = paste0(interp, ' (', z, ')'))  %>%
    rename(Year = year) %>%
    select(Year, season, Anomaly) %>%
    pivot_wider(names_from = season, 
                values_from = Anomaly) %>%
    add_column(.before = 1, Variable = c('NDVI', '', '', '')) %>%
    arrange(Year)
  # YTD precip x burn season 
  prcp <- 
    WxData$Rainfall %>% 
    filter(month(date) %in% c(04:09)) %>%
    separate(date, into = c('Year', 'Month', 'Day'),sep = '-') %>%
    group_by(Year) %>%
    mutate(CumPrecip = cumsum(Rainfall), 
           Month = recode(Month, '05' ="May", '08'='Aug'), 
           Month = paste0(Month, ' ', Day)) %>%
    ungroup() %>%
    filter(Month %in% c('May 15', 'Aug 01' )) %>%
    mutate(season = recode(Month, 'May 15' = 'Spring', 'Aug 01' = 'Summer' )) %>%
    select(Year, season, CumPrecip)
  prcp_z <-
    prcp %>%
    filter(as.numeric(Year) %in% c(2017:2020)) %>%
    group_by(Year, season) %>%
    summarize(Mean = mean(CumPrecip), 
              .groups = 'drop') %>%
    left_join(by = 'season', 
              prcp %>%
                group_by(season) %>%
                summarize(HistMean = mean(CumPrecip, na.rm = T),
                          HistSD = sd(CumPrecip, na.rm = T),
                          .groups = 'drop') ) %>%
    mutate(z = (Mean - HistMean)/HistSD, 
           interp = case_when(
             z < -1 ~ "Below", 
             z > 1 ~ "Above", 
             TRUE ~ 'Within'
           ), 
           z = round(z, 2), 
           Anomaly = paste0(interp, ' (', z, ')'))  %>%
    select(Year, season, Anomaly) %>%
    pivot_wider(names_from = season, 
                values_from = Anomaly) %>%
    add_column(.before = 1, Variable = c('Precipitation', '', '', '')) %>%
    #select(variable, Year, Spring, Summer)  %>%
    arrange(Year) 
  # Add to z score object
    z_tab$ndvi_prcp <- bind_rows(ndvi_z, prcp_z) 
  
    # Fire weather 
    # Grab rainy days for exclusion
    RainyDays <- 
      WxData$Rainfall %>%
      filter( ! is.na(season), Rainfall > 0.1) 
    # Wrangle fire weather data 
    wxd <- 
      WxData$HourlyWx %>%
      filter(! date %in% RainyDays$date, # remove rainy days with no chance of burning
             ! is.na(season)) %>%
      group_by(Year, date, season) %>%
      summarise_at(.vars = vars(c(DewPoint, RelHum, WindSpeed,VPD)),
                   .funs = c("mean")) %>%
      ungroup() %>%
      pivot_longer(names_to = 'variable', 
                   values_to = 'value', 
                   cols = DewPoint:VPD) %>%
      group_by(Year, season, variable) %>%
      summarise(Value = mean(value), 
                .groups = 'drop') %>%
      mutate(Variable = recode(variable, 
                               'DewPoint' = 'Dew Point', 
                               'RelHum' = 'Relative Humidity', 
                               'VPD' = 'Vapor Pressure Deficit', 
                               'WindSpeed' = 'Wind speed') , 
             Year = as.character(Year), 
             season = fct_rev(season))
    z_tab$wxd <-  
      wxd %>%
        filter(as.numeric(Year) %in% c(2017:2020)) %>%
        group_by(Year, season, Variable) %>%
        summarize(Mean = mean(Value), 
                  .groups = 'drop') %>%
        left_join(by = c('season', 'Variable'), 
                  wxd %>%
                    group_by(season, Variable) %>%
                    summarize(HistMean = mean(Value, na.rm = T),
                              HistSD = sd(Value, na.rm = T),
                              .groups = 'drop') ) %>%
        mutate(z = (Mean - HistMean)/HistSD, 
               interp = case_when(
                 z < -1 ~ "Below", 
                 z > 1 ~ "Above", 
                 TRUE ~ 'Within'), 
               z = round(z, 2) , 
               season = fct_rev(season) ) %>%
        mutate(Anomaly = paste0(interp, ' (', z, ')'))  %>%
        select(Year, season, Variable, Anomaly) %>%
        pivot_wider(names_from = season, 
                    values_from = Anomaly) %>%
        select(Variable, Year, Spring, Summer) %>%
        arrange(Variable, Year) %>%
        mutate(Variable = ifelse(row_number() %in% c(1,5,9,13), Variable, " "))
    
  # Historical patterns 
    StudySums <- 
      LandsatSums %>%
      filter(year %in% c(2017:2020)) %>%
      group_by(year, season, index) %>%
      summarize(Mean = mean(Value),
                SEM = sd(Value)/sqrt(n()), 
                .groups = 'drop') %>%
      mutate(status = case_when(
        season == 'Summer' & year %in% c('2018', '2019') ~ "Not completed", 
        TRUE ~ "Completed"), 
        index = toupper(index), 
        index = fct_rev(index) ) 
    
    HistoricalSpectralData <- 
      bind_rows(
        LandsatSums %>%
          filter(year %in% c(1990:1999))%>% 
          mutate(range = '1990-1999', 
                 index = toupper(index), 
                 index = fct_rev(index)), 
        LandsatSums %>%
          filter(year %in% c(2012:2022))%>% 
          mutate(range = '2012-2022', 
                 index = toupper(index), 
                 index = fct_rev(index)), 
        LandsatSums %>%
          mutate(range = '1990-2022', 
                 index = toupper(index), 
                 index = fct_rev(index)) )
  hist_wx <-
    WxData$HourlyWx %>%
      filter(! date %in% RainyDays$date, # remove rainy days with no chance of burning
             ! is.na(season)) %>%
      group_by(Year, date, season) %>%
      summarise_at(.vars = vars(c(DewPoint, RelHum, WindSpeed,VPD)),
                   .funs = c("mean")) %>%
      ungroup() %>%
      pivot_longer(names_to = 'variable', 
                   values_to = 'value', 
                   cols = DewPoint:VPD) %>%
      group_by(Year, season, variable) %>%
      summarise(Value = mean(value), 
                .groups = 'drop') %>%
      mutate(Year = as.character(Year), 
             #season = fct_rev(season), 
             period = case_when(
               Year %in% c(1990:1999) ~  '1990-1999', 
               Year %in% c(2012:2022) ~  '2012-2022', 
               TRUE ~ NA
             )) %>%
      filter(!is.na(period)) %>%
      bind_rows(WxData$Rainfall %>% 
                  filter(month(date) %in% c(04:09)) %>%
                  separate(date, into = c('Year', 'Month', 'Day'),sep = '-') %>%
                  group_by(Year) %>%
                  mutate(CumPrecip = cumsum(Rainfall), 
                         Month = recode(Month, '05' ="May", '08'='Aug'), 
                         season = paste0(Month, ' ', Day)) %>%
                  ungroup() %>%
                  filter(season %in% c('May 15', 'Aug 01' )) %>%
                  mutate(season = ifelse(season == 'May 15', 'Spring', 'Summer'), 
                         period = case_when(
                           Year %in% c(1990:1999) ~  '1990-1999', 
                           Year %in% c(2012:2022) ~  '2012-2022', 
                           TRUE ~ NA
                         ) )  %>%
                  filter(!is.na(period)) %>%
                  select(Year, season, CumPrecip, period) %>%
                  pivot_longer(names_to = 'variable', 
                               values_to = 'Value', 
                               cols = CumPrecip))  %>%
      bind_rows(HistoricalSpectralData  %>% 
                  select(-pasture) %>%
                  filter(index == 'NDVI', 
                         range != '1990-2022') %>%
                  rename(Year = year, period = range, variable = index)
      ) %>%
      mutate( variable = factor(variable, levels = c('NDVI', 'CumPrecip', 'DewPoint', 
                                                     'RelHum', 'VPD', 'WindSpeed')), 
              variable = recode(variable, 
                                'NDVI' = "Fuelbed Greenness (NDVI)", 
                                'CumPrecip' = 'Cumulative rainfall',
                                'DewPoint' = 'Dew Point', 
                                'RelHum' = 'Relative Humidity', 
                                'VPD' = 'Vapor Pressure Deficit', 
                                'WindSpeed' = 'Wind speed'), 
              period = recode(period, '1990-1999' = '1990-99', '2012-2022' = '2012-22'))
  z_tab$hist <- 
    hist_wx %>%
      filter(as.numeric(Year) %in% c(2017:2020)) %>%
      group_by(Year, season, variable) %>%
      slice(1) %>%
      summarize(Mean = mean(Value), 
                .groups = 'drop') %>%
      left_join(by = c('season', 'variable'), 
                relationship = "many-to-many",
                hist_wx %>%
                  group_by(period, season, variable) %>%
                  summarize(HistMean = mean(Value, na.rm = T),
                            HistSD = sd(Value, na.rm = T),
                            .groups = 'drop') ) %>%
      mutate(z = (Mean - HistMean)/HistSD, 
             interp = case_when(
               z < -1 ~ "Below", 
               z > 1 ~ "Above", 
               TRUE ~ 'Within'), 
             z = round(z, 2) , 
             season = fct_rev(season) ) %>%
      mutate(Anomaly = paste0(interp, ' (', z, ')'), 
             SeasonPeriod = paste0(season, ' ', period))  %>%
      rename(Variable = variable) %>%
      select(Variable, Year, SeasonPeriod, Anomaly) %>%
      pivot_wider(names_from = SeasonPeriod, 
                  values_from = Anomaly) %>%
      arrange(Variable, Year) %>%
      mutate(Variable = ifelse(row_number() %in% c(1,5,9,13,17,21), as.character(Variable), " "))
      
  
  # save(z_tab, file = './data/z_tab.Rdata')

#
# ANOVA
#
  
  # Test NDVI in study years across seasons 
  ndvi_dat <-
    NoFireTrends %>%
      group_by(ImageDate, pasture) %>%
      summarise(NDVI = median(ndvi) , 
                .groups = 'drop') %>%
      mutate(ImageDate = as.Date(ImageDate, '%Y-%m-%d'), 
             Year = lubridate::year(ImageDate), 
             Year = as.factor(Year), 
             SampleDate = format(ImageDate, '%m-%d'), 
             SampleDate = as.Date(SampleDate, '%m-%d'), 
             season = case_when(
               between(SampleDate, seasons$start[1], seasons$end[1]) ~ 'Spring',
               between(SampleDate, seasons$start[2], seasons$end[2]) ~ 'Summer', 
               TRUE ~ NA ) ) %>%
      filter(!is.na(season)) %>%
    select(pasture, Year, season, NDVI)
    
    m0 <- lme4::lmer(NDVI ~ 1 + (1|Year), REML=FALSE, data = ndvi_dat) 
    m1 <- lme4::lmer(NDVI ~ season + (1|Year), REML=FALSE, data = ndvi_dat)
  
    anova(m0, m1)
  
  # test historical fuel and fire weather across time periods 
    comp_tab <-
      hist_wx %>%
        group_by(season, variable) %>%
        group_modify(~ broom::tidy(lm(Value ~ period, data = .x))) %>%
        ungroup() %>%
        filter(term != '(Intercept)') %>%
        mutate(P = case_when(
          p.value > 0.05 ~ 'P > 0.05', 
          between(p.value, 0.00099, 0.01) ~ 'P < 0.01', 
          p.value < 0.001 ~ 'P < 0.001', 
          TRUE ~ paste0('P = ', round(p.value, 2)) ), 
          result = paste0('t = ', signif(statistic, 2), ", ", P)) %>%
        select(variable, season, result) %>%
        pivot_wider(names_from = season, 
                    values_from = result) 
  
   # save(comp_tab, file = './data/comp_tab.Rdata')
  