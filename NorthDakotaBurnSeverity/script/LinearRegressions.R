pacman::p_load(tidyverse)


load('./data/PlotBurnIndices.Rdata') # plot-level data for 26 Rx burns

length(unique(filter(PlotBurnIndices, ! is.na(ros))$burn) ) 
  

BurnModels <- lst() 

# Plot-level data 

# Testing drivers of burn severity
  # Canopy temperature
    mc_0 <- lme4::lmer(dNBR ~ 1 + (1|location/burn), REML = FALSE, data = PlotBurnIndices)
    mc_1 <- lme4::lmer(dNBR ~ MaxC + (1|location/burn), REML = FALSE, data = PlotBurnIndices)
  
    BurnModels$Sev_v_MaxC <- anova(mc_0, mc_1)
    summary(mc_1)$coefficients

  # Soil surface temperature
    sc_0 <- lme4::lmer(dNBR ~ 1 + (1|location/burn), REML = FALSE, 
                       data = filter(PlotBurnIndices, !is.na(SoilMaxC) ) )
    sc_1 <- lme4::lmer(dNBR ~ SoilMaxC + (1|location/burn), REML = FALSE, 
                       data = filter(PlotBurnIndices, !is.na(SoilMaxC)) )
    
    BurnModels$Sev_v_SoilMaxC <- anova(sc_0, sc_1)

  # Rate of spread
    rs_0 <- lme4::lmer(dNBR ~ 1 + (1|location/burn), REML = FALSE,  
                        data = filter(PlotBurnIndices, ! is.na(ros)) )
    rs_1 <- lme4::lmer(dNBR ~ ros + (1|location/burn), REML = FALSE,  
                        data = filter(PlotBurnIndices, ! is.na(ros)) )
    
    BurnModels$Sev_v_ROS <- anova(rs_0, rs_1)
    summary(rs_1)$coefficients
    
# Testing responses to fuelbed greenness
    # Canopy temperature
      mc_0 <- lme4::lmer(MaxC ~ 1 + (1|location/burn), REML = FALSE, data = PlotBurnIndices)
      mc_1 <- lme4::lmer(MaxC ~ ndvi + (1|location/burn), REML = FALSE, data = PlotBurnIndices)
      
      BurnModels$MaxC_v_NDVI <- anova(mc_0, mc_1)
    
    # Soil surface temperature
      sc_0 <- lme4::glmer(SoilMaxC ~ 1 + (1|location/burn), 
                          family = Gamma(link = "identity"), 
                          data = filter(PlotBurnIndices, !is.na(SoilMaxC) ) )
      sc_1 <- lme4::glmer(SoilMaxC ~ ndvi + (1|location/burn), 
                          family = Gamma(link = "identity"), 
                          data = filter(PlotBurnIndices, !is.na(SoilMaxC)) )
      BurnModels$SoilMaxC_v_NDVI <- anova(sc_0, sc_1)
    
    # Rate of spread
      rs_0 <- lme4::glmer(ros ~ 1 + (1|location/burn), 
                         family = Gamma(link = "log"),
                         data = filter(PlotBurnIndices, ! is.na(ros)) )
      rs_1 <- lme4::glmer(ros ~ ndvi + (1|location/burn), 
                         family = Gamma(link = "log"),
                         data = filter(PlotBurnIndices, ! is.na(ros)) )
      
      BurnModels$ROS_v_NDVI <-anova(rs_0, rs_1)
    
    # Burn severity
      bs_0 <- lme4::lmer(dNBR ~ 1 + (1|location/burn), REML = FALSE, data = PlotBurnIndices)
      bs_1 <- lme4::lmer(dNBR ~ ndvi + (1|location/burn), REML = FALSE, data = PlotBurnIndices)

      BurnModels$dNBR_v_NDVI <- anova(bs_0, bs_1)
      
      
     BurnIndices %>%
       mutate(zone = ifelse(location == 'CGREC', 'east', 'west')) %>% 
       pivot_longer(names_to = 'response', 
                    values_to = 'value', 
                    cols = c(MaxC:dNBR)) %>%
       ggplot(aes(x = ndvi, y = value, color = zone)) + theme_bw() +
        geom_smooth(method = 'lm') +
       geom_point() + 
       facet_wrap(~response, scale = 'free_y')
       
     anova(m1, m3)
    
  # save(BurnModels, file = './data/BurnModels.Rdata')
      bind_rows(
      BurnModels$Sev_v_MaxC[2,c(6,8)] %>% 
        tibble() %>%
        add_column(Predictor = 'Canopy temperature', .before = 1), 
      BurnModels$Sev_v_SoilMaxC[2,c(6,8)] %>% 
        tibble() %>%
        add_column(Predictor = 'Soil surface temperature', .before = 1), 
      BurnModels$Sev_v_ROS[2,c(6,8)] %>% 
        tibble() %>%
        add_column(Predictor = 'Rate of spread', .before = 1) ) %>%
        mutate(Chisq = signif(Chisq, 3), 
               Chisq = round(Chisq, 1)) %>%
        arrange(desc(Chisq)) %>%
      rename('$\\Chi^2$' = Chisq, 
            P = `Pr(>Chisq)` ) %>%
      mutate(P = case_when(
              between(P,0.0009, 0.01) ~ '< 0.01',
              P <= 0.0009 ~ '< 0.001', 
              P >= 0.05 ~ '> 0.05', 
              TRUE ~ as.character(round(P, 2) ) ))  %>%
        pander::pander( )
      
      bind_rows(
        BurnModels$MaxC_v_NDVI[2,c(6,8)] %>% 
          tibble() %>%
          add_column(Predictor = 'Canopy temperature', .before = 1), 
        BurnModels$SoilMaxC_v_NDVI[2,c(6,8)] %>% 
          tibble() %>%
          add_column(Predictor = 'Soil surface temperature', .before = 1), 
        BurnModels$ROS_v_NDVI[2,c(6,8)] %>% 
          tibble() %>%
          add_column(Predictor = 'Rate of spread', .before = 1), 
        BurnModels$dNBR_v_NDVI[2,c(6,8)] %>% 
          tibble() %>%
          add_column(Predictor = 'Burn severity', .before = 1)  ) %>%
        mutate(Chisq = signif(Chisq, 3), 
               Chisq = round(Chisq, 1)) %>%
        arrange(desc(Chisq)) %>%
        rename('$\\Chi^2$' = Chisq, 
               P = `Pr(>Chisq)` ) %>%
        mutate(P = case_when(
          between(P,0.0009, 0.01) ~ '< 0.01',
          P <= 0.0009 ~ '< 0.001', 
          P >= 0.05 ~ '> 0.05', 
          TRUE ~ as.character(round(P, 2) ) ))  %>%
        pander::pander( )
      
##
##  Wildfires vs. Rx burns
##
  load('./data/BurnSeverityData.Rdata') #  data from the full set of burns
  load('./data/BurnModels.Rdata') # list to tuck results into
  
  BurnSeverityData %>%
    group_by(type) %>%
    summarize(Fires = n() )
  
  reg_dat <- 
    BurnSeverityData %>%
    mutate(zone = recode(L3, 'Northwestern Great Plains' = "west", 
                               'Northwestern Glaciated Plains' = 'east') ) %>%
    rename(dNBR = dNBR_Mean, NDVI = ndvi_Mean) %>%
    select(zone, type, dNBR, NDVI)
  
  # Burn severity (dNBR)
    # Distribution
      reg_dat %>%
        ggplot(aes(x=dNBR)) + theme_bw(16) + 
        geom_histogram(aes(y=after_stat(density)),      
                       binwidth=0.05,
                       colour="black",
                       fill="lightgreen") + 
        geom_density(alpha=.5, fill="lightblue")
      
    # Global models
      sev_0 <- lm(dNBR ~ 1, data = reg_dat)
      sev_add <- lm(dNBR ~ type + zone, data = reg_dat)
      sev_int <- lm(dNBR ~ type*zone, data = reg_dat)
      
      anova(sev_0, sev_add, sev_int)
      
      # anova(sev_0, sev_int)
      # car::Anova(sev_int)
      
      summary(sev_int)$r.squared 
      
      BurnModels$type_comp <- emmeans::emmeans(sev_int, ~  type | zone) 
      emmeans::joint_tests(BurnModels$type_comp)  
               
      emmeans::joint_tests(BurnModels$type_comp, by = 'zone') 
      
      # save(BurnModels, file = './data/BurnModels.Rdata')
      
  # Fuelbed greenness (NDVI)
    # Distribution
        reg_dat %>%
          ggplot(aes(x=NDVI)) + theme_bw(16) + 
          geom_histogram(aes(y=after_stat(density)),      
                         binwidth=0.05,
                         colour="black",
                         fill="lightgreen") + 
          geom_density(alpha=.5, fill="lightblue")
        
        # Global models
        grn_0 <- lm(NDVI ~ 1, data = reg_dat)
        grn_L3 <- lm(NDVI ~ region, data = reg_dat)
        grn_type <- lm(NDVI ~ type, data = reg_dat)
        grn_add <- lm(NDVI ~ type + region, data = reg_dat)
        grn_int <- lm(NDVI ~ type*region, data = reg_dat)
        
        anova(grn_0, grn_L3, grn_type, grn_add, grn_int)
        
        anova(grn_0, grn_type)
        car::Anova(grn_type)
        summary(grn_type)
  # severity vs. greenness 
        
        comp_0 <- lm(dNBR ~ 1, data = reg_dat)
        comp_1 <- lm(dNBR ~ NDVI, data = reg_dat)
        comp_int1 <- lm(dNBR ~ NDVI*type2, data = reg_dat)
        comp_int2 <- lm(dNBR ~ NDVI*region, data = reg_dat)
        comp_int3 <- lm(dNBR ~ NDVI:region:type2, data = reg_dat)
        
        anova(comp_0, comp_1, comp_int1, comp_int2, comp_int3)

        summary(comp_int1)
        summary(comp_int2) 
        summary(comp_int3)          
        
