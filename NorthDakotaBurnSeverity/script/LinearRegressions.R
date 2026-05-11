pacman::p_load(tidyverse)


load('./data/RxPointSeverity.Rdata') # plot-level data 

BurnModels <- lst() 

# Plot-level data 

  sev_dat <-   RxPointSeverity %>%
                filter(between(dNBR, 0, 0.6)) 

# Testing drivers of burn severity
  # Canopy temperature
    mc_0 <- lme4::lmer(dNBR ~ 1 + (1|location/burn), REML = FALSE, data = sev_dat )
    mc_1 <- lme4::lmer(dNBR ~ MaxC + (1|location/burn), REML = FALSE, data = sev_dat )
    mc_2 <- lme4::lmer(dNBR ~ MaxC + zone + (1|location/burn), REML = FALSE, data = sev_dat )

    BurnModels$Sev_v_MaxC <- anova(mc_0, mc_1)
    BurnModels$Sev_v_MaxC2 <- car::Anova(mc_2)
    BurnModels$mc_1 <- mc_1
    BurnModels$mc_2 <- mc_2
    
  # Soil surface temperature
    sc_0 <- lme4::lmer(dNBR ~ 1 + (1|location/burn), REML = FALSE, 
                       data = filter(sev_dat , !is.na(SoilMaxC) ) )
    sc_1 <- lme4::lmer(dNBR ~ SoilMaxC + (1|location/burn), REML = FALSE, 
                       data = filter(sev_dat , !is.na(SoilMaxC)) )
    sc_2 <- lme4::lmer(dNBR ~ SoilMaxC + zone + (1|location/burn), REML = FALSE, 
                       data = filter(sev_dat , !is.na(SoilMaxC)) )
    
    BurnModels$Sev_v_SoilMaxC <- anova(sc_0, sc_1)
    BurnModels$Sev_v_SoilMaxC2 <- car::Anova(sc_2)
    BurnModels$sc_1 <- sc_1
    BurnModels$sc_2 <- sc_2
    
  # Rate of spread
    rs_0 <- lme4::lmer(dNBR ~ 1 + (1|location/burn), REML = FALSE,  
                        data = filter(sev_dat , ! is.na(ros)) )
    rs_1 <- lme4::lmer(dNBR ~ ros + (1|location/burn), REML = FALSE,  
                        data = filter(sev_dat , ! is.na(ros)) )
    rs_2 <- lme4::lmer(dNBR ~ ros + zone + (1|location/burn), REML = FALSE,  
                       data = filter(sev_dat , ! is.na(ros)) )
    
    
    BurnModels$Sev_v_ROS <- anova(rs_0, rs_1)
    BurnModels$Sev_v_ROS2 <- car::Anova(rs_2)
    BurnModels$rs_1 <- rs_1
    BurnModels$rs_2 <- rs_2
    
  # save(BurnModels, file = './data/BurnModels.Rdata')
    
##
##  Wildfires vs. Rx burns
##
  load('./data/BurnSeverityData.Rdata') #  data from the full set of burns
  load('./data/BurnModels.Rdata') # list to tuck results into
  
  reg_dat <-
  BurnSeverityData %>%
    filter(Mean_dNBR < 0.6) %>%
    rename(dNBR = Mean_dNBR)
  
  BurnSeverityData %>%
    group_by(type) %>%
    summarize(Fires = n() )
  
  # Burn severity (dNBR)

      
    # Global models
      sev_0 <- lm(dNBR ~ 1, data = reg_dat)
      sev_add <- lm(dNBR ~ type + zone, data =  reg_dat)
      sev_int <- lm(dNBR ~ type * zone, data =  reg_dat)
      
      anova(sev_0, sev_add, sev_int)
      
      # anova(sev_0, sev_int)
      # car::Anova(sev_int)
      
      summary(sev_int)$r.squared 
      
      BurnModels$type_comp <- emmeans::emmeans(sev_int, ~  type | zone) 
      emmeans::joint_tests(BurnModels$type_comp)  
               
      emmeans::joint_tests(BurnModels$type_comp, by = 'zone') 
      
      # save(BurnModels, file = './data/BurnModels.Rdata')
      
      
    # Distribution
      BurnSeverityData %>%
        filter(Mean_dNBR < 0.6) %>% 
        ggplot(aes(x=Mean_dNBR)) + theme_bw(16) + 
        geom_histogram(aes(y=after_stat(density)),      
                       binwidth=0.05,
                       colour="black",
                       fill="lightgreen") + 
        geom_density(alpha=.5, fill="lightblue")
        
