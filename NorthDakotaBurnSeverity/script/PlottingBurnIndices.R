pacman::p_load(tidyverse)


load('./data/BurnIndices.Rdata')

# Burn severity as the response 
  BurnIndices %>%
    mutate(ros = ifelse(ros >= 13, NA, ros)) %>%
    pivot_longer(names_to = 'behavior', 
                 values_to = 'value', 
                 cols = c(MaxC:ros)) %>%
    ggplot(aes(x = value, y = dNBR)) + theme_bw() +
    geom_smooth(data = . %>%
                  filter(behavior != 'SoilMaxC'), 
                method = 'lm', color = 'black') +
    geom_smooth(data = . %>%
                  filter(behavior == 'SoilMaxC'), 
                method = 'lm', lty = 2,
                se = F, color = 'black') +
    geom_point() +
    facet_wrap(~behavior, scales = 'free_x')
  
# NDVI as the predictor 
  BurnIndices %>%
    mutate(ros = ifelse(ros >= 13, NA, ros)) %>%
    pivot_longer(names_to = 'response', 
                 values_to = 'value', 
                 cols = c(MaxC:dNBR)) %>%
    ggplot(aes(x = ndvi, y = value, color = location)) + theme_bw() +
    geom_smooth(data = . %>%
                  filter(response != 'SoilMaxC'), 
                method = 'lm', color = 'black') +
   #  geom_smooth(method = 'lm', se = FALSE) +
    geom_point() +
    facet_wrap(~response, scales = 'free_y')

# Histograms 

  # Maximum canopy temp
    MASS::fitdistr(BurnIndices$MaxC, 'normal')
    MASS::fitdistr(BurnIndices$MaxC, 'Gamma')
    
    BurnIndices %>%
    ggplot(aes(x=MaxC)) + theme_bw(16) + 
      geom_histogram(aes(y=after_stat(density)),      
                     binwidth=20,
                     colour="black",
                     fill="lightgreen") +
      coord_cartesian(xlim = c(0,500)) + 
      geom_density(alpha=.5, fill="lightblue")  +
      stat_function(data= BurnIndices , 
                    fun = dnorm, 
                    args=list(mean = 261.1,
                              sd = 103.9),
                    colour="blue", 
                    size=1.1)  +
      stat_function(data= BurnIndices , 
                    fun = dgamma, 
                    args=list(shape = 5.18,
                              rate = 0.02),
                    colour="darkred", 
                    size=1.1) 
  
  # Maximum soil surface temp
    SoilMaxC <- BurnIndices %>%
                filter(! is.na(SoilMaxC)) 
    MASS::fitdistr(SoilMaxC$SoilMaxC, 'Gamma')
       
    BurnIndices %>%
      ggplot(aes(x=SoilMaxC)) + theme_bw(16) + 
      geom_histogram(aes(y=after_stat(density)),      
                     binwidth=20,
                     colour="black",
                     fill="lightgreen") +
      #coord_cartesian(xlim = c(0,500)) + 
      geom_density(alpha=.5, fill="lightblue")  +
      stat_function(data= BurnIndices , 
                    fun = dgamma, 
                    args=list(shape = 1.39,
                              rate = 0.009),
                    colour="darkred", 
                    size=1.1) 
    
  # Rate of spread
    ros <- BurnIndices %>%
      filter(! is.na(ros), ros < 15) 
    MASS::fitdistr(ros$ros + 0.001, 'Gamma')
    MASS::fitdistr(ros$ros , 'lognormal')
    
    BurnIndices %>%
      filter(! is.na(ros), ros < 15) %>%
      ggplot(aes(x=ros + 0.001)) + theme_bw(16) + 
      geom_histogram(aes(y=after_stat(density)),      
                     binwidth=0.3,
                     colour="black",
                     fill="lightgreen") +
      coord_cartesian(xlim = c(0,15)) + 
      geom_density(alpha=.5, fill="lightblue")  +
      stat_function(data= BurnIndices , 
                    fun = dlnorm, 
                    args=list(meanlog = 0.72,
                              sdlog = 1.29),
                    colour="orange", 
                    size=1.1) +
      stat_function(data= BurnIndices , 
                    fun = dgamma, 
                    args=list(shape = 1.02,
                              rate = 0.282),
                    colour="darkred", 
                    size=1.1)
  # dNBR
    MASS::fitdistr(BurnIndices$dNBR, 'normal')

    BurnIndices %>%
      ggplot(aes(x=dNBR)) + theme_bw(16) + 
      geom_histogram(aes(y=after_stat(density)),      
                     binwidth=0.05,
                     colour="black",
                     fill="lightgreen") +
      coord_cartesian(xlim = c(-0.1,0.55)) + 
      geom_density(alpha=.5, fill="lightblue")  +
      stat_function(data= BurnIndices , 
                    fun = dnorm, 
                    args=list(mean = 0.257,
                              sd = 0.12),
                    colour="blue", 
                    size=1.1)  
    
# Wildfire - Rx comparison 
    
# dNBR boxplot 
    
  BurnSeverityData  %>% 
    mutate(zone = fct_rev(zone), 
           type = fct_relevel(type, c("Wildfire",'Rx' )), 
           type = recode(type, 'Rx'= "Prescribed\nburn")) %>%
    ggplot(aes(x= type, y = dNBR_Mean)) + theme_bw(16) +
    geom_boxplot(varwidth = TRUE, 
                 fill = 'lightblue', 
                 staplewidth = 0.25) +
    geom_point(data = . %>%
                 group_by(type, zone) %>%
                 summarize(Mean = mean(dNBR_Mean)), 
               aes(y = Mean), 
               pch = 24, size = 4, 
               color = 'white', fill = 'darkblue') + 
    labs(x = 'Fire type', 
         y = 'Burn severity (dNBR)') +           
    facet_wrap(~zone, scales = 'free_x') +
    theme(panel.grid.major.x = element_blank(), 
          axis.text.x = element_text(color = 'black'))
  
# NDVI boxplot 
  
  BurnSeverityData  %>% 
    mutate(zone = fct_rev(zone), 
           type = fct_relevel(type, c("Wildfire",'Rx' )), 
           type = recode(type, 'Rx'= "Prescribed\nburn")) %>%
    ggplot(aes(x= type, y = ndvi_Mean)) + theme_bw(16) +
    geom_boxplot(varwidth = TRUE, 
                 fill = 'lightgreen') +
    geom_point(data = . %>% 
                 group_by(type, zone) %>%
                 summarize(Mean = mean(ndvi_Mean)), 
               aes(y = Mean), 
               pch = 24, size = 4, 
               color = 'white', fill = 'darkgreen') + 

    labs(x = 'Fire type', 
         y = 'Fuelbed greenness (NDVI)') +           
    facet_wrap(~zone, scales = 'free_x') +
    theme(panel.grid.major.x = element_blank(), 
          axis.text.x = element_text(color = 'black'))
  
# dNBR vs NDVI scatterplot
    
  BurnSeverityData %>%
    mutate(zone = fct_rev(zone), 
           type = fct_relevel(type, c("Wildfire",'Rx' )), 
           type = recode(type, 'Rx'= "Prescribed\nburn")) %>%
    ggplot(aes(x = ndvi_Mean, y = dNBR_Mean)) + theme_bw(16) +
    geom_smooth(aes(color = type, fill = type), 
                method = 'lm') + 
    geom_errorbar(aes(ymin = dNBR_Mean - dNBR_SEM, 
                      ymax = dNBR_Mean + dNBR_SEM)) +
    geom_errorbarh(aes(xmin = ndvi_Mean - ndvi_SEM, 
                       xmax = ndvi_Mean + ndvi_SEM)) +
    geom_point() +
    facet_wrap(~zone) +
    labs(x = 'NDVI (fuelbed mean)', 
         y = 'dNBR (burn unit mean)')
    