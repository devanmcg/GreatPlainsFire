pacman::p_load(tidyverse)


load('./data/BurnIndices.Rdata')

# Burn severity as the response 
RxPointSeverity %>%
  filter(dNBR < 0.6)%>%
    mutate(ros = ifelse(ros >= 13, NA, ros)) %>%
    pivot_longer(names_to = 'behavior', 
                 values_to = 'value', 
                 cols = c(MaxC:ros)) %>%
    ggplot(aes(x = value, y = dNBR, color = zone)) + theme_bw() +
    geom_smooth(data = . %>%
                filter(behavior != 'SoilMaxC'), 
                method = 'lm', color = 'black') +
    geom_smooth(data = . %>%
                  filter(behavior == 'SoilMaxC'),
                method = 'lm') +
    geom_point() +
    facet_wrap(~behavior, scales = 'free_x')

RxPointSeverity %>%
  mutate(ros = ifelse(ros >= 13, NA, ros), 
         zone = ifelse(location == 'CGREC', 'East', 'West')) %>%
  pivot_longer(names_to = 'behavior', 
               values_to = 'value', 
               cols = c(MaxC:ros)) %>%
  ggplot(aes(x = value, y = dNBR, color = zone)) + theme_bw() +
  geom_smooth(method = 'lm') +
  geom_point() +
  facet_wrap(~behavior, scales = 'free_x')
  
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
    MASS::fitdistr(RxPointSeverity$dNBR, 'normal')

    RxPointSeverity %>%
      filter(dNBR < 0.6) %>%
      ggplot(aes(x=dNBR)) + theme_bw(16) + 
      geom_histogram(aes(y=after_stat(density)),      
                     binwidth=0.05,
                     colour="black",
                     fill="lightgreen") +
      #coord_cartesian(xlim = c(-0.1,0.55)) + 
      geom_density(alpha=.5, fill="lightblue")  +
      stat_function(data= BurnIndices , 
                    fun = dnorm, 
                    args=list(mean = 0.32,
                              sd = 0.16),
                    colour="blue", 
                    lwd=1.1)  
    
# Wildfire - Rx comparison 
    
# dNBR boxplot 
    sev_cat <-
      tibble(Severity= c('Unburned', 'Low', 'Mod. low', ' Mod. high', 'High'), 
             dNBR = c(0.1, 0.27, 0.44, 0.66, 1.3) ) %>%
      mutate(Severity = as.factor(Severity), 
             Severity = fct_reorder(Severity, dNBR))
    
    BurnSeverityData  %>% 
      filter(Mean_dNBR < 0.6) %>%
      mutate(zone = fct_rev(zone), 
             zone = paste0(str_to_title(zone), 'ern zone'),
             type = fct_relevel(type, c("Wildfire",'Rx' )), 
             type = recode(type, 'Rx'= "Prescribed\nburn")) %>%
      ggplot(aes(x= type, y = Mean_dNBR)) + theme_bw(16) +
      geom_hline(data = filter(sev_cat, dNBR < 0.7),
                 aes(yintercept = dNBR, 
                     lty = Severity, 
                     color = Severity) , 
                 lwd = 1.1 ) + 
      geom_boxplot(varwidth = TRUE, 
                   fill = 'lightblue', 
                   staplewidth = 0.25) +
      geom_point(data = . %>%
                   group_by(type, zone) %>%
                   summarize(Mean = mean(Mean_dNBR)), 
                 aes(y = Mean), 
                 pch = 24, size = 4, 
                 color = 'white', fill = 'darkblue') + 
      labs(x = 'Fire type', 
           y = 'Burn severity (ΔNBR)') +           
      facet_wrap(~zone, scales = 'free_x') +
      coord_cartesian(ylim = c(0.08,0.65)) +
      scale_linetype_manual("Severity class", values = c(1, 3, 2, 5)) + 
      scale_color_manual("Severity class", 
                         values = c('grey80', 'grey60', 'grey40', 'grey20')) + 
      scale_y_continuous(breaks = c(0.1, 0.25, 0.44, 0.66), 
                         labels = c(0.1, 0.25, 0.44, 0.66)) + 
      theme(strip.text = element_text(face = 'bold'),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.text.x = element_text(color = 'black'), 
            legend.key.width = unit(2.0, 'cm'), 
            legend.key = element_rect(hjust = 0.5)) +  
      guides(linetype = guide_legend(reverse=TRUE), 
             color = guide_legend(reverse=TRUE)) 

  # Thresholds

    