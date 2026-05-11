
# Linear regression results 
  bind_rows(
    BurnModels$Sev_v_MaxC2 %>% 
      as.data.frame() %>%
      rownames_to_column('Term') %>%
      tibble() %>%
      mutate(Term = replace_values(Term, 'MaxC' ~ 'Flame temperature')) %>%
      cbind(MuMIn::r.squaredGLMM(BurnModels$mc_2) %>% 
              as_tibble() %>%
              add_row( R2m = NA,  R2c = NA) ), 
    BurnModels$Sev_v_SoilMaxC2 %>% 
      as.data.frame() %>%
      rownames_to_column('Term') %>%
      tibble() %>%
      mutate(Term = replace_values(Term, 'SoilMaxC' ~ 'Soil Surface temperature'))%>%
      cbind(MuMIn::r.squaredGLMM(BurnModels$sc_2) %>% 
              as_tibble() %>%
              add_row( R2m = NA,  R2c = NA) ), 
    BurnModels$Sev_v_ROS2 %>%
      as.data.frame() %>%
      rownames_to_column('Term') %>%
      tibble() %>%
      mutate(Term = replace_values(Term, 'ros' ~ 'Rate of spread')) %>%
      cbind(MuMIn::r.squaredGLMM(BurnModels$rs_2) %>% 
              as_tibble() %>%
              add_row( R2m = NA,  R2c = NA) ) ) %>%
    mutate(Chisq = signif(Chisq, 3), 
           Chisq = round(Chisq, 2), 
           across(c(R2m, R2c), ~ as.character(round(.x, 2)) ), 
           across(c(R2m, R2c), ~ replace_values(.x, NA ~ ''))) %>%
    rename('$\\Chi^2$' = Chisq, 
           P = `Pr(>Chisq)`, 
           `Marg. R$^2$` = R2m,
           `Cond. R$^2$` = R2c ) %>%
    mutate(P = case_when(
      between(P,0.0009, 0.01) ~ '< 0.01',
      P <= 0.0009 ~ '< 0.001', 
      P >= 0.05 ~ '> 0.05', 
      TRUE ~ as.character(round(P, 2) ) ) )  %>%
    select(-Df) %>% 
    xtable() %>%
    print(include.rownames = F, 
          comment = F, 
          sanitize.text.function=identity) 
  
# RS - FB comparison 
  RxPointSeverity %>%
    filter(between(dNBR, 0, 0.6)) %>%
    pivot_longer(names_to = 'Response', 
                 values_to = 'value', 
                 cols = c(MaxC:dNBR)) %>%
    filter(!is.na(value)) %>%
     group_by(zone, Response) %>%
    summarise_at(.vars = vars(value),
                 .funs = c(Minimum ='min',
                           Mean="mean", 
                           SEM = function(x) {sd(x)/sqrt(n()) }, 
                           Maximum = 'max')) %>%
    ungroup() %>%
    mutate(across(c(Minimum:Maximum), ~ round(.x, 2)), 
           Response = replace_values(Response, 
                                     'MaxC' ~ 'Flame temperature (\\degC)', 
                                     'SoilMaxC' ~ 'Soil surface temperature (\\degC)',
                                     'ros' ~ 'Rate of spread (m\\cdot min\\textsuperscript{-1})', 
                                     'dNBR' ~ 'Burn severity ($\\Delta$NBR)')) %>%
    unite('Mean', Mean:SEM, sep = ' $\\pm$ ') %>%
    rename(`Mean $\\pm$ S.E.` = Mean) %>% 
    xtable() %>%
    print(include.rownames = F, 
          comment = F, 
          sanitize.text.function=identity)  
  
# WF - Rx comparison 
  BurnSeverityData %>%
    filter(Mean_dNBR < 0.6) %>%
    rename(dNBR = Mean_dNBR) %>%
    group_by(zone, type) %>%
    summarise_at(.vars = vars(dNBR),
                 .funs = c(Minimum ='min',
                           Mean="mean", 
                           SEM = function(x) {sd(x)/sqrt(n()) }, 
                           Maximum = 'max')) %>%
    ungroup() %>%
    mutate(across(c(Minimum:Maximum), ~ round(.x, 2)), 
           type = replace_values(type, 'Rx' ~ 'Prescribed burn')) %>%
    unite('Mean', Mean:SEM, sep = '$\\pm$ ') %>%
    rename(Zone = zone, `Fire type` = type, 
           `Mean $\\pm$ S.E.` = Mean) %>% 
    xtable() %>%
    print(include.rownames = F, 
          comment = F, 
          sanitize.text.function=identity) 
  