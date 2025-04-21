{
  pacman::p_load(tidyverse, readxl)
pacman::p_load_gh('devanmcg/wesanderson')
}


keys <- read_xlsx('./Data/DigitizedData.xlsx', 'OptionKeys' )

responses <- 
  read_xlsx('./Data/DigitizedData.xlsx', 'AgreementIndices' ) %>%
    separate(Coordinates, into = c('X', 'Y'), sep = ', ') %>%
    mutate(across(c(X, Y), ~ as.numeric(.)), 
           Y = round(Y, 0) ) %>%
    pivot_wider(names_from = Endpoint, 
                values_from = X, 
                id_cols = -Y) %>%
    rowwise() %>% 
    mutate(mid = mean(c(min, max))) %>%
    ungroup() %>% 
    full_join(by = c('QuestionKey', 'OptionKey'), 
              keys) 

# save(responses, file = './Data/responses.Rdata')

#
# Public grazing land management and personal recreation
#
responses %>%
  filter(QuestionKey == "PublicGrazingBeliefs", 
         OptionKey != 'public_fire')  %>%
  mutate(response = case_when(
           OptionKey == 'increase_public_fire' ~ 'There should be more Rx fire\non public grazing lands', 
           OptionKey == 'amount_public_fire' ~   'There is an appropriate amount of\nRx fire on public grazing lands', 
           OptionKey == 'admin_users' ~          'Administrators should manage to meet\nuser needs or preferences', 
           OptionKey == 'admin_science' ~        "Administrators should manage land\naccording to the best available science", 
           OptionKey == 'admin_good' ~           'Public grazing land administrators\n do a good job managing the land' )) %>%
  ggplot() + theme_bw(16) + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_errorbarh(aes(xmin = min, 
                     xmax = max, 
                     y = fct_reorder(response, desc(SurveyOrder))) , 
                 color = wes_palette("FantasticFox1")[3], 
                 height = 0.5,
                 lwd = 0.9 ) +
  geom_point(aes(x = mid, y = response ),
             fill = wes_palette("FantasticFox1")[3], 
             pch = 21,
             size = 4, 
             stroke = 1 ) + 
  # coord_cartesian(xlim = c(-1.15, 1.9)) + 
  labs(title = '"I personally believe the following about public grazingland management"', 
       x = 'Agreement Index') + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 14), 
        plot.title.position = "plot" )

responses %>%
  filter(QuestionKey == 'Recreation' ) %>%
  ggplot() + theme_bw(16) + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_errorbarh(aes(xmin = min, 
                     xmax = max, 
                     y = fct_reorder(SurveyText, mid) ) , 
                 color = wes_palette("FantasticFox1")[3], 
                 height = 0.5,
                 lwd = 0.9 ) +
  geom_point(aes(x = mid, y = SurveyText ),
             fill = wes_palette("FantasticFox1")[3], 
             pch = 21,
             size = 4, 
             stroke = 1 ) + 
  # coord_cartesian(xlim = c(-1.15, 1.9)) + 
  labs(title = '"Myself and/or my family use public grazingland for these recreation activities"', 
       x = 'Agreement Index') + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 14), 
        plot.title.position = "plot" )

  

#
# Beliefs about Rx fire
#
  responses %>%
    filter(QuestionKey == "CommunityFireBeliefs")  %>%
    mutate(OptionKey = str_remove(OptionKey, 'burn_'), 
           response = case_when(
              OptionKey == 'wildlife' ~ 'Negative impacts to wildlife', 
              OptionKey == 'soil_erosion' ~   'Results in soil erosion', 
              OptionKey == 'smoke' ~    'Produces harmful smoke', 
              OptionKey == 'safety' ~   'Poses risk to human safety', 
              OptionKey == 'property_risk' ~   "Poses risk to nearby property", 
              OptionKey == 'forage' ~      'Results in loss of forage' )) %>%
    ggplot() + theme_bw(16) + 
    geom_vline(xintercept = 0, lty = 2) + 
    geom_errorbarh(aes(xmin = min, 
                       xmax = max, 
                       y = fct_reorder(response, desc(SurveyOrder))) , 
                   color = wes_palette("FantasticFox1")[3], 
                   lwd = 0.9 ) +
    geom_point(aes(x = mid, y = response ),
               fill = wes_palette("FantasticFox1")[3], 
               pch = 21,
               size = 4, 
               stroke = 1 ) + 
    # coord_cartesian(xlim = c(-1.15, 1.9)) + 
    labs(title = '"Members of my community generally believe the following about Rx fire"', 
         x = 'Agreement Index') + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(color = 'black', size = 14), 
          plot.title.position = "plot" )
  
  responses %>%
    filter(str_starts(QuestionKey, "Attitudes") ) %>%
    mutate(OptionKey = str_remove(OptionKey, 'burn_'), 
           response = case_when(
             OptionKey == 'beneficial' ~ 'Rx fire is a beneficial\ntool for managing rangelands', 
             OptionKey == 'frequent' ~   'People should be able to\nburn as frequently as they\n want for management', 
             OptionKey == 'limited' ~    'Burning can be used in a\nlimited set of instances', 
             OptionKey == 'man_plan' ~   'My management plan includes\nusing prescribed fire', 
             OptionKey == 'prepared' ~   "I'm prepared to use Rx fire\nif I choose to", 
             OptionKey == 'pros' ~      'Fire should only be used\nby professionals', 
             OptionKey == 'shouldnt' ~   'Burning is a poor management\noption that shouldn’t be used', 
             OptionKey == 'training' ~ 'Burning is a good option\nfor anyone with training')) %>%
    ggplot() + theme_bw(16) + 
    geom_vline(xintercept = 0, lty = 2) + 
    geom_errorbarh(aes(xmin = min, 
                       xmax = max, 
                       y = fct_reorder(response, desc(SurveyOrder))) , 
                   color = wes_palette("FantasticFox1")[3], 
                   lwd = 0.9 ) +
    geom_point(aes(x = mid, y = response ),
               fill = wes_palette("FantasticFox1")[3], 
               pch = 21,
               size = 4, 
               stroke = 1 ) + 
    # coord_cartesian(xlim = c(-1.15, 1.9)) + 
    labs(title = '"I personally believe the following"', 
         x = 'Agreement Index') + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(color = 'black', size = 14), 
          plot.title.position = "plot" )

#
# Questions about management decisionmaking
#
  #decisions_importance <- 
    responses %>%
      filter(str_starts(QuestionKey, "Importance") ) %>%
      mutate(response = case_when(
              OptionKey == 'restore_prairie' ~      'Restore prairie/grassland', 
              OptionKey == 'reduce_wildfire' ~        'Reduce wildfire risk', 
              OptionKey == 'protect_wildlife' ~     'Protect wildlife habitat', 
              OptionKey == 'heterogeneity' ~   'Increase plant diversity', 
              OptionKey == 'control_woody' ~ 'Woody plant control', 
              OptionKey == 'control_invasives' ~      'Invasive species control' )) %>%
  ggplot() + theme_bw(16) + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_errorbarh(aes(xmin = min, 
                     xmax = max, 
                     y = response), 
                 color = wes_palette("FantasticFox1")[3], 
                 lwd = 0.9 ) +
  geom_point(aes(x = mid, y = response ),
             fill = wes_palette("FantasticFox1")[3], 
             pch = 21,
             size = 4, 
             stroke = 1 ) + 
  # coord_cartesian(xlim = c(-1.15, 1.9)) + 
  labs(title = '"This environmental concern is important to me"', 
       x = 'Agreement Index') + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(color = 'black', size = 14), 
        plot.title.position = "plot" )
    
  responses %>%
    filter(str_starts(QuestionKey, "Influence") ) %>%
    mutate(response = case_when(
              OptionKey == 'research' ~      'Scientific research/\nUniversity publications', 
              OptionKey == 'neighbors' ~     'How my neighbors\nmanage their land', 
              OptionKey == 'good' ~          'Being a good steward', 
              OptionKey == 'gen' ~   'What previous generations\ntaught me', 
              OptionKey == 'advice' ~ 'Professional advice' )) %>%
    ggplot() + theme_bw(16) + 
    geom_vline(xintercept = 0, lty = 2) + 
    geom_errorbarh(aes(xmin = min, 
                       xmax = max, 
                       y = fct_reorder(response, mid)), 
                   color = wes_palette("FantasticFox1")[5], 
                   lwd = 0.9 ) +
    geom_point(aes(x = mid, y = response ),
               fill = wes_palette("FantasticFox1")[5], 
               pch = 21,
               size = 4, 
               stroke = 1 ) + 
    # coord_cartesian(xlim = c(-1.15, 1.9)) + 
    labs(title = '"These considerations affect my management decisionmaking"', 
         x = 'Agreement Index') + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(color = 'black', size = 14), 
          plot.title.position = "plot" )

#
# Questions about agency associations & trust
#
  agency_responses <- 
    responses %>%
      filter(QuestionKey != 'Agency_Resources', 
             str_starts(QuestionKey, "Agency") ) %>%
      mutate(QuestionKey = str_remove(QuestionKey, "Agency_"), 
             QuestionKey = str_replace_all(QuestionKey,  "([[:upper:]])"," \\1"), 
             QuestionKey = str_trim(QuestionKey)) %>% 
      separate(QuestionKey, into = c('category', 'topic'), sep = ' ') %>%
      mutate(agency = case_when(
                        OptionKey == 'NDGF' ~      'ND Game & Fish', 
                        OptionKey == 'PF' ~        'Pheasants Forever', 
                        OptionKey == 'USFWS' ~     'US Fish & Wildlife', 
                        OptionKey == 'audubon' ~   'Audubon Society', 
                        OptionKey == 'extension' ~ 'County Extension', 
                        OptionKey == 'USFS' ~      'US Forest Service', 
                        TRUE ~ str_to_upper(OptionKey)  ))

# Resources in general 
  agency_responses %>%
    filter(category != "Fire") %>%
    mutate(Response = recode(topic, 
                             "Trust" = "Trust", 
                             "Use" = "Use")) %>%
    ggplot() + theme_bw(16) + 
    geom_vline(xintercept = 0, lty = 2) + 
    geom_errorbarh(aes(xmin = min, 
                       xmax = max, 
                       y = agency, 
                       color = Response), 
                   lwd = 0.9,
                   position = position_dodge(width = 0.5)) +
    geom_point(aes(x = mid, y = agency, 
                   fill = Response, 
                   shape = Response), 
               size = 4, 
               stroke = 1,
               position = position_dodge(width = 0.5)) + 
    coord_cartesian(xlim = c(-1.15, 1.9)) + 
    labs(title = '"In general, I personally use/trust resources from this organization"', 
         x = 'Agreement Index') + 
    scale_shape_manual(values = c(21, 24)) + 
    scale_color_manual(values = wes_palette("FantasticFox1")[c(3,5)]) + 
    scale_fill_manual(values = wes_palette("FantasticFox1")[c(3,5)]) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(color = 'black'), 
          plot.title.position = "plot", 
          legend.position = 'top')

# WRT fire, specifically 
  agency_responses %>%
    filter(category == "Fire") %>%
    mutate(Response = recode(topic, 
                          "Trust" = "Trust regarding Rx fire", 
                          "Use" = "Associate with Rx fire")) %>%
    ggplot() + theme_bw(16) + 
    geom_vline(xintercept = 0, lty = 2) + 
      geom_errorbarh(aes(xmin = min, 
                         xmax = max, 
                         y = agency, 
                         color = Response), 
                     lwd = 0.9,
                     position = position_dodge(width = 0.5)) +
      geom_point(aes(x = mid, y = agency, 
                     fill = Response, 
                     shape = Response), 
                 size = 4, 
                 stroke = 1,
                 position = position_dodge(width = 0.5)) + 
    coord_cartesian(xlim = c(-1.15, 1.9)) + 
    labs(title = '"I associate/trust this organization with Rx fire activity or information"', 
         x = 'Agreement Index') + 
     scale_shape_manual(values = c(21, 24)) + 
    scale_color_manual(values = wes_palette("FantasticFox1")[c(3,5)]) + 
    scale_fill_manual(values = wes_palette("FantasticFox1")[c(3,5)]) + 
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_text(color = 'black'), 
            plot.title.position = "plot")
  


