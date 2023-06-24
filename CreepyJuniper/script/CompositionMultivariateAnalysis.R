pacman::p_load(tidyverse, vegan)
pacman::p_load_gh('devanmcg/wesanderson')
source("https://raw.githubusercontent.com/devanmcg/IntroRangeR/master/11_IntroMultivariate/ordinationsGGplot.R")
source("https://raw.githubusercontent.com/pmartinezarbizu/pairwiseAdonis/master/pairwiseAdonis/R/pairwise.adonis2.R")


veg_data <-
read_csv('./CreepyJuniper/data/TransectComposition.csv', 
         show_col_types = FALSE) %>%
  select(-Observer, -Date ) %>%
  distinct() %>% 
  mutate(FireYear = ifelse(FireYear %in% c('2006', '2007'), '2007', FireYear), 
         FireYear = as.numeric(FireYear), 
         TSF = as.character(2021 - FireYear), 
         TSF = case_when(
           TSF %in% c('6', '9') ~ '6-9', 
           TSF %in% c('14', '17') ~ '14-17', 
           TRUE ~ TSF
         )) %>% 
  group_by(TSF, FireName, Burned, Transect, SpeciesCode) %>%
  summarize(MeanCover = mean(PercentCover), 
            .groups = 'drop') %>% 
  pivot_wider(names_from = SpeciesCode, 
              values_from = MeanCover) %>%
  rownames_to_column("rowID")


veg <-
  veg_data %>% 
  select(rowID, achmil:poldod) %>%
    replace(is.na(.), 0) %>% 
    labdsv::dropspc(minocc = 10, 
                    minabu = 5)

veg_nmds <- metaMDS(dplyr::select(veg, -rowID), 'bray', 4, trace = F)

nmds_spp <- 
  scores(veg_nmds, "species") %>%
  as.data.frame() %>%
  as_tibble(rownames = "Species")

nmds_scores <- lst(species = nmds_spp)

nmds_scores$spiders <- 
  gg_ordiplot(veg_nmds, 
              groups = paste0(veg_data$FireName, '_', veg_data$TSF, '_', veg_data$Burned), 
              plot=FALSE)$df_spiders %>% 
  as_tibble() %>%
  rename(NMDS1 = x, NMDS2 = y) %>%
  separate(Group, into=c('Fire', 'TSF', 'Burned'), sep = '_') %>%
  mutate(TSF = factor(TSF, levels = c('4','6-9','14-17', '22')), 
         Burned = ifelse(Burned == 'N', 'Not burned', 'Burned'))


# perMANOVA

perm = how(nperm = 99) 
setBlocks(perm) <- with(veg_data, FireName)

adonis2(select(veg, - rowID) ~ TSF*Burned, data = veg_data, permutations = perm, by = 'term') %>%
  pander::pander() 

# Plotting 

# Plot ordination 
# site scores by treatment and year
ggplot() + theme_bw(20) + 
  labs(x="NMDS Axis 1", 
       y="NMDS Axis 2") + 
  geom_vline(xintercept = 0, lty=3, color="darkgrey") +
  geom_hline(yintercept = 0, lty=3, color="darkgrey") +
  geom_point(data=nmds_scores$species, 
             aes(x=NMDS1, y=NMDS2), 
             pch = '+', 
             size = 3,
             colour="darkblue") +
  geom_segment(data=nmds_scores$spiders, 
               aes(x=cntr.x, y=cntr.y,
                   xend=NMDS1, yend=NMDS2, 
                   group = Fire,
                   color=Burned), 
               size=1.2) +
  geom_point(data=nmds_scores$spiders, 
             aes(x=NMDS1, y=NMDS2, 
                 bg=Burned, 
                 shape = Burned), 
             colour="black", 
             size=3, stroke=2) +
  facet_wrap(~TSF, nrow = 1) + 
  scale_shape_manual(" ", values = c(21, 24) ) + 
  scale_color_manual(" ", values = wes_palette('DeadLiveLight')[c(2,1)]) + 
  scale_fill_manual(" ", values = wes_palette('DeadLiveLight')[c(2,1)]) + 
  theme(panel.grid=element_blank(), 
        legend.position="top", 
        axis.title = element_text(size = 18), 
        axis.text = element_text(color = 'darkgrey', size = 12))

# species scores
ggplot() + theme_bw(16) + 
  labs(x="NMDS Axis 1", 
       y="NMDS Axis 2") + 
  geom_vline(xintercept = 0, lty=3, color="darkgrey") +
  geom_hline(yintercept = 0, lty=3, color="darkgrey") +
  geom_text(data=nmds_scores$species, 
            aes(x=NMDS1, y=NMDS2, 
                label=Species), 
            colour="darkblue") +
  theme(panel.grid=element_blank() ) 
