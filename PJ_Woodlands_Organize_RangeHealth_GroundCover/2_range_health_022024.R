library(tidyverse)
library(ggpubr)
library(cowplot)

pj_rh_aim <- read_csv('data/rh_pj_aim_ind.csv') %>% 
  mutate(PrimaryKey = as.factor(PrimaryKey))

# NRI not available without confidentiality agreement, private data
pj_rh_nri <- read_csv('data/rh_pj_nri_ind.csv') %>% 
  mutate(PrimaryKey = as.factor(PrimaryKey))

aim_data_all_clean <- pj_rh_aim %>% 
  mutate(source = as.factor(source),
species = as.factor(species),
state = as.factor(state),
ecositeID = as.factor(ecositeID),
modis = as.factor(modis_landcover))

nri_data_all_clean <- pj_rh_nri %>% 
  mutate(source = as.factor(source),
         species = as.factor(species),
         state = as.factor(state),
         ecositeID = as.factor(ecositeID))


pj_samps_aim <- aim_data_all_clean  %>% mutate(across(where(is.numeric),~replace_na(.,0)),
                                          across(where(is.factor),~fct_explicit_na(.,'0')))%>% 
  unique()%>% 
  filter(!species %in% c("JUGR7","JUCO", "PICE", "PIEL", "PIRE5")) %>%  #not enough data
  droplevels() %>% 
  filter(species %in% c("JUOS","JUMO", "PIED", "JUAS", "JUDE2","JUOC", "JUPI", "JUSC2", "JUVI", "PIMO")) %>%  #not enough data
  droplevels() %>% 
  # fix mis-IDs
  mutate(species = as.factor(case_when(species == "JUOC" & state == "MT" ~ "JUOS",
                                       species == "JUOC" & state == "WY" ~ "JUOS",
                                       species == "JUOC" & state == "UT" ~ "JUOS",
                                       species == "JUOC" & state == "CO" ~ "JUOS",
                                       species == "JUOC" & state == "AZ" ~ "NA",
                                       species == "JUPI" & state == "AZ" ~ "JUCO11", # AZ JUPI is more likely JUCO11/redberry, same common name
                                       species == "JUPI" & state == "NM" & county == "San Juan"  ~ "JUSC2", # High Mountain JUPI? Miss ID
                                       species == "JUOC" & state == "NM" ~ "JUOS",
                                       species == "JUOC" & state == "ND" ~ "JUVI",
                                       species == "JUOC" & state == "SD" ~ "JUVI",
                                       species == "JUVI" & state == "MT" ~ "JUVI",
                                       TRUE ~ as.factor(species)))) %>% 
  filter(!species %in% c("JUCO11", "NA")) %>% #not enough data
  droplevels()



pj_samps_small_aim <- pj_samps_aim %>% 
  filter(species %in% c("JUOS","JUMO", "PIED", "JUOC", "JUVI", "PIMO")) %>%  #only species with enough data
  droplevels() %>% 
  select(PrimaryKey, species, speciescount, density, source, rh_hf_sum, rh_bi_sum, rh_sss_sum, rh_plantcommunitycomp,
         AH_PerenForbGrassCover, AH_ShrubCover, BareSoilCover) # keep only variables of interest

pj_samps_nri <- nri_data_all_clean  %>% mutate(across(where(is.numeric),~replace_na(.,0)),
                                               across(where(is.factor),~fct_explicit_na(.,'0')))%>% 
  unique()%>% 
  filter(!species %in% c("JUGR7","JUCO", "PICE", "PIEL", "PIRE5")) %>%   #only species with enough data
  droplevels() %>% 
  filter(species %in% c("JUOS","JUMO", "PIED", "JUAS", "JUDE2","JUOC", "JUPI", "JUSC2", "JUVI", "PIMO")) %>%   #only species with enough data
  mutate(species = as.factor(case_when(species == "JUOC" & state == "MT" ~ "JUOS",
                                       species == "JUOC" & state == "WY" ~ "JUOS",
                                       species == "JUOC" & state == "UT" ~ "JUOS",
                                       species == "JUOC" & state == "CO" ~ "JUOS",
                                       species == "JUOC" & state == "AZ" ~ "NA",
                                       species == "JUPI" & state == "AZ" ~ "JUCO11", # AZ JUPI is more likely JUCO11/redberry, same common name
                                       species == "JUPI" & state == "NM" & county == "San Juan"  ~ "JUSC2", # High Mountain JUPI? Miss ID
                                       species == "JUOC" & state == "NM" ~ "JUOS",
                                       species == "JUOC" & state == "ND" ~ "JUVI",
                                       species == "JUOC" & state == "SD" ~ "JUVI",
                                       species == "JUVI" & state == "MT" ~ "JUVI",
                                       TRUE ~ as.factor(species)))) %>% 
  filter(!species %in% c("JUCO11", "NA")) %>% #not enough data
  droplevels()


pj_samps_small_nri <- pj_samps_nri %>% 
  filter(species %in% c("JUOS","JUMO", "PIED", "JUOC", "JUVI", "PIMO")) %>%   #only species with enough data
  droplevels() %>% 
  select(PrimaryKey, species, speciescount, density, source, rh_hf_sum, rh_bi_sum, rh_sss_sum, rh_plantcommunitycomp,
         AH_PerenForbGrassCover, AH_ShrubCover, BareSoilCover)

pj_samps <- rbind(pj_samps_small_aim, pj_samps_small_nri) %>% 
  unique()%>% 
  mutate(speciescount = recode(speciescount, `0` = 1), # fix AIM protocol notation to match NRI, species recorded in census so there's at least one individual
         density = recode(density, `0` = 1))# fix AIM protocol notation to match NRI, species recorded in census so there's at least one individual

pj_samps %>% 
  filter(rh_hf_sum >= 1) %>% # ensure complete data
  ggscatter(y = "density", x = "rh_hf_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_bi_sum >= 1) %>% # ensure complete data
  ggscatter(y = "density", x = "rh_bi_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_sss_sum >= 1) %>% # ensure complete data
  ggscatter(y = "density", x = "rh_sss_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ is there
  filter(density > 1) %>% # ensure complete data
  ggscatter(y = "speciescount", x = "rh_hf_sum", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(speciescount > 1) %>%  # ensure PJ is there
  ggscatter(y = "speciescount", x = "rh_bi_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(speciescount > 1) %>%  # ensure PJ is there
  ggscatter(y = "speciescount", x = "rh_sss_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_hf_sum >= 1) %>%  # ensure PJ is there
  ggscatter(y = "AH_PerenForbGrassCover", x = "rh_hf_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_bi_sum >= 1) %>%  # ensure PJ is there
  ggscatter(y = "AH_PerenForbGrassCover", x = "rh_bi_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_hf_sum >=  1) %>%  # ensure PJ is there
  ggscatter(y = "AH_PerenForbGrassCover", x = "rh_sss_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_hf_sum >= 1) %>%  # ensure PJ is there
  ggscatter(y = "BareSoilCover", x = "rh_hf_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_bi_sum >= 1) %>%  # ensure PJ is there
  ggscatter(y = "BareSoilCover", x = "rh_bi_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")

pj_samps %>% 
  filter(rh_hf_sum >=  1) %>%  # ensure PJ is there
  ggscatter(y = "BareSoilCover", x = "rh_sss_sum", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman")


# summary calcs
pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ is there
  group_by(species) %>% 
  summarise(mean = mean(rh_hf_sum),
            mead_den = mean(density),
            mean_rich = mean(speciescount),
            sd = sd(rh_hf_sum),
            sd_den = sd(density),
            sd_rich = sd(speciescount))

pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ is there
  group_by(species) %>% 
  summarise(mean = mean(rh_bi_sum),
            mead_den = mean(density),
            mean_rich = mean(speciescount),
            sd = sd(rh_bi_sum),
            sd_den = sd(density),
            sd_rich = sd(speciescount))

pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ is there
  group_by(species) %>% 
  summarise(mean = mean(rh_sss_sum),
            mead_den = mean(density),
            mean_rich = mean(speciescount),
            sd = sd(rh_sss_sum),
            sd_den = sd(density),
            sd_rich = sd(speciescount))


rh_hf_rich_fig <- pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ is there
  filter(rh_hf_sum > 1) %>% # ensure complete data
  ggplot(mapping = aes((rh_hf_sum), (speciescount))) + 
  xlab("Hydologic Function")+
  ylab("Richness (#)")+
  geom_point(aes(group = species), alpha = 0.2, position = "jitter", color = "darkblue")+
  geom_smooth(aes(color = "Richness"), method = "gam", span = 0.1, linewidth =2, show.legend = T)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.0, 0.5))+
  theme_pubr(legend = "top")+
  labs_pubr(base_size = 24)

rh_hf_rich_fig

rh_bi_rich_fig <- pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ is there
  filter(rh_bi_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_bi_sum), (speciescount))) + 
  xlab("Biotic Integrity")+
  ylab("Richness (#)")+
  geom_point(aes(group = species), alpha = 0.2, position = "jitter", color = "mediumpurple3")+
  geom_smooth(aes(color = "Richness"), method = "gam", span = 0.1, linewidth =2, show.legend = T)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.0, 1))+
  theme_pubr(legend = "top")+
  labs_pubr(base_size = 24)

rh_bi_rich_fig

rh_sss_rich_fig <- pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ is there
  filter(rh_sss_sum > 1) %>% # ensure complete data
  ggplot(mapping = aes((rh_sss_sum), (speciescount))) + 
  xlab("Soil and Site Stability")+
  ylab("Richness (#)")+
  geom_point(aes(group = species), alpha = 0.2, position = "jitter", color = "darkorange")+
  geom_smooth(aes(color = "Richness"), method = "gam", span = 0.1, linewidth =2, show.legend = T)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.0, 0.5))+
  theme_pubr(legend = "top")+
  labs_pubr(base_size = 24)

rh_sss_rich_fig 


rh_rich_combo <- plot_grid(rh_hf_rich_fig + theme(legend.position="none"),
                         rh_bi_rich_fig+ theme(legend.position="none"),
                         rh_sss_rich_fig+ theme(legend.position="none"),
                         align = 'vh',
                         nrow = 1,
                         ncol = 3)
rh_rich_combo

ggsave(filename = "figures/rh_combo_rich_fig_02132024.tiff",
       plot = rh_rich_combo,
       dpi = 800,
       width = 16,
       height = 12,
       units = "in")

#separate density figs?

rh_hf_rich_fig_den <- pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ there
  filter(rh_hf_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_hf_sum), (density))) + 
  xlab("Hydrologic Function")+
  ylab("Categorical PJ Density (#/plot)")+
  geom_point(aes(group = species), alpha = 0.2, position = "jitter", color = "darkblue")+
  geom_smooth(mapping = aes(rh_hf_sum, density, color = "Density"), color = "black", method = "lm", linewidth = 2, show.legend = TRUE )+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.0, 0.5))+
  theme_pubr(legend = "top")+
  labs_pubr(base_size = 22)

rh_hf_rich_fig_den

ggsave(filename = "figures/rh_hf_den_fig_02132024.tiff",
       plot = rh_hf_rich_fig_den,
       dpi = 800,
       width = 12,
       height = 10,
       units = "in")

rh_bi_rich_fig_den <- pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ there
  filter(rh_bi_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_bi_sum), (density))) + 
  xlab("Biotic Integrity")+
  ylab("Categorical PJ Density (#/plot)")+
  geom_point(aes(group = species), alpha = 0.2, position = "jitter", color = "mediumpurple3")+
  geom_smooth(mapping = aes(rh_bi_sum, density, color = "Density"), color = "black", method = "lm", linewidth = 2, show.legend = TRUE )+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.0, 1))+
  theme_pubr(legend = "top")+
  labs_pubr(base_size = 24)

rh_bi_rich_fig_den

ggsave(filename = "figures/rh_bi_den_fig_02132024.tiff",
       plot = rh_bi_rich_fig_den,
       dpi = 800,
       width = 12,
       height = 10,
       units = "in")

rh_sss_rich_fig_den <- pj_samps %>% 
  filter(speciescount > 1) %>% # ensure PJ there
  filter(rh_sss_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_sss_sum), (density))) + 
  xlab("Soil and Site Stability")+
  ylab("Categorical PJ Density (#/plot)")+
  geom_point(aes(group = species), alpha = 0.2, position = "jitter", color = "darkorange")+
  geom_smooth(mapping = aes(rh_sss_sum, density, color = "Density"), color = "black",method = "lm", linewidth = 2, show.legend = TRUE )+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.0, 0.5))+
  theme_pubr(legend = "top")+
  labs_pubr(base_size = 24)

rh_sss_rich_fig_den

ggsave(filename = "figures/rh_sss_den_fig_02132024.tiff",
       plot = rh_sss_rich_fig_den,
       dpi = 800,
       width = 12,
       height = 10,
       units = "in")

rh_den_combo <- plot_grid(rh_hf_rich_fig_den + theme(legend.position="none"),
                           rh_bi_rich_fig_den+ theme(legend.position="none"),
                           rh_sss_rich_fig_den+ theme(legend.position="none"),
                           align = 'vh',
                           nrow = 1,
                           ncol = 3)
rh_den_combo

ggsave(filename = "figures/rh_combo_den_fig_02132024.tiff",
       plot = rh_den_combo,
       dpi = 800,
       width = 16,
       height = 12,
       units = "in")


# summary calcs
pj_samps %>% 
  group_by(species) %>% 
  summarise(mean = mean(rh_hf_sum),
            mead_grass = mean(AH_PerenForbGrassCover),
            mean_soil = mean(BareSoilCover),
            sd = sd(rh_hf_sum),
            sd_grass = sd(AH_PerenForbGrassCover),
            sd_soil = sd(BareSoilCover))

pj_samps %>% 
  group_by(species) %>% 
  summarise(mean = mean(rh_bi_sum),
            mead_grass = mean(AH_PerenForbGrassCover),
            mean_soil = mean(BareSoilCover),
            sd = sd(rh_bi_sum),
            sd_grass = sd(AH_PerenForbGrassCover),
            sd_soil = sd(BareSoilCover))

pj_samps %>% 
  group_by(species) %>% 
  summarise(mean = mean(rh_sss_sum),
            mead_grass = mean(AH_PerenForbGrassCover),
            mean_soil = mean(BareSoilCover),
            sd = sd(rh_sss_sum),
            sd_grass = sd(AH_PerenForbGrassCover),
            sd_soil = sd(BareSoilCover))




rh_hf_veg_fig <- pj_samps %>% 
  filter(rh_hf_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_hf_sum), (AH_PerenForbGrassCover))) + 
  xlab("Hydologic Function")+
  ylab("Perennial Forb + Grass Cover (%)")+
  geom_point(aes(shape = species, color = species), size = 1.5, position = "jitter")+
  geom_smooth(method = "gam", span = 0.3, color = "black", linewidth =2, se=F)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.1, 1))+
  ylim(-1, 100)+
  theme_pubr(legend = "right")+
  labs_pubr(base_size = 24)

rh_bi_veg_fig <- pj_samps %>% 
  filter(rh_bi_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_bi_sum), (AH_PerenForbGrassCover))) + 
  xlab("Biotic Integrity")+
  ylab("Perennial Forb + Grass Cover (%)")+
  geom_point(aes(shape = species, color = species), size = 1.5, position = "jitter")+
  geom_smooth(method = "gam", span = 0.3, color = "black", linewidth =2, se=F)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.1, 1.2))+
  ylim(-1, 100)+
  theme_pubr(legend = "right")+
  labs_pubr(base_size = 24)

rh_sss_veg_fig <- pj_samps %>% 
  filter(rh_sss_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_sss_sum), (AH_PerenForbGrassCover))) + 
  xlab("Soil and Site Stability")+
  ylab("Perennial Forb + Grass Cover (%)")+
  geom_point(aes(shape = species, color = species), size = 1.5, position = "jitter")+
  geom_smooth(method = "gam", span = 0.3, color = "black", linewidth =2, se=F)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.1, 1))+
  ylim(-1, 100)+
  theme_pubr(legend = "right")+
  labs_pubr(base_size = 24)

#bare cover
rh_hf_bs_fig <- pj_samps %>% 
  filter(rh_hf_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_hf_sum), (BareSoilCover))) + 
  xlab("Hydologic Function")+
  ylab("Bare Soil Cover (%)")+
  geom_point(aes(shape = species, color = species), size = 1.5, position = "jitter")+
  geom_smooth(method = "gam", span = 0.3, color = "darkorange4", linewidth =2, se=F)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.1, 1))+
  ylim(-1, 100)+
  theme_pubr(legend = "right")+
  labs_pubr(base_size = 24)

rh_bi_bs_fig <- pj_samps %>% 
  filter(rh_bi_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_bi_sum), (BareSoilCover))) + 
  xlab("Biotic Integrity")+
  ylab("Bare Soil Cover (%)")+
  geom_point(aes(shape = species, color = species), size = 1.5, position = "jitter")+
  geom_smooth(method = "gam", span = 0.3, color = "darkorange4", linewidth =2, se=F)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.1, 1.2))+
  ylim(-1, 100)+
  theme_pubr(legend = "right")+
  labs_pubr(base_size = 24)

rh_sss_bs_fig <- pj_samps %>%
  filter(rh_sss_sum > 1) %>% #ensure complete data
  ggplot(mapping = aes((rh_sss_sum), (BareSoilCover))) + 
  xlab("Soil and Site Stability")+
  ylab("Bare Soil Cover (%)")+
  geom_point(aes(shape = species, color = species), size = 1.5, position = "jitter")+
  geom_smooth(method = "gam", span = 0.3, color = "darkorange4", linewidth =2, se=F)+
  xlim(1, 5)+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels=c('NS', 'SM', 'M', 'ME', 'ET'), expand = c(-0.1, 1))+
  ylim(-1, 100)+
  theme_pubr(legend = "right")+
  labs_pubr(base_size = 24)

rh_veg_combo <- plot_grid(rh_hf_veg_fig + theme(legend.position="none"),
                      rh_bi_veg_fig + theme(legend.position="none"),
                      rh_sss_veg_fig + theme(legend.position="none"),
                      align = 'vh',
                      hjust = 0,
                      nrow = 1)

rh_bs_combo <- plot_grid(rh_hf_bs_fig + theme(legend.position="none"),
                          rh_bi_bs_fig + theme(legend.position="none"),
                          rh_sss_bs_fig + theme(legend.position="none"),
                          align = 'vh',
                          hjust = 0,
                          nrow = 1)

legend_b <- get_legend(rh_hf_veg_fig +
                         guides(shape = guide_legend(override.aes = list(size = 5))) +
                         theme(legend.position="top", legend.box.background = element_rect(fill = "white"),
                                             legend.text = element_text(size=22, face = "bold"),
                                             legend.title = element_text(size=22)))

rh_combo <- plot_grid(rh_veg_combo, rh_bs_combo, legend_b, ncol = 1, nrow = 3, rel_heights = c(1, 1, 0.2))
  
rh_combo

ggsave(filename = "figures/rh_combo_fig_021324.tiff",
                             plot = rh_combo,
                             dpi = 800,
                              width = 14,
                              height = 16,
                              units = "in")

