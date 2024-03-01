# Load packages
library(tidyverse)
library(tidymodels)
library(tidytext)
library(spatialsample)
library(sf)

# Read in data
pj_assoc_data_og <- read_csv('Data/pj_woodland_assoc_gee.csv')
pj_assoc_data_all <- read_csv('Data/All_Combined_PJ_Assoc_NoRH.csv') %>% 
  mutate(PrimaryKey = as.factor(PrimaryKey),
         AssocKey = as.factor(AssocKey))

pj_assoc_og_trim <- pj_assoc_data_og %>% 
  dplyr::select(PrimaryKey, AssocKey, County, Longitude_NAD83, Latitude_NAD83) %>% 
  mutate(PrimaryKey = as.factor(PrimaryKey),
         AssocKey = as.factor(AssocKey))

by <- join_by(PrimaryKey, AssocKey, county==County)

pj_assoc_data_joined <- left_join(pj_assoc_data_all, pj_assoc_og_trim, by) %>% 
  unique()

# Check out the data
summary(pj_assoc_data_all)
glimpse(pj_assoc_data_all)

# Replace missing data value with NA, 

pj_data_all_clean <- pj_assoc_data_joined %>%
  mutate(
    AssocKey = as.factor(AssocKey),
    county = as.factor(county),
    DBKey = as.factor(DBKey),
    ecositeID = as.factor(ecositeID),
    mod_lc_2001 = as.factor(mod_lc_2001),
    mod_lc_2002 = as.factor(mod_lc_2002),
    mod_lc_2003 = as.factor(mod_lc_2003),
    mod_lc_2004 = as.factor(mod_lc_2004),
    mod_lc_2005 = as.factor(mod_lc_2005),
    mod_lc_2006 = as.factor(mod_lc_2006),
    mod_lc_2007 = as.factor(mod_lc_2007),
    mod_lc_2008 = as.factor(mod_lc_2008),
    mod_lc_2009 = as.factor(mod_lc_2009),
    mod_lc_2010 = as.factor(mod_lc_2010),
    mod_lc_2011 = as.factor(mod_lc_2011),
    mod_lc_2012 = as.factor(mod_lc_2012),
    mod_lc_2013 = as.factor(mod_lc_2013),
    mod_lc_2014 = as.factor(mod_lc_2014),
    mod_lc_2015 = as.factor(mod_lc_2015),
    mod_lc_2016 = as.factor(mod_lc_2016),
    mod_lc_2017 = as.factor(mod_lc_2017),
    mod_lc_2018 = as.factor(mod_lc_2018),
    mod_lc_2019 = as.factor(mod_lc_2019),
    mod_lc_2020 = as.factor(mod_lc_2020),
    severity = as.factor(severity),
    prescribed_freq = as.factor(prescribed_freq),
    wildfire_freq = as.factor(wildfire_freq),
    source = as.factor(source),
    species = as.factor(species),
    state = as.factor(state),
    na_l1name = as.factor(na_l1name),
    na_l2name = as.factor(na_l2name),
    us_l3name = as.factor(us_l3name),
    us_l4name = as.factor(us_l4name))
#%>% 
#  slice_sample(prop = 0.25)

# Look at data again
glimpse(pj_data_all_clean)

# Drop NAs and create data subset (half of the data)
pj_samps_jumo <- pj_data_all_clean  %>% mutate(across(where(is.numeric),~replace_na(.,0)),
                                          across(where(is.factor),~fct_explicit_na(.,'0')))%>% 
  unique()%>% 
  filter(!species %in% c("JUGR7","JUCO", "PICE", "PIEL", "PIRE5")) %>%  #not enough data
  droplevels() %>% 
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
  droplevels()%>%
  filter(species == "JUMO") %>% 
  droplevels()


jumo_fit <- pj_samps_jumo %>% dplyr::select(minTempColdMonth,
                                     meanTempDryQuart,
                                     pptDryMonth,
                                     dep_bedrock,
                                     pptSeasonality,
                                     pptWarmQuart,
                                     isotherm,
                                     pptColdQuart,
                                     ksat_depth_100_200,
                                     usgs_slope
) %>% prcomp(scale = TRUE)

jumo_all<-jumo_fit %>%
  augment(pj_samps_jumo)

jumo_all %>% # add original dataset back in
  ggplot(aes(Longitude_NAD83, Latitude_NAD83)) + 
  geom_point(aes(color = .fittedPC1), size = 1.5) +
  xlim(-125, -90)+
  ylim(25, 50)
#+
#  scale_color_manual( )

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(10, "pt"))

jumo_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = 0, nudge_y = -0.02,
    color = "#904C2F", size =5) +
  #geom_point(aes(Longitude_NAD83, Latitude_NAD83), data = jumo_all)
  xlim(-.8, 0.8) + ylim(-0.5, 0.8)


jumo_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01)))

# Set up training and testing split
set.seed(4242)

# Train and test split
#data_split <- initial_split(pj_samps_jumo, prop = 0.70)
#pj_train <- training(data_split)
#pj_test <- testing(data_split)

# Preprocess data
pj_rec_jumo <-  recipe(~minTempColdMonth+
                       meanTempDryQuart+
                       pptDryMonth+
                       dep_bedrock+
                       pptSeasonality+
                       pptWarmQuart+
                       isotherm+
                       pptColdQuart+
                       ksat_depth_100_200+
                       usgs_slope+
                       #  AssocKey + 
                       PrimaryKey +
                      Latitude_NAD83 +
                      Longitude_NAD83 + 
                      species, data = pj_samps_jumo) %>% 
  update_role(PrimaryKey, Latitude_NAD83, Longitude_NAD83, species, new_role = "id") %>% # make point an id for back tracking
  step_filter_missing(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% # use dummy variables for factors in model
  step_zv(all_predictors()) %>% # remove single value variables
  step_normalize(all_predictors(), -all_outcomes()) %>% # normalize numeric data
  step_nzv(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), keep_original_cols = TRUE, threshold = 0.99) 

pj_pca_prep <- prep(pj_rec_jumo)

pj_pca_baked <- bake(pj_pca_prep, new_data = NULL)

pj_tidied_pca_rot <- tidy(pj_pca_prep, 6, matrix = "rotation")

pj_tidied_pca_rot %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(10, "pt")
)

pj_tidied_pca_rot_wide <-  pj_tidied_pca_rot %>%
  pivot_wider(names_from = "component", values_from = "value")

# plot rotation matrix
jumo_biplot <- pj_tidied_pca_rot_wide %>%
  ggplot(aes(PC1, PC2)) +
  #geom_point(aes(PC1, PC2, color = species), alpha = 0.7, size = 2, data = pj_pca_baked) +
  #scale_color_manual(values = "tan3")+
  geom_segment(aes(10*PC1, 10*PC2), xend = 0, yend = 0, size=2, arrow = arrow_style) +
  geom_text(
    aes(10*PC1, 10*PC2,label = terms, fontface = "bold"),
    hjust = 0, nudge_x = -0.5, nudge_y = 0.5, size = 6, angle = 25,
    color =   "thistle3") + 
  ggtitle("JUMO")+
  #stat_ellipse(mapping = aes(PC1, PC2),data = pj_pca_baked) +
  xlim(-6, 10) + ylim(-8, 8) +
  #coord_fixed()+
  ggpubr::theme_pubr(legend = "right")+
  # theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -15),
  #   panel.background = element_rect(fill='transparent'), #transparent panel bg
  #   plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  #   panel.grid.major = element_blank(), #remove major gridlines
  #   panel.grid.minor = element_blank(), #remove minor gridlines
  #   legend.background = element_rect(fill='transparent'), #transparent legend bg
  #   legend.box.background = element_rect(fill='transparent') #transparent legend panel
  # )+
  ggpubr::labs_pubr(base_size = 18)

jumo_biplot

ggsave(filename = "Graphs/jumo_biplot_120123.tiff",
       plot = jumo_biplot,
       dpi = 800, 
       width = 8,
       height = 6,
       units = "in")

pj_tidied_pca_eig <- tidy(pj_pca_prep, 6, matrix = "eigenvalues")

pj_tidied_pca_eig %>%
  ggplot(aes(component, value)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  #scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01)))

pj_tidied_pca_eig %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  group_by(component) %>%
  top_n(10, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )



