# Load packages
library(tidyverse)
library(tidymodels)
library(tidytext)
library(spatialsample)
library(sf)
library(usmap)
library(raster)
library(randomForest)
library(mapedit)
library(basemaps)
library(ggmap)
library(ranger)
library(vip)
library(blockCV)
library(ggpubr)
library(ggalt)
library(ggforce)
library(lmerTest)


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

# Look at data again
glimpse(pj_data_all_clean)

# Drop NAs and create data subset (half of the data)
pj_samps <- pj_data_all_clean  %>% mutate(across(where(is.numeric),~replace_na(.,0)),
                                          across(where(is.factor),~fct_explicit_na(.,'0')))%>% 
  unique()%>% 
  unique()%>% 
  filter(!species %in% c("JUGR7","JUCO", "PICE", "PIEL", "PIRE5")) %>%  #not enough data
  droplevels() %>% 
  filter(species %in% c("JUOS","JUMO", "PIED", "JUAS", "JUDE2","JUOC", "JUPI", "JUSC2", "JUVI", "PIMO")) %>%  
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
  droplevels()

## SPATIAL
# Set up training and testing split
set.seed(4242)


# Train and test split
data_split <- initial_split(pj_samps, prop = 0.70)
pj_train <- training(data_split)
pj_test <- testing(data_split)

pj_samps_sf <- sf::st_as_sf(
  pj_samps,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("Longitude_NAD83", "Latitude_NAD83"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326, remove = F)

pj_train_sf <- sf::st_as_sf(
  pj_train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("Longitude_NAD83", "Latitude_NAD83"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326, remove = F)

pj_test_sf <- sf::st_as_sf(
  pj_test,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("Longitude_NAD83", "Latitude_NAD83"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326, remove = F)

pj_train_df <-as.data.frame(pj_train_sf) %>% dplyr::select(-geometry)

# get data frame for all samps for vip later
pj_samps_df <-as.data.frame(pj_samps_sf) %>% dplyr::select(-geometry)

# Preprocess data
pj_rec_sf <- recipe(species ~., data = pj_train_df) %>%
  step_rm(DBKey, dupe_count, ecositeID,
          county, 
          source, 
          state, 
          speciescount, 
          na_l1code, 
          na_l1name,
          na_l2code, 
          na_l2name, 
          us_l3code, 
          us_l3name, 
          us_l4code,
          us_l4name, 
          mod_lc_2001,
          mod_lc_2002,
          mod_lc_2003,
          mod_lc_2004,
          mod_lc_2005,
          mod_lc_2006,
          mod_lc_2007,
          mod_lc_2008,
          mod_lc_2009,
          mod_lc_2010,
          mod_lc_2011,
          mod_lc_2012,
          mod_lc_2013,
          mod_lc_2014,
          mod_lc_2015,
          mod_lc_2016,
          mod_lc_2017,
          mod_lc_2018,
          mod_lc_2019,
          mod_lc_2020, 
          usgs_elevation, 
          elevation_orig) %>% # remove from model
  update_role(AssocKey, PrimaryKey, Latitude_NAD83, Longitude_NAD83, new_role = "id") %>% # make point an id for back tracking
  step_filter_missing(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% # use dummy variables for factors in model
  step_zv(all_predictors()) %>% # remove single value variables
  step_normalize(all_predictors(), -all_outcomes()) %>% # normalize numeric data
  step_nzv(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), threshold = 0.95, keep_original_cols = TRUE, num_comp = 5) 


# Apply recipe preprocessing to training data
pj_prepped_sf <- prep(pj_rec_sf, training = pj_train_df) # preps data, applies recipe

# Run (bake) prepped preprocessng to training data to see the number of final dummy variables
pj_train_bake_sf <- bake(pj_prepped_sf, new_data = NULL) # use all data 

# Run (prep and bake) prepped preprocessng to all data to test vip later
pj_samps_prep_sf <- prep(pj_rec_sf, training = pj_samps_df) # preps data, applies recipe
pj_samps_bake_sf <- bake(pj_samps_prep_sf, new_data = NULL) # use all data

# Setup our model (using rpart)
#pj_spec_sf <- 
#  rand_forest(
#    mtry = tune(),
#    trees = 1000,
#    min_n = tune()
#  ) %>% 
#  set_engine("ranger", importance = "impurity") %>% 
#  set_mode("classification")

#pj_spec_sf

pj_spec_sf_dwn <- 
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>% 
  set_engine("randomForest", strata = as.factor(species), importance = TRUE, replace = TRUE) %>% 
  set_mode("classification")

pj_spec_sf_dwn

# Build workflow to pair model and cross validation and tuning with data preprocessing
pj_wflow <- workflow() %>% 
  add_recipe(pj_rec_sf) %>% 
  add_model(pj_spec_sf_dwn)

set.seed(123)
#cluster_folds <- spatial_clustering_cv(pj_samps_sf, cluster_function="hclust", v = 10)
cluster_folds <- cv_spatial(pj_samps_sf,
                            column = "species",
                            k =5,
                            selection = "random",
                            hexagon = FALSE,
                            size = 1000,
                            iteration = 50,
                            biomod2 = TRUE)


# create location folds
location_folds <-
  spatial_leave_location_out_cv(
    pj_train_df,
    group = species,
    repeats = 1,
    v = 10)

# group folds for comparing to location
group_folds <- group_vfold_cv(pj_train_sf, group = species, balance = "groups", v = 5)

# stratify training data by species for testing
strat_folds <- vfold_cv(pj_train_df, v = 5, repeats=10, strata = species)


# Set up parallel processing
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores()-4)
registerDoParallel(cl)

# below is for linux-based machines
# num_cores <- parallel::detectCores()-1
# cl <- parallel::makeCluster(num_cores, outfile = "", type = "FORK")
# parallel::clusterEvalQ(cl, library(tidymodels))
# doParallel::registerDoParallel(cl)
#

set.seed(42)
pj_res_sf <- tune_grid(
  pj_wflow,
  resamples = strat_folds,
  grid = 5,
  metrics = metric_set(roc_auc, accuracy, precision, sens))

pj_res_sf %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  dplyr::select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


rf_grid_sf <- grid_regular(
  mtry(range = c(1, 25)),
  min_n(range = c(30, 75)),
  levels = 5
)

rf_grid_sf

pj_res_sf_update <- tune_grid(
  pj_wflow,
  resamples = strat_folds,
  grid = rf_grid_sf,
  metrics = metric_set(roc_auc, accuracy, precision, sens))

#saveRDS(pj_res_sf_update, file = "Data/post_tune_pj_model.rds")
pj_res_sf_update <- readRDS(file = "Data/post_tune_pj_model.rds")

pj_res_sf_update %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

best_auc_sf <- select_best(pj_res_sf_update, "accuracy")

final_rf_sf <- finalize_model(
  pj_spec_sf_dwn,
  best_auc_sf
)

vip_plot_sf <- final_rf_sf %>%
  set_engine("randomForest", importance = T) %>%
  fit(species ~ .,
      data = pj_train_bake_sf %>% dplyr::select(-AssocKey,-PrimaryKey, -Longitude_NAD83, -Latitude_NAD83)
  ) %>%
  vip(geom = "col", num_features = 10)

vip_plot_sf

vip_vars_sf <- as.data.frame(vip_plot_sf$data)

final_wf_sf <- workflow() %>%
  add_recipe(pj_rec_sf) %>%
  add_model(final_rf_sf)

set.seed(42)
final_res_sf <- final_wf_sf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, accuracy, precision, sens))

#saveRDS(final_res_sf, file = "Data/final_pj_model.rds")
final_res_sf <- readRDS(file = "Data/final_pj_model.rds")

final_res_sf %>%
  collect_metrics()

# VIP (10) using all the samples
vip_plot_sf_final <- final_res_sf %>%
  extract_spec_parsnip() %>%
  set_engine("randomForest", importance = T) %>%
  fit(species ~ .,
      data = pj_samps_bake_sf %>% dplyr::select(-AssocKey,-PrimaryKey, -Longitude_NAD83, -Latitude_NAD83)
  ) %>%
  vip(geom = "col", num_features = 10)

vip_plot_sf_final

vip_vars_sf_final <- as.data.frame(vip_plot_sf_final$data)


vip_fig <- vip_vars_sf_final %>%
  ggplot(aes(x = reorder(Variable,Importance, median, decreasing = F), y = Importance)) +
  geom_point(size = 4, color = "blue")+
  geom_segment(aes(xend=Variable), yend=0) +
  #expand_limits(y=0) +
  xlab("")+
  ylim(c(20,35))+
  coord_flip()+
  theme_pubr()+
  labs_pubr(base_size = 24)

vip_fig

ggsave(filename = "Graphs/VIP_species_figs/VIP_02122024.tiff",
       plot = vip_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

# where are good and bad classifications occurring
final_res_sf %>%
  collect_predictions() %>%
  mutate(correct = case_when(
    species == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) %>%
  bind_cols(pj_test_sf) %>%
  ggplot(aes(Longitude_NAD83, Latitude_NAD83, color = correct)) +
  geom_point(size = 0.5, alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c("gray80", "darkred"))

# relationships between VIP vars and species
temp_wet_qt_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species,meanTempWetQuart, median, decreasing = T), y = meanTempWetQuart/10, group = species))+
  geom_point(aes(color = usgs_elevation), position = "jitter")+
  geom_violin(aes(),draw_quantiles = c(0.5),linewidth = 1.5, color = "salmon", show.legend = F)+
  ylim(-10, 30)+
  labs(x = "Species", y = "Mean Temperature Wettest Quarter (°C)", color = "Elevation (m)")+
  theme_pubr(legend = "right")+
  labs_pubr()

temp_wet_qt_fig

hist(pj_samps$meanTempColdQuart)
hist((pj_samps$usgs_elevation))

fit_twq <- lmerTest::lmer(meanTempWetQuart~species*usgs_elevation + (1|species:usgs_elevation) , data=pj_samps)
summary(fit_twq)
anova(fit_twq)


ggsave(filename = "Graphs/VIP_species_figs/1_temp_wet_qt_112923.tiff",
       plot = temp_wet_qt_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

ppt_season_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species, pptSeasonality, median, decreasing = T), y = pptSeasonality, group = species))+
  geom_point(aes(color = Latitude_NAD83), position = "jitter")+#latitude...rain to snow dom
  geom_violin(aes(), draw_quantiles = c(0.5),linewidth = 1.5, color = "blue", show.legend = F)+
  scale_color_distiller(palette="BrBG", direction=1) +
  ylim(0, 100)+
  labs(x = "Species", y = "PPT Seasonality (%)", color = "Latitude (°)")+ # CV of ppt over a year
  theme_pubr(legend = "right")+
  labs_pubr()

ppt_season_fig

fit_ppt_season <- lmerTest::lmer(pptSeasonality~species*Latitude_NAD83 + (1|species:Latitude_NAD83) , data=pj_samps)
summary(fit_ppt_season)
anova(fit_ppt_season)

ggsave(filename = "Graphs/VIP_species_figs/2_ppt_seasonality_112923.tiff",
       plot = ppt_season_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

ppt_warm_qt_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species, pptWarmQuart, median, decreasing = T), y = pptWarmQuart, group = species))+
  geom_point(aes(color = Latitude_NAD83), position = "jitter")+#latitude...rain to snow dom
  geom_violin(aes(), draw_quantiles = c(0.5),linewidth = 1.5, color = "blue3", show.legend = F)+
  scale_color_distiller(palette="BrBG", direction=1) +
  ylim(0, 350)+
  labs(x = "Species", y = "PPT Warmest Quarter (mm)", color = "Latitude (°)")+
  theme_pubr(legend = "right")+
  labs_pubr()

ppt_warm_qt_fig

fit_ppt_warm_qt <- lmerTest::lmer(pptWarmQuart~species*Latitude_NAD83 + (1|species:Latitude_NAD83) , data=pj_samps)
summary(fit_ppt_warm_qt)
anova(fit_ppt_warm_qt)


ggsave(filename = "Graphs/VIP_species_figs/3_ppt_warm_qt_112923.tiff",
       plot = ppt_warm_qt_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

temp_dry_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species,meanTempDryQuart, median, decreasing = T), y = meanTempDryQuart/10, group = species))+
  geom_point(aes(color = usgs_elevation), position = "jitter")+
  geom_violin(aes(),draw_quantiles = c(0.5),linewidth = 1.5, color = "salmon3", show.legend = F)+
  #ylim(-10, 30)+
  labs(x = "Species", y = "Mean Temperature Driest Quarter (°C)", color = "Elevation (m)")+
  theme_pubr(legend = "right")+
  labs_pubr()

temp_dry_fig

fit_temp_dry <- lmerTest::lmer(meanTempDryQuart~species*usgs_elevation + (1|species:usgs_elevation) , data=pj_samps)
summary(fit_temp_dry)
anova(fit_temp_dry)

ggsave(filename = "Graphs/VIP_species_figs/4_temp_dry_qt_112923.tiff",
       plot = temp_dry_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

isotherm_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species, isotherm, median, decreasing = T), y = isotherm, group = species))+
  geom_point(aes(color = Latitude_NAD83), position = "jitter")+
  geom_violin(aes(), draw_quantiles = c(0.5),linewidth = 1.5, color = "salmon2", show.legend = F)+
  scale_color_distiller(palette="BrBG", direction=1) +
  #ylim(20, 60)+
  labs(x = "Species", y = "Isothermality (%)", color = "Latitude (°)")+
  theme_pubr(legend = "right")+
  labs_pubr()

isotherm_fig

fit_isotherm <- lmerTest::lmer(isotherm~species*Latitude_NAD83 + (1|species:Latitude_NAD83) , data=pj_samps)
summary(fit_isotherm)
anova(fit_isotherm)


ggsave(filename = "Graphs/VIP_species_figs/5_isothermality_112923.tiff",
       plot = isotherm_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

ppt_cold_qt_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species, pptColdQuart, median, decreasing = T), y = pptColdQuart, group = species))+
  geom_point(aes(color = Latitude_NAD83), position = "jitter")+ #latitude...rain to snow dom
  geom_violin(aes(), draw_quantiles = c(0.5),linewidth = 1.5, color = "blue4", show.legend = F)+
  scale_color_distiller(palette="BrBG", direction=1) +
  ylim(0, 350)+
  labs(x = "Species", y = "PPT Coldest Quarter (mm)", color = "Latitude (°)")+
  theme_pubr(legend = "right")+
  labs_pubr()

ppt_cold_qt_fig

fit_ppt_cold_qt <- lmerTest::lmer(pptColdQuart~species*Latitude_NAD83+ (1|species:Latitude_NAD83), data=pj_samps)
summary(fit_ppt_cold_qt)
anova(fit_ppt_cold_qt)


ggsave(filename = "Graphs/VIP_species_figs/6_ppt_cold_qt_112923.tiff",
       plot = ppt_cold_qt_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

temp_cold_qt_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species,meanTempColdQuart, median, decreasing = T), y = meanTempColdQuart/10, group = species))+
  geom_point(aes(color = usgs_elevation), position = "jitter")+
  geom_violin(aes(),draw_quantiles = c(0.5),linewidth = 1.5, color = "salmon4", show.legend = F)+
  ylim(-15, 15)+
  labs(x = "Species", y = "Mean Temperature Coldest Quarter (°C)", color = "Elevation (m)")+
  theme_pubr(legend = "right")+
  labs_pubr()

temp_cold_qt_fig

fit_temp_cold_qt <- lmerTest::lmer(meanTempColdQuart~species*usgs_elevation + (1|species:usgs_elevation) , data=pj_samps)
summary(fit_temp_cold_qt)
anova(fit_temp_cold_qt)

ggsave(filename = "Graphs/VIP_species_figs/7_temp_cold_qt_112923.tiff",
       plot = temp_cold_qt_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

temp_warm_qt_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species, meanTempWarmQuart, median, decreasing = T), y = meanTempWarmQuart/10, group = species))+
  geom_point(aes(color = usgs_elevation), position = "jitter")+
  geom_violin(aes(),draw_quantiles = c(0.5),linewidth = 1.5, color = "darkorange", show.legend = F)+
  ylim(0, 30)+
  labs(x = "Species", y = "Mean Temperature Warmest Quarter (°C)", color = "Elevation (m)")+
  theme_pubr(legend = "right")+
  labs_pubr()

temp_warm_qt_fig

fit_temp_warm_qt <- lmerTest::lmer(meanTempWarmQuart~species*usgs_elevation + (1|species:usgs_elevation) , data=pj_samps)
summary(fit_temp_warm_qt)
anova(fit_temp_warm_qt)

ggsave(filename = "Graphs/VIP_species_figs/8_temp_warm_qt_112923.tiff",
       plot = temp_warm_qt_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

temp_season_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species, tempseasonality, median, decreasing = T), y = ((tempseasonality/100)), group = species))+
  geom_point(aes(color = usgs_elevation), position = "jitter")+
  geom_violin(aes(),draw_quantiles = c(0.5),linewidth = 1.5, color = "red3", show.legend = F)+
 # ylim(0, 30)+
  labs(x = "Species", y = "Temperature Seasonality (%)", color = "Elevation (m)")+ # CV of temp over a year
  theme_pubr(legend = "right")+
  labs_pubr()

temp_season_fig

fit_temp_season <- lmerTest::lmer(tempseasonality~species*usgs_elevation + (1|species:usgs_elevation) , data=pj_samps)
summary(fit_temp_season)
anova(fit_temp_season)


ggsave(filename = "Graphs/VIP_species_figs/9_temp_seasonality_112923.tiff",
       plot = temp_season_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

temp_di_range_fig <- pj_samps %>%
  ggplot(aes(x= reorder(species, diurnalRange, median, decreasing = T), y = diurnalRange/10, group = species))+
  geom_point(aes(color = Latitude_NAD83), position = "jitter")+ #latitude...rain to snow dom, warm to cold
  geom_violin(aes(), draw_quantiles = c(0.5),linewidth = 1.5, color = "gold", show.legend = F)+
  scale_color_distiller(palette="BrBG", direction=1) +
  ylim(10, 25)+
  labs(x = "Species", y = "Mean Diurnal Temperature Range (°C)", color = "Latitude (°)")+
  theme_pubr(legend = "right")+
  labs_pubr()

temp_di_range_fig

fit_temp_di_range <- lmerTest::lmer(diurnalRange~species*Latitude_NAD83+ (1|species:Latitude_NAD83), data=pj_samps)
summary(fit_temp_di_range)
anova(fit_temp_di_range)

ggsave(filename = "Graphs/VIP_species_figs/10_temp_di_range_112923.tiff",
       plot = temp_di_range_fig,
       dpi = 600,
       width = 8,
       height = 5,
       units = "in")

#mean clay for triangle
pj_samps$clay_mean <- rowMeans(pj_samps[,43:48], na.rm=TRUE)
pj_samps$silt_mean <- rowMeans(pj_samps[,37:42], na.rm=TRUE)
pj_samps$sand_mean <- rowMeans(pj_samps[,31:36], na.rm=TRUE)

tern_fig <- ggtern::ggtern(data=pj_samps, aes(x=dep_bedrock/100,y=clay_mean, z=usgs_slope, group = species)) +
  geom_point(aes(color = species)) +
  labs(z = "Slope", x = "Bedrock Depth", y = "Clay") +
  scale_color_manual(values = c(
    "darkorange4", #JUAS
    "blue", #JUDE2
    "thistle1",#JUMO
    "darkorange",#JUOC
    "lightsalmon3",#JUOS
    "chocolate2", #JUPI
    "lightblue", #JUSC2
    "indianred2", #JUVI                          
    "mediumpurple3",#PIED        
    "goldenrod1"))+#PIMO
  #facet_grid(.~species, scales = "free", space='free')+
  ggtern::theme_rgbw(base_size = 16)+
  theme(legend.position = "none",
        tern.axis.title.L = element_text(hjust = -0.1, vjust = 1.75),
        tern.axis.title.R = element_text(hjust = 1,vjust = 2),
        panel.spacing=unit(1,"lines"),
        strip.text = element_text(face = "bold", size = 24))

tern_fig

tern_fig_1 <- tern_fig +
  facet_wrap_paginate(~species, nrow=1, ncol=4, shrink = FALSE, page = 1)

tern_fig_2 <- tern_fig_1 + 
  facet_wrap_paginate(~species, nrow=1, ncol=4, shrink = FALSE, page = 2)

tern_fig_3 <- tern_fig_1 + 
  facet_wrap_paginate(~species, nrow=1, ncol=4, shrink = FALSE, page = 3)

ggtern::ggsave(filename = "Graphs/tern_fig_121923_1.tiff",
       plot = tern_fig_1,
       dpi = 800,
       scale = 4,
       width = 7,
       height = 2,
       units = "in")

ggtern::ggsave(filename = "Graphs/tern_fig_121923_2.tiff",
               plot = tern_fig_2,
               dpi = 800,
               scale = 4,
               width = 7,
               height = 2,
               units = "in")

ggtern::ggsave(filename = "Graphs/tern_fig_121923_3.tiff",
               plot = tern_fig_3,
               dpi = 800,
               scale = 4,
               width = 7,
               height = 2,
               units = "in")



# soil only...no big patterns
ggtern::ggtern(data=pj_samps, aes(x=clay_mean,y=silt_mean, z=sand_mean), group = species) +
  geom_point(aes(color = species)) +
  labs(z = "Percent Sand (0-200cm)", x = "Percent Clay (0-200cm)", y = "Percent Silt (0-200cm)") +
  scale_color_manual(values = c(
    "darkorange4", #JUAS
    "blue", #JUDE2
    "thistle1",#JUMO
    "darkorange",#JUOC
    "lightsalmon3",#JUOS
    "chocolate2", #JUPI
    "lightblue", #JUSC2
    "indianred2", #JUVI                          
    "mediumpurple3",#PIED        
    "goldenrod1"))+#PIMO
  facet_wrap(~species)+
  ggtern::theme_rgbg()+
  theme(panel.spacing = unit(1, "lines"), legend.position = "none")


bedrock_fig <- pj_samps %>%
  ggplot(aes(x= (usgs_slope), y = dep_bedrock/100, group = species))+
  geom_point(aes(color = species))+ #latitude...rain to snow dom, warm to cold
  #geom_violin(aes(), draw_quantiles = c(0.5),linewidth = 1.5, color = "gold", show.legend = F)+
  #scale_color_distiller(palette="BrBG", direction=1) +
  #ylim(10, 25)+
  geom_smooth(method = "lm")+
  labs(x = "Slope (°)", y = "Depth to Bedrock (m)", color = "Elevation (m)")+
  theme_pubr(legend = "right")+
  facet_wrap(~species)+
  labs_pubr()

bedrock_fig

# set.seed(234)
# cluster_folds <- spatial_clustering_cv(pj_samps_sf, v = 10, cluster_function = c("kmeans"), repeats = 1)
# 
# autoplot(cluster_folds)

#bbox <- st_bbox(c(xmin = -125, xmax = -90, ymax = 50, ymin = 25), crs = st_crs(4326))

bbox <- c(left = -125, bottom = 25, right = -89.5, top = 50)

# Below will let you draw you're own box to map

#ext <- draw_ext()

set_defaults(map_service = "osm", map_type = "topographic")

#m2 <- basemap_raster(bbox, "esri", "world_imagery", map_res=1)   # hi res

#m2 <- basemap_raster(bbox, "esri", "world_terrain_base", map_res=1)   # hi res

#register_stadiamaps("fb58c402-3ed9-4074-8f83-28542cf62252", write = TRUE)

m2 <- get_stadiamap(bbox, maptype = "stamen_terrain_background", zoom=5)   # hi res

#m2 <- basemap_raster(bbox, "osm_stamen", "terrain_bg", map_res=1)   # hi res

#m2_trans <- projectRaster(m2,
#                          crs = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0",
#                          method = "ngb")

#m2_xy <- as.data.frame(m2, xy=TRUE)

#m3<-na.omit(m2_xy)

# extract only 17 western states as in NRI
west <- us_map(include = c(.west_region,
                           "TX", "OK", "SD", "ND", "NE", "KS"),
               exclude = c("AK", "HI"))

# filter to transform projection, match names for transformation
west_states <- map_data("state") %>% 
  rename(lon = long) %>% 
  filter(
    region == "nevada" |
      region == "arizona" |
      region == "washington" |
      region == "oregon" |
      region == "california" |
      region == "utah" |
      region == "new mexico" |
      region == "texas" |
      region == "colorado" |
      region == "oklahoma" |
      region == "kansas" |
      region == "nebraska" |
      region == "south dakota" |
      region == "north dakota" |
      region == "montana" |
      region == "wyoming" |
      region == "idaho"
  )

# transform west states to have x, y
west_transformed <- usmap_transform(west_states)

#cluster_plot <- autoplot(cluster_folds)
#clust_data <- as.data.frame(cluster_plot$data) %>%
#  mutate(fold = as.factor(.fold.))

# ggplot() +
#   geom_raster(data=m3,
#               aes(x=x, y=y,
#                   fill=rgb(red=red, green=green, blue=blue, maxColorValue=255))) +
#   geom_polygon(data=west_transformed,
#                aes(x=lon, y=lat, group=group),
#                colour="black", linewidth = 1, alpha = 0) +
#   geom_sf(data = clust_data, aes(geometry = geometry, color = fold))+
#   #geom_point(data = pj_samps, aes(Longitude_NAD83, Latitude_NAD83, color = species))+
#   scale_fill_identity(guide="none") +
#   coord_sf()+
#   ggpubr::theme_pubr()+
#   theme(panel.ontop = TRUE, panel.background = element_rect(color = NA, fill = NA))


set.seed(123)
location_folds <-
  spatial_leave_location_out_cv(
    pj_samps_sf,
    #strata = species,
    group = species,
    repeats = 1,
    v = 10)

loc_plot <- autoplot(location_folds)
loc_data <- as.data.frame(loc_plot$data) %>% 
  mutate(fold = as.factor(.fold.))

#autoplot(location_folds)
# use ggmap
spatial_cluster_fig <- ggmap(m2) + 
 # geom_raster(data=m3, 
 #             aes(x=x, y=y, 
 #                 fill=rgb(red=red, green=green, blue=blue, maxColorValue=255))) + 
  geom_point(data = loc_data, aes(x=Longitude_NAD83, y= Latitude_NAD83, color = fold),alpha = 0.3, size = 2)+
  #geom_point(data = pj_samps, aes(Longitude_NAD83, Latitude_NAD83, color = species))+
  geom_polygon(data=west_transformed,
               aes(x=lon, y=lat, group=group),
               colour="black", linewidth = 1, alpha = 0) +
  scale_fill_identity(guide="none") + 
  scale_x_continuous(limits = c(-125,-90), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(25,50), expand = c(0, 0)) +
  xlab("Longitude (°)")+
  ylab("Latitude (°)") +
  #ylim(25, 50)+
  stat_ellipse(data = loc_data, aes(Longitude_NAD83, Latitude_NAD83, group=fold, color = fold),
               level = 0.95, linetype = 1, linewidth= 2, na.rm = T, type = "norm", segments = 1000)+
  scale_color_manual(values = c("thistle1",#PIED
                                "lightsalmon3",#JUOS
                                "lightblue", #JUSC2
                                "blue", #JUDE2
                                "chocolate2", #JUPI
                                "goldenrod1",#PIMO
                                "darkorange4", #JUAS
                                "indianred2", #JUVI
                                "mediumpurple3",#JUMO
                                "darkorange"))+#JUOC
  coord_sf()+
  ggpubr::theme_pubr(legend = "none")+
  ggpubr::labs_pubr()#+
# theme(panel.grid = element_blank(),
#      panel.border = element_blank())
spatial_cluster_fig


ggsave(filename = "Graphs/spatial_cluster_fig_112323.tiff",
       plot = spatial_cluster_fig,
       dpi = 800,
       width = 18,
       height = 10,
       units = "in")


spatial_density_fig <- ggmap(m2) + 
  # geom_raster(data=m3, 
  #             aes(x=x, y=y, 
  #                 fill=rgb(red=red, green=green, blue=blue, maxColorValue=255))) + 
  geom_point(data = loc_data, aes(x=Longitude_NAD83, y= Latitude_NAD83, color = fold),alpha = 0.3, size = 1.5)+
  #geom_point(data = pj_samps, aes(Longitude_NAD83, Latitude_NAD83, color = species))+
  geom_polygon(data=west_transformed,
               aes(x=lon, y=lat, group=group),
               colour="black", linewidth = 1, alpha = 0) +
  scale_fill_identity(guide="none") + 
  scale_x_continuous(limits = c(-125,-90), expand = c(0.03, 0)) +
  scale_y_continuous(limits = c(25,50), expand = c(0, 0)) +
  xlab("Longitude (°)")+
  ylab("Latitude (°)") +
  #ylim(25, 50)+
  #geom_encircle(data = loc_data, aes(x=Longitude_NAD83, y= Latitude_NAD83, group = fold,
  #                                   color = fold), s_shape=.8, expand=0.01, size = 2) +
  # stat_density_2d(data = loc_data, 
  #                 aes(x=Longitude_NAD83, y= Latitude_NAD83, group = fold,
  #                     color = fold, fill = after_stat(level)), alpha = 0, size = 2, geom = "polygon",
  #                 #breaks = c(0.00009, 100),
  #                 breaks = c(0.0002, 100),
  #                 bins = 5,
  #                 #contour_var = "density",
  #                 n=100,
  #                 contour = T)+
  stat_density_2d(data = loc_data, 
                  aes(x=Longitude_NAD83, y= Latitude_NAD83, group = fold,
                      color = "black", fill = after_stat(level)), alpha = 0, size = 2, geom = "polygon",
                  #breaks = c(0.00009, 100),
                  breaks = c(0.0002, 100),
                  bins = 5,
                  #contour_var = "density",
                  n=100,
                  contour = T)+
  scale_color_manual(values = c("thistle1",#JUMO
                                "lightsalmon3",#JUOS
                                "lightblue", #JUSC2
                                "blue", #JUDE2
                                "chocolate2", #JUPI
                                "goldenrod1",#PIMO
                                "darkorange4", #JUAS
                                "indianred2", #JUVI
                                "mediumpurple3",#PIED
                                "darkorange", #JUOC
                                "black"))+ # contour lines
  coord_sf()+
  ggspatial::annotation_north_arrow(location = "bl", pad_x = unit(1, "cm"),
                                    pad_y =  unit(1, "cm"))+
  ggpubr::theme_pubr(legend = "none", x.text.angle = 25)+
  #facet_grid(cols = vars(species))+
  ggpubr::labs_pubr(base_size = 24)+
  theme(panel.spacing=unit(1.5,"lines"),
        strip.text = element_text(face = "bold", size = 24))

spatial_density_fig_1 <- spatial_density_fig +
  facet_wrap_paginate(~species, nrow=1, ncol=4, shrink = FALSE, page = 1)

spatial_density_fig_2 <- spatial_density_fig_1 + 
  facet_wrap_paginate(~species, nrow=1, ncol=4, shrink = FALSE, page = 2)

spatial_density_fig_3 <- spatial_density_fig_1 + 
  facet_wrap_paginate(~species, nrow=1, ncol=4, shrink = FALSE, page = 3)

ggsave(filename = "Graphs/spatial_cluster_fig_21324_1.tiff",
       plot = spatial_density_fig_1,
       dpi = 800,
       scale = 4,
       width = 7,
       height = 2,
       units = "in")

ggsave(filename = "Graphs/spatial_cluster_fig_21324_2.tiff",
       plot = spatial_density_fig_2,
       dpi = 800,
       scale = 4,
       width = 7,
       height = 2,
       units = "in")

ggsave(filename = "Graphs/spatial_cluster_fig_21324_3.tiff",
       plot = spatial_density_fig_3,
       dpi = 800,
       scale = 4,
       width = 7,
       height = 2,
       units = "in")

# all woodlands together
spatial_density_all_fig <- ggmap(m2) + 
  # geom_raster(data=m3, 
  #             aes(x=x, y=y, 
  #                 fill=rgb(red=red, green=green, blue=blue, maxColorValue=255))) + 
  geom_point(data = loc_data, aes(x=Longitude_NAD83, y= Latitude_NAD83, color = fold),alpha = 0.3, size = 1.5)+
  #geom_point(data = pj_samps, aes(Longitude_NAD83, Latitude_NAD83, color = species))+
  geom_polygon(data=west_transformed,
               aes(x=lon, y=lat, group=group),
               colour="black", linewidth = 1, alpha = 0) +
  scale_fill_identity(guide="none") + 
  scale_x_continuous(limits = c(-125,-90), expand = c(0.03, 0)) +
  scale_y_continuous(limits = c(25,50), expand = c(0, 0)) +
  xlab("Longitude (°)")+
  ylab("Latitude (°)") +
  #ylim(25, 50)+
  #geom_encircle(data = loc_data, aes(x=Longitude_NAD83, y= Latitude_NAD83, group = fold,
  #                                   color = fold), s_shape=.8, expand=0.01, size = 2) +
  stat_density_2d(data = loc_data, 
                  aes(x=Longitude_NAD83, y= Latitude_NAD83, group = fold,
                      color = "black", fill = after_stat(level)), alpha = 0, size = 1.25, geom = "polygon",
                  #breaks = c(0.00009, 100),
                  breaks = c(0.002, 100),
                  bins = 5,
                  #contour_var = "density",
                  n=100,
                  contour = T)+
  scale_color_manual(values = c("thistle1",#JUMO
                                "lightsalmon3",#JUOS
                                "lightblue", #JUSC2
                                "blue", #JUDE2
                                "chocolate2", #JUPI
                                "goldenrod1",#PIMO
                                "darkorange4", #JUAS
                                "indianred2", #JUVI
                                "mediumpurple3",#PIED
                                "darkorange",#JUOC
                                "black"))+ # contour lines
  coord_sf()+
  ggspatial::annotation_north_arrow(location = "bl", pad_x = unit(1, "cm"),
                                    pad_y =  unit(1, "cm"))+
  ggpubr::theme_pubr(legend = "none")+
  #facet_grid(cols = vars(species))+
  ggpubr::labs_pubr(base_size = 18)+
  theme(panel.spacing=unit(1.8,"lines"),
        strip.text = element_text(face = "bold", size = 18))

spatial_density_all_fig

ggsave(filename = "Graphs/spatial_density_all_fig_120423.tiff",
       plot = spatial_density_all_fig,
       dpi = 800,
       width = 18,
       height = 10,
       units = "in")


# get point legend for map

loc_data_color <- loc_data %>% 
  mutate(fold_color = case_when(
    fold == "Resample01" ~ "PIED",
    fold == "Resample02" ~ "JUOS",
    fold == "Resample03" ~ "JUSC2",
    fold == "Resample04" ~ "JUDE2",
    fold == "Resample05" ~ "JUPI",
    fold == "Resample06" ~ "PIMO",
    fold == "Resample07" ~ "JUAS",
    fold == "Resample08" ~ "JUVI",
    fold == "Resample09" ~ "JUMO",
    fold == "Resample10" ~ "JUOC"
  ))

spatial_cluster_fig_leg <- ggmap(m2) + 
  geom_line(data = loc_data_color, aes(x=Longitude_NAD83, y= Latitude_NAD83, color = fold_color),alpha = 1, size = 2)+
  scale_fill_identity(guide="none") + 
  scale_x_continuous(limits = c(-125,-90), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(25,50), expand = c(0, 0)) +
  xlab("Longitude (°)")+
  ylab("Latitude (°)") +
  scale_color_manual(values = c("thistle1",#PIED
                                "lightsalmon3",#JUOS
                                "lightblue", #JUSC2
                                "blue", #JUDE2
                                "chocolate2", #JUPI
                                "goldenrod1",#PIMO
                                "darkorange4", #JUAS
                                "indianred2", #JUVI
                                "mediumpurple3",#JUMO
                                "darkorange"))+#JUOC
  coord_sf()+
  ggpubr::theme_pubr()+
  ggpubr::labs_pubr()

spatial_cluster_fig_leg

#ggsave(filename = "Graphs/spatial_cluster_legend_112923.tiff",
#       plot = spatial_cluster_fig_leg,
#       dpi = 600,
#       width = 6,
#       height = 5,
#       units = "in")
