# Clustering of PJ Communities in Western US
# Austin Rutherford
# austin.rutherford@usda.gov
# 2023-07-17

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
  select(PrimaryKey, AssocKey, County, Longitude_NAD83, Latitude_NAD83) %>% 
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
pj_samps_juoc <- pj_data_all_clean  %>% mutate(across(where(is.numeric),~replace_na(.,0)),
                                          across(where(is.factor),~fct_explicit_na(.,'0')))%>% 
  unique()%>% 
  unique()%>% 
  filter(!species %in% c("JUGR7","JUCO", "PICE", "PIEL", "PIRE5")) %>%  #not enough data
  droplevels() %>% 
  filter(species == "JUOC") %>% 
  droplevels() %>% 
  filter(!AssocKey %in% c("JUOC_3","JUOC_4")) %>% #not enough data
  droplevels()

# Set up training and testing split
set.seed(4242)

# Train and test split
data_split <- initial_split(pj_samps_juoc, prop = 0.70, strata = 'AssocKey')
pj_train <- training(data_split)
pj_test <- testing(data_split)

# Preprocess data
pj_rec_juoc <-  recipe(~ pptSeasonality +
                       tempAnnualRange +
                       meanTempDryQuart +
                       meanTempWetQuart +
                       pptDryMonth +
                       diurnalRange +
                       ph_depth_30_60 +
                       ksat_depth_60_100 +
                       ksat_depth_100_200 +
                       om_depth_30_60 +
                       #  AssocKey + 
                       PrimaryKey +
                       Latitude_NAD83 +
                       Longitude_NAD83 + 
                       species
                       , data = pj_train) %>%
#  step_rm(DBKey, dupe_count, ecositeID,
#          county, 
#          source, 
#          state, 
#          speciescount, 
#          na_l1code, 
#          na_l1name,
#          na_l2code, 
#          na_l2name, 
#          us_l3code, 
#          us_l3name, 
#          us_l4code,
#          us_l4name, 
#          mod_lc_2001,
#          mod_lc_2002,
#          mod_lc_2003,
#          mod_lc_2004,
#          mod_lc_2005,
#          mod_lc_2006,
#          mod_lc_2007,
#          mod_lc_2008,
#          mod_lc_2009,
#          mod_lc_2010,
#          mod_lc_2011,
#          mod_lc_2012,
#          mod_lc_2013,
#          mod_lc_2014,
#          mod_lc_2015,
#          mod_lc_2016,
#          mod_lc_2017,
#          mod_lc_2018,
#          mod_lc_2019,
#          mod_lc_2020, 
#          usgs_elevation, 
#          elevation_orig) %>% # remove from model
  update_role(PrimaryKey, Latitude_NAD83, Longitude_NAD83, species, new_role = "id") %>% # make point an id for back tracking
  step_filter_missing(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% # use dummy variables for factors in model
  step_zv(all_predictors()) %>% # remove single value variables
  step_normalize(all_predictors(), -all_outcomes()) %>% # normalize numeric data
  step_nzv(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), keep_original_cols = TRUE, threshold = 0.95) 

pj_pca_prep <- prep(pj_rec_juoc)

pj_tidied_pca <- tidy(pj_pca_prep, 6)

pj_tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

pj_tidied_pca %>%
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

pj_pca_baked <- bake(pj_pca_prep, new_data = NULL)

pj_pca_baked %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = species), alpha = 0.7, size = 2) +
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL) + 
  stat_ellipse(mapping = aes(group = species))

#autoplot(pca2, data = penguins_no_na, loadings = TRUE, loadings.label = TRUE,
#         colour = 'species', 
#         frame.type = "norm", frame.level = 0.90)   # frame.type convex, norm, euclid, t; see ?ggbiplot


## SPATIAL
# Set up training and testing split
set.seed(4242)

# Train and test split
data_split <- initial_split(pj_samps, prop = 0.80)
pj_train_sf <- training(data_split)
pj_test_sf <- testing(data_split)

# Preprocess data
pj_rec_sf <- recipe(~., data = pj_samps) %>% # use all data
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
  update_role(AssocKey, PrimaryKey, Latitude_NAD83, Longitude_NAD83, species, new_role = "id") %>% # make point an id for back tracking
  step_filter_missing(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% # use dummy variables for factors in model
  step_zv(all_predictors()) %>% # remove single value variables
  step_normalize(all_predictors(), -all_outcomes()) %>% # normalize numeric data
  step_nzv(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 5, keep_original_cols = TRUE, threshold = 0.95) 


# Apply recipe preprocessing to training data
pj_prepped_sf <- prep(pj_rec_sf, training = pj_train_sf) # preps data, applies recipe

# Run (bake) prepped preprocessng to training data to see the number of final dummy variables
pj_train_bake_sf <- bake(pj_prepped_sf, new_data = pj_samps) # use all data


pj_samps_sf <- sf::st_as_sf(
  pj_train_bake_sf,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("Longitude_NAD83", "Latitude_NAD83"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326)

set.seed(123)
cluster_folds <- spatial_clustering_cv(pj_samps_sf, v = 14)

autoplot(cluster_folds)


  
set.seed(123)
block_folds <- spatial_block_cv(pj_samps_sf, v = 14)

autoplot(block_folds)

set.seed(123)
location_folds <-
  spatial_leave_location_out_cv(
    pj_samps_sf,
    group = species,
    #repeats = 5,
    strata = species,
    v = 14)

autoplot(location_folds)

cluster_folds$type <- "cluster"
block_folds$type <- "block"
location_folds$type <- "location"

resamples <-
  dplyr::bind_rows(
    cluster_folds,
    block_folds,
    location_folds
  )

# `splits` will be the `rsplit` object
compute_preds <- function(splits) {
  # fit the model to the analysis set
  mod <-  lm(species ~ PC01+PC02,
            data = analysis(splits)
  )
  # identify the assessment set
  holdout <- assessment(splits)
  # return the assessment set, with true and predicted price
  tibble::tibble(
    geometry = holdout$geometry,
    species = (holdout$species),
    .pred = predict(mod, holdout)
  )
}

cv_res <- resamples %>%
  mutate(.preds = map(splits, compute_preds))

cv_rmse <- cv_res %>%
  unnest(.preds) %>%
  group_by(id, type) %>%
  rmse(species, .pred)

cv_rmse

cv_res %>%
  unnest(.preds) %>%
  left_join(cv_rmse, by = c("id", "type")) %>%
  ggplot(aes(color = .estimate)) +
  geom_sf(aes(geometry = geometry), alpha = 0.5) +
  labs(color = "RMSE") +
  scale_color_viridis_c() +
  facet_wrap(vars(type), ncol = 1)


