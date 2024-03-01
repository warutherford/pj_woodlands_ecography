
# Load packages
library(tidymodels)
library(ISLR)
library(rpart.plot)
library(vip)
library(tidyverse)
library(tidyclust)
library(tidypredict)


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
    dupe_count = as.factor(dupe_count),
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
    na_l1code = as.factor(na_l1code),
    na_l2name = as.factor(na_l2name),
    na_l2code = as.factor(na_l2code),
    us_l3name = as.factor(us_l3name),
    us_l3code = as.factor(us_l3code),
    us_l4name = as.factor(us_l4name),
    us_l4code = as.factor(us_l4code))

# Explore var correlations
#pj_data_all_corr <- pj_data_all_clean %>% 
#  corrr::correlate() %>%    # Create correlation data frame 
  #  corrr::focus(-point, -psu, -DBKey, -PrimaryKey,
  #              -county, -county_number, -date_visited,
  #               -loctype, -percentcovESD, -source, -state, mirror = TRUE) %>%  # Focus on df
#  corrr::rearrange(method = "HC") %>%  # rearrange by correlations
#  corrr::shave() # Shave off the upper triangle for a clean result

#corrr::fashion(pj_data_all_corr)

#corrr::rplot(pj_data_all_corr)


#pj_data_all_corr_small <- pj_data_all_clean%>% 
#  corrr::correlate(method = "spearman", diagonal = 1) %>%    # Create correlation data frame 
#  corrr::rearrange(method = "HC") %>%  # rearrange by correlations
#  corrr::shave()  # Shave off the upper triangle for a clean result

#pj_data_all_corr_small %>% 
#  mutate_if(is.numeric, coalesce, 0) %>%
#  corrr::network_plot(min_cor = 0.1)

#corrr::fashion(pj_data_all_corr_small)

#corrr::rplot(pj_data_all_corr_small)

# Look at data again
glimpse(pj_data_all_clean)

# Drop NAs and create data subset (half of the data)
pj_samps <- pj_data_all_clean  %>% mutate(across(where(is.numeric),~replace_na(.,0)),
                                          across(where(is.factor),~fct_explicit_na(.,'0')))%>% 
  unique()%>% 
  filter(!species %in% c("JUGR7","JUCO", "PICE", "PIEL", "PIRE5")) %>%  #not enough data
  droplevels() %>% 
  filter(species == "JUAS") %>% 
  droplevels()

# Set up training and testing split
set.seed(4242)

# Train and test split
data_split <- initial_split(pj_samps, prop = 0.70)
pj_train <- training(data_split)
pj_test <- testing(data_split)

# Preprocess data
pj_rec <- pj_train %>% 
  recipe(AssocKey~.) %>%
  step_rm(PrimaryKey, DBKey, dupe_count, ecositeID,
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
          Latitude_NAD83, 
          Longitude_NAD83,
          prescribed_freq, # not enough diff to remain in model
          usgs_elevation, 
          elevation_orig) %>% # remove from model
  update_role(species, new_role = "id") %>% # make point an id for back tracking
  step_normalize(all_numeric_predictors(), -all_outcomes()) %>% 
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman")


# Apply recipe preprocessing to training data
pj_prepped <- prep(pj_rec) # preps data, applies recipe

# Run (bake) prepped preprocessng to training data to see the number of final dummy variables
pj_train_bake <- bake(pj_prepped, new_data = NULL)

# Setup our model (using ranger)
pj_spec <- 
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

pj_spec

# Build workflow to pair model and cross validation and tuning with data preprocessing
pj_wflow <- workflow() %>% 
  add_recipe(pj_rec) %>% 
  add_model(pj_spec)

set.seed(234)
pj_cv <- vfold_cv(pj_train)

doParallel::registerDoParallel(10)                     

set.seed(345)
pj_res <- tune_grid(
  pj_wflow,
  resamples = pj_cv,
  grid = 20,
  metrics = metric_set(roc_auc, accuracy, precision, sens)
  )

pj_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

rf_grid <- grid_regular(
  mtry(range = c(1, 15)),
  min_n(range = c(20, 30)),
  levels = 10
)

rf_grid

pj_res_update <- tune_grid(
  pj_wflow,
  resamples = pj_cv,
  grid = rf_grid,
  metrics = metric_set(roc_auc, accuracy, precision, sens)
)

pj_res_update %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

best_auc <- select_best(pj_res_update, "roc_auc")

final_rf <- finalize_model(
  pj_spec,
  best_auc
)

final_rf

vip_plot <- final_rf %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(AssocKey ~ .,
      data = bake(pj_prepped, new_data = NULL) %>% select(-species)
  ) %>%
  vip(geom = "col", num_features = 10)

vip_plot

vip_vars <- as.data.frame(vip_plot$data)

final_wf <- workflow() %>%
  add_recipe(pj_rec) %>%
  add_model(final_rf)

set.seed(42)
final_res <- final_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, accuracy, precision, sens))

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>%
  mutate(correct = case_when(
    AssocKey == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) %>%
  bind_cols(pj_test) %>%
  ggplot(aes(Longitude_NAD83, Latitude_NAD83, color = correct)) +
  geom_point(size = 0.5, alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c("gray80", "darkred"))

