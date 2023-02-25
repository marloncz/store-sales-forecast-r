# Packages ----
library(dplyr)
library(lubridate)
library(catboost)
library(ggplot2)
library(readr)
library(tidyr)
library(readr)

# sourcing R functions
function_scripts = list.files("R", full.names = TRUE)
sapply(function_scripts, source)

# Data ----
df_train <- readRDS("01_data/intermediate/df_mod_train_scaled.RDS")
df_test <- readRDS("01_data/intermediate/df_mod_test_scaled.RDS")
df_params <- readRDS("01_data/intermediate/scale_params.RDS")
df_test_id <- read_csv("01_data/raw/test.csv")
df_0 <- readRDS("01_data/intermediate/df_0_families.RDS")
df_cor_by_store <- readRDS("01_data/results/df_cor_by_family.RDS")

df_train <- df_train %>% 
  select(-contains("sales_ma_60"), -contains("sales_unnormed_ma_60")) %>% 
  select(date, store, family, sales, onpromotion, oilprice,
         type, cluster, holiday, weekend, wday_cat, seasonal_cat, month_cat,
         days_since_pay, oilprice, contains("sales_"), chrismas,
         contains("transactions_")) %>%
  # training since 2015 instead of 2013
  filter(year(date) >= 2015) %>% 
  remove_missing()

# Subsetting data based on EDA in regards to sales history
df_train <- df_train %>%
  mutate(
    train_exclusion = case_when(
      family == "CELEBRATION" & date < "2015-08-15" ~ TRUE,
      family == "BOOKS" & date < "2017-05-01" ~ TRUE,
      family == "LADIESWEAR" & date < "2015-08-15" ~ TRUE,
      family == "MAGAZINES" & date < "2016-02-01" ~ TRUE,
      family == "LIQUOR,WINE,BEER" & date < "2016-08-05" ~ TRUE,
      family == "SCHOOL AND OFFICE SUPPLY" & date >= "2015-01-01" & date <= "2015-07-01" ~ TRUE,
      family == "HOME AND KITCHEN" & date < "2015-02-15" ~ TRUE,
      family == "BABY CARE" & date < "2016-03-01" ~ TRUE,
      family == "PLAYERS AND ELECTRONICS" & date < "2015-07-01" ~ TRUE,
      family == "PRODUCE" & date < "2015-08-01" ~ TRUE,
      family == "PET SUPPLIES" & date < "2015-08-01" ~ TRUE,
      family == "HOME CARE" & date < "2015-04-01" ~ TRUE,
      TRUE ~ FALSE
    )) %>% 
  filter(!train_exclusion)

df_train <- df_train %>% 
  mutate(exclude_extreme = case_when(
    date >= "2016-04-16" & date <= as.Date("2016-04-16") + weeks(4) ~ TRUE,
    TRUE ~ FALSE
  )
  ) %>% 
  # exclude earthquake period
  filter(!exclude_extreme)

# initial params for first inspection
cat_params <- list(
  iterations = 800,
  loss_function = "RMSE",
  depth = 10,
  learning_rate = 0.04,
  metric_period = 400)

# defining max features that should be used
max_features <- 25

# define lists for result saving
cat_models <- list()
cat_preds <- list()
full_preds <- list()
full_models <- list()

# get relevant vectors
test_dates <- unique(df_test$date)
families <- unique(df_train$family)

# A direct strategy was choosen and therefore different models are trained
# for the corresponding horizons
# > 4 Model specifications will be used for training
max_steps <- c(3, 7, 14, 16)

for (f in seq_along(families)) {
  # logging...
  print(paste0("Family: ", families[f], " [", f, "/", length(families), "]"))
  
  # subsetting test data
  tmp_train <- df_train %>% 
    filter(family == families[f])
  
  if (nrow(tmp_train) == 0) {
    print("No rows for family")
    next
  }
  
  for (s in seq_along(max_steps)) {
    # Defining Formulas for Training
    ms <- max_steps[s]
    
    # logging...
    print(paste0("Lag: ", ms, " [", s, "/", length(max_steps), "]"))
    
    if (s == 1) {
      pred_dates <- test_dates[1:ms]
    } else {
      prev_lag <- max_steps[s - 1]
      pred_dates <- test_dates[(prev_lag + 1):ms]
    }
    
    # subsetting test data
    tmp_test <- df_test %>% 
      filter(family == families[f]) %>% 
      filter(date %in% pred_dates)
    
    # getting relevant features
    features <- df_cor_by_family %>% 
      filter(family == families[f]) %>% 
      pull(feature)
    
    # ensure that all features are present within training data
    features <- features[features %in% colnames(tmp_train)]
    
    # subsetting features based on lags
    sub_lag_str <- stringr::str_extract_all(features, "lag_[0-9]+") %>%
      unlist()
    
    lags_to_exclude <- sub_lag_str[
      as.integer(stringr::str_extract_all(sub_lag_str, "(\\d)+$")) < ms
    ]
    
    if (length(lags_to_exclude) > 0) {
      rgx_dtc <- paste0(lags_to_exclude, collapse = "|")
    } else {
      rgx_dtc <- "NOTHING"
    }
    
    # subsetting features
    feature_sub <- features[!stringr::str_detect(features, rgx_dtc)]
    
    # defining formula features
    if (length(feature_sub) < max_features) {
      form_feats <- feature_sub
    } else {
      form_feats <- feature_sub[1:max_features]
    }
    
    # define formula for model training
    cat_form <- as.formula(
      paste0(
        "sales ~ ", "store + oilprice + onpromotion + weekend + days_since_pay",
        " + wday_cat + ", 
        paste0(form_feats, collapse = " + ")
      )
    )
    
    # fitting model
    fit_cat <- catboost_train(
      x = tmp_train, 
      formula = cat_form, 
      params = cat_params
    )
    
    # making predictions
    predictions <- predict_new_data(
      x = tmp_test,
      mod = fit_cat, 
      dates = pred_dates,
      formula = cat_form
    )
    
    # adding results to list
    cat_models[[s]] <- fit_cat
    cat_preds[[s]] <- predictions
  }
  full_preds[[f]] <- bind_rows(cat_preds)
  full_models[[f]] <- cat_models
}

# prepare results
assignment_pred <- full_preds %>% 
  # binding results to one dataframe
  bind_rows() %>% 
  # select relevant columns
  select(date, store, family, actual = sales_unnormed, sales = sales_pred) %>% 
  # adding params for scaling and rescale predictions
  left_join(df_params, by = c("family", "store" = "store_nbr")) %>% 
  reverse_scaling() %>% 
  # join all cases with 0 sales to predictions
  left_join(df_0, by = c("store", "family")) %>% 
  mutate(exclude = ifelse(is.na(exclude), FALSE, exclude)) %>% 
  # turn sales to 0 if store family combination is a zero product or sales were
  # predicted negative (rare cases)
  mutate(sales = case_when(
    exclude ~ 0,
    sales < 0 ~ 0,
    TRUE ~ sales
  )) %>% 
  # select relevant columns
  select(date, store, family, sales)

# save results as RDS
saveRDS(assignment_pred, "01_data/results/df_asignment_20230225.RDS")

# prepare df_test_id to add ID to data
df_test_id <- df_test_id %>% 
  select(id, date, family, store = store_nbr) %>% 
  mutate(store = str_pad(store, width = 3, side = "left", pad = "0"))

# 
final_assignment <- assignment_pred %>% 
  left_join(df_test_id, by = c("date", "store", "family")) %>% 
  select(id, sales)

# saving submission as csv
# SCORE: 0.41368
write_csv(final_assignment, "submissionv_20230225.csv")

