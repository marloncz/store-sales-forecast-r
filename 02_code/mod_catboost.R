# Packages ----
library(dplyr)
library(lubridate)
library(catboost)
library(ggplot2)
library(readr)
library(tidyr)

# sourcing R functions
function_scripts = list.files("R", full.names = TRUE)
sapply(function_scripts, source)

# Data ----
df_train <- readRDS("01_data/intermediate/df_mod_train_scaled.RDS")
df_test <- readRDS("01_data/intermediate/df_mod_test_scaled.RDS")
df_params <- readRDS("01_data/intermediate/scale_params.RDS")
df_test_id <- read_csv("01_data/raw/test.csv")
df_0 <- readRDS("01_data/intermediate/df_0_families.RDS")

df_train <- df_train %>% 
  select(-contains("sales_ma_60"), -contains("sales_unnormed_ma_60")) %>% 
  select(date, store, family, sales, onpromotion, oilprice,
         type, cluster, holiday, weekend, wday_cat, seasonal_cat, month_cat,
         days_since_pay, oilprice, contains("sales_"), chrismas,
         contains("transactions_")) %>%
  # training since 2015 instead of 2013
  filter(year(date) >= 2015) %>% 
  ggplot2::remove_missing()

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

# Defining Formulas for Training
# A direct strategy was choosen and therefore different models are trained
# for the corresponding horizons
# > 4 Model specifications will be used for training
form_3 <- as.formula(
  "sales ~ cluster + store + type + month_cat + days_since_pay + onpromotion +
    holiday + weekend + wday_cat + oilprice + chrismas + sales_lag_3 + 
    sales_lag_7 + sales_lag_14 + sales_lag_30 + transactions_ma_14_lag_3 + 
    transactions_ma_30_lag_3"
)

from_7 <- as.formula(
  "sales ~ cluster + store + type + month_cat + days_since_pay + onpromotion +
    holiday + weekend + wday_cat + oilprice + sales_lag_7 + 
    sales_lag_14 + sales_lag_30 + sales_ma_14_lag_7 + transactions_ma_14_lag_7 + 
    transactions_ma_30_lag_7"
)

form_14 <- as.formula(
  "sales ~ cluster + store + type + month_cat + days_since_pay + onpromotion +
    holiday + weekend + wday_cat + oilprice + sales_lag_14 + 
    sales_lag_30 + sales_ma_14_lag_14 + transactions_ma_14_lag_14"
)

form_16 <- as.formula(
  "sales ~ cluster + store + type + month_cat + days_since_pay + onpromotion +
    holiday + weekend + wday_cat + oilprice + sales_lag_16 + 
    sales_lag_30 + transactions_ma_14_lag_16 + transactions_ma_30_lag_16"
)

formulas <- list(
  "3" = form_3,
  "7" = from_7,
  "14" = form_14,
  "16" = form_16
)

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

# define lists for result saving
cat_models <- list()
cat_preds <- list()
full_preds <- list()
full_models <- list()

# get relevant vectors
test_dates <- unique(df_test$date)
families <- unique(df_train$family)

# iteration of model formula type and families
for (form in seq_along(formulas)) {
  form_lag <- as.numeric(names(formulas[form]))
  
  if (form == 1) {
    pred_dates <- test_dates[1:form_lag]
  } else {
    prev_lag <- as.numeric(names(formulas[form - 1]))
    pred_dates <- test_dates[(prev_lag + 1):form_lag]
  }
  
  cat_form <- formulas[[form]]
  
  for (f in seq_along(families)) {
    print(paste0("Family: ", families[f], " [", f, "/", length(families), "]"))

    tmp_train <- df_train %>% 
      filter(family == families[f])
    
    if (nrow(tmp_train) == 0) {
      print("No rows for family")
      next
    }
    
    tmp_test <- df_test %>% 
      filter(family == families[f]) %>% 
      filter(date %in% pred_dates)
    
    fit_cat <- catboost_train(
      x = tmp_train, 
      formula = cat_form, 
      params = cat_params
    )
    
    predictions <- predict_new_data(
      x = tmp_test,
      mod = fit_cat, 
      dates = pred_dates,
      formula = cat_form
    )
    
    # adding results to list
    cat_models[[f]] <- fit_cat
    cat_preds[[f]] <- predictions
  }
  full_preds[[form]] <- bind_rows(cat_preds)
  full_models[[form]] <- cat_models
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
saveRDS(assignment_pred, "01_data/results/df_asignment_20221227.RDS")
  
# prepare df_test_id to add ID to data
df_test_id <- df_test_id %>% 
  select(id, date, family, store = store_nbr) %>% 
  mutate(store = str_pad(store, width = 3, side = "left", pad = "0"))

# 
final_assignment <- assignment_pred %>% 
  left_join(df_test_id, by = c("date", "store", "family")) %>% 
  select(id, sales)

# saving submission as csv
# SCORE: 0.404033
write_csv(final_assignment, "submissionv3.csv")

