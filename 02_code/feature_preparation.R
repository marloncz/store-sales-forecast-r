# Packages ----
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(data.table)
library(tidyr)

# sourcing R functions
function_scripts = list.files("R", full.names = TRUE)
sapply(function_scripts, source)

scale <- TRUE

# Reading Data ----
# Note: data is based on the script data_preparation.R
df <- readRDS("01_data/intermediate/df_master.RDS")

# renaming oil column
df <- df %>% 
  # NOTE: oilprice is available for the full test period. This information is
  # not available under real conditions. However, as we do have this information
  # provided, no lags will be build.
  rename(oilprice = dcoilwtico)

# get test start
test_start <- df %>% 
  filter(is.na(sales)) %>% 
  pull(date) %>% 
  min()

# 0 families
# getting product families without any sales for each store. As no sales 
# occured in the given history the assumption can be made that these families
# will have no sales in the future for the corresponding stores.
df_0_families <- df %>% 
  filter(!is.na(sales)) %>% 
  mutate(store = str_pad(store_nbr, width = 3, side = "left", pad = "0")) %>% 
  group_by(store, family) %>% 
  summarize(sales = sum(sales, na.rm = TRUE),
            n = n()) %>% 
  filter(sales == 0) %>% 
  mutate(exclude = TRUE) %>% 
  select(-sales, -n) %>% 
  ungroup()

# Scaling ----
if (scale) {
  df_split_scale <- df %>% 
    group_by(store_nbr, family) %>% 
    group_split()
  
  # loop over each df and scale data
  df_split_scaled <- purrr::map(.x = df_split_scale, .f = scale_data)
  
  # extract params for rescaling purpose
  scale_params <- purrr::map(.x = df_split_scaled,
                             .f = ~ .x$params) %>% 
    bind_rows() %>% 
    # adjust store_nbr for later joining purpose
    mutate(store_nbr = str_pad(store_nbr, width = 3, side = "left", pad = "0"))
  
  # extract data and binding rows together
  df_split_scaled <- purrr::map(.x = df_split_scaled,
                                .f = ~ .x$data) 
  
  # scaling transactions as it is also target specific
  df_split_scaled <- purrr::map(.x = df_split_scaled, 
                                .f = ~scale_data(.data = .x, 
                                                 target = "transactions"))
  
  # no rescaling for transactions needed, params can be ignored
  df_scaled_data <- purrr::map(.x = df_split_scaled,
                               .f = ~ .x$data)
  
  # binding rows together
  df <- df_scaled_data %>% 
    bind_rows()
  
}

# Building Features ----
# adding lags and moving averages to data 
# for sales and transaction information
df_features <- df %>% 
  add_ts_features(
    target = "sales",
    lags = c(1, 3, 7, 14, 16, 30),
    mas = c(14, 30, 60),
    groups = c("store_nbr", "family")
    ) %>% 
  add_ts_features(
    target = "transactions",
    lags = c(1, 3, 7, 14, 16, 30),
    mas = c(14, 30),
    groups = c("store_nbr", "family")
  )

# adjust colname based on sclaing param
if (scale) {
  df_features <- df_features %>% 
    add_ts_features(
      target = "sales_unnormed",
      lags = c(1),
      mas = c(30, 60),
      groups = c("store_nbr", "family")
    )
  
  df_features <- df_features %>% 
    mutate(ma_inactive_30 = sales_unnormed_ma_30,
           ma_inactive_60 = sales_unnormed_ma_60)
} else {
  df_features <- df_features %>% 
    mutate(ma_inactive_30 = sales_ma_30,
           ma_inactive_60 = sales_ma_60)
}

# adding date features
df_features <- df_features %>% 
  add_date_features()

# ...Additional Features ----
# Based on the additional notes we can create two more features
# 1. Wages in the public sector are paid every two weeks on the 15 th and on 
#    the last day of the month. Supermarket sales could be affected by this.
# 2. A magnitude 7.8 earthquake struck Ecuador on April 16, 2016. People 
#    rallied in relief efforts donating water and other first need products 
#    which greatly affected supermarket sales for several weeks after the 
#    earthquake.
df_features <- df_features %>%
  # adding days since pay information
  # TODO: it should be evaluated if there are patterns concerning the case
  # that the payday falls to a weekend day
  mutate(year = year(date),
         day = day(date)) %>% 
  group_by(year, month) %>% 
  mutate(
    days_since_pay = case_when(
      day == max(day) ~ 0L,
      day < 15L ~ day,
      TRUE ~ day - 15L
    )
  ) %>% 
  ungroup() %>% 
  # earthquake period is initially defined by 3 weeks, starting from the 
  # given date
  # TODO: evaluate patterns in more detail concerning the earthquake peridod
  mutate(
    earthquake_peridod = case_when(
      date >= "2016-04-16" & date <= as.Date("2016-04-16") + weeks(3) ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  # adding chrismas indicator
  mutate(chrismas = case_when(
    lubridate::month(date) == 12 & lubridate::mday(date) == 25 ~ "Chrismas",
    TRUE ~ "No Chrismas"
  ))

# removing columns that are not needed an make some further adjustments
df_features <- df_features %>%
  mutate(
    # turn store ID to a categorical column
    store_nbr = str_pad(store_nbr, width = 3, side = "left", pad = "0"),
    cluster = str_pad(cluster, width = 3, side = "left", pad = "0"),
    weekend = ifelse(weekend == 0, "Workday", "Weekend")
  ) %>%
  select(date, store = store_nbr, oilprice, family, sales, everything(), -id)

df_features <- df_features %>% 
  dplyr::mutate(
    # if no sale occured within the past month, the product will be classified
    # as inactive
    inactive_30 = case_when(
      ma_inactive_30 == 0 ~ 1,
      TRUE ~ 0
    ),
    inactive_60 = case_when(
      ma_inactive_60 == 0 ~ 1,
      TRUE ~ 0
    ),
  )

# splitting data into train and test as it was initally provided. The split is
# defined by the sales column. All rows that have an NA for sales are part of
# the test.csv and need to be predicted for the submission
df_mod_train <- df_features %>% 
  # remove observations with NA sales
  # NA resulting from test period or scaling
  filter(!is.na(sales))

# identify first valid observation for each family on store level
if (scale) {
  df_first_valid_obs <- df_mod_train %>% 
    # turn value to NA if sale is zero
    mutate(sales_filter = case_when(
      sales_unnormed == 0 ~ as.double(NA),
      TRUE ~ sales_unnormed
    ))
} else {
  df_first_valid_obs <- df_mod_train %>% 
    # turn value to NA if sale is zero
    mutate(sales_filter = case_when(
      sales == 0 ~ as.double(NA),
      TRUE ~ sales
    ))
}

# getting first valid observation for every product on store level
df_first_valid_obs <- df_first_valid_obs %>%
  select(date, store, family, sales_filter) %>% 
  # remove all rows containing NA values within new created column
  filter(!is.na(sales_filter)) %>% 
  # get the minimum date for each family on store level representing the first
  # valid observation
  group_by(store, family) %>% 
  summarize(first_valid_obs = min(date)) %>% 
  ungroup()

# adding created df with first valid obs and removing all rows that are not
# part of actual sales history
df_mod_train <- df_mod_train %>% 
  left_join(df_first_valid_obs, by = c("store", "family")) %>% 
  filter(date >= first_valid_obs)

# filtering is based on NA values that are resulting from the join for all 
# store-family combinations that do not have 0 sales only
df_mod_train <- df_mod_train %>% 
  left_join(df_0_families, by = c("store", "family")) %>%
  filter(is.na(exclude)) %>%
  # adding filter based on test_start
  filter(date < test_start)

df_mod_test <- df_features %>% 
  # only keep rows that are part of the text period
  filter(date >= test_start)

# define inactive products
# products that are not sold since 2017-06-01 will be classified as
# inactive. All future observations will be set to 0. These products will be 
# also not included within the training
df_inactive <- df_mod_train %>% 
  filter(date >= "2017-07-01") %>% 
  group_by(store, family) %>% 
  summarise(n = n(),
            inactive = sum(inactive_30)) %>% 
  filter(n == inactive) %>% 
  mutate(prod_inactive = TRUE) %>% 
  select(-n, -inactive)

# exclude families that are classified as inactive and will be predicted with
# 0 by default
df_mod_train <- df_mod_train %>% 
  left_join(df_inactive, by = c("store", "family")) %>% 
  mutate(prod_inactive = ifelse(is.na(prod_inactive), FALSE, prod_inactive)) %>% 
  filter(!prod_inactive)

# adding inactive products to df_0_families
df_0_families <- df_0_families %>% 
  bind_rows(df_inactive %>% 
              rename(exclude = prod_inactive))

# saving data
saveRDS(df_0_families, "01_data/intermediate/df_0_families.RDS")

if (scale) {
  saveRDS(df_mod_train, "01_data/intermediate/df_mod_train_scaled.RDS")
  saveRDS(df_mod_test, "01_data/intermediate/df_mod_test_scaled.RDS")
  saveRDS(scale_params, "01_data/intermediate/scale_params.RDS")
  rm("scale_params", "df_split_scale", "df_split_scaled")
} else {
  saveRDS(df_mod_train, "01_data/intermediate/df_mod_train_unscaled.RDS")
  saveRDS(df_mod_test, "01_data/intermediate/df_mod_test_unscaled.RDS")
}

# removing objects
rm(
  "df", "df_features", "df_mod_train", 
  "df_mod_test", "df_0_families"
)