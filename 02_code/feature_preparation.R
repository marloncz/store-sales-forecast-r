# Packages ----
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

# sourcing R functions
function_scripts = list.files("R", full.names = TRUE)
sapply(function_scripts, source)

# Reading Data ----
# Note: data is based on the script data_preparation.R
df <- readRDS("01_data/intermediate/df_master.RDS")

# Building Features ----

# ...TS Features ----
# splitting data to different data frames based on stores
df_split <- df %>% 
  group_split(store_nbr)

# features will be build for each store and family within each store
# the result is a single data.frame that contains all time-series features 
# based on the sales column
df_features <- map(.x = df_split,
                   .f = ~ build_ts_features(
                     .data = .x,
                     target = "sales",
                     group = "family",
                     # defined by the maximum number of days that need
                     # to be predicted for the provided test.csv
                     max_horizon = 16)) %>% 
  bind_rows()

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
  # TODO: evaluate patterns concerning this period.
  mutate(
    earthquake_peridod = case_when(
      date >= "2016-04-16" & date <= as.Date("2016-04-16") + weeks(3) ~ 1,
      TRUE ~ 0
    )
  )

# removing columns that are not needed an make some further adjustments
df_features <- df_features %>%
  mutate(
    # turn store ID to a categorical column
    store_nbr = str_pad(store_nbr, width = 3, side = "left", pad = "0"),
    cluster = str_pad(cluster, width = 3, side = "left", pad = "0"),
    weekend = ifelse(weekend == 0, "Workday", "Weekend")
  ) %>%
  select(date, store = store_nbr, oilprice = dcoilwtico, family, sales,
          everything(), -id) 

# splitting data into train and test as it was initally provided. The split is
# defined by the sales column. All rows that have an NA for sales are part of
# the test.csv and need to be predicted for the submission
df_mod_train <- df_features %>% 
  dplyr::filter(!is.na(sales))

# getting product families without any sales for each store. As no sales 
# occured in the given history the assumption can be made that these families
# will have no sales in the future for the corresponding stores.
df_0_families <- df_mod_train %>% 
  group_by(store, family) %>% 
  summarize(sales = sum(sales, na.rm = TRUE),
            n = n()) %>% 
  filter(sales == 0) %>% 
  mutate(exclude = TRUE) %>% 
  select(-sales, -n) %>% 
  ungroup()

# filtering is based on NA values that are resulting from the join for all 
# store-family combinations that do not have 0 sales only
df_mod_train <- df_mod_train %>% 
  left_join(df_0_families, by = c("store", "family")) %>%
  filter(is.na(exclude)) %>% 
  select(exclude)

df_mod_test <- df_features %>% 
  dplyr::filter(is.na(sales))

# saving data
saveRDS(df_mod_train, "01_data/intermediate/df_mod_train.RDS")
saveRDS(df_mod_test, "01_data/intermediate/df_mod_test.RDS")
saveRDS(df_0_families, "01_data/intermediate/df_0_families.RDS")

# removing objects
rm(
  "df", "df_split", "df_features", "df_mod_train", 
  "df_mod_test", "df_0_families"
)