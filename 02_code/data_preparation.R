# Packages ----
library(readr)
library(dplyr)
library(tidyr)

# Reading Raw Data ----
df_holiday <- read_csv("01_data/raw/holidays_events.csv")
df_oil <- read_csv("01_data/raw/oil.csv")
df_stores <- read_csv("01_data/raw/stores.csv")
df_test <- read_csv("01_data/raw/test.csv")
df_train <- read_csv("01_data/raw/train.csv")

# create master dataframe with train and test data
df_master <- df_train %>% 
  bind_rows(df_test) %>% 
  # adding store information
  left_join(df_stores, by = "store_nbr") %>% 
  # adding holiday information
  left_join(df_holiday, by = "date", suffix = c("", "_holiday")) %>% 
  # adjust holiday information based on locale as holiday information was
  # only added by date
  mutate(
    holiday_event = case_when(
      locale == "National" ~ TRUE,
      locale == "Local" & locale_name == city ~ TRUE,
      locale == "Regional" & locale_name == state ~ TRUE,
      # transferred holidays will be celebrated on another date
      # all entries that are transferred will be not considered as a holiday
      # event. 
      transferred ~ FALSE,
      TRUE ~ FALSE
    ),
    # adding additional type to column
    type_holiday = ifelse(
      holiday_event, type_holiday, "Nothing"
    )
  ) %>% 
  left_join(df_oil, by = "date") %>% 
  # oil price information contains several NAs 
  # these NAs will be filled up with a downup logic
  fill(dcoilwtico, .direction = "downup")

# saving prepared data
saveRDS(df_master, "01_data/intermediate/df_master.RDS")

# deleting objects from memory
rm("df_holiday", "df_oil", "df_stores", "df_test", "df_train", "df_master")

