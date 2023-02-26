# Packages ----
library(readr)
library(dplyr)
library(tidyr)
library(tsibble)

function_scripts = list.files("R", full.names = TRUE)
sapply(function_scripts, source)

# Reading Raw Data ----
df_holiday <- read_csv("01_data/raw/holidays_events.csv")
df_oil <- read_csv("01_data/raw/oil.csv")
df_stores <- read_csv("01_data/raw/stores.csv")
df_test <- read_csv("01_data/raw/test.csv")
df_train <- read_csv("01_data/raw/train.csv")
df_transactions <- read_csv("01_data/raw/transactions.csv")

# Fill potential Gaps ----
# There are some gaps that exist at the end of December in many years. These
# gaps are filled with 0s for the training data
rows <- nrow(df_train)
df_train <- df_train %>% 
  as_tsibble(key = c("store_nbr", "family")) %>% 
  fill_gaps(sales = 0, onpromotion = 0) %>% 
  as_tibble()

added_rows <- nrow(df_train) - rows
print(paste0("Added ", added_rows, " rows due gap filling"))

rows <- nrow(df_test)
df_test <- df_test %>% 
  as_tsibble(key = c("store_nbr", "family")) %>% 
  fill_gaps(sales = NA, onpromotion = NA) %>% 
  as_tibble()

added_rows <- nrow(df_test) - rows
print(paste0("Added ", added_rows, " rows due gap filling"))


# prepare holiday information
df_holiday_prep <- prepare_holiday_data(df_holiday)

# create master dataframe with train and test data
df_master <- df_train %>% 
  bind_rows(df_test) %>% 
  # adding store information
  left_join(df_stores, by = "store_nbr") %>%
  # adding holiday information
  left_join(
    df_holiday_prep$national, by = "date", suffix = c("", "_holiday_nat")
  ) %>% 
  left_join(
    df_holiday_prep$local, 
    by = c("date" = "date", "city" = "locale_name"), 
    suffix = c("", "_holiday_loc")
  ) %>% 
  left_join(
    df_holiday_prep$regional,
    by = c("date" = "date", "state" = "locale_name"),
    suffix = c("", "_holiday_reg")
  ) %>%
  # adjust holiday information based on locale as holiday information was
  # only added by date
  mutate(
    holiday_event = case_when(
      !is.na(type_holiday_nat) ~ TRUE,
      !is.na(type_holiday_loc) ~ TRUE,
      !is.na(type_holiday_reg) ~ TRUE,
      TRUE ~ FALSE
    ),
    holiday_type = case_when(
      !is.na(type_holiday_nat) ~ "National",
      !is.na(type_holiday_loc) ~ "Local",
      !is.na(type_holiday_reg) ~ "Regional",
      TRUE ~ "Nothing"
    ),
    # adding additional type to column
    holiday = ifelse(
      holiday_event, "Holiday", "Nothing"
    )
  ) %>%
  select(-type_holiday_nat:-type_holiday_reg) %>%
  left_join(df_oil, by = "date") %>% 
  # oil price information contains several NAs 
  # these NAs will be filled up with a downup logic
  fill(dcoilwtico, .direction = "downup")

# adding transaction data
df_master <- df_master %>% 
  left_join(df_transactions, by = c("date", "store_nbr"))

# saving prepared data
saveRDS(df_master, "01_data/intermediate/df_master.RDS")

# deleting objects from memory
rm(
  "df_holiday", "df_oil", "df_stores", "df_test", "rows", "added_rows",
  "df_train", "df_master", "df_holiday_prep"
)

