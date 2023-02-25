# Description
# This script is used for a broad feature evaluation of the created lag and 
# moving average features. No distinction is made between the different families
# as the goal is to identify the most relevat features.

# Packages ----
library(ggplot2)
library(dplyr)
library(stringr)
library(ppsr)
library(tibble)
library(tidyr)
library(magrittr)
library(purrr)

# sourcing R functions
function_scripts = list.files("R", full.names = TRUE)
sapply(function_scripts, source)

# helper function ----
# This function is only for evaluation purpose
# cor plot
get_cor <- function(data, target, features, cutoff = 0, return_df = FALSE) {
  if (!is.null(features)) {
    m_cor <- data %>%
      select(!!sym(target), all_of(features)) %>%
      remove_missing() %>%
      cor() %>%
      round(2)
  } else {
    m_cor <- data %>%
      select_if(is.numeric) %>%
      select(!!sym(target), everything()) %>%
      remove_missing() %>%
      cor() %>%
      round(2)
  }
  
  df_plt <- m_cor[, 1] %>%
    data.frame() %>%
    rownames_to_column() %>%
    tibble() %>%
    rename(cor = ".") %>%
    arrange(desc(cor)) %>% 
    filter(cor >= cutoff)
  
  if (return_df) {
    return(df_plt)
  } else {
    df_plt %>%
      filter(rowname != target) %>%
      ggplot(aes(x = cor, y = reorder(rowname, cor))) +
      geom_bar(stat = "identity", fill = "skyblue4") +
      gg_vdz() +
      labs(
        x = "Correlation", y = "Feature",
        title = paste0("Correlation between Feature and Target '", target, "'")
      )
  }
}

# Params ----
start_date <- "2015-01-01"

# Data ----
# reading prepared data containing features
df_mod <- readRDS("01_data/intermediate/df_mod_train_scaled.RDS")

# removing all rows containing missing values
df_mod <- df_mod %>%
  select(
    date, family, sales, 
    contains("lag_"), -contains("_unnormed_")
    ) %>% 
  remove_missing() %>% 
  filter(date >= start_date)

# getting all families within data
families <- df_mod %>%
  pull(family) %>%
  unique()

# splitting data and name each list element with the corresponding family
df_mod_split <- df_mod %>%
  group_split(family) %>%
  set_names(sort(families))

# PPSR ----
# predictive power score for target column
# selecting relevant columns
df_ppsr <- map(
    .x = stores,
    .f = ~ get_predictive_power_scores(
      data = df_mod_split[[.x]], 
      target = "sales", 
      parallel = TRUE, 
      subset_ratio = .2
    ) %>%
      mutate(family = .x)
  ) %>%
  bind_rows()

saveRDS(df_ppsr, "01_data/results/df_ppsr.RDS")

# plot ppsr score of the top 20
plt_ppsr <- df_ppsr %>%
  arrange(desc(pps)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(predictor, pps), y = pps)) +
  geom_bar(stat = "identity", fill = "skyblue4", col = "black", size = 0.1) +
  coord_flip() +
  theme_bw() +
  labs(
    y = "Predictive Power Score", x = "Predictor",
    title = "Predictive Power Score for Sales based on Decision Tree Models",
    subtitle = paste0("Data since January 2015")
  )

plt_ppsr

ggsave(
  filename = "03_figures/pps_score_top_20.png",
  width = 30, height = 30, units = "cm", dpi = 400
)

saveRDS(df_ppsr, "01_data/results/df_ppsr.RDS")

# Correlations ----
# getting all correaltions
df_cor <- df_mod %>% 
  get_cor(
    target = "sales",
    features = NULL,
    cutoff = 0.1,
    return_df = TRUE
  ) %>% 
  filter(rowname != "sales") %>% 
  rename(feature = rowname)

# getting all correaltions by family
df_cor_by_family <- map2(
  .x = df_mod_split,
  .y = names(df_mod_split),
  .f = ~ get_cor(
    data = .x,
    target = "sales",
    features = NULL,
    cutoff = 0.1,
    return_df = TRUE
  ) %>%
    mutate(family = .y)
) %>%
  bind_rows() %>% 
  filter(rowname != "sales") %>% 
  rename(feature = rowname)

saveRDS(df_cor, "01_data/results/df_cor.RDS")
saveRDS(df_cor_by_family, "01_data/results/df_cor_by_family.RDS")
