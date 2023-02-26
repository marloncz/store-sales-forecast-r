library(ggplot2)
library(dplyr)
library(tidyr)

# Data ----
df_train <- readRDS("01_data/intermediate/df_mod_train_scaled.RDS")
df_pred <- readRDS("01_data/results/df_asignment_20230225.RDS")
df_master <- readRDS("01_data/intermediate/df_master.RDS") 
df_0 <- readRDS("01_data/intermediate/df_0_families.RDS")

df_master <- df_master %>% 
  left_join(df_0, by = "family") %>% 
  filter(!is.na(exclude)) %>% 
  filter(store_nbr == 1)

# subsetting df_train
df_train <- df_train %>% 
  select(date, store, family, sales = sales_unnormed) %>% 
  mutate(type = "Actual")

# binding train and pred together
df_stacked <- df_pred %>% 
  mutate(type = "Prediction") %>% 
  bind_rows(df_train) %>% 
  filter(date > "2017-07-01")

# get mean sales for every family within each store
df_mean <- df_stacked %>% 
  group_by(store, family) %>% 
  summarise(mean = mean(sales, na.rm = TRUE)) %>% 
  ungroup()

# store
f_store <- "001"

f_high_vol <- df_mean %>% 
  filter(store == f_store) %>% 
  arrange(desc(mean)) %>% 
  head(9) %>% 
  pull(family)

f_low_vol <- df_mean %>% 
  filter(store == f_store) %>% 
  arrange(desc(mean)) %>% 
  tail(9) %>% 
  pull(family)

df_stacked %>% 
  filter(store == f_store & family %in% f_high_vol) %>% 
  filter(date > "2017-07-01") %>% 
  ggplot(aes(x = date, y = sales, col = type)) +
  facet_wrap("family", scales = "free") +
  geom_line() +
  scale_color_manual(values = c("#999999", "#E69F00")) +
  theme_bw() +
  scale_x_date(
    date_breaks = "month", 
    date_labels = "%b-%y", 
    limits = as.Date(c("2017-07-01", "2017-09-08"))
    ) +
  theme(legend.position = "bottom") +
  labs(
    col = NULL, x = NULL, y = "Sales",
    title = "Predictions for High Volume Families",
    subtitle = paste0("Store: ", f_store)
  )

ggsave(
  filename = "03_figures/pred_examples_001.png",
  width = 25, height = 20, units = "cm", dpi = 400
)

# saving final results
saveRDS(df_stacked, "01_data/results/df_predictions_long.RDS")

df_train %>% 
  filter(store == f_store[1]) %>% 
  filter(date < "2015-01-01" & date > "2014-01-01") %>% 
  filter(family %in% c(f_low_vol[1:2], f_high_vol[1:2])) %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line() +
  facet_wrap("family", ncol = 2, scales = "free") +
  theme_bw() +
  labs(
    col = NULL, x = NULL, y = "Sales",
    title = "Example Family Sales for the Year 2014",
    subtitle = paste0("Store: ", f_store)
  )

ggsave(
  filename = "03_figures/hist_examples_001.png",
  width = 25, height = 25, units = "cm", dpi = 400
)
