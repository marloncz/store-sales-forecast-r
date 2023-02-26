# Packages ----
library(dplyr)
library(ggplot2)
library(tsibble)
library(lubridate)
library(forcats)

# set language to en for plotting purpose
Sys.setlocale("LC_TIME", "C")

# Loading Data ----
df <- readRDS("01_data/intermediate/df_mod_train.RDS")

# General EDA ----
# data structure
glimpse(df)

# col palette
cbPalette <- c(
  "#999999", 
  "#E69F00", 
  "#56B4E9", 
  "#009E73", 
  "#F0E442", 
  "#0072B2", 
  "#D55E00", 
  "#CC79A7"
  )

# data will be aggregated on a daily level only to inspect general behavioor

df_family_agg <- df %>% 
  group_by(date) %>% 
  summarize(sales = sum(sales),
            days_since_pay = mean(days_since_pay),
            earthquake_peridod = max(earthquake_peridod),
            month_cat = min(month_cat),
            wday_cat = min(wday_cat),
            wday = min(wday),
            weekend = min(weekend),
            dcoilwtico = mean(dcoilwtico))


# sales distribution by weekday
df_family_agg %>% 
  mutate(wday_cat = factor(wday_cat,
                           levels = c(
                             "Monday", "Tuesday",
                             "Wednesday", "Thursday",
                             "Friday", "Saturday",
                             "Sunday"))) %>%
  ggplot(aes(x = wday_cat, y = sales)) +
  geom_boxplot(width = 0.2, fill = cbPalette[3]) +
  geom_violin(alpha = 0.2, fill = cbPalette[3]) +
  theme_bw() +
  labs(x = NULL, y = "Sales",
       title = "Distribution of Sales by Weekday",
       subtitle = "Fully Aggregated")

ggsave(filename = "03_figures/00_sales_distribution_by_weekday.png",
       width = 18, height = 15, units = "cm")

# sales distribution by month
df_family_agg %>% 
  arrange(date) %>% 
  mutate(month_cat = as_factor(month_cat)) %>% 
  ggplot(aes(x = month_cat, y = sales)) +
  geom_boxplot(width = 0.2, fill = cbPalette[3]) +
  geom_violin(alpha = 0.2, fill = cbPalette[3]) +
  theme_bw() +
  labs(x = NULL, y = "Sales",
       title = "Distribution of Sales by Month",
       subtitle = "Fully Aggregated")

ggsave(filename = "03_figures/00_sales_distribution_by_month.png",
       width = 18, height = 15, units = "cm")

sales_series <- df_family_agg %>% 
  filter(date >= "2016-02-15" & date <= "2016-08-01") %>% 
  pull(sales)

# defining values for geom_rect
df_rect <- tibble(
  x_min = as.Date("2016-04-16"),
  x_max = as.Date("2016-05-07"),
  y_min = min(sales_series),
  y_max = max(sales_series),
  sales = min(sales_series),
  date = as.Date("2016-04-16")
  )

# aggregated sales with earthquake peridod highlight
df_family_agg %>% 
  filter(date >= "2016-02-15" & date <= "2016-08-01") %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line(size = .6) +
  geom_point(size = .6) +
  geom_rect(
    data = df_rect, 
    mapping = aes(xmin = x_min, xmax = x_max, 
                  ymin = y_min, ymax = y_max),
    alpha = 0.3, fill = cbPalette[2]
  ) +
  scale_x_date(date_labels = "%b-%y", breaks = "month") +
  theme_bw() +
  labs(x = NULL, y = "Sales", 
       title = "Daily Aggregated Sales",
       subtitle = "Highlighted Earthquake Period")

ggsave(filename = "03_figures/00_daily_sales_earthquake.png",
       width = 18, height = 15, units = "cm")

# sales share within family by year
df %>% 
  group_by(family, year) %>% 
  summarize(sales = sum(sales)) %>% 
  mutate(perc = sales / sum(sales)) %>% 
  filter(year != 2017) %>% 
  ggplot(aes(x = reorder(family, -perc), y = perc, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", col = "black", size = .1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = cbPalette) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Percentage Share of Sales within Family", fill = NULL,
       title = "Percentage Sales Share by Year within Family",
       subtitle = "Fully Aggregated") +
  coord_flip()

ggsave(filename = "03_figures/00_perc_share_by_family_year.png",
       width = 18, height = 26, units = "cm")

df %>% 
  group_by(days_since_pay) %>% 
  summarize(sales = sum(sales),
            n = n()) %>% 
  mutate(sales_weighted = sales / n) %>% 
  ggplot(aes(x = days_since_pay, y = sales_weighted)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Days Since Payday", y = "Sales weighted by Observations",
       title = "Sales by Days Since Payday",
       subtitle = "Fully Aggregated")

ggsave(filename = "03_figures/00_sales_by_days_since_payday.png",
       width = 18, height = 15, units = "cm")
