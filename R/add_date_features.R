#' Adds Time-Series Features
#'
#' @description This function adds time series features based on the date 
#' column only.
#'  
#' @param .data `data.frame` which should be processed.
#' @param date_column name of date column in `.data`, must be a valid
#'   `Date` object.
#'
#' @return
#' @export
add_date_features <- function(.data,
                              date_column = "date") {
  # building weekday features
  .data <- .data %>%
    dplyr::mutate(
      # weekday calculation
      wday = lubridate::wday(!!sym(date_column)),
      # create one categorical column
      wday_cat = dplyr::case_when(
        wday == 1 ~ "Monday",
        wday == 2 ~ "Tuesday",
        wday == 3 ~ "Wednesday",
        wday == 4 ~ "Thursday",
        wday == 5 ~ "Friday",
        wday == 6 ~ "Saturday",
        wday == 7 ~ "Sunday"
      ),
      # encoding weekdays to dummies
      mon = ifelse(wday == 1, 1, 0),
      tue = ifelse(wday == 2, 1, 0),
      wed = ifelse(wday == 3, 1, 0),
      thu = ifelse(wday == 4, 1, 0),
      fri = ifelse(wday == 5, 1, 0),
      sat = ifelse(wday == 6, 1, 0),
      sun = ifelse(wday == 7, 1, 0),
      # encoding weekend
      weekend = ifelse(wday %in% c(6, 7), 1, 0),
      # trend information
      trend_linear = 1:dplyr::n(),
      trend_squared = trend_linear^2,
      trend_cubic = trend_linear^3,
      # quarters
      seasonal_q1 = ifelse(
        lubridate::month(!!sym(date_column)) %in% c(1, 2, 3), 1, 0
      ),
      seasonal_q2 = ifelse(
        lubridate::month(!!sym(date_column)) %in% c(4, 5, 6), 1, 0
      ),
      seasonal_q3 = ifelse(
        lubridate::month(!!sym(date_column)) %in% c(7, 8, 9), 1, 0
      ),
      seasonal_q4 = ifelse(
        lubridate::month(!!sym(date_column)) %in% c(10, 11, 12), 1, 0
      ),
      # defining categorical column
      seasonal_cat = dplyr::case_when(
        seasonal_q1 == 1 ~ "Q1",
        seasonal_q2 == 1 ~ "Q2",
        seasonal_q3 == 1 ~ "Q3",
        seasonal_q4 == 1 ~ "Q4"
      ),
      # Monthy (trend and dummy encoding)
      month = lubridate::month(!!dplyr::sym(date_column)),
      month_1 = ifelse(month == 1, 1, 0),
      month_2 = ifelse(month == 2, 1, 0),
      month_3 = ifelse(month == 3, 1, 0),
      month_4 = ifelse(month == 4, 1, 0),
      month_5 = ifelse(month == 5, 1, 0),
      month_6 = ifelse(month == 6, 1, 0),
      month_7 = ifelse(month == 7, 1, 0),
      month_8 = ifelse(month == 8, 1, 0),
      month_9 = ifelse(month == 9, 1, 0),
      month_10 = ifelse(month == 10, 1, 0),
      month_11 = ifelse(month == 11, 1, 0),
      month_12 = ifelse(month == 12, 1, 0),
      # defining categorical column
      month_cat = dplyr::case_when(
        month_1 == 1 ~ "Jan",
        month_2 == 1 ~ "Feb",
        month_3 == 1 ~ "Mar",
        month_4 == 1 ~ "Apr",
        month_5 == 1 ~ "May",
        month_6 == 1 ~ "Jun",
        month_7 == 1 ~ "Jul",
        month_8 == 1 ~ "Aug",
        month_9 == 1 ~ "Sep",
        month_10 == 1 ~ "Oct",
        month_11 == 1 ~ "Nov",
        month_12 == 1 ~ "Dec"
      )
    )
  
  return(.data)
}