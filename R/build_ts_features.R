#' @title Build Time-Series Features
#'
#' @description This function prepares a data frame and builds features in 
#'    regards to forecasting. 
#'
#' @details The function returns the following new features:
#'  \describe{
#'     \item{trend_}{Trends (linear, square, cubic)}
#'     \item{lag_}{Lags of order 1 to `max_horizon` of \code{target}}
#'     \item{ma_, sd_, min_, max_}{Moving window statistics (average, standard
#'     deviation, min, max) of \code{target}, with varying window lengths (3,
#'     6, 9, and 12 horizons), built on different lags to prevent data leakage}
#'     \item{seasonal_, month_}{Month and Quarter dummies}
#'   }
#'
#' @param .data \code{data.frame} which should be processed.
#' @param target name of target column in \code{.data}.
#' @param date_column name of date column in \code{.data}, must be a valid
#'   \code{Date} object.
#' @param start_horizon an integer which indicates the starting forecasting 
#' horizon for defining the full range.
#' @param max_horizon The maximum horizon for which features should be build.
#' This parameter should be set to the maximum horizon that will be forecasted. 
#' @param lags_only if \code{TRUE}, only lags will be calculated.
#' @param group `chr` that represents the column of the product group.
#' @param add_ma if `TRUE` moving averages of the lags will be created 
#' (Default: `TRUE`).
#'
#' @return Returns a \code{data.frame} with the original data and additional 
#' features.
#' @export
build_ts_features <- function(.data,
                              group = "family",
                              target = "sales",
                              date_column = "date",
                              start_horizon = 1,
                              max_horizon = 16,
                              add_ma = TRUE,
                              lags_only = FALSE) {
  
  # Check existence of var names in input
  if (!(target %in% names(.data))) stop("target not found")
  if (!(date_column %in% names(.data))) stop("date_column not found")
  
  # Check variable types
  if (!lubridate::is.Date(dplyr::pull(.data, !!sym(date_column)))) {
    stop("'date_column' must be a valid date")
  }
  
  # Sort by date and turning integer to double
  .data <- .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, as.double)
  
  # Ungroup input
  if (!is.null(group)) {
    .data <- .data %>% 
      dplyr::group_by(!!dplyr::sym(group))
  } else {
    .data <- .data %>% 
      dplyr::ungroup()
  }
  
  # arrange data based on date_column to ensure correct ordering
  .data <- .data %>% 
    dplyr::arrange(!!dplyr::sym(date_column))
  
  # dynamically create 3-step rolling window widths
  rolling_window_width <- seq(
    from = 3, to = max_horizon, by = 3
    )
  
  # Dynamic and horizon-agnostic creation of var names
  lag_names <- paste(target, "lag", 1:max_horizon, sep = "_")
  ma_names <- paste(target, "ma", rolling_window_width, sep = "_")
  
  h_range <- start_horizon:max_horizon
  lag_range <- 1:max_horizon

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
      weekend = ifelse(wday %in% c(6, 7), 1, 0)
    )
  
  lag_range <- c(1:6, lag_range[lag_range %% 7 == 0])
  # Create features
  for (h in lag_range) {
    .data <- .data %>%
      dplyr::mutate(
        # Target lags (set to NaN if lag_x < current horizon to prevent leakage)
        # e.g. if horizon = 2 then set lag_1 to NA because it is unknown
        !!dplyr::sym(lag_names[h]) := dplyr::case_when(
          h %in% h_range ~ dplyr::lag(!!sym(target), h)
          )
      )
      #   !!dplyr::sym(
      #     paste0("roll_", lag_names[h], "_w7")
      #     ) := RcppRoll::roll_meanr(!!dplyr::sym(lag_names[h]), 7),
      #   !!dplyr::sym(
      #     paste0("roll_", lag_names[h], "_w14")
      #     ) := RcppRoll::roll_meanr(!!dplyr::sym(lag_names[h]), 14)
      # )
  }
  
  # subsetting lag_range to define ma_range
  ma_range <- lag_range[lag_range %% 3 == 0]
  # ensure that the maximum value of lag_range is present
  ma_range <- unique(c(ma_range, max(lag_range)))
  if (add_ma) {
    for (r in seq_along(rolling_window_width)) {
      for (h in ma_range) {
      .data <- .data %>%
        dplyr::mutate(
          # Moving averages with rolling window based on lag h (dependend on 
          # different horizons/lags to prevent leakage)
          !!dplyr::sym(paste0(ma_names[r], "_lag_", h)) := zoo::rollapply(
            !!dplyr::sym(lag_names[h]),
            width = rolling_window_width[r],
            FUN = mean,
            na.rm = TRUE,
            align = "right",
            fill = NA,
            partial = TRUE
          )
        )
      }
    }
  }
  
  .data <- .data %>%
    dplyr::mutate(
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
  
  .data <- .data %>% 
    dplyr::ungroup()
  
  return(.data)
}