#' Adding Time-Series Features for Target Column
#' 
#' @description This function prepares a data frame and builds features in 
#'  regards to forecasting. 
#'
#' @param .data `data.frame` which should be processed.
#' @param lags vector of type `int` defining the lags.
#' @param mas vector of type `int` defining the width of the moving averages.
#' @param groups group columns that should be considered as `chr` vector
#' @param target name of target column in `.data`.
#' @param date_column name of date column in `.data`, must be a valid
#'   `Date` object.
#' @param add_ma if `TRUE` moving averages of the lags will be created 
#' (Default: `TRUE`).
#'
#' @return `data.frame` containing lags and moving averages.
#' @export
add_ts_features <- function(.data, 
                            lags,
                            mas,
                            groups,
                            target = "sales",
                            date_column = "date",
                            add_ma = TRUE) {
  
  # Check existence of var names in input
  if (!(target %in% names(.data))) stop("target not found")
  if (!(date_column %in% names(.data))) stop("date_column not found")
  
  # Check variable types
  if (!lubridate::is.Date(dplyr::pull(.data, !!sym(date_column)))) {
    stop("'date_column' must be a valid date")
  }
  
  print(paste0("Adding Features for Target '", target, "'"))
  
  # ungroup data and numeric to double
  .data <- .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, as.double)
  
  # Groupiing
  if (!is.null(groups)) {
    .data <- .data %>% 
      tidyr::unite("group", groups, remove = FALSE)
  }
  
  # arrange data based on date_column to ensure correct ordering
  .data <- .data %>% 
    dplyr::arrange(!!dplyr::sym(date_column)) %>% 
    dplyr::mutate(target_col = !!dplyr::sym(target))
  
  # Dynamic and horizon-agnostic creation of var names
  lag_names <- paste(target, "lag", lags, sep = "_")
  ma_names <- paste(target, "ma", mas, sep = "_")
  
  dt <- data.table::as.data.table(.data)
  
  # Moving Averages
  for (m in mas) {
    col_ma <- paste0(target, "_ma_", m)
    dt[, (col_ma) := frollmean(target_col, m, na.rm = TRUE), by = group]
  }
  
  for (l in lags) {
    col <- paste0(target, "_lag_", l)
    dt[, (col) := shift(target_col, n = l), by = group]
  }
  
  .data <- tibble::as_tibble(dt) %>% 
    dplyr::group_by_at(groups)
  
  for (l in lags) {
    for (m in mas) {
      col_ma <- paste0(target, "_ma_", m)
      col_ma_lag <- paste0(col_ma, "_lag_", l)
      .data <- .data %>% 
        dplyr::mutate(
          !!dplyr::sym(col_ma_lag) := dplyr::lag(!!dplyr::sym(col_ma), n = l)
        )
    }
  }
  
  .data <- .data %>%
    dplyr::ungroup() %>% 
    dplyr::select(-group, -target_col)
  
  return(.data)
}