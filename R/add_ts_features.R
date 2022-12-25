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
      dplyr::group_by_at(groups)
  }
  
  # arrange data based on date_column to ensure correct ordering
  .data <- .data %>% 
    dplyr::arrange(!!dplyr::sym(date_column))
  
  # Dynamic and horizon-agnostic creation of var names
  lag_names <- paste(target, "lag", lags, sep = "_")
  ma_names <- paste(target, "ma", mas, sep = "_")

  # Create features
  for (h in seq_along(lags)) {
    .data <- .data %>%
      dplyr::mutate(
        !!dplyr::sym(lag_names[h]) := dplyr::lag(!!sym(target), lags[h])
      )
  }
  
  # TODO: refactor to data.table
  if (add_ma) {
    for (r in seq_along(mas)) {
      print(paste0("MA Width ", r, "/", length(mas)))
      for (h in seq_along(lags)) {
        print(paste0("Lag ", h, "/", length(lags)))
        .data <- .data %>%
          dplyr::mutate(
            # Moving averages with rolling window based on lag h (dependend on 
            # different horizons/lags to prevent leakage)
            !!dplyr::sym(paste0(ma_names[r], "_lag_", lags[h])) := zoo::rollapply(
              !!dplyr::sym(lag_names[h]),
              width = mas[r],
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
    dplyr::ungroup()
  
  return(.data)
}