#' Scale Time Series Data
#'
#' @description This function can be used to scale a time series. This allows
#' the scaling of different time series to be standardized. Two approaches are
#'  implemented:
#'
#' Normalization with Minimum and Maximum:
#' \deqn{x_{norm} = \frac{x-x_{min}}{x_{max}-x_{min}}}
#'
#' StandardScaler by using SD and Mean:
#' \deqn{x_{norm} = \frac{x-x_{mean}}{x_{sd}}}
#'
#' @param x \code{data.frame} that should be scaled.
#' @param max_date the maximum date that should be considered for scaling. If 
#' not specified, the maximum date within the data will be used.
#' @param target name of target column as `chr`.
#' @param method `chr` that specifies the method for scaling.
#' Current possibilities are `"min-max"` and `"standard"`. By default 
#' `"standard"` is used.
#' @param date_column column name as `chr` that contains the date information 
#' of type `date`.
#' @param group_column column name as `chr` that contains the group information.
#'
#' @return A \code{data.frame} with scaled target column and
#' attributes related to the method used.
#'
#' @export
scale_data <- function(.data,
                       max_date = NULL,
                       target = "sales",
                       method = "standard",
                       date_column = "date",
                       group_column = "family") {
  
  if (is.null(max_date)) {
    max_date <- max(.data[[date_column]])
  }
  
  t <- dplyr::pull(
    dplyr::filter(.data, !!dplyr::sym(date_column) <= max_date), !!dplyr::sym(target)
  )
  
  if (method == "min-max") {
    t_min <- min(t, na.rm = TRUE)
    t_max <- max(t, na.rm = TRUE)
    
    df_normed <- .data %>%
      dplyr::mutate(
        !!dplyr::sym(paste0(target, "_unnormed")) := !!dplyr::sym(target),
        !!dplyr::sym(target) := dplyr::case_when(
          (t_max - t_min) == 0 ~ 0,
          TRUE ~ (!!dplyr::sym(target) - t_min) / (t_max - t_min)
        )
      )
    
    norm_params <- tibble::tibble(
      !!dplyr::sym(group_column) := unique(
        dplyr::pull(df_normed, !!dplyr::sym(group_column))
      ),
      !!dplyr::sym(paste0(target, "_x_min")) := t_min,
      !!dplyr::sym(paste0(target, "_x_max")) := t_max,
      method = "min-max"
    )
    
  } else if (method == "standard") {
    t_mean <- mean(t, na.rm = TRUE)
    t_sd <- sd(t, na.rm = TRUE)
    
    df_normed <- .data %>%
      dplyr::mutate(
        !!dplyr::sym(paste0(target, "_unnormed")) := !!dplyr::sym(target),
        !!dplyr::sym(target) := (!!dplyr::sym(target) - t_mean) / t_sd
      )
    
    norm_params <- tibble::tibble(
      !!dplyr::sym(group_column) := unique(dplyr::pull(df_normed, !!dplyr::sym(group_column))),
      !!dplyr::sym(paste0(target, "_x_mean")) := t_mean,
      !!dplyr::sym(paste0(target, "_x_sd")) := t_sd,
      method = "standard"
    )
  }
  
  # print warning, if NAs exists
  if (anyNA(.data[[target]])) {
    warning(paste0("Missing values in ", target, " column"))
  }
  
  out <- list(data = df_normed, params = norm_params)
  
  return(out)
}