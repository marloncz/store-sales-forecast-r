#' Reverse Scaling of Time Series
#'
#' @description This function can be used for reverse scaling of scaled
#' time series. Since all information must be provided within the dataframe as
#' columns, the reverse scaling can take place simultaneously for all
#' time series groups.
#'
#' Reverse Scaling based on Minimum and Maximum:
#' \deqn{x_{norm} = x*(x_{max}-x_{min})+x_{min}}
#'
#' Reverse Scaling based on SD and Mean:
#' \deqn{x_{norm} = x*x_{sd} + x_{mean}}
#'
#' @param .data `data.frame` that should be rescaled. The `data.frame` must
#' contain the method of the previous normalization and the associated 
#' parameters as columns.
#' @param target `chr` that specifies the name of the scaled target column 
#' in \code{.data}.
#' @param method A `str` specifying the scale method. One of `"min-max"` 
#' or `"standard"`. Default is `standard`.
#'
#' @return Returns a `data.frame` containing de-normalized observations.
#' @export
reverse_scaling <- function(.data,
                            target = "sales",
                            method = "standard") {
  
  if (method == "min-max") {
    name_x_max <- paste0(target, "_x_max")
    name_x_min <- paste0(target, "_x_min")
    
    .data <- .data %>%
      dplyr::mutate(
        !!dplyr::sym(target[i]) :=
          !!dplyr::sym(target[i]) * (!!dplyr::sym(name_x_max) - !!dplyr::sym(name_x_min)) + !!dplyr::sym(name_x_min)
      )
    out <- .data
  }
  
  if (method == "standard") {
    name_x_mean <- paste0(target, "_x_mean")
    name_x_sd <- paste0(target, "_x_sd")
    
    out <- .data %>%
      dplyr::mutate(
        !!dplyr::sym(target) :=
          !!dplyr::sym(target) * !!dplyr::sym(name_x_sd) + !!dplyr::sym(name_x_mean)
      )
  }
  
  return(out)
}