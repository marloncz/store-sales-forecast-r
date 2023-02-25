#' Get Predictive Power Score
#'
#' @param data `data.frame` containing features and target.
#' @param target name of target column
#' @param top_n number of features as `int` that should be returned based on
#' predictive power score. If `NULL` all features will be returned with the
#' corresponding predictive power score. Defualt is `NULL`.
#' @param parallel `bool`, whether to perform score calls in parallel.
#' @param subset_ratio `dbl`, sample of observations that should be used for
#' scroing.
#' 
#' @return `data.frame` containing pps for all features.
#' @export
get_predictive_power_scores <- function(
    data, 
    target, 
    top_n = NULL, 
    parallel = TRUE,
    subset_ratio = NULL
    ) {
  # selecting all numerical columns
  data <- data %>%
    dplyr::select_if(is.numeric) %>%
    ggplot2::remove_missing(na.rm = TRUE)
  
  if (!is.null(subset_ratio)) {
    data <- data %>% 
      dplyr::sample_n(size = nrow(data) * subset_ratio)
  }
  
  # get scores
  scores <- ppsr::score_predictors(df = data, y = target, do_parallel = parallel)
  
  # shaping results
  out <- scores %>%
    # turn results to tibble
    dplyr::as_tibble() %>%
    # renaming columns
    dplyr::rename(predictor = x, target = y) %>%
    # removing predictor sales from data
    dplyr::filter(predictor != target) %>%
    # select relevant columns
    dplyr::select(target, predictor, pps) %>%
    # arranging results based on pps score (higher = better)
    dplyr::arrange(desc(pps))
  
  # subsetting features based on pps
  if (!is.null(top_n)) {
    out <- out %>%
      head(top_n)
  }
  
  return(out)
}
