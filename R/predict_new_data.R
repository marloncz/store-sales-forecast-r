#' Predict New Data
#'
#' @description This function is used to made predictions based on a
#' given `data.frame` containing all needed information.
#'
#' @param x `data.frame` that contains the data for prediction purpose.
#' @param target target column as `chr`.
#' @param mod fitted model.
#' @param dates vector of dates as `date` that should be predicted.
#' @param formula `formula` that was used for the training.
#' @param na_rm `boolean` that indicates wether or not rows with missing values
#' should be excluded (Default: `FALSE`). If `TRUE` some product groups
#' will not be predicted because of systematic missing values (e.g. new
#' product launches).
#'
#' @return `data.frame` with predicted values.
predict_new_data <- function(x,
                             target = "sales",
                             keep_origin = TRUE,
                             mod,
                             formula,
                             dates,
                             na_rm = FALSE) {
  x_dates <- unique(x$date)
  missing_dates <- dates[!dates %in% x_dates]
  
  if (length(missing_dates) > 0) {
    print(glue::glue(
      "=============================================\n",
      "Warning: Dates missing\n",
      "{paste0(missing_dates, collapse = ',')}\n\n",
      "============================================="
    ))
  }
  
  x <- dplyr::filter(x, date %in% dates)
  
  # get all terms based on formula
  terms <- attributes(terms(formula))
  
  x_pred <- x %>%
    dplyr::select(date, dplyr::all_of(terms$term.labels))
  
  if (na_rm) {
    x_pred_sub <- x_pred %>%
      ggplot2::remove_missing()
  } else {
    x_pred_sub <- x_pred
  }
  
  # predict new data
  if (class(mod) == "catboost.Model") {
    # adjust data frame for catboost prediction
    x_pred_sub_cat <- x_pred_sub %>%
      # deselecting date to ensure pool loading
      dplyr::select(-date) %>%
      # transfrom all chr columns to fct type
      dplyr::mutate_if(is.character, forcats::as_factor)
    
    # transofrm data to catboost specific data type
    pool_pred <- catboost::catboost.load_pool(data = x_pred_sub_cat)
    
    # prediction based on the created pool
    target_preds <- catboost::catboost.predict(model = mod, pool = pool_pred)
  } else {
    target_preds <- predict(mod, x_pred_sub)
  }
  
  if (keep_origin) {
    out <- x %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!dplyr::sym(paste0(target, "_pred")) := target_preds)
  } else {
    out <- x %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!dplyr::sym(target) := target_preds) 
  }
  
  return(out)
}