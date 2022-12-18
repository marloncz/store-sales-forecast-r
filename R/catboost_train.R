#' Train Catboost Model
#'
#' @param x `data.frame` that contains the training data.
#' @param cat_weights column name of type `chr` that indicates the column within
#' the provided `data.frame` that represents the weights for each observation.
#' This vector must have the same length as the rows of the `data.frame` (Default: `NULL`).
#' @param validation should a validation take place while training?
#' If `TRUE` the data for training will be additionally splitted into training and validation (Default: `FALSE`).
#' @param formula formula of type `formula` that should be used for training
#' @param params `list` that contains all parameters for training (default: `NULL`. See
#' [catboost.train](https://catboost.ai/en/docs/concepts/r-reference_catboost-train#parameters)
#' for details.
#'
#' @return catboost model
#' @export
catboost_train <- function(x,
                           cat_weights = NULL,
                           validation = FALSE,
                           formula,
                           params = NULL) {
  
  # input checks
  if (!inherits(formula, "formula")) stop("formula is not of type formula")
  if (!is.data.frame(x)) stop("x is not of type data.frame")
  if (!is.null(params)) {
    if (!is.list(params)) stop("params is not of type list")
  }
  
  # get terms of formula that are needed for subsetting the provided
  # data.frame
  terms <- attributes(terms(formula))
  # extract target from formula
  target <- all.vars(formula[[2]])
  
  # if validation is TRUE the training data will be splitted into
  # training and validation
  # the validation data will represent ~25% of all observations
  if (validation) {
    # get sample size based on the number of rows within x
    smpl_size <- floor(0.75 * nrow(x))
    # get a random sample of the index based on the sample size
    smpl_indx <- sample(seq_len(nrow(x)), size = smpl_size)
    # subsetting x for the validation data
    x_val <- x[-smpl_indx, ]
    
    # get target as vector
    y_val <- x_val[[target]]
    
    # get weights for validation data if column was specified
    if (!is.null(cat_weights)) {
      val_weights <- x_val[[cat_weights]]
    } else {
      val_weights <- NULL
    }
    
    x_val <- x_val %>%
      # only keep cols that are needed for training
      dplyr::select(dplyr::all_of(terms$term.labels)) %>%
      # turning chr values to fct
      dplyr::mutate_if(is.character, forcats::as_factor)
    
    # creating validation pool for catboost
    pool_val <- catboost::catboost.load_pool(
      data = x_val,
      label = y_val,
      weight = val_weights)
    
    # subsetting x concerning training
    x <- x[smpl_indx, ]
  } else {
    pool_val <- NULL
  }
  
  # get target as vector
  y_train <- x[[target]]
  
  # get weights for training data if column was specified
  if (!is.null(cat_weights)) {
    train_weights <- x[[cat_weights]]
  } else {
    train_weights <- NULL
  }
  
  x_train <- x %>%
    # only keep cols that are needed for training
    dplyr::select(dplyr::all_of(terms$term.labels)) %>%
    # turning chr values to fct
    dplyr::mutate_if(is.character, forcats::as_factor)
  
  # creating training pool for catboost
  pool_train <- catboost::catboost.load_pool(
    data = x_train,
    label = y_train,
    weight = train_weights)
  
  # training catboost model
  # if no params are provided, the default will be used
  fit_catboost <- catboost::catboost.train(
    learn_pool = pool_train,
    # specify validation pool only if argument is TRUE
    test_pool = pool_val,
    params = params)
  
  return(fit_catboost)
}