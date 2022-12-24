

build_formula <- function(target = "sales", non_lag_features, lag = 1) {
  
  # current definition of basic features
  basic_feats <- c(
    "cluster", "store", "type", "month_cat", "seasonal_cat", 
    "holiday", "weekend",  "wday_cat"
  )
  
  # lag features
  lag_feats <- c(
    "sales_ma_6_", "sales_", "oilprice_"
  )
  
  # adjust lag feat
  lag_feats <- paste0(lag_feats, "lag_", lag)
  
  # binding all features together
  all_feats <- c(non_lag_features, lag_feats)
  x_form <- paste(all_feats, collapse = " + ")
  
  # create formula
  form <- formula(
    paste0(target, " ~ ", x_form)
  )
  
  return(form)

}