

build_formula <- function(target = "sales", non_lag_features, lag = 1) {
  
  # current definition of basic features
  basic_feats <- c(
    "cluster", "store", "type", "month_cat", "seasonal_cat", 
    "holiday", "weekend",  "wday_cat"
  )
  
  # lag features
  lag_feats_base <- c(
    "sales_", "oilprice_"
  )
  
  ma_lag_base <- c(
    "sales_ma_6_"
  )
  
  # adjust lag feat
  lag_feats <- paste0(lag_feats_base, "lag_", lag)
  
  ma_lags <- paste0(ma_lag_base, "lag_", lag)
  
  if (lag < 7) {
    tmp_lag_feats <- paste0(lag_feats_base, "lag_", 7)
    lag_feats <- c(lag_feats, tmp_lag_feats)
  } 
  
  if (lag < 14) {
    tmp_lag_feats <- paste0(lag_feats_base, "lag_", 14)
    lag_feats <- c(lag_feats, tmp_lag_feats)
  }
  
  # binding all features together
  all_feats <- c(non_lag_features, lag_feats)
  x_form <- paste(all_feats, collapse = " + ")
  
  # create formula
  form <- formula(
    paste0(target, " ~ ", x_form)
  )
  
  return(form)

}