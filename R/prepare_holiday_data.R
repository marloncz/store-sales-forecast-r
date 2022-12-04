#' Prepare Holiday Data
#'
#' @param .data `data.frame` containing holiday information.
#' @param simple Whether a simple preparation should be implemented.
#'
#' @return `list` containing three `data.frames` with the prepared holiday
#' information.
#' @export
prepare_holiday_data <- function(.data, simple = TRUE) {
  
  # transferred holidays will be celebrated on another date
  # all entries that are transferred will be not considered as a holiday
  # event. 
  .data <- .data %>% 
    dplyr::filter(!transferred)
  
  # splitting the data into different regional types
  national <- .data %>% 
    dplyr::filter(locale == "National")
  
  local <- .data %>% 
    dplyr::filter(locale == "Local")
  
  regional <- .data %>% 
    dplyr::filter(locale == "Regional")
  
  # for the simple preparation there will be no differentiation between 
  # holiday types like Event, Holiday, Bridge, Addition and so on
  if (simple) {
    national <- national %>% 
      dplyr::mutate(type = "Holiday") %>% 
      dplyr::select(date, type) %>% 
      dplyr::distinct()
    
    local <- local %>% 
      dplyr::mutate(type = "Holiday") %>% 
      dplyr::select(date, type, locale_name) %>% 
      dplyr::distinct()
    
    regional <- regional %>% 
      dplyr::mutate(type = "Holiday") %>% 
      dplyr::select(date, type, locale_name) %>% 
      dplyr::distinct()
  } else {
    # a more detailed preparation of the given holiday information is 
    # currently not implemented. This potential step can be implemented later
    # if an improvement of the model is needed.
    stop("Currently not implemented")
  }
  
  # define list that contains all data.frames
  out <- list(
    national = national,
    local = local,
    regional = regional
  )
  
  return(out)
}