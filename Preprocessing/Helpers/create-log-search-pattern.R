#' Creates a search pattern for particular logs
#'
#'@param log_name
#'@param log_timestamp what timestamps can the log havedefault:NULL
#'@return string with pattern
#'
create_log_search_pattern <- function(log_name, log_timestamp){
  ptr <- paste0("_", log_name, "_")
  if(!is.null(log_timestamp)){
    ptr <- paste0(ptr, "*", log_timestamp)
  }
  return(ptr)
}