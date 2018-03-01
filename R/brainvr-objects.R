#' Title 
#'
#' @return 
#' @export 
#'
#' @examples
BrainvrObject <- function(){
  obj <- list()
  obj$participant_id <- ""
  obj$experiment_name <- ""
  obj$session <- NA
  obj$timestamp <- NA
  obj$map_limits <- NULL
  obj$data <- list()
  obj$data$experiment_info <- NA
  obj$data$player_log <- NA
  obj$data$experiment_log <- NA
  obj$data$results_log <- NA
  class(obj) <- append(class(obj), "brainvr")
  return(obj)
}