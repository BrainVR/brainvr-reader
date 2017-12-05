UnityObject <- function(){
  obj <- list()
  obj$participant_id <- ""
  obj$experiment_name <- ""
  obj$session <- NA
  obj$data <- list()
  obj$data$experiment_info <- NA
  obj$data$player_log <- NA
  obj$data$experiment_log <- NA
  obj$data$results_log <- NA
  return(obj)
}