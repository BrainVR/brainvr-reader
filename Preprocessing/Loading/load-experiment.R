#' Loads files form a folder into UnityObject
#' @param folder path to the folder respective to the working directory
#' @param obj created UnityObject to fill in data. If none passed, new one gets created
#' @returns UnityObject object
#' @example 
#' 
#' 
load_experiment <- function(folder, objectFun = UnityObject, exp_timestamp = NULL){
  if (is.null(folder)) stop("no folder set")
  #open experiment_logs to see how many do we have
  experiment_info <- open_experiment_info(folder, log_timestamp = exp_timestamp, returnSingle = T)
  
  if(is.null(experiment_info)) stop("Experiment info not found")
  #if multiple logs or no logs, quit
  if(is.null(exp_timestamp)) exp_timestamp <- experiment_info$header$Timestamp
  ## TODO separate preprocess adn opening
  player_log <- open_player_log(folder, log_timestamp = exp_timestamp, override = FALSE)
  if(is.null(player_log)) stop("Player log not found")
  #preprocesses player log
  #checks if there is everything we need and if not, recomputes the stuff
  
  test_logs <- open_experiment_logs(folder)
  
  obj <- objectFun()
  obj$timestamp <- exp_timestamp
  obj$data$experiment_info <- experiment_info
  obj$data$player_log <- player_log
  ##TODO - redo this part
  obj$data$experiment_log <- test_logs[[1]]
  #obj$data$results_log <- results_log
  return (obj)
}