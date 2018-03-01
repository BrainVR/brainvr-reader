#' Returns how long the trial took and removes potential pauses in the log
#'
#' @param obj 
#' @param without_pauses Defaults to true
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
get_trial_duration <- function(obj, trialId, without_pauses = T, ...){
  UseMethod("get_trial_duration")
}