#' Returns entire player log
#'
#' @param obj 
#'
#' @return
#' @export
#'
#' @examples
get_log.brainvr <- function(obj){
  return(obj$data$player_log)
}

#' Returns log between designated times
#'
#' @param obj 
#' @param start 
#' @param end 
#'
#' @return
#' @export
#'
#' @examples
get_log_timewindow.brainvr <- function(obj, start, end){
  log <- obj$data$player_log[Time > start & Time < end, ]
  return(log)
}

#' Returns data.table with player log for a particular trial
#'
#' @param obj
#' @param 
#' @return player log for particulat trial
#' 
#' @export
get_trial_log.brainvr <- function(obj, trialId) {
  timewindow <- get_trial_times.brainvr(obj, trialId)
  return(get_log_timewindow.brainvr(obj, timewindow$start, timewindow$end))
}

#' Gets start and finish times of trial
#' 
#' @param obj
#' @param trialId
#' @return list with waitingToStart, start and finish 
#' 
#' @export
get_trial_times.brainvr <- function(obj, trialId){
  df_experiment <- obj$data$experiment_log$data
  #correction for c# indexing
  trialId <- trialId - 1
  ls <- list()
  df_trial <- df_experiment[df_experiment$Sender == "Trial" & df_experiment$Index == trialId, ]
  ls$WaitingToStart <-  df_trial[df_trial$Event == "WaitingToStart", "Time"][1]
  ls$start <- df_trial[df_trial$Event == "Running", "Time"][1]
  #selects only hte first element - its because fome of hte old logs had potential two finished tiems 
  #if the experiment or trial was force finished before closed (finished effectively twice)
  ls$end <- df_trial[df_trial$Event == "Finished", "Time"][1]
  #replaces missing values with NAs
  newValues <- sapply(ls, function(x) if(length(x)== 0) {x <- as.numeric(NA)} else {x <- x})
  ls <- as.list(newValues)
  return(ls)
}

#' Returns how long the trial took and removes potential pauses in the log
#'
#' @param obj BrainVr object with preprocessed player log
#' @param trialId 
#' @param pause_limit minimum time to be considered pause. Defaults to 5
#' @param path_limit maximum distance to be considered not moving. Defaults to 1
#' @param without_pauses Defaults to true
#'
#' @return 
#' @export
#'
#' @examples
get_trial_duration.brainvr <- function(obj, trialId, without_pauses = T, pause_limit = 5, path_limit = 1){
  times <- get_trial_times.brainvr(obj, trialId)
  dur <- times$end - times$start
  if(without_pauses){
    log <- get_trial_log.brainvr(obj, trialId)
    freq <- round(pause_limit/mean(diff(log$Time[1:100]))) #how many rows is the pause
    distance_in_limit <- navr::rolling_sum(log$distance, freq)
    if(is.null(distance_in_limit)) return(dur) #trial shorter than pause
    is_stationary <- distance_in_limit < path_limit
    pause_time <- sum(is_stationary * 1/freq)
    dur <- dur - pause_time
  }
  return(dur)
}

get_trial_event_indices <- function(test, event){
  indices <- unique(which(test$data$Sender == "Trial" & test$data$Event == event))
  return(indices + 1)
}

get_walked_distnace_timewindow <- function(dt_position, timeweindow){
  dt_position <- get_log_timewindow.brainvr(dt_position, timeweindow$start, timeweindow$finish)
  if (dt_position[, .N] < 2) {
    print("The player log doesn't cover given timewindows")
    walkedDistance <- as.numeric(NA)
  } else {
    start <- head(dt_position, 1)$cumulative_distance
    end <- tail(dt_position, 1)$cumulative_distance
    walkedDistance <- end - start
  }
  return(walkedDistance)
}

#' REturns if the trial has been force finished
#'
#' @param obj 
#' @param trialID 
#'
#' @return
#' @export
#'
#' @examples
was_trial_force_finished <- function(obj, trialId){
  return(nrow(filter(obj$data$experiment_log$data, 
                      Sender == "Trial" & 
                      Index == (trialId - 1) & 
                      Event == "ForceFinished")) > 1)
}