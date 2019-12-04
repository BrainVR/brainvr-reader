#' Returns entire player log
#'
#' @param obj Brainvr object
#'
#' @return player log data.frame
#' @export
#'
#' @examples
get_log <- function(obj, ...){
  UseMethod("get_log")
}
#' @export
get_log.brainvr <- function(obj){
  return(obj$data$position$data)
}

#' Returns experiment log data
#'
#' @param obj Brainvr object
#'
#' @return Dataframe with all experiment events
#' @export
#'
#' @examples
get_experiment_log <- function(obj, ...){
  UseMethod("get_experiment_log")
}
#' @export
get_experiment_log.brainvr <- function(obj){
  return(obj$data$experiment_log$data)
}

#' Return results log data
#'
#' @param obj Brainvr object with loaded results
#' @param ... 
#'
#' @return results data.frame or NULL if empty
#' @export
#'
#' @examples
get_results_log <- function(obj, ...){
  UseMethod("get_results_log")
}
#' @export
get_results_log.brainvr <- function(obj){
  res <- try(obj$data$results_log$data, silent = T)
  if(!class(res) == "data.frame"){
    warning("There is no results log or it doesn't contain valid data frame")
    return(NULL)
  }
  return(res)
}

#' Returns experiment settings in a list
#'
#' @param obj Brainvr Object
#'
#' @return List with saved experiment settings
#' @export
#'
#' @examples
get_experiment_settings <- function(obj, ...){
  UseMethod("get_experiment_settings")
}
#' @export
get_experiment_settings.brainvr <- function(obj){
  return(obj$data$experiment_log$settings)
}

#' Returns log between designated times
#'
#' @param obj 
#' @param start 
#' @param end 
#'
#' @return navr object with preprocessed times
#' @export
#'
#' @examples
get_position_timewindow <- function(obj, start, end, ...){
  UseMethod("get_position_timewindow")
}
#' @export
get_position_timewindow.brainvr <- function(obj, start, end){
  navr_obj <- navr::filter_times(obj$data$position, c(start, end))
  return(navr_obj)
}

#' Returns navr_object with data for a particular trial
#'
#' @param obj BrainvrObject
#' @param 
#' @return navr object
#' 
#' @export
get_trial_position <- function(obj, iTrial, ...){
  UseMethod("get_trial_position")
}
#' @export
get_trial_position.brainvr <- function(obj, iTrial) {
  timewindow <- get_trial_times(obj, iTrial)
  navr_obj <- get_position_timewindow.brainvr(obj, timewindow$start, timewindow$end)
  return(navr_obj)
}

#' Returns data.table with player log for a particular trial
#'
#' @param obj BrainvrObject
#' @param 
#' @return player log for particulat trial
#' 
#' @export
get_trial_log <- function(obj, iTrial, ...){
  UseMethod("get_trial_log")
}
#' @export
get_trial_log.brainvr <- function(obj, iTrial) {
  navr_obj <- get_trial_position.brainvr(obj, iTrial)
  return(navr_obj$data)
}

#' Gets start and finish times of trial
#' 
#' @param obj BrainvrObject
#' @param iTrial trial index(starting with 1)
#' @return list with waitingToStart, start and end 
#' 
#' @export
get_trial_times <- function(obj, iTrial, ...){
  UseMethod("get_trial_times")
}
#' @export
get_trial_times.brainvr <- function(obj, iTrial){
  df_experiment <- get_experiment_log(obj)
  #correction for c# indexing
  iTrial <- iTrial - 1
  ls <- list()
  df_trial <- df_experiment[df_experiment$Sender == "Trial" & df_experiment$Index == iTrial, ]
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
#' @param iTrial trial index (starting with 1)
#' @param pause_limit minimum time to be considered pause. Defaults to 5
#' @param path_limit maximum distance to be considered not moving. Defaults to 1
#' @param without_pauses Defaults to true
#'
#' @return 
#' @export
#'
#' @examples
get_trial_duration <- function(obj, iTrial, without_pauses = TRUE, pause_limit = 5, path_limit = 1, ...){
  UseMethod("get_trial_duration")
}
#' @export
get_trial_duration.brainvr <- function(obj, iTrial, without_pauses = TRUE, pause_limit = 5, path_limit = 1){
  times <- get_trial_times.brainvr(obj, iTrial)
  dur <- times$end - times$start
  if(without_pauses & dur > pause_limit){# cannot be paused shorter times than the trial is long
    log <- get_trial_log.brainvr(obj, iTrial)
    freq <- round(pause_limit/mean(diff(log$timestamp[1:100]))) #how many rows is the pause
    distance_in_limit <- navr::rolling_sum(log$distance, freq)
    if(is.null(distance_in_limit)) return(dur) #trial shorter than pause
    is_stationary <- distance_in_limit < path_limit
    pause_time <- sum(is_stationary * 1/freq)
    dur <- dur - pause_time
  }
  return(dur)
}

#' Returns walked distance in a particular trial
#'
#' @param obj BrainvrObject
#' @param iTrial trial index (startin with 1)
#'
#' @return 
#' @export
#'
#' @examples
get_trial_distance <- function(obj, iTrial, ...){
  UseMethod("get_trial_distance")
}
#' @export
get_trial_distance.brainvr <- function(obj, iTrial){
  log <- get_trial_log.brainvr(obj, iTrial)
  return(diff(range(log$distance_total)))
}

#' Returns times of certain events happening in particular trials
#'
#' @param obj 
#' @param iTrial vector or trial indices starting with 1
#'
#' @return times of certain events
#' @export
#'
#' @examples
get_trial_event_times <- function(obj, iTrial, event_name = "", ...){
  UseMethod("get_trial_event_times")
}
#' @export
get_trial_event_times.brainvr <- function(obj, iTrial, event_name = ""){
  exp_log <- get_experiment_log(obj)
  trial_log <- get_trial_events(exp_log, iTrial)
  event_times <- get_times_events(trial_log, event_name)
  return(event_times)
}

#' Returns amount of moved distance in given timewindow
#' @param obj brainvr object
#' @param start movement start timestamp
#' @param end movement end timestamp
#'
#' @return moved distance or NA if the 
#' @export
#'
#' @examples
get_distance_timewindow <- function(obj, start, end){
  #TODO - fix
  pos <- get_position_timewindow.brainvr(obj, start, end)
  dt_position <- pos$data
  if (dt_position[, .N] < 2) {
    print("The player log doesn't cover given timewindows")
    moved_distance <- NA_real_
  } else {
    moved_distance <- diff(range(dt_position$distance_total))
  }
  return(moved_distance)
}

#' Returns which trials were finished
#'
#' @param obj BrainvrObject
#' @param zero_based if T, it keeps the indices as they are reported by the framework, beginning with 0
#' @param ... 
#'
#' @return indices of finished trials. One based
#' @export
#'
#' @examples
get_finished_trials_indices <- function(obj, zero_based = F, ...){
  UseMethod("get_finished_trials_indices")
}

#' Returns which trials were finished
#'
#' @param obj BrainvrObject
#' @param zero_based if T, it keeps the indices as they are reported by the framework, beginning with 0
#' @param exclude_force_finished if TRUE, removes trial indeices for trials that were force finished
#' 
#' @return indices of finished trials. One based
#' @export
#'
#' @examples
get_finished_trials_indices <- function(obj, zero_based = FALSE, exclude_force_finished = FALSE){
  df_experiment <- get_experiment_log(obj)
  indices <- df_experiment[df_experiment$Sender == "Trial" & df_experiment$Event == "Finished", "Index"]
  if(exclude_force_finished){
    was_force_finished <- sapply(indices, function(x){was_trial_force_finished(obj, x, zero_based = TRUE)})
    indices <- indices[!was_force_finished]
  }
  if(!zero_based) indices <- indices + 1L
  return(indices)
}

#' Returns if the trial has been force finished
#'
#' @param obj \code{\link{BrainvrObject}}
#' @param iTrial trial index starting with 1 or 0 if \code{zero_based=TRUE}
#' @param zero_based if TRUE, it keeps the indices as they are reported 
#' by the framework, beginning with 0. Otherwise trials start with 1
#'
#' @return logical
#' @export
#'
#' @examples
was_trial_force_finished <- function(obj, iTrial, zero_based = FALSE){
  if(!zero_based) iTrial <- iTrial - 1
  df_experiment <-get_experiment_log(obj)
  selected <- df_experiment[df_experiment$Sender == "Trial" & 
                          df_experiment$Index == iTrial &
                          df_experiment$Event == "ForceFinished", ]
  return(nrow(selected) >= 1)
}