get_trial_event_indices <- function(test, event){
  indices <- unique(which(test$data$Sender == "Trial" & test$data$Event == event))
  return(indices + 1)
}

#' Returns pointing direction during given trial. If there are more than two pointings, selects the first one
#' If target poistion is passed, also returnes what should have been the correct pointing angle
#' @param obj UnityObject
#' @param target_pos vector 2 of target position
#' 
get_trial_pointing <- function(obj, trialId, target_pos = NULL){
  ls <- list()
  quest_log <- get_player_log_trial(obj, trialId)
  point_situation <- quest_log[Input == "Point", ]
  ls$target <- NA
  if(nrow(point_situation) < 1){
    print(paste0("Warning", "get_trial_pointing", "no point event found"))
    ls$time <- NA
    ls$chosen <- NA
  } else { 
    point_situation = point_situation[1]
    player_pos <- c(point_situation$Position.x, point_situation$Position.z)
    ls$time <- point_situation$Time
    ls$chosen <- point_situation$Rotation.X
    if (!is.null(target_pos)){
      ls$target <- angle_from_positions(player_pos, target_pos)
    }
  }
  return(ls)
}

#' Gets start and finish times of trial
#' 
#' @param obj
#' @param trialId
#' @return list with waitingToStart, start and finish 
#' 
#' @export
get_trial_timewindow <- function(obj, trialId){
  df_experiment <- obj$data$experiment_log$data
  #correction for c# indexing
  trialId <- trialId - 1
  
  ls <- list()
  df_trial <- df_experiment[df_experiment$Sender == "Trial" & df_experiment$Index == trialId, ]
  ls$WaitingToStart <-  df_trial[df_trial$Event == "WaitingToStart", "Time"][1]
  ls$start <- df_trial[df_trial$Event == "Running", "Time"][1]
  #selects only hte first element - its because fome of hte old logs had potential two finished tiems 
  #if the experiment or trial was force finished before closed (finished effectively twice)
  ls$finish <- df_trial[df_trial$Event == "Finished", "Time"][1]
  #replaces missing values with NAs
  newValues <- sapply(ls, function(x) if(length(x)== 0) {x <- as.numeric(NA)} else {x <- x})
  ls <- as.list(newValues)
  return(ls)
}

get_walked_distnace_timewindow <- function(dt_position, timeWindow){
  dt_position <- get_player_log_timewindow(dt_position, timeWindow)
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

#' Returns data.table with player log for particular trial
#'
#' @param obj
#' @param 
#' @return player log for particulat trial
#' 
#' @export

get_player_log_trial <- function(obj, trialId) {
  timewindow <- get_trial_timewindow(obj, trialId)
  return(get_player_log_timewindow(obj$data$player_log, timewindow))
}

get_player_log_timewindow <- function(dt_player, timewindow){
  log <- dt_player[Time > timewindow$start & Time < timewindow$finish, ]
  return(log)
}

was_force_finished <- function(test, trialID){
  return(nrow(filter(test$data, Sender == "Trial" & 
                       Index == (trialID - 1) & 
                       Event == "ForceFinished")) > 1)
}