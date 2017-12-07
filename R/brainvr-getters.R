get_goal_position <- function(test, i_goal, onlyXY = F){
  goalPosition <- test$positionSettings$GoalPositions[i_goal, ]
  if (onlyXY){
    return(c(goalPosition$Position.x, goalPosition$Position.z))
  } else return(goalPosition)
}

get_trial_event_indices <- function(test, event){
  indices <- unique(filter(test$data, Sender == "Trial" & Event == event) %>% select(Index))[[1]]
  return(indices + 1)
}

#' Returns pointing direction during given trial. If there are more than two pointings, selects the first one
#' If target poistion is passed, also returnes what should have been the correct pointing angle
#' @param dt_player player log
#' @param expeiremnt_log expeirment log
#' @param target_pos vector 2 of target position
#' 
get_trial_pointing <- function(dt_player, experiment_log, iTrial, target_pos = NULL){
  ls <- list()
  timewindow <- get_trial_timewindow(experiment_log, iTrial)
  if (is.null(timewindow)) return(NULL)
  quest_log <- dt_player[Time > timewindow$start & Time < timewindow$finish, ]
  point_situation <- quest_log[Input == "Point", ]
  ls$target <- NA
  if(nrow(point_situation) < 1){
    smart_print(c("Warning", "get_trial_pointing", "no point event found"))
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

get_trial_timewindow <- function(test, trialID){
  #correction for c# indexing
  trialID <- trialID - 1
  ls <- list()
  ls$WaitingToStart <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "WaitingToStart") %>% 
    select(Time) %>% first
  ls$start <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "Running") %>% select(Time) %>% first
  #old versions had typo in the running evvent with only a single n
  if (length(ls$start) < 1){ 
    ls$start <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "Runing") %>% select(Time) %>% first
  }
  #selects only hte first element - its because fome of hte old logs had potential two finished tiems 
  #if the experiment or trial was force finished before closed (finished effectively twice)
  ls$finish <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "Finished") %>% select(Time) %>% first
  
  #replaces missing values with NAs
  newValues <- sapply(ls, function(x) if(length(x)== 0) {x <- as.numeric(NA)} else {x <- x})
  ls <- as.list(newValues)
  return(ls)
}

get_walked_distnace_timewindow <- function(dt_position, timeWindow){
  dt_position <- get_player_log_timewindow(timeWindow)
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

get_player_log_timewindow <- function(dt_player, timewindow){
  #checking for entirety
  log <- dt_player[Time > timewindow$start & Time < timewindow$finish, ]
  #checking log
  return(log)
}

was_force_finished <- function(test, trialID){
  return(nrow(filter(test$data, Sender == "Trial" & 
                       Index == (trialID - 1) & 
                       Event == "ForceFinished")) > 1)
}