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