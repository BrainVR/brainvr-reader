#' preprocesses player log and returns if it changed. Passes by referennce
#' @param player_log 

#' Preprocesses player log by reference and returs if it was changed for saving purposes
#' @param player_log loaded DATA.TABLE player log
#' @param type what type of log we have - depending on the brainvr-unity-framework type of player controller used? Posiible types are: "rigidbody", "virtualizer"
#' @return boolean if changed. Default false


## TODO - change so that the log is not passed by reference
preprocess_player_log = function(player_log, type = "rigidbody"){
  changed <- F
  ## Converting position
  if (!is_column_present(player_log, "Position.x")){
    playerLog <- vector3_to_columns(player_log, "Position")
    changed <- T
  }
  ## Adding distance from position
  if (!is_column_present(player_log, "cumulative_distance")){
    playerLog <- add_distance_moved (player_log)
    changed <- T
  }
  ## Adds rotation difference
  if (!is_column_present(player_log, "angle_diff_x")){
    player_log <- add_angle_difference(player_log, "x")
    changed <- T
  }
  if(type == "rigidbody"){
    changed <- rigidbody_preprocess(player_log, changed)
  }
  if(type == "virtualizer"){
    changed <- virtualizer_preprocess(player_log, changed)
  }
  if (changed) print("Log modified") else print("Log ok")
  return(changed) 
}

rigidbody_preprocess <- function(player_log, changed){
  return(changed)
}

virtualizer_preprocess <- function(player_log, changed){
  changed = F
  ## Adding angle differences
  if (!is_column_present(player_log, "angle_diff_y")){
    player_log <- add_angle_difference(player_log, "y")
    changed <- T
  }
  ## Adding angle differences
  if (!is_column_present(player_log, "angle_diff_virtualizer")){
    player_log <- add_angle_difference(player_log, "virtualizer")
    changed <- T
  }
  if (!is_column_present(player_log, "angle_diff_cotroller.x")){
    player_log <- add_angle_difference(player_log, "controller.x")
    changed <- T
  }
  if (!is_column_present(player_log, "angle_diff_controller.y")){
    player_log <- add_angle_difference(player_log, "controller.y")
    changed <- T
  }
  return(changed)
  
}