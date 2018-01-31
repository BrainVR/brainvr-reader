#' preprocesses player log and returns if it changed. Passes by referennce
#' @param player_log 

#' Preprocesses player log by reference and returs if it was changed for saving purposes
#' @param player_log loaded DATA.TABLE player log
#' @param type what type of log we have - depending on the brainvr-unity-framework type of player controller used? Possible types are: "rigidbody", "virtualizer"
#' @return boolean if changed. Default false
#' @export

## TODO - change so that the log is not passed by reference
preprocess_player_log <- function(player_log, type = "rigidbody"){
  if(!requireNamespace("stringr", quietly = T)){
    print("Cannot continue withouth stringr package. Please install it")
    return(F)
  }
  changed <- F
  ## Converting position
  if (!is_column_present(player_log, "Position.X")){
    playerLog <- vector3_to_columns(player_log, "Position")
    changed <- T
  }
  ## Adding distance from position
  if (!is_column_present(player_log, "cumulative_distance")){
    playerLog <- add_distance_moved(player_log)
    changed <- T
  }
  ## Adds rotation difference
  if (!is_column_present(player_log, "angle_diff_x")){
    player_log <- navr::add_angle_difference(player_log, player_log$x, "x")
    changed <- T
  }
  if(type == "rigidbody") changed <- rigidbody_preprocess(player_log, changed)
  if(type == "virtualizer") changed <- virtualizer_preprocess(player_log, changed)
  if (changed) print("Log modified") else print("Log ok")
  return(changed) 
}

rigidbody_preprocess <- function(player_log, changed){
  return(changed)
}

virtualizer_preprocess <- function(player_log, changed){
  changed <- F
  ## Adding angle differences
  if (!is_column_present(player_log, "angle_diff_y")){
    player_log <- navr::add_angle_difference(player_log, player_log$y, "y")
    changed <- T
  }
  ## Adding angle differences
  if (!is_column_present(player_log, "angle_diff_virtualizer")){
    player_log <- navr::add_angle_difference(player_log, player_log$virtualizer, "virtualizer")
    changed <- T
  }
  if (!is_column_present(player_log, "angle_diff_cotroller.x")){
    player_log <- navr::add_angle_difference(player_log, player_log$controller.x, "controller.x")
    changed <- T
  }
  if (!is_column_present(player_log, "angle_diff_controller.y")){
    player_log <- navr::add_angle_difference(player_log, player_log$controller.y, "controller.y")
    changed <- T
  }
  return(changed)
}

#' Saves preprocessed player ot hte given folder. Receives either specific name, or gets the name from already present player logs
#' If there are multiple player logs in the folder,takes the name from the first available
#' @param directory Where shoudl i save the file. Should end with a slash!
#' @param player_log table to be saved
#' @param orig_filename what is the name of the original file
#' @return 
#' 
#' @export
save_preprocessed_player <- function(directory, player_log, exp_timestamp = NULL, orig_filename = NULL){
  if(is.null(orig_filename)) {
    ptr <- create_log_search_pattern("player", exp_timestamp)
    logs <- list.files(directory, pattern = ptr, full.names = T)
    if(length(logs) != 1) stop("More player logs in the saving directory")
    filename <- logs[1]
    #writes preprocessed file
    preprocessed_filename <- gsub(".txt","_preprocessed.txt", filename)
  } else {
    preprocessed_filename <- paste0(directory, orig_filename)
    #writes preprocessed file
    preprocessed_filename <- gsub(".txt","_preprocessed.txt", filename)
  }
  print(paste0("Saving processed player log as", preprocessed_filename))
  write.table(player_log, preprocessed_filename, sep = ";", 
              dec = ".", quote = F, row.names = F)
}