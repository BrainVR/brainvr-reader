#' preprocesses player log and returns if it changed. Passes by referennce
#' @param player_log 

#' Preprocesses player log by reference and returs if it was changed for saving purposes
#' @param player_log loaded DATA.TABLE player log
#' @return preprocessed player log
#' @export

## TODO - change so that the log is not passed by reference
preprocess_player_log <- function(player_log){
  if(!requireNamespace("stringr", quietly = T)){
    print("Cannot continue withouth stringr package. Please install it")
    return(F)
  }
  ## Converting position
  player_log <- vector3_to_columns(player_log, "Position")
  ## Adding distance from position
  player_log <- add_distance_moved(player_log)
  player_log <- add_angle_differences(player_log)
  return(player_log) 
}

#' Returns if the player log has been preprocessed 
#'
#' @param player_log 
#'
#' @return boolean if the Player log looks to be preprocessed
#' @export
#'
#' @examples
is_player_preprocessed <- function(player_log){
  return(is_column_present(player_log, "Position.x"))
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
  write.table(format(player_log, digits = 4), preprocessed_filename, sep = ";", 
              dec = ".", quote = F, row.names = F)
}