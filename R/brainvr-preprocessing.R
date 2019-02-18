#' Preprocesses player log and returns it
#' @param navr_object loaded NavrObject
#' @return preprocessed player log
#' @export
preprocess_player_log <- function(navr_object){
  ## Adding distance from position
  navr_object <- navr::add_distances(navr_object)
  ## TODO - make it work again
  #player_log <- add_angle_differences(player_log)
  return(navr_object) 
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
  return(is_column_present(player_log, "position_x"))
}

#' Saves preprocessed player ot hte given folder. Receives either specific name, or gets the name from already present player logs
#' If there are multiple player logs in the folder,takes the name from the first available
#' @param directory Where shoudl i save the file. Should end with a slash!
#' @param player_log table to be saved
#' @param orig_filename what is the name of the original file
#' @return 
#' 
#' @export
save_preprocessed_player <- function(directory, exp_timestamp = NULL, log, orig_filename = NULL){
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
  write.table(format(log, digits = 4), preprocessed_filename, sep = ";", 
              dec = ".", quote = F, row.names = F)
}