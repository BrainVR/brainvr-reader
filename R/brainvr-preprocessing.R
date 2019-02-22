#' Preprocesses player log and returns it
#' @param navr_object loaded NavrObject
#' @return preprocessed player log
#' @export
preprocess_player_log <- function(navr_object){
  ## Adding distance from position
  navr_object <- navr::add_distances(navr_object)
  navr_object <- navr::add_angle_differences(navr_object)
  navr_object <- navr::add_time_columns(navr_object)
  navr_object <- navr::add_speeds(navr_object)
  return(navr_object) 
}

#' Saves preprocessed player ot hte given folder. Receives either specific name, or gets the name from already present player logs
#' If there are multiple player logs in the folder,takes the name from the first available
#' @param directory Where shoudl i save the file. Should end with a slash!
#' @param player_log table to be saved
#' @param orig_filename what is the name of the original file
#' @return 
#' 
#' @export
save_preprocessed_player <- function(directory, exp_timestamp = NULL, log, orig_filename = NULL, precision=2){
  if(is.null(orig_filename)) {
    ptr <- create_log_search_pattern("player", exp_timestamp)
    logs <- list.files(directory, pattern = ptr, full.names = T)
    # Allow for overwrite
    if(length(logs) != 1) stop("More player logs in the saving directory. Have you deleted the preprocessed one?")
    filename <- logs[1]
    preprocessed_filename <- gsub(".txt","_preprocessed.txt", filename)
  } else {
    preprocessed_filename <- paste0(directory, orig_filename)
    preprocessed_filename <- gsub(".txt","_preprocessed.txt", filename)
  }
  print(paste0("Saving processed player log as", preprocessed_filename))
  write.table(format(log, digits=precision, nsmall=precision), preprocessed_filename, sep = ";", 
              dec = ".", quote = F, row.names = F)
}