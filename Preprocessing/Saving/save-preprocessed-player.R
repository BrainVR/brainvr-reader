#' Saves preprocessed player ot hte given folder. Receives either specific name, or gets the name from already present player logs
#' If there are multiple player logs in the folder,takes the name from the first available
#' @param directory Where shoudl i save the file. Should end with a slash!
#' @param player_log table to be saved
#' @param orig_filename what is the name of the original file
#' 
save_preprocessed_player = function(directory, player_log, orig_filename = NULL){
  if(is.null(orig_filename)) {
    ptr = paste("_player_", sep = "", collapse = "")
    logs = list.files(directory, pattern = ptr, full.names = T)
    if(length(logs) != 1) stop("More player logs in the saving directory")
    filename = logs[1]
    #writes preprocessed file
    preprocessed_filename = gsub(".txt","_preprocessed.txt", filename)
  } else {
    preprocessed_filename <- paste0(directory, orig_filename)
    #writes preprocessed file
    preprocessed_filename <- gsub(".txt","_preprocessed.txt", filename)
  }
  smart_print(c("Saving processed player log as", preprocessed_filename))
  write.table(player_log, preprocessed_filename, sep = ";", 
              dec = ".", quote = F, row.names = F)
}