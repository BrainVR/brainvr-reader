#' Searches the directory for experiment log files. Returs single one if multiple are found
#' @param directory path to the directory where to search
#' @return list with a single loaded info log
open_experiment_info <- function(directory, log_timestamp = NULL, returnSingle = FALSE){
  ls <- list()
  ptr <- create_log_search_pattern("ExperimentInfo", log_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = T)
  if(length(logs) < 1){
    print("Could not find the file for experiment log")
    return(NULL)
  }
  for(i in 1:length(logs)){
    ls[[i]] <- load_experiment_info(logs[i])
    ls[[i]]$filename <- logs[i]
  }
  if (length(ls) == 1 || returnSingle){
    print("Returning only one experiment log.")
    ls <- ls[[1]]
  }
  return(ls)
}