#' Searches the directory for experiment log files. Returs single one if multiple are found
#' @param directory path to the directory where to search
#' @return list with a single loaded info log
open_experiment_info = function(directory = ""){
  ls = list()
  logs = list.files(directory, pattern = "_ExperimentInfo_", full.names = T)
  if(length(logs) < 1){
    print("Could not find the file for experiment log")
    return(NULL)
  }
  for(i in 1:length(logs)){
    ls[[i]] = load_experiment_info(logs[i])
    ls[[i]]$filename = logs[i]
  }
  if (length(ls) == 1){
    print("Found several expeirment logs. Returning the first one.")
    ls = ls[[1]]
  }
  return(ls)
}