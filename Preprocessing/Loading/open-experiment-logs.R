#' Iterates over all _test_ files in a folder asnd saves them one by one to a return list
#'

open_experiment_logs = function(directory, exp_timestamp = NULL){
  ls = list()
  ptr <- "_test_"
  if(!is.null(timestamp)) ptr <- paste0(exp_timestamp)
  logs = list.files(directory, pattern = , full.names = T)
  if(length(logs) < 1){
    smart_print(c("Could not find any test logs in ", directory))
    next
  }
  for(i in 1: length(logs)){
    log = logs[i]
    ls[[i]] = load_experiment_log(log)
  }
  return(ls)
}