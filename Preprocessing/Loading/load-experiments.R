#' Goes through the folder and loads every experiment info into separate object 
#' @param folder where to look
#' @param objectFun what function to create the object that will hold the info
#' @return list of objecs

load_experiments <- function(folder, objectFun){
  if (is.null(folder)) stop("no folder set")
  #open experiment_logs to see how many do we have
  experiment_infos <- open_experiment_info(folder)
  
  if(is.null(experiment_infos)) stop("Experiment info not found")
  
  # else
  ls <- list()
  i <- 1 
  for(info in experiment_infos){
    ls[[i]] <- load_experiment(folder, objectFun, exp_timestamp = info$header$Timestamp)
    i <- i + 1
  }
  return(ls)
}