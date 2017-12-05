#' Goes through the folder and loads every experiment info into separate object 
#' @param folder where to look
#' @param objectFun what function to create the object that will hold the info
#' @return list of objecs

load_experiments <- function(folder, objectFun){
  if (is.null(folder)) stop("no folder set")
  #open experiment_logs to see how many do we have
  experiment_infos <- open_experiment_info(folder)
  
  if(is.null(experiment_info)) stop("Experiment info not found")
  
  #if only a single log
  if (length(experiment_info) == 1){
    return(load_experiment(folder, objectFun))
  }
  
  # else
  for(info in experiment_infos){
      
  }
}