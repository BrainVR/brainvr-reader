#' Goes through the folder and loads every experiment info into separate object 
#' @param folder where to look
#' @param objectFun what function to create the object that will hold the info
#' @return list of objecs
#' @export 

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

#' Loads files form a folder into UnityObject
#' @param folder path to the folder respective to the working directory
#' @param obj created UnityObject to fill in data. If none passed, new one gets created
#' @returns UnityObject object
#' @example 
#' @export
load_experiment <- function(folder, objectFun = UnityObject, exp_timestamp = NULL){
  if (is.null(folder)) stop("no folder set")
  #open experiment_logs to see how many do we have
  experiment_info <- open_experiment_info(folder, log_timestamp = exp_timestamp, returnSingle = T)
  
  if(is.null(experiment_info)) stop("Experiment info not found")
  #if multiple logs or no logs, quit
  if(is.null(exp_timestamp)) exp_timestamp <- experiment_info$header$Timestamp
  ## TODO separate preprocess adn opening
  player_log <- open_player_log(folder, log_timestamp = exp_timestamp, override = FALSE)
  if(is.null(player_log)) stop("Player log not found")
  #preprocesses player log
  #checks if there is everything we need and if not, recomputes the stuff
  
  test_logs <- open_experiment_logs(folder)
  
  obj <- objectFun()
  obj$participant_id <- experiment_info$header$Participant
  obj$experiment_name <- obj$data$experiment_log$name
  obj$timestamp <- exp_timestamp
  obj$data$experiment_info <- experiment_info
  obj$data$player_log <- player_log
  ##TODO - redo this part
  obj$data$experiment_log <- test_logs[[1]]
  #obj$data$results_log <- results_log
  return (obj)
}

#' Loads particular info file into a list
#' @param filepath path to the file
#' @return list object 
load_experiment_info <- function(filepath){
  ls <- list()
  #reads into a text file at first
  text <- readLines(filepath, warn = F)
  ls$header <- get_json_between(text, "SESSION HEADER")
  ls$Experiment <- get_json_between(text, "EXPERIMENT INFO")
  return(ls)     
}

#' Loads expeirment log into a predefined list
#' 
#' @param filepath path tot he expeirment log
#' @return list with loaded settings files and data
#' @export

load_experiment_log <- function(filepath){
  ls <- list()
  #reads into a text file at first
  
  text <- readLines(filepath, warn = F)
  #needs to be before resaving text
  bottomHeaderIndex <- get_indicies_between(text, "TEST HEADER")$end
  
  text <- get_text_between(text, "TEST HEADER")
  ls$name <- experiment_name_from_filename(filepath)
  ls$settings <- get_json_between(text, "EXPERIMENT SETTINGS")
  ls$positions <- get_json_between(text, "POSITIONS")
  
  #ls$positionSettings = position_to_vector(ls$positionSettings)
  
  ls$data <- read.table(filepath, header = T, sep = ";", 
                        stringsAsFactors = F, skip = bottomHeaderIndex)
  #deleting the last column - always empty
  ls$data[ncol(ls$data)] <- NULL
  return(ls)
}

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

#' Iterates over all _test_ files in a folder asnd saves them one by one to a return list
#' @param directory directory where the file is located
#' @param exp_timestamp time of the 
#' @return 
open_experiment_logs <- function(directory, exp_timestamp = NULL){
  ls <- list()
  ptr <- create_log_search_pattern("test", exp_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = T)
  if(length(logs) < 1){
    print(paste0("Could not find any test logs in ", directory))
    next
  }
  for(i in 1: length(logs)){
    log <- logs[i]
    ls[[i]] <- load_experiment_log(log)
  }
  return(ls)
}

#' Searches a directory for a player log. Returns player log data.table
#'
#' @param directory where the log should be located
#' @param log_timestamp provides timestamp of a log to load
#' @param override if true, deletes processed player log and loads the unprocessed. if FALSE, load preprocessed log if present
#' @return data.table with the loaded player log or NULL.

open_player_log <- function(directory, log_timestamp = NULL, override = F){
  ptr <- create_log_search_pattern("player", log_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = T)
  if(length(logs) < 1){
    print(paste0("Could not find the file for player log in ", directory))
    return(NULL)
  }
  log_columns_types <- c(Time = "numeric", Position = "numeric", Rotation.X = "numeric", 
                         Rotation.Y = "numeric", FPS = "numeric", Input = "character")
  preprocessed_log_column_types <- c(log_columns_types, Position.x = "numeric", Position.y = "numeric", Position.z = "numeric", 
                                     distance = "numeric", cumulative_distance = "numeric", angle_diff_x = "numeric")
  if (length(logs) > 1){
    #check if there is a preprocessed player file
    preprocessed_index <- grep("*_preprocessed", logs)
    if(length(preprocessed_index) > 0){
      if(override){
        print(paste0("Removing preprocessed log", ptr))
        file.remove(logs[preprocessed_index])
      } else {
        print(paste0("Loading preprocessed player log", ptr))
        log <- logs[preprocessed_index]
        return(fread(log, header = T, sep = ";", dec = ".", stringsAsFactors = F, 
                     colClasses = preprocessed_log_column_types))
      }
    } else{
      print("There is more player logs with appropriate timestamp in the same folder. Have you named and stored everything appropriately?")
      return(NULL)
    }
  } else {
    if(length(logs) > 1){
      print(paste0("Multiple player logs in ", directory))
      return(NULL)
    } 
    log <- logs[1]
  }
  print(paste0("Loading unprocessed player log", ptr))
  #reads into a text file at first
  text <- readLines(log, warn = F)
  
  bottomHeaderIndex <- get_indicies_between(text, "SESSION HEADER")$end
  #reads the data without the header file
  pos_tab <- fread(log, header = T, sep = ";", dec=".", skip = bottomHeaderIndex, 
                   stringsAsFactors = F, colClasses = log_columns_types)
  #deletes the last column - it's there for the easier logging from unity 
  # - its here because of how preprocessing works
  pos_tab[, ncol(pos_tab) := NULL]
  
  return(pos_tab)
}