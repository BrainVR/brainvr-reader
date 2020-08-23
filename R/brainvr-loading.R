#' Goes through the folder and loads every experiment info into separate object 
#'
#' @param override if TRUE, deletes and recomputes preprocessed player. defaults to FALSE
#' @param folder where to look for brainvr files
#'
#' @return list of objecs
#' @export 
load_experiments <- function(folder, override = FALSE){
  if (is.null(folder)) stop("no folder set")
  #open experiment_logs to see how many do we have
  experiment_infos <- open_experiment_infos(folder)
  if(is.null(experiment_infos)) stop("Experiment info not found")
  ls <- list()
  for(i in 1:length(experiment_infos)){
    info <- experiment_infos[[i]]
    ls[[i]] <- load_experiment(folder, exp_timestamp = info$header$Timestamp, override = override)
  }
  return(ls)
}

#' Loads files form a folder into BrainvrObject
#' @param folder path to the folder respective to the working directory
#' @returns BrainvrObject object
#' @example 
#' @export
load_experiment <- function(folder, exp_timestamp = NULL, override = FALSE){
  if (is.null(folder)) stop("no folder set")
  #TODO - this should return only a single one per timestamp
  experiment_info <- open_experiment_infos(folder, exp_timestamp = exp_timestamp, flatten = T)
  if(is.null(experiment_info)) stop("Experiment info not found")
  #if multiple logs or no logs, quit
  if(is.null(exp_timestamp)) exp_timestamp <- experiment_info$header$Timestamp
  ## TODO separate preprocess adn opening
  navr_object <- open_player_log(folder, exp_timestamp = exp_timestamp, override = override)
  if(is.null(navr_object)) stop("Player log not found")
  # preprocesses player log
  # checks if there is everything we need and if not, recomputes the stuff
  test_log <- open_experiment_logs(folder, exp_timestamp, flatten = T)
  result_log <- open_result_log(folder, exp_timestamp)
  obj <- BrainvrObject()
  obj$participant_id <- experiment_info$header$Participant
  obj$timestamp <- exp_timestamp
  obj$data$experiment_info <- experiment_info
  obj$data$position <- navr_object
  #TODO - this might be an issue
  obj$data$experiment_log <- test_log
  obj$data$results_log <- result_log
  obj$experiment_name <- obj$data$experiment_log$name
  
  return (obj)
}

#' Searches the directory for experiment log files. Returs single one if multiple are found
#'
#' @param exp_timestamp 
#' @param directory path to the directory where to search
#'
#' @return list with a single loaded info log
#' @export
open_experiment_infos <- function(directory, exp_timestamp = NULL, flatten = F){
  # We CANNOT flatten the experiment info because then it looks like there is more of them and it loads the experiment multiple times
  ls <- open_brainvr_logs(directory, "ExperimentInfo", func = load_experiment_info, exp_timestamp, flatten)
  return(ls)
}

#' Loads particular info file into a list
#'
#' @param filepath path to the file
#'
#' @return list object 
#' @export
load_experiment_info <- function(filepath){
  ls <- list()
  #reads into a text file at first
  text <- readLines(filepath, warn = FALSE, encoding="UTF-8")
  ls$header <- get_json_between(text, "SESSION HEADER")
  ls$Experiment <- get_json_between(text, "EXPERIMENT INFO")
  return(ls)     
}

#' Iterates over all _test_ files in a folder asnd saves them one by one to a return list
#'
#' @param directory directory where the file is located
#' @param flatten in case of only a single list is returned, unnests the list. Beware, unnested list causes issues with opening experiments
#' @param exp_timestamp time of the 
#'
#' @return 
#' @export
open_experiment_logs <- function(directory, exp_timestamp = NULL, flatten = F){
  ls <- open_brainvr_logs(directory, "test", load_experiment_log, exp_timestamp, flatten)
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
  text <- readLines(filepath, warn = FALSE, encoding="UTF-8")
  #needs to be before resaving text
  bottomHeaderIndex <- get_indicies_between(text, "TEST HEADER")$end

  text <- get_text_between(text, "TEST HEADER")
  ls$name <- experiment_name_from_filename(filepath)
  ls$settings <- get_json_between(text, "EXPERIMENT SETTINGS")
  ls$positions <- get_json_between(text, "POSITIONS")
  ls$positions <- position_to_vector(ls$positions)
  ls$data <- read.table(filepath, header = T, sep = ";",
                        stringsAsFactors = F, skip = bottomHeaderIndex,
                        encoding="UTF-8")
  #deleting the last column - always empty
  ls$data[, ncol(ls$data)] <- NULL
  return(ls)
}

#' Searches for results logs and returns loaded list. 
#' 
#' @description REsults log have a _results_ in their filename. In case your's doesn't, use load_result_log instead
#'
#' @param directory where to search
#' @param exp_timestamp timestamp of a particular results log
#'
#' @return list with loaded result log
#' @export
#'
#' @examples
open_result_log <- function(directory, exp_timestamp = NULL){
  logs <- find_brainvr_logs(directory, "results", exp_timestamp, warning_missing = F)
  if(is.null(logs)) return(NULL)
  ls <- load_result_log(logs[1])
  return(ls)
}

#' Loads results log at a specific path
#'
#' @param filepath 
#'
#' @return
#' @export
#'
#' @examples
load_result_log <- function(filepath){
  result <- load_headers(filepath)
  bottom_index <- get_bottom_header_index(filepath)
  ## See if there is a data.frame
  df_data <- try(read.table(filepath, skip = bottom_index, sep=";", stringsAsFactors = FALSE, 
                            encoding="UTF-8", header=TRUE), silent = TRUE)
  if(class(df_data) == "data.frame") result$data <- df_data
  return(result)
}

#' Generic loading of all the results, experiment and other logs
#'
#' @param directory 
#' @param log_name 
#' @param exp_timestamp 
#' @param flatten in case only a single file is found, should it be unnested? defaults to false
#'
#' @return
#'
#' @examples
open_brainvr_logs <- function(directory, log_name, func, exp_timestamp = NULL, flatten = F){
  logs <- find_brainvr_logs(directory, log_name, exp_timestamp)
  if(is.null(logs)) return(NULL)
  ls <- list()
  for(i in 1:length(logs)){
    ls[[i]] <- func(logs[i])
  }
  if(flatten && (length(ls) == 1)) ls <- ls[[1]]
  return(ls)
}

find_brainvr_logs <- function(directory, log_name, exp_timestamp = NULL, warning_missing = TRUE){
  ptr <- create_log_search_pattern(log_name, exp_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = T)
  if(length(logs) < 1){
    if(warning_missing) warning(paste0("Could not find any ", log_name, " logs in ", directory, " for timestamp ", exp_timestamp))
    return(NULL)
  }
  if(length(logs) > 1 & !is.null(exp_timestamp)){
    warning(paste0("There are multiple ", log_name, " in the ", directory, " with timestamp ", exp_timestamp))
    return(NULL)
  }
  else return(logs)
}

#' Searches a directory for a player log. Returns player log data.table
#'
#' @param directory where the log should be located
#' @param exp_timestamp provides timestamp of a log to load
#' @param override if true, deletes processed player log and loads the unprocessed. if FALSE, load preprocessed log if present
#' @return data.table with the loaded player log or NULL.
#' @export
#' @import data.table
open_player_log <- function(directory, exp_timestamp = NULL, override = F, save = T){
  ls_log_path <- find_player_path(directory, exp_timestamp)
  if(nchar(ls_log_path$path) == 0) return(NULL)
  if(nchar(ls_log_path$path_preprocessed) > 0){
    if(override){
      print(paste0("Removing preprocessed log", ls_log_path$path_preprocessed))
      file.remove(ls_log_path$path_preprocessed)
    } else {
      print(paste0("Loading preprocessed player log", ls_log_path$path_preprocessed))
      #TODO - remove data.table
      navr_object <- navr::NavrObject()
      navr_object$data <- fread(ls_log_path$path_preprocessed, header = T, 
                                sep = ";", dec = ".", stringsAsFactors = F,
                                encoding = "UTF-8")
      return(navr_object)
    }
  }
  print(paste0("Loading unprocessed player log", ls_log_path$path))
  text <- readLines(ls_log_path$path, warn = F, encoding="UTF-8") #TODO - chagne so it doesn't read text so friggin much :(
  bottomHeaderIndex <- get_indicies_between(text, "SESSION HEADER")$end #get beginning of the log
  #reads the data without the header file
  #TODO - remove data.table
  df_position <- fread(ls_log_path$path, header = T, sep = ";", dec=".", 
                       skip = bottomHeaderIndex, stringsAsFactors = F)
  #deletes the last column - it's there for the easier logging from unity 
  # - its here because of how preprocessing works
  df_position[, ncol(df_position) := NULL]
  df_position <- prepare_navr_log(df_position)
  navr_object <- navr::load_position_data(navr::NavrObject(), df_position)
  navr_object <- preprocess_player_log(navr_object)
  if(save) save_preprocessed_player(directory, exp_timestamp, navr_object$data)
  return(navr_object)
}

find_player_path <- function(directory, exp_timestamp = NULL){
  ls <- list(path="", path_preprocessed="")
  ptr <- create_log_search_pattern("player", exp_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = T)
  if(length(logs) == 0)   print(paste0("Could not find the file for player log in ", directory))
  if(length(logs) > 2) print(paste0("Multiple player logs in ", directory))
  if(length(logs) == 1) ls$path <- logs[1]
  if(length(logs) == 2){
    #check if there is a preprocessed player file
    preprocessed_index <- grep("*_preprocessed", logs)
    if(length(preprocessed_index) == 1){
      ls$path_preprocessed <- logs[preprocessed_index]
      ls$path <- logs[-preprocessed_index]
    } else{
      print("There is more player logs with appropriate timestamp in the same folder. Have you named and stored everything appropriately?")
    }
  }
  return(ls)
}