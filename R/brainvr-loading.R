#' Goes through the folder and loads every experiment info into separate object
#'
#' @param override if TRUE, deletes and recomputes preprocessed player. defaults to FALSE
#' @param folder where to look for brainvr files
#' @param save if true, then preprocessed logs are saved to the folder
#'
#' @return list of objects
#' @export
load_experiments <- function(folder, override = FALSE, save = TRUE) {
  if (is.null(folder)) stop("no folder set")
  # open experiment_logs to see how many do we have
  experiment_infos <- open_brainvr_logs(folder, "ExperimentInfo",
                                        func = load_experiment_info,
                                        flatten = FALSE)
  if (is.null(experiment_infos)) stop("Experiment info not found")
  ls <- list()
  for (i in 1:length(experiment_infos)) {
    info <- experiment_infos[[i]]
    ls[[i]] <- load_experiment(folder, exp_timestamp = info$header$Timestamp,
                               override = override, save = save)
  }
  return(ls)
}

#' Loads files form a folder into BrainvrObject
#'
#' @param folder path to the folder respective to the working directory
#' @param exp_timestamp timestamp of a particular experiment to search for
#' @param override if a preprocessed file is found, should it be overridden? 
#' IF TRUE, then all files are preprocessed again. Default is FALSE
#' @param save logical should the files be saved after being preprocessed? 
#' default is TRUE
#'
#' @returns BrainvrObject object
#' @example
#' @export
load_experiment <- function(folder, exp_timestamp = NULL,
                            override = FALSE, save = TRUE) {
  if (is.null(folder)) stop("no folder set")
  # TODO - this should return only a single one per timestamp
  experiment_info <- open_brainvr_log(folder, log_name = "ExperimentInfo",
                                      exp_timestamp = exp_timestamp,
                                      func = load_experiment_info)
  if (is.null(experiment_info)) stop("Experiment info not found")
  # if multiple logs or no logs, quit
  if (is.null(exp_timestamp)) exp_timestamp <- experiment_info$header$Timestamp
  ## TODO separate preprocess adn opening
  navr_object <- open_player_log(folder, exp_timestamp = exp_timestamp,
                                 override = override, save = save)
  if (is.null(navr_object)) stop("Player log not found")
  # preprocesses player log
  # checks if there is everything we need and if not, recomputes the stuff
  test_log <- open_experiment_logs(folder, exp_timestamp, flatten = TRUE)
  result_log <- open_brainvr_log(folder, "results", exp_timestamp)
  obj <- BrainvrObject()
  obj$participant_id <- experiment_info$header$Participant
  obj$timestamp <- exp_timestamp
  obj$data$experiment_info <- experiment_info
  obj$data$position <- navr_object
  # TODO - this might be an issue due to uneven terminology
  obj$data$experiment_log <- test_log
  obj$data$results_log <- result_log
  obj$experiment_name <- obj$data$experiment_log$name

  return(obj)
}

#' Searches the directory for experiment log files. Returs single one if multiple are found
#'
#' @param exp_timestamp
#' @param directory path to the directory where to search
#'
#' @return list with a single loaded info log
#' @export
open_experiment_infos <- function(directory, exp_timestamp = NULL,
                                  flatten = FALSE) {
  .Deprecated("open_brainvr_logs(log_name = \"ExperimentInfo\"")
  #' We CANNOT flatten the experiment info because then it looks like there is m
  #' more of them and it loads the experiment multiple times
  out <- open_brainvr_logs(directory, log_name = "ExperimentInfo",
    func = load_experiment_info, exp_timestamp = exp_timestamp, 
    flatten = flatten)
  return(out)
}

#' Loads particular info file into a list
#'
#' @param filepath path to the file
#'
#' @return list object
#' @export
load_experiment_info <- function(filepath) {
  res <- load_headers(filepath)
  names(res)[names(res) == "session_header"] <- "header"
  names(res)[names(res) == "experiment_info"] <- "Experiment"
  return(res)
}

#' Iterates over all _test_ files in a folder asnd saves them one by one to 
#' a return list
#'
#' @param directory directory where the file is located
#' @param flatten in case of only a single list is returned, unnests the list.
#' Beware, unnested list causes issues with opening experiments
#' @param exp_timestamp time of the
#'
#' @return
#' @export
open_experiment_logs <- function(dir, exp_timestamp = NULL, flatten = FALSE) {
  out <- open_brainvr_logs(dir, log_name = "test", exp_timestamp = exp_timestamp,
                           func = load_experiment_log, flatten = flatten)
  return(out)
}

#' Loads expeirment log into a predefined list
#'
#' @param filepath path tot he expeirment log
#' @return list with loaded settings files and data
#' @export
load_experiment_log <- function(filepath) {
  res <- load_brainvr_log(filepath)
  res$name <- experiment_name_from_filename(filepath)
  return(res)
}

#' Searches for results logs and returns loaded list.
#'
#' @description REsults log have a _results_ in their filename. 
#' In case your's doesn't, use load_result_log instead
#'
#' @param directory where to search
#' @param exp_timestamp timestamp of a particular results log
#'
#' @return list with loaded result log
#' @export
#'
#' @examples
open_result_log <- function(dir, exp_timestamp = NULL) {
  .Deprecated("open_brainvr_log",
              msg = "Use open_brainvr_log(dir, \"results\", exp_timestamp) instead")
  return(open_brainvr_log(dir, "results", exp_timestamp))
}

#' Loads results log at a specific path
#'
#' @param filepath
#'
#' @return
#' @export
#'
#' @examples
load_result_log <- function(filepath) {
  .Deprecated("load_brainvr_log")
  result <- load_brainvr_log(filepath)
  return(result)
}

#' Generic loading of all the results, experiment and other logs
#'
#' @param directory directory where to look for the log
#' @param log_name name of the log to be searched for and loaded
#' @param exp_timestamp timestamp of the particular type of log
#' @param flatten in case only a single file is found, should it be unnested? defaults to false
#' @param func R functions which actually loads the object (contains code to preprocess the 
#' log, extract some log specific information etc.). If null, default function is used
#'
#' @return
#'
#' @examples
open_brainvr_logs <- function(directory, log_name, exp_timestamp = NULL,
                              func = NULL, flatten = FALSE) {
  logs <- find_brainvr_logs(directory, log_name, exp_timestamp)
  if (is.null(logs)) return(NULL)
  out <- list()
  for (i in seq_len(length(logs))) {
    out[[i]] <- load_brainvr_log(logs[i], func = func)
  }
  if (flatten && (length(out) == 1)) out <- out[[1]]
  return(out)
}

#' Searches for and loads a generic brainvr framework log. 
#' Contains framework specific header, and optionally data.frame data. See 
#' \code{\link{load_brainvr_log}} for specifics
#'
#' @param directory Where to search for the log
#' @param log_name name of the log (e.g.)
#' @param exp_timestamp necessary if multiple logs are in the same folder
#' @param func function used to load the log. Optional. IF NULL, default loading
#' function is used
#'
#' @return
#' @export
#'
#' @examples
open_brainvr_log <- function(directory, log_name, exp_timestamp = NULL,
                             func = NULL) {
  pths <- find_brainvr_logs(directory, log_name, exp_timestamp)
  if(is.null(pths)) return(NULL)
  if(length(pths) > 1){
    warning("Cannot open log ", log_name, " in ", directory, 
    ". Multiple logs of the same name.  You need to specify the timestamp")
    return(NULL)
  }
  res <- load_brainvr_log(pths[1], func = func)
  return(res)
}

#' Loads a generic brainvr framework log. These logs have specific header and
#' data notaions. 
#'
#' @param filepath path to the log
#' @param func optional loading function
#'
#' @return list with parsed data and optionally $data field with log's dataframe
#' @export
#'
#' @examples
load_brainvr_log <- function(filepath, func = NULL) {
  if(!is.null(func)){
    result <- func(filepath)
    return(result)
  }
  result <- load_headers(filepath)
  i_bottom <- get_bottom_header_index(filepath)
  df_data <- try(read.table(filepath,
                            skip = i_bottom, sep = ";", header = TRUE,
                            stringsAsFactors = FALSE, encoding = "UTF-8"
  ), silent = TRUE)
  if (class(df_data) == "data.frame"){
    result$data <- df_data
    result$data[, ncol(result$data)] <- NULL
  }
  return(result)
}


find_brainvr_logs <- function(directory, log_name, exp_timestamp = NULL,
                              warning_missing = TRUE) {
  ptr <- create_log_search_pattern(log_name, exp_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = TRUE)
  if (length(logs) < 1) {
    if (warning_missing) {
      warning("Could not find any ", log_name, " logs in ", directory,
              " for timestamp ",exp_timestamp)
    }
    return(NULL)
  }
  if (length(logs) > 1 & !is.null(exp_timestamp)) {
    warning("There are multiple ", log_name, " in the ", directory,
      " with timestamp ", exp_timestamp)
    return(NULL)
  } else {
    return(logs)
  }
}

#' Searches a directory for a player log. Returns player log data.table
#'
#' @param directory where the log should be located
#' @param exp_timestamp provides timestamp of a log to load
#' @param override if true, deletes processed player log and loads the unprocessed.
#' if FALSE, load preprocessed log if present
#' @param save Should the log be saved after being preprocessed
#' @return data.table with the loaded player log or NULL.
#' @export
#' @import data.table
open_player_log <- function(directory, exp_timestamp = NULL, override = FALSE,
                            save = TRUE) {
  ls_log_path <- find_player_path(directory, exp_timestamp)
  if (nchar(ls_log_path$path) == 0) return(NULL)
  if (nchar(ls_log_path$path_preprocessed) > 0) {
    if (override) {
      message("Removing preprocessed log ", ls_log_path$path_preprocessed)
      file.remove(ls_log_path$path_preprocessed)
    } else {
      message("Loading preprocessed player log ", ls_log_path$path_preprocessed)
      # TODO - remove data.table
      navr_object <- navr::NavrObject()
      navr_object$data <- fread(ls_log_path$path_preprocessed,
        header = TRUE,
        sep = ";", dec = ".", stringsAsFactors = FALSE,
        encoding = "UTF-8"
      )
      return(navr_object)
    }
  }
  message("Loading unprocessed player log ", ls_log_path$path)
  # TODO - chagne so it doesn't read text so friggin much :(
  text <- readLines(ls_log_path$path, warn = FALSE, encoding = "UTF-8")
  bottomHeaderIndex <- get_indicies_between(text, "SESSION HEADER")$end # get beginning of the log
  # TODO - remove data.table
  df_position <- fread(ls_log_path$path,
    header = TRUE, sep = ";", dec = ".",
    skip = bottomHeaderIndex, stringsAsFactors = FALSE
  )
  # deletes the last column - it's there for the easier logging from unity
  # - its here because of how preprocessing works
  df_position[, ncol(df_position) := NULL]
  df_position <- prepare_navr_log(df_position)
  navr_object <- navr::load_position_data(navr::NavrObject(), df_position)
  navr_object <- preprocess_player_log(navr_object)
  if (save) save_preprocessed_player(directory, exp_timestamp, navr_object$data)
  return(navr_object)
}

find_player_path <- function(directory, exp_timestamp = NULL) {
  ls <- list(path = "", path_preprocessed = "")
  ptr <- create_log_search_pattern("player", exp_timestamp)
  logs <- list.files(directory, pattern = ptr, full.names = TRUE)
  if (length(logs) == 0) warning("Could not find the file for player log in ", directory)
  if (length(logs) > 2) warning("Multiple player logs in ", directory)
  if (length(logs) == 1) ls$path <- logs[1]
  if (length(logs) == 2) {
    # check if there is a preprocessed player file
    preprocessed_index <- grep("*_preprocessed", logs)
    if (length(preprocessed_index) == 1) {
      ls$path_preprocessed <- logs[preprocessed_index]
      ls$path <- logs[-preprocessed_index]
    } else {
      warning("There is more player logs with appropriate timestamp in the 
            same folder. Have you named and stored everything appropriately?")
    }
  }
  return(ls)
}


