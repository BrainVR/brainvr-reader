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
    smart_print(c("Could not find the file for player log in ", directory))
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
        smart_print(c("Removing preprocessed log", ptr))
        file.remove(logs[preprocessed_index])
      } else {
        smart_print(c("Loading preprocessed player log", ptr))
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
      smart_print(c("Multiple player logs in ", directory))
      return(NULL)
    } 
    log <- logs[1]
  }
  smart_print(c("Loading unprocessed player log", ptr))
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