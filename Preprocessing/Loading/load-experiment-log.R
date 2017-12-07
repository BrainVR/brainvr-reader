#' Loads expeirment log into a predefined list
#' @param filepath path tot he expeirment log
#' @return list with loaded settings files and data
#' 
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

experiment_name_from_filename <- function(filename){
  ptr <- "_test_(.*)_"
  capture_groups <- str_match(filename, ptr)
  return(capture_groups[, 2])
}