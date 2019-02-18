create_log_search_pattern <- function(log_name, log_timestamp){
  ptr <- paste0("_", log_name, "_")
  if(!is.null(log_timestamp)){
    ptr <- paste0(ptr, ".*", log_timestamp)
  }
  return(ptr)
}

create_separator <- function(string){
  ls <- list()
  ls$beginning <- paste("\\*\\*\\*\\", string, "\\*\\*\\*", sep = "")
  ls$end <- paste("\\-\\-\\-", string, "\\-\\-\\-", sep = "")
  return(ls)
}

### TODO
### Can massively speed it up if only reads part of the text or do it line by line
get_indicies_between = function(text, string){
  ls <- list()
  ls$beginning <- which(grepl(create_separator(string)$beginning, text))
  ls$end <- which(grepl(create_separator(string)$end, text))
  return(ls)
}

get_json_between <- function(text, string){
  ls <- json_to_list(get_text_between(text, string))
  return(ls)
}

get_text_between <- function(text, string){
  indices <- get_indicies_between(text, string)
  if (length(indices$beginning) != 1 || length(indices$end) != 1) return (NULL)
  text <- text[(indices$beginning + 1):(indices$end - 1)]
  return(text)
}

experiment_name_from_filename <- function(filename){
  ptr <- "_test_(.*)_"
  if(!requireNamespace("stringr", quietly = T)){
    stop("Needs stringr to continue")
  }
  capture_groups <- stringr::str_match(filename, ptr)
  return(capture_groups[, 2])
}

##Helper for escaping characters in quest names
escape_regex <- function(string){
  return(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string))
}

unlist_to_data_frame <- function(ls){
  listNames <- names(ls)
  for(name in listNames){
    row <- unlist(ls[[name]])
  }
}