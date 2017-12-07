create_log_search_pattern <- function(log_name, log_timestamp){
  ptr <- paste0("_", log_name, "_")
  if(!is.null(log_timestamp)){
    ptr <- paste0(ptr, "*", log_timestamp)
  }
  return(ptr)
}
create_separator = function(string){
  ls = list()
  ls$beginning = paste("\\*\\*\\*\\",string, "\\*\\*\\*", sep="")
  ls$end = paste("\\-\\-\\-",string, "\\-\\-\\-", sep="")
  return(ls)
}

get_indicies_between = function(text, string){
  ls = list()
  ls$beginning = which(grepl(create_separator(string)$beginning, text))
  ls$end = which(grepl(create_separator(string)$end, text))
  return(ls)
}

get_json_between = function(text, string){
  ls = TextToJSON(get_text_between(text, string))
  return(ls)
}
get_text_between = function(text, string){
  indices = get_indicies_between(text, string)
  if (length(indices$beginning) != 1 || length(indices$end) != 1) return (NULL)
  text = text[(indices$beginning + 1):(indices$end - 1)]
  return(text)
}