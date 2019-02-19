prepare_navr_log <- function(position_log){
  ## Converting position
  position_log <- vector3_to_columns(position_log)
  #TODO - remove data.table
  position_log[, Position:= NULL]
  position_log <- navr::prepare_column_names(position_log)
  return(position_log)
}

is_column_present <- function(table, name){
  return(name %in% names(table))
}

json_to_list <- function(text){
  if(!requireNamespace("jsonlite", quietly = T)){
    stop("needs jsonlite to continue")
  }
  if(is.null(text)) return(NULL)
  if(length(text) == 0) return(NULL)
  ls <- jsonlite::fromJSON(text)
  return(ls)
}

replace_strings <- function(vec, strings, replacements){
  if(length(strings) != length(replacements)){
    cat("Strings and replacements need to have the same length")
    return(NULL)
  }
  for (i in 1:length(strings)){
    vec[vec == strings[i]] <- replacements[i]
  }
  return(vec)
}