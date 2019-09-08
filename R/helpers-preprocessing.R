#' @noRd
prepare_navr_log <- function(position_log){
  ## Converting position
  position_log <- vector3_to_columns(position_log)
  #TODO - remove data.table
  position_log[, Position := NULL]
  position_log <- prepare_navr_column_names(position_log)
  #' SUPER IMPORTANT - renames Unity Z to Y and vice versa, because NAVR calculates
  #' speeds from x and y not x and z
  position_log <- switch_y_and_z(position_log)
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
  if(length(text) <= 1) return(NULL)
  if(!is_json(text)) return(NULL)
  ls <- jsonlite::fromJSON(text)
  return(ls)
}

is_json <- function(text){
  bool <- c()
  bool <- c(bool, any(grepl("\\{", text))) #has braces
  return(all(bool))
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

switch_y_and_z <- function(dt_log){
  dt_log <- dt_log[, position_temp := position_z]
  dt_log <- dt_log[, position_z := position_y]
  dt_log <- dt_log[, position_y := position_temp]
  dt_log <- dt_log[, position_temp := NULL]
  return(dt_log)
}

#' Tries to rename columns so they correspond to proper naming conventions
#' @description changes "." to "_" to correspond with python conventions,
#' makes everything lowecase, renames Time column if present to "timestamp"
#'
#' @param df dataframe
#'
#' @return modified dataframe
#' @noRd
prepare_navr_column_names <- function(df){
  df <- rename_column(df, "Time", "timestamp")
  new_names <- tolower(gsub("[.]", "_", colnames(df))) #replaces . with _
  colnames(df) <- new_names
  return(df)
}

rename_column <- function(df, old_column, new_column){
  colnames(df)[colnames(df)==old_column] <- new_column
  return(df)
}