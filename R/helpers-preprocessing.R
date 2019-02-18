prepare_navr_log <- function(position_log){
  #renames timestamp
  # TODO - more elegant solution
  rename_column(position_log, "Time", "timestamp")
  rename_column(position_log, "Rotation.X", "rotation_x")
  rename_column(position_log, "Rotation.Y", "rotation_y")
  if(!requireNamespace("stringr", quietly = T)){
    print("Cannot continue withouth stringr package. Please install it")
    return(F)
  }
  ## Converting position
  position_log <- vector3_to_columns(position_log)
  #TODO - remove data.table
  position_log[, Position:= NULL]
  return(position_log)
}
add_angle_differences <- function(player_log){
  cols <- colnames(player_log)
  for(i in grep("Rotation", cols)){
    colname <- cols[i]
    new_name <- gsub("Rotation.", "", colname)
    new_name <- tolower(gsub("[.]", "_", new_name))
    player_log <- navr::add_angle_difference(player_log, player_log[[colname]], new_name)
  }
  return(player_log)
}

is_column_present <- function(table, name){
  return(name %in% names(table))
}

rename_column <- function(df, old_column, new_column){
  colnames(df)[colnames(df)==old_column] <- new_column
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