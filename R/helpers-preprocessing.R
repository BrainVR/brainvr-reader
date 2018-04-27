#calculates the distance walked between each two points of the position table and returns the table
add_distance_moved <- function(player_log){
  player_log[, distance := navr::euclid_distance_between_rows(data.frame(Position.x, Position.z))]
  player_log[, cumulative_distance := cumsum(distance)]
  return(player_log)
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