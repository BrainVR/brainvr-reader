#' Adds new colum angle_diff_axis where it calculates angle difference between rows
#' @param player_log data.table log that gets modified by reference
#' @param axis axis X, Y or Z. the player log shoudl have Rotation.Axis colum 
#' @example 
#' add_angle_difference(player_log, "Y")
#' 
add_angle_difference = function(player_log, axis = "x"){
  rotation_col_name <- paste0("Rotation.", str_to_title(axis))
  new_col_name <- paste0("angle_diff_", axis)
  
  axis_angles <- player_log[[rotation_col_name]]
  if(is.null(axis_angles)){
    smart_print(c("There isn't a rotation column of name", rotation_col_name))
    return()
  }
  axis_angle_diffs <- c(0, diff(axis_angles))
  axis_angle_diffs <- convert_angle(axis_angle_diffs)
  
  player_log[, (new_col_name):= axis_angle_diffs]
  return(player_log)
}

#calculates the distance walked between each two points of the position table and returns the table
add_distance_moved = function(player_log){
  for (i in 2:nrow(player_log)){
    player_log[c(i - 1, i), distance := euclid_distance(.(Position.x, Position.z)[1], 
                                                        .(Position.x, Position.z)[2])]
  }
  player_log[, cumulative_distance := cumsum(distance)]
  return(player_log)
}