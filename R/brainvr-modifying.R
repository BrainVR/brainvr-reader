#' Moves all positions to correspond to the new 0, 0 coordinate
#' 
#' Operates on given BrainvrObject, as it needs to fix not only player log, but potential goal positions in the object as well
#' @param obj BrainvrObject with player log and positions data to be translated
#' @param offset New X, Z and Y vector. Needs to be a int vector of lenght 3 (X, Y, Z). 
#' @returns BrainvrObject
#' @example 
#' obj <- translate_position(obj, (3, 0, -6))
#' 
#' @export
translate_positions <- function(obj, offset){
  obj <- transform_object(obj, "mirror", translate_positions_df, translate_positions_list, offset)
  return(obj)
}

#' Mirrors X and Y to negative and recomputes rotation
#' 
#' @param obj
#' @return BrainvrObject with mirrored axes and rotation
#' @example 
#' obj <- mirror_axes(obj)
#' 
#' @export
#' 
mirror_axes <- function(obj){
  obj <- transform_object(obj, "mirror", mirror_positions_df, mirror_positions_list)
  return(obj)
}

#' Resizes the map to fit the new constraints. 
#' 
#' @param multiplier Conversion of a single unit to a new metric
#' @param obj BrainvrObject
#' @return ModifiedUnity object
#' 
#' @example 
#' obj <- resize_layout(obj, 0.5) #makes it half as large
#' 
#' @export
#' 
resize_layout <- function(obj, multiplier){
  obj <- transform_object(obj, "resize", resize_positions_df, resize_positions_list, multiplier)
  return(obj)
}

#' Loads goal positions to the Brainvr object
#'
#' @param obj Brainvr object
#' @param df data.frame with goal positions. Goal row must correspond to goal order  
#'
#' @return
#' @export
#'
#' @examples
add_goal_positions.brainvr <- function(obj, df){
  return(obj)
}