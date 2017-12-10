#' Moves all positions to correspond to the new 0, 0 coordinate
#' 
#' Operates on given UnityObject, as it needs to fix not only player log, but potential goal positions in the object as well
#' @param obj UnityObject with player log and positions data to be translated
#' @param offset New X, Z and Y vector. Needs to be a int vector of lenght 3 (X, Y, Z). 
#' @returns UnityObject
#' @example 
#' obj <- translate_position(obj, (3, 0, -6))
#' 
#' @export
translate_positions <- function(obj, offset){
  # translate player log
  translated_player <- translate_positions_df(obj$data$player_log, offset)
  if(is.null(translated_player)){
    print("Couldn't translate player log. Have you preprocessed it correctly? Quitting.")
    return(obj)
  }
  # translate positions in experiment_log
  translated_positions <- translate_positions_list(obj$data$experiment_log$positions, offset)
  if(is.null(translated_positions)){
    print("Couldn't translate positions in expeirment log. Have you preprocessed it correctly? Quitting.")
    return(obj)
  }
  # Assign is all went well
  # Better to assign here. So that if one goes well, we can retry later and we don¨t end up
  # With player log translated and goal positions not
  obj$data$player_log <- translated_player
  obj$data$experiment_log$positions <- translated_positions
  return(obj)
}

#' Mirrors X and Y to negative and recomputes rotation
#' 
#' @param obj
#' @return UnityObject with mirrored axes and rotation
#' @example 
#' obj <- mirror_axes(obj)
#' 
#' @export
#' 
mirror_axes <- function(obj){
  mirrored_player <- mirror_positions_df(obj$data$player_log)
  if(is.null(mirrored_player)){
    print("Couldn't mirror positions in player log. Have you preprocessed it correctly? Quitting.")
    return(obj)
  }
  mirrored_positions <- mirror_positions_list(obj$data$experiment_log$positions)
  if(is.null(mirrored_positions)){
    print("Couldn't mirror positions in expeirment log. Have you preprocessed it correctly? Quitting.")
    return(obj)
  }

  obj$data$player_log <- mirrored_player
  obj$data$experiment_log$positions <- mirrored_positions
  return(obj)
}


#' Resizes the map to fit the new constraints. 
#' 
#' @param multiplier Conversion of a single unit to a new metric
#' @param obj UnityObject
#' @return ModifiedUnity object
#' 
#' @example 
#' obj <- resize_layout(obj, 0.5) #makes it half as large
#' 
#' @export
#' 
resize_layout <- function(obj, multiplier){
  # translate player log
  resized_player <- resize_positions_df(obj$data$player_log, offset)
  if(is.null(translated_player)){
    print("Couldn't translate player log. Have you preprocessed it correctly? Quitting.")
    return(obj)
  }
}