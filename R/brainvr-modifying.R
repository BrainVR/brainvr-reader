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


flip <- function(){
  
}