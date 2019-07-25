#' Plots trial path
#'
#' @param obj 
#' @param trialId 
#'
#' @return
#' @export
#'
#' @examples
#' 
plot_trial_path <- function(obj, iTrial, ...){
  UseMethod("plot_trial_path")
}
#' @export
plot_trial_path.brainvr <- function(obj, iTrial){
  if(length(iTrial == 1)) return(brainvr.plot_trial_path(obj, iTrial))
  #calculate numbe of columns
  if(length(iTrial > 1)) return(brainvr.plot_trials_paths(obj, indices = iTrial))
}

#' Plots trial path
#' 
#' @param iTrial 
#'
#' @return ggplot objectg
#' @import ggplot2 
#' @keywords internal
#' @noRd
brainvr.plot_trial_path <- function (obj, iTrial){
  navr_obj <- get_trial_position(obj, iTrial)
  plt <- navr::plot_path(navr_obj)
  plt <- plt + navr::geom_navr_limits(navr_obj)
  return(plt)
}

#' PLots multiple paths
#'
#' @param obj 
#' @param columns
#' @param indices 
#'
#' @return
#' @keywords internal
#' @noRd
brainvr.plot_trials_paths <- function(obj, columns = 5, indices = c()){
  if(!requireNamespace("grid", quietly = T)){
    stop("Cannot continue without grid")
  }
  indices <- if (length(indices) == 0) get_trial_with_event_indices(test, "Finished") else indices
  plots <- list()
  for(i in 1:length(indices)){
    plots[[i]] <- make_trial_image(obj, indices[i])
  }
  navr::multiplot(plots, cols = columns)
}

#' Title
#'
#' @param plt 
#' @param obj 
#' @param trialId 
#'
#' @return
#'
#' @examples
#' @noRd
brainvr.plot_add_trial_start_goal <- function(plt, obj, trialId){
  ls <- list(goal = get_goal_position.brainvr(obj, trialId), start = get_start_position(obj, trialId))
  if(is.null(ls$goal)) return(plt)
  plt <- navr::plot_add_points(plt, ls, color = "green")
  return(plt)
}
