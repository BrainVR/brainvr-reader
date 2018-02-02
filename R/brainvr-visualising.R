#' Plots trial path
#'
#' @param obj 
#' @param trialId 
#'
#' @return
#' @export
#'
#' @examples
plot_trial_path.brainvr <- function(obj, trialId){
  if(length(trialId == 1)) return(brainvr.plot_trial_path(obj, trialId))
  #calculate numbe of columns
  if(length(trialId > 1)) return(brainvr.plot_trials_paths(obj, indices = trialId))
}

#' Plots trial path
#' 
#' @param trialId 
#'
#' @return ggplot objectg
#' @import ggplot2 
#' @keywords internal
#' @noRd
brainvr.plot_trial_path <- function (obj, trialId){
  plt <- navr::create_plot()
  if(!is.null(obj$map_limits)){
    plt <- plt + xlim(obj$map_limits$x)+ ylim(obj$map_limits$y)
  }
  dt_player <- get_trial_log.brainvr(obj, trialId)
  plt <- navr::plot_add_path(plt, dt_player$Position.x, dt_player$Position.z)
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
  indices <- if (length(indices) == 0) get_trial_event_indices(test, "Finished") else indices
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