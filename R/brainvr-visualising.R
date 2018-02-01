
#' Title
#' 
#' @param trialId 
#'
#' @return ggplot objectg
#' @export
#' @import ggplot2 
make_trial_image <- function (obj, trialId){
  plt <- navr::create_plot()
  if(!is.null(obj$map_limits)){
    plt <- plt + xlim(obj$map_limits$x)+ ylim(obj$map_limits$y)
  }
  dt_player <- get_player_log_trial(obj, trialId)
  plt <- navr::plot_add_path(plt, dt_player$Position.x, dt_player$Position.z)
  return(plt)
}

#' Title
#' 
#' @param trialId
#'
#' @return ggplot objectg
#' @export
#' @import ggplot2 
plot_trial_path <- function (obj, trialId){
  return(make_trial_image)
}

#' Title
#'
#' @param obj 
#' @param columns
#' @param indices 
#'
#' @return
#' @export
make_trial_images <- function(obj, columns = 5, indices = c()){
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
