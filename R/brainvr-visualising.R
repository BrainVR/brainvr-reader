#' Title
#' 
#' @param trialId 
#'
#' @return ggplot objectg
#' @export
#' @import ggplot2 
make_trial_image <- function (obj, trialId){
  plot <- ggplot2::ggplot()
  dt_player <- get_player_log_trial(obj, trialId)
  plot <- navr::plot_add_path(plot, dt_player$Position.x, dt_player$Position.z)
  return(plot)
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
