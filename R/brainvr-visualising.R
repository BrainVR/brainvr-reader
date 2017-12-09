#' Title
#'
#' @param dt_position 
#' @param test 
#' @param trialID 
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
make_trial_image <- function (obj, trialID){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plot <- ggplot() + theme_void()
  plot <- add_player_path(plot, obj, trialID)
  return(plot)
}

#' Title
#'
#' @param dt_position 
#' @param test 
#' @param columns 
#' @param indices 
#'
#' @return
#' @export
#'
#' @examples
make_trial_images <- function(dt_position, test, columns = 5, indices = c()){
  indices <- if (length(indices) == 0) get_trial_event_indices(test, "Finished") else indices
  plots <- list()
  for(i in 1:length(indices)){
    plots[[i]] <- make_trial_image(dt_position, test, indices[i])
  }
  multiplot(plots, cols = columns)
}