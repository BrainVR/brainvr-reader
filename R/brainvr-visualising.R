#' Title
#' 
#' @param trialId 
#'
#' @return ggplot objectg
#' @export
#' @import ggplot2 
make_trial_image <- function (obj, trialId){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plot <- ggplot2::ggplot()
  dt_player <- get_player_log_trial(obj, trialId)
  plot <- plot_add_player_path(plot, dt_player)
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
  multiplot(plots, cols = columns)
}

#' Creates empty stylised plot with given constriaints, if set in the object
#' 
#' @param obj UnityObject
#' @return ggplot plot with x and y lims if set#' 
#' @export
#' 
create_plot <- function(obj){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plot <- ggplot2::ggplot()
  if (!is.null(obj$map_limits)){
    plot <- plot + xlim(obj$map_limits$x) + ylim(obj$map_limits$y)
  }
  return(plot)
}

#' Adds player path to the given plot
#' 
#' @param plot 
#' @param data Data frame with Position.x and Position.z columns
#' @return plot with plotted pat
#' 
#' @export
plot_add_player_path <- function(plt, df_pos){
  #some validations aobu the data frame
  plt <- plt + geom_path(data = df_pos, aes(Position.x, Position.z))
  return(plt)
}

#' Adds specified points to the given plot
#' 
#' @param plot already created ggplot
#' @param ls list with XY vectors. eg. (list(start = c(0, 0), end = C(10, 5)))
#' @return modified plot
#' 
#' @export
plot_add_points <- function(plt, ls){
  list_names <- names(ls)
  df <- data.frame(point.x = numeric(0), point.y = numeric(0), point.name = character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    df[i, 1] <- ls[[i]][1]
    df[i, 2] <- ls[[i]][2]
    df[i, 3] <- list_names[i]
  }
  plt <- plt + geom_point(data = df, aes(point.x, point.y),size = 4, color = "blue") + 
    geom_text(data = df, aes(point.x, point.y, label = point.name))
  return(plt)
}


#' Adds arrow pointing from a point in a specified angle
#'
#' @param position_df data.frame. Needs to have columns x, y, angle, length, type
#' @param plt PLot to which to add the arrow
#' @return built ggplot2
#'
#' @example 
#' plt <- plot_add_direction(plt, 
#' 
#' @export
plot_add_direction <- function(plt, position, angle, len){
  ARROW_DEF <- arrow(length = unit(0.25, "cm"))
  arrow_line <- create_direction_line(position, angle, len)
  plt <- plt + geom_segment(data = arrow_line, aes(x = x, y = y, xend = xend, yend = yend), 
                            size = 1, arrow = ARROW_DEF)
}

#' Adds arrow pointing from a point in a specified angle
#'
#' @param position_df data.frame. Needs to have columns x, y, angle, length, type
#' @param plt PLot to which to add the arrow
#' @return built ggplot2
#'
#' @example 
#' direction_df <- data.frame(x = c(0, 0), y = (0:1), angle = c(45, 90), length = c(1, 2), type = c("first", "second"))
#' plt <- ggplot()
#' plt <- plot_add_directions(plt, direction_df)
#' @export
plot_add_directions <- function(plt, direction_df){
  ARROW_DEF <- arrow(length = unit(0.25, "cm"))
  arrow_df <- create_direction_line_df(direction_df)
  plt <- plt + geom_segment(data = arrow_df, aes(x = x, y = y, xend = xend, yend = yend, color = type), 
    size = 1, arrow = ARROW_DEF)
}