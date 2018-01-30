make_circle <- function(center = c(0, 0), radius = 1, precision = 100){
  tt <- seq(0, 2 * pi, length.out = precision)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}

save_plot <- function(plt, name){
  if(!requireNamespace("png", quietly = T)){
    stop("Needs png package for outputting")
  }
  mypath <- paste("../images/", name, sep = "")
  file <- png(mypath, width = 1200, height = 800, units = "px")
  plot(plt)
  dev.off()
}

create_direction_line <- function(position, angle, len){
  vec <- c(position, position + navr::vector_from_angle(angle) * len)
  df <- data.frame(x = vec[1], y = vec[2], xend = vec[3], yend = vec[4])
  return(df)
}

create_direction_line_df <- function(df){
  #TODO - redo to apply
  df$xend <- 0
  df$yend <- 0
  for(i in 1:nrow(df)){
    df_i <- df[i, ]
    df[i, c("xend", "yend")] <- df_i[, c("x", "y")] + navr::vector_from_angle(df_i$angle) * df_i$length
  }
  return(df)
}

