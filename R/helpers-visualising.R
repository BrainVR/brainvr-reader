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