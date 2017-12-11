add_special_paths <- function(position_table, ls){
  list_names <- names(ls)
  position_table <- position_table[, special:= "normal"]
  list_names <- names(ls)
  for (i in 1:length(ls)){
    position_table <- position_table[is_between(Time, ls[[i]]$start, ls[[i]]$finish), special:= list_names[i]]
  }
  return(position_table)
}

make_circle <- function(center = c(0, 0), radius = 1, precision = 100){
  tt <- seq(0, 2*pi, length.out = precision)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow = 2, byrow = TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(plotlist = NULL, file, cols = 1, layout = NULL) {
  library(grid)
  numPlots <- length(plotlist)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plotlist[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plotlist[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                         layout.pos.col = matchidx$col))
    }
  }
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
  vec <- c(position, position + vector_from_angle(angle) * len)
  df <- data.frame(x = vec[1], y = vec[2], xend = vec[3], yend = vec[4])
  return(df)
}


create_direction_line_df <- function(df){
  #TODO - redo to apply
  df$xend <- 0
  df$yend <- 0
  for(i in 1:nrow(df)){
    df_i <- df[i, ]
    df[i, c("xend", "yend")] <- df_i[, c("x", "y")] + vector_from_angle(df_i$angle) * df_i$length
  }
  return(df)
}

