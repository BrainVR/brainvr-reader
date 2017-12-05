get_walked_distnace_timewindow <- function(dt_position, timeWindow){
  dt_position <- dt_position[Time > timeWindow$start & Time < timeWindow$finish, ]
  if (dt_position[, .N] < 2) {
    print("The player log doesn¨t cover given timewindows")
    walkedDistance <- as.numeric(NA)
  } else {
    start <- head(dt_position, 1)$cumulative_distance
    end <- tail(dt_position, 1)$cumulative_distance
    walkedDistance <- end - start
  }
  return(walkedDistance)
}