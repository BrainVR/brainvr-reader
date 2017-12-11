angle_difference <- function(angle1, angle2){
  return(180 - abs(abs(angle1 - angle2) - 180))
}

angle_coordinate_circle <- function(angle, radius = NULL, center = NULL){
  if(is.null(center)) center <- c(0, 0)
  if(is.null(radius)) radius <- 1
  x <- center[1] + radius * cos(angle)
  y <- center[2] + radius * sin(angle)
  return(c(x, y))
}

# calculates angle difference from -180 to 180
angle_to_difference <- function(angle){
  angle <- ((angle + 180) %% 360) - 180
  return(angle)
}

# converts positive and negative angles to 0-360
# asumes it is not below -360
# 390 is converted to 30, -40 to 320 etc
angle_to_360 <- function(angle){
  return((angle + 360) %% 360)
}

angle_to_radian <- function(angle){
  return(angle/180 * pi)
}

euclid_distance <- function(x_values, y_values){
  x <- c(x_values[[1]][1], y_values[[1]][1])
  y <- c(x_values[[1]][2], y_values[[1]][2])
  return(sqrt(sum((x - y) ^ 2)))
}

is_between <- function(numbers, between_low, between_high){
  return(sapply(numbers, function(x) (x > between_low && x < between_high)))
}

vector_from_angle <- function(angle){
  rad <- angle_to_radian(angle)
  vector <- c(sin(rad), cos(rad))
  return(vector)
}
