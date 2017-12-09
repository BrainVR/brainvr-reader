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

angle_from_positions <- function(pos_from, pos_to){
  #' Zero vector is the vector given by the unity for calculation rotation of oabjects. 
  #' In 3D unity (different in UNREAL!) are exes X horizontal left/right, Y vertical up/down, and Z plane horizontal up/down
  #' The zero vector is vector when GameObject only changes its Z position, therefore if we calculate position as [X Z], 
  #' the normalised zero vector shoudl be 0 on X and 1 on Z - [0, 1]
  ZERO_VECTOR <- c(0, 1)
  target_vector <- pos_to - pos_from
  
  if(length(pos_from) != 2 ){print("ERROR:angle_from_positions, DESCRIPTION:input does not have two 2d position")}
  if(length(pos_to) != 2){print("ERROR:angle_from_positions, DESCRIPTION:input does not have two 2d position")}
  
  # ATAN takes Y and X, but we want to scale it against Z axis, therefore Y in carthesian, so the input is reversed
  theta <- atan2(target_vector[1], target_vector[2])
  angle <- radian_to_angle(theta)
  return(angle)
}

convert_angle <- function(difference){
  return(((difference + 180) %% 360) - 180)
}

euclid_distance <- function(x_values, y_values){
  x <- c(x_values[[1]][1], y_values[[1]][1])
  y <- c(x_values[[1]][2], y_values[[1]][2])
  return(sqrt(sum((x - y) ^ 2)))
}

is_between <- function(numbers, between_low, between_high){
  return(sapply(numbers, function(x) (x > between_low && x < between_high)))
}

radian_to_angle <- function(radian){
  angle <- radian/pi * 180
  if(angle < 0) angle <- 360 + angle
  return(angle)
}

