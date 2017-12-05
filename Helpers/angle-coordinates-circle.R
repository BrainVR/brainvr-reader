angle_coordinate_circle = function(angle, radius = NULL, center = NULL){
  if(is.null(center)) center = c(0, 0)
  if(is.null(radius)) radius = 1
  x = center[1] + radius * cos(angle)
  y = center[2] + radius * sin(angle)
  return(c(x, y))
}