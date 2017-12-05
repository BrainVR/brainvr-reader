euclid_distance = function(x_values, y_values){
  x = c(x_values[[1]][1], y_values[[1]][1])
  y = c(x_values[[1]][2], y_values[[1]][2])
  return(sqrt(sum((x - y) ^ 2)))
}