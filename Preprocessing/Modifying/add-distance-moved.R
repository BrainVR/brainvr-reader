#calculates the distance walked between each two points of the position table and returns the table
add_distance_moved = function(player_log){
  for (i in 2:nrow(player_log)){
    player_log[c(i - 1, i), distance := euclid_distance(.(Position.x, Position.z)[1], 
                                                          .(Position.x, Position.z)[2])]
  }
  player_log[, cumulative_distance := cumsum(distance)]
  return(player_log)
}