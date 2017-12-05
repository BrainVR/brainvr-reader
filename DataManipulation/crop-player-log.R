crop_player_log = function(dt_player, timewindow){
  #checking for entirety
  log = dt_player[Time > timewindow$start & Time < timewindow$finish, ]
  #checking log
  return(log)
}