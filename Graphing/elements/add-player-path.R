add_player_path = function(plot, test, trialID, dt_position){
  trial_timewindow = get_trial_timewindow(test, trialID)
  dt_player = crop_player_log(dt_position, trial_timewindow)
  plot = plot + geom_path(data = dt_player, aes(Position.x, Position.z))
  return(plot)
}