make_trial_image = function (dt_position, test, trialID){
  plot = ggplot() + theme_void()

  #plots player
  plot = add_player_path(plot, test, trialID, dt_position)
  
  return(plot)
}