svg_add_player_path %<c-% function(test, trialID, dt_position, ...){
  attrs = list(...)
  trial_timewindow = get_trial_timewindow(test, trialID)
  dt_player = get_player_log_timewindow(dt_position, trial_timewindow)
  #http://r.789695.n4.nabble.com/combine-vectors-in-an-alternating-way-td3003742.html
  #Z is reversed because of SVG
  xy = c(rbind(dt_player$Position.x, -dt_player$Position.z)) #♦sequence of alternating stuff 
  g(id = "player-path",
    attrs,
    path(d = c("M", xy[1:2], "L", xy[3:length(xy)]), stroke = "black", fill = "none")
    )
}