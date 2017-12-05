make_trial_images = function(dt_position, test, columns = 5, indices = c()){
  indices = if (length(indices) == 0) get_trial_event_indices(test, "Finished") else indices
  plots = list()
  for(i in 1:length(indices)){
    plots[[i]] = make_trial_image(dt_position, test, indices[i])
  }
  multiplot(plots, cols = columns)
}