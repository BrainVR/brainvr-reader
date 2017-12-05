get_trial_event_indices = function(test, event){
  indices = unique(filter(test$data, Sender == "Trial" & Event == event) %>% select(Index))[[1]]
  return(indices + 1)
}