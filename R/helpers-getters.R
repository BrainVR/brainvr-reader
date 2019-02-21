get_trial_with_event_indices <- function(test, event){
  indices <- unique(which(test$data$Sender == "Trial" & test$data$Event == event))
  return(indices + 1)
}

get_trial_events <- function(exp_log, iTrial){
  events_log <- exp_log[exp_log$Sender == "Trial" & exp_log$Index == iTrial,]
  return(events_log)
}

get_times_events <- function(exp_log, event_name){
  times <- exp_log[exp_log$Event == event_name, "Time"]
  return(times)
}