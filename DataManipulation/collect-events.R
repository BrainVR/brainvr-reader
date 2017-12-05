#'
#'
#' @param name: string with the name of the event, e.g. ArduinoPulseStop
#' @param path: where to save the file. Defaults ot hte workig path/exports
collect_events <- function(test, dt_player){
  iFinished = get_trial_event_indices(test, "Finished")
  trialIDs = iFinished[!(iFinished %in% get_trial_event_indices(test, "ForceFinished"))]
  
  finishedTrials <- test$data %>% filter(Index %in% (trialIDs-1))
  
  synchropulse <- finishedTrials  %>% filter(Sender == "Trial") %>% filter(Event == "ArduinoPulseStart") %>% .$Time
  trialSetup <- finishedTrials %>% filter(Sender == "Trial") %>% filter(Event == "WaitingToStart") %>% .$Time
  trialStarted <- finishedTrials %>% filter(Sender == "Trial") %>% filter(Event == "Running") %>% .$Time
  #I had a mistake in the old logs with running spelled with only a single N
  if (length(trialStarted) == 0) trialStarted <- finishedTrials %>% filter(Sender == "Trial") %>% filter(Event == "Runing") %>% .$Time
  trialEnded <- finishedTrials %>% filter(Sender == "Trial") %>% filter(Event == "Finished") %>% .$Time
  pointingStarted <- trialStarted
  pointingEnded <- c()
  pointingError <- c()
  type <- c()
  for(trialID in trialIDs){
    pointing <- get_trial_pointing(dt_player, test, trialID)
    pointingEnded <- c(pointingEnded, pointing$time)
    pointingError <- c(pointingError, angle_difference(pointing$target, pointing$chosen))
    type <- c(type, get_trial_type(test, trialID))
  }
  df <- data.frame(trialIDs, trialSetup, trialStarted, trialEnded, pointingStarted, pointingEnded, pointingError, synchropulse, type)
}