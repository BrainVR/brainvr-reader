#' Exports preprocessed player log
#'
#' @param playerLog: data table log as preprocessed by the Analysis fuynction
#' @param path: path to the file. Defaults to workign directory
#' @export
export_player_log = function(playerLog, id, path = getwd()){
  playerLog[, Position := NULL]
  filePath = paste(path, "/", id, "_player.log", sep = "")
  colnames(playerLog) = c("Time", "RotationX", "RotationY", "FPS", "Input", "PositionX", 
                          "PositionY", "PositionZ", "distance", "cummulativeDistance", "angleDiff")
  write.table(playerLog, filePath, sep = ";", quote = F, row.names = F)
}

collect_events <- function(test, dt_player){
  iFinished <- get_trial_event_indices(test, "Finished")
  trialIDs <- iFinished[!(iFinished %in% get_trial_event_indices(test, "ForceFinished"))]
  
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