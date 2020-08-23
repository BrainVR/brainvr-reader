#' Exports preprocessed player log
#'
#' @param log data table log as preprocessed by the Analysis fuynction
#' @param path: path to the file. Defaults to workign directory
#'
#' DOESN'T WORK ... need to fix all the getters
export_position_log <- function(log, id, path = getwd()) {
  log[, Position := NULL]
  filePath <- paste(path, "/", id, "_player.log", sep = "")
  colnames(log) <- c(
    "timstamp", "RotationX", "RotationY", "FPS", "Input", "PositionX",
    "PositionY", "PositionZ", "distance", "cummulativeDistance", "angleDiff"
  )
  write.table(log, filePath, sep = ";", quote = FALSE, row.names = FALSE)
}

collect_events <- function(test, dt_player) {
  iFinished <- get_trial_with_event_indices(test, "Finished")
  trialIDs <- iFinished[!(iFinished %in% get_trial_with_event_indices(test, "ForceFinished"))]

  df_trials <- test$data[test$data$Index %in% (trialIDs - 1) & test$data$Sender == "Trial", ]

  synchropulse <- df_trials[df_trials$Event == "ArduinoPulseStart", "Time"]
  trialSetup <- df_trials[df_trials$Event == "WaitingToStart", "Time"]
  trialStarted <- df_trials[df_trials$Event == "Running", "Time"]
  trialEnded <- df_trials[df_trials$Event == "Finished", "Time"]
  pointingStarted <- trialStarted
  pointingEnded <- c()
  pointingError <- c()
  type <- c()
  for (trialID in trialIDs) {
    pointing <- get_trial_pointing(dt_player, test, trialID)
    pointingEnded <- c(pointingEnded, pointing$time)
    pointingError <- c(pointingError, angle_difference(pointing$target, pointing$chosen))
    type <- c(type, get_trial_type(test, trialID))
  }
  df <- data.frame(trialIDs, trialSetup, trialStarted, trialEnded, 
                   pointingStarted, pointingEnded, pointingError, 
                   synchropulse, type)
}
