#'
#'
#' @param playerLog: data table log as preprocessed by the Analysis fuynction
#' @path path: path to the file. Defaults to workign directory
export_player_log = function(playerLog, id, path = getwd()){
  playerLog[, Position := NULL]
  filePath = paste(path, "/", id, "_player.log", sep = "")
  colnames(playerLog) = c("Time", "RotationX", "RotationY", "FPS", "Input", "PositionX", 
                          "PositionY", "PositionZ", "distance", "cummulativeDistance", "angleDiff")
  write.table(playerLog, filePath, sep = ";", quote = F, row.names = F)
}