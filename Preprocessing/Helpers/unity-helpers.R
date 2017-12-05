GetGoalIndex = function(test, trialID){
  return(get_goal_index(test, trialID))
}
GoalPosition = function(test, goalIndex, onlyXY = F){
  return(get_goal_position(test, i_goal = goalIndex, onlyXY = onlyXY))
}
StartIndex = function(test, trialID){
  return(test$experimentSettings$StartOrder[trialID] + 1)
}
MarkIndex = function(test, trialID){
  return(test$experimentSettings$MarkOrder[trialID] + 1)
}
StartPosition = function(test, startIndex, onlyXY = F){
  return(get_start_positsions(test, i_start = startIndex, onlyXY = onlyXY))
}
MarkPosition = function(test, MarkIndex, onlyXY = F){
  return(get_mark_position(test, i_mark = MarkIndex, onlyXY = onlyXY))
}
WasForceFinished = function(test, trialID){
  return(was_force_finished(test, trialID))
}