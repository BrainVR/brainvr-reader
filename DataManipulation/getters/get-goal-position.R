get_goal_position = function(test, i_goal, onlyXY = F){
  goalPosition = test$positionSettings$GoalPositions[i_goal, ]
  if (onlyXY){
    return(c(goalPosition$Position.x, goalPosition$Position.z))
  } else return(goalPosition)
}