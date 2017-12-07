text_to_vector3 = function(text){
  splitted = strsplit(substring(text,2,nchar(text)-1),",")
  if(length(splitted[[1]]) > 2) return(sapply(splitted[[1]], as.numeric, 
                                              warning = F, USE.NAMES = F))
  return(NULL)
}

#turns vector columns in string "(x, y, z)" into three columns(Position.x, Position.y, Position.z) and returns the table
vector3_to_columns = function(tab, column_name){
  xyz = c("x", "y", "z")
  splitted = strsplit(substring(tab[, get(column_name)], 2, nchar(tab[, get(column_name)]) - 1), ",")
  #turns the Vector3 into lists of 3 values
  i = 1
  for (letter in xyz){
    new_name = paste(column_name, letter, sep = ".")
    tab[, (new_name) := as.numeric(sapply(splitted, "[", i))]
    i = i + 1
  }
  return(tab)
}

#pure helpers for my particular unity logging 
position_to_vector = function(list){
  listNames = names(list)
  for(name in listNames){
    ls = list[[name]]
    numberOfItems = length(ls)
    df = data.frame(Position.x = numeric(numberOfItems), 
                    Position.y = numeric(numberOfItems),
                    Position.z = numeric(numberOfItems))
    for (i in 1:length(ls)){
      stringVector = ls[i]
      df[i, ] = text_to_vector3(stringVector)
    }
    list[[name]] = df
  }
  return(list)
}

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