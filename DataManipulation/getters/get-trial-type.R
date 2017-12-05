egoallobva.get_trial_type = function(test, trialID){
  return(test$experimentSettings$RandomOrdering[trialID])
}