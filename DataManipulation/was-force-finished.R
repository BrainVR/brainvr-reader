was_force_finished = function(test, trialID){
  return(nrow(filter(test$data, Sender == "Trial" & 
                       Index == (trialID - 1) & 
                       Event == "ForceFinished")) > 1)
}