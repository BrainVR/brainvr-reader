get_trial_timewindow <- function(test, trialID){
  #correction for c# indexing
  trialID <- trialID - 1
  ls <- list()
  ls$WaitingToStart <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "WaitingToStart") %>% 
                         select(Time) %>% first
  ls$start <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "Running") %>% select(Time) %>% first
  #old versions had typo in the running evvent with only a single n
  if (length(ls$start) < 1){ 
    ls$start <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "Runing") %>% select(Time) %>% first
  }
  #selects only hte first element - its because fome of hte old logs had potential two finished tiems 
  #if the experiment or trial was force finished before closed (finished effectively twice)
  ls$finish <- filter(test$data, Index == trialID & Sender == "Trial" & Event == "Finished") %>% select(Time) %>% first
  
  #replaces missing values with NAs
  newValues <- sapply(ls, function(x) if(length(x)== 0) {x <- as.numeric(NA)} else {x <- x})
  ls <- as.list(newValues)
  return(ls)
}