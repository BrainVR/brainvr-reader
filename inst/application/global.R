library(brainvr.reader)
library(ggplot2)
obj <- load_experiment("../extdata/CFNS/")
obj <- mirror_axes(obj)
obj <- translate_positions(obj, c(33.5, 0, 47.75))
n_trials <- length(unique(obj$data$experiment_log$data$Index))

#initialises first trial
dt_trial <- get_trial_log(obj, 1)
log_nrow <- nrow(dt_trial)
trial_end <- dt_trial$Time[nrow(dt_trial)]
