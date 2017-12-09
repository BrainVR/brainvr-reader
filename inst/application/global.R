library(brainvr.R)
library(ggplot2)
obj <- load_experiment("../extdata/")
obj <- mirror_axes(obj)
obj <- translate_positions(obj, c(33.5, 0, 47.75))
n_trials <- length(unique(obj$data$experiment_log$data$Index))

#initialises first trial
dt_trial <- get_player_log_trial(obj, 1)
log_nrow <- nrow(dt_trial)
trial_end <- dt_trial$Time[nrow(dt_trial)]
