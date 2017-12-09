library(brainvr.R)
library(ggplot2)
obj <- load_experiment("../extdata/")
obj <- mirror_axes(obj)
obj <- translate_positions(obj, c(33.5, 0, 47.75))
dt_player <- get_player_log_trial(obj, 2)
dt_player[, Time := Time-dt_player$Time[1]]