library(ggpubr)
library(ggplot2)
dir_path <- "inst/extdata/"
obj <- load_experiment(dir_path)
changed <- preprocess_player_log(obj$data$player_log)
if(changed) save_preprocessed_player(dir_path, obj$data$player_log, obj$timestamp)
obj <- mirror_axes(obj)
obj <- translate_positions(obj, c(33.5, 0, 47.75))

trialId <- 2
pth <- make_trial_image(obj, trialId)
dt_player <- get_player_log_trial(obj, trialId)
rot <- ggplot(dt_player, aes(Time, Rotation.Virtualizer)) + geom_line()
ggarrange(pth, rot,ncol = 1, nrow = 2)
