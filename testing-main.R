library(ggpubr)
library(ggplot2)
library(brainvr.R)
dir_path <- "inst/extdata/"
obj <- load_experiment(dir_path, exp_timestamp = '17-41-52-03-12-2017')
if(!is_player_preprocessed(obj$data$player_log)){
  obj$data$player_log <- preprocess_player_log(obj$data$player_log, type = "virtualizer")
  save_preprocessed_player(dir_path, obj$data$player_log, obj$timestamp)
}
obj <- translate_positions(obj, c(33.5, 0, 47.75))
obj <- mirror_axes(obj)
obj <- resize_layout(obj, 0.01)

trialId <- 2
pth <- plot_trial_path(obj, trialId)
dt_player <- get_trial_log(obj, trialId)
rot <- ggplot(dt_player, aes(Time, Rotation.Virtualizer)) + geom_line()
ggarrange(pth, rot,ncol = 1, nrow = 2)

obj$map_limits <- list(x = c(-2, 105), y = c(0, 100))

## Pointing ----
sop_dir <- "D:/GoogleDrive/Davis/Data/pilot/neo1/"
sop <- load_experiment(sop_dir, exp_timestamp = '18-07-09-03-12-2017')

##
obj <- add_goal_positions(obj, obj$data$experiment_log$positions$GoalPositions)
obj <- add_goal_order(obj, obj$data$experiment_log$settings$GoalOrder + 1)

