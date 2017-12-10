library(ggpubr)
library(ggplot2)
dir_path <- "inst/extdata/"
obj <- load_experiment(dir_path)
changed <- preprocess_player_log(obj$data$player_log)
obj <- translate_positions(obj, c(33.5, 0, 47.75))
obj <- mirror_axes(obj)
if(changed) save_preprocessed_player(dir_path, obj$data$player_log, obj$timestamp)


trialId <- 2
pth <- make_trial_image(obj, trialId)
dt_player <- get_player_log_trial(obj, trialId)
rot <- ggplot(dt_player, aes(Time, Rotation.Virtualizer)) + geom_line()
ggarrange(pth, rot,ncol = 1, nrow = 2)


obj$map_limits <- list(x = c(-2, 105), y = c(0, 100))
plt <- create_plot(obj)
dt <- get_player_log_trial(obj, 2)
plt <- plot_add_player_path(plt, dt)
## adds start and end
plt <- plot_add_points(plt, list(start = c(50, 50)))
plt
