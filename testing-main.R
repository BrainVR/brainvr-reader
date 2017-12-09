dir_path <- "inst/extdata/"
obj <- load_experiment(dir_path)
changed <- preprocess_player_log(obj$data$player_log)
if(changed) save_preprocessed_player(dir_path, obj$data$player_log, obj$timestamp)
obj <- translate_positions(obj, c(33.5, 0, 47.75))

make_trial_image(obj, 1)
