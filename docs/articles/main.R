## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(brainvr.R)
dir_path <- system.file("extdata", package = "brainvr.R")
obj <- load_experiment(dir_path)

## ------------------------------------------------------------------------
if(!is_player_preprocessed(obj$data$player_log)){
  obj$data$player_log <- preprocess_player_log(obj$data$player_log)
  save_preprocessed_player(dir_path, obj$data$player_log, obj$timestamp)
}

## ------------------------------------------------------------------------
obj <- load_experiment(dir_path)

## ------------------------------------------------------------------------
obj <- translate_positions(obj, c(33.5, 0, 47.75))
obj <- mirror_axes(obj)


## ------------------------------------------------------------------------
#set constraints
obj$map_limits <- list(x = c(-5, 105), y = c(-5, 105))
plot_trial_path(obj, 1)
