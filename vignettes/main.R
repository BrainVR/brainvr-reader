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
changed <- preprocess_player_log(obj$data$player_log)
if(changed) save_preprocessed_player(dir_path, obj$data$player_log, obj$timestamp)

## ------------------------------------------------------------------------
obj <- load_experiment(dir_path)

## ------------------------------------------------------------------------
obj <- translate_positions(obj, c(33.5, 0, 47.75))

## ------------------------------------------------------------------------
#set constraints
obj$map_size <- c()
make_trial_image(obj, 1)

