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
obj <- translate_positions(obj, c(33.5, 0, 47.75))
obj <- mirror_axes(obj)

## ------------------------------------------------------------------------
#set constraints
obj$data$position$area_boundaries <- list(x = c(-5, 105), y = c(-5, 105))

