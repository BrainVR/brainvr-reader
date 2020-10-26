## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(brainvr.reader)
dir_path <- system.file("extdata/CFNS/", package = "brainvr.reader")
obj <- load_experiments(dir_path)
obj <- obj[[1]]

## -----------------------------------------------------------------------------
obj <- translate_positions(obj, c(33.5, 0, 47.75))
obj <- mirror_axes(obj)

## -----------------------------------------------------------------------------
#set constraints
obj$data$position$area_boundaries <- list(x = c(-5, 105), y = c(-5, 105))

