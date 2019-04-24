context("Loading")

DIR <- system.file("extdata", package = "brainvr.R")

test_that("Loading from a folder",{
  dir_loaded <- load_experiments(DIR)
  expect_length(dir_loaded, 2)
  
  ## CAnnot load experiment if there are multiple exp infos
  expect_error(load_experiment(DIR))
})