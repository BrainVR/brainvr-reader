context("Loading")

DIR <- system.file("extdata", package = "brainvr.R")

test_that("Loading from a folder",{
  obj_loaded <- load_experiment(DIR)
  expect_s3_class(obj_loaded, "brainvr")
})