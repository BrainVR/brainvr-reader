context("Loading")

test_that("Loading from a folder",{
  obj_loaded <- load_experiment("../../inst/extdata/")
  expect_s3_class(obj_loaded, "brainvr")
})