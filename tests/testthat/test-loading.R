context("Loading")

test_that("Loading from a folder",{
  expect_silent(load_experiment("inst/ext/"))
})