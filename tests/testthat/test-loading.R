context("Loading")

HEADER <- system.file("extdata/example-header.txt", package = "brainvr.reader")
DIR <- system.file("extdata/CFNS/", package = "brainvr.reader")

test_that("Loading helpers", {
  example_header <- file.path(HEADER)
  header <- load_headers(example_header)
})
 
test_that("Loading from a folder", {
  dir_loaded <- load_experiments(DIR)
  expect_length(dir_loaded, 2)
  ## Cannot load experiment if there are multiple exp infos
  expect_error(load_experiment(DIR))
})

test_that("Loading all separately works", {
  results_filepath <- file.path(DIR, "NEO_results_CFNSLearning_17-41-52-03-12-2017.txt")
  expect_silent(load_result_log(results_filepath))
  test_filepath <- file.path(DIR, "NEO_test_CFNSLearning_17-41-52-03-12-2017.txt")
  expect_silent(load_experiment_log(test_filepath))
})

test_that("Loaded data have expected structure", {
  exps <- load_experiments(DIR)
  exp <- exps[[1]]
  expect_equal(names(exp$data), c("experiment_info", "position", 
                                  "experiment_log", "results_log"))
  expect_equal(names(exp$data$experiment_info), c("header","Experiment"))
})