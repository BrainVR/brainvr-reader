context("Loading")

HEADER <- system.file("extdata/example-header.txt", package = "brainvr.reader")
DIR <- system.file("extdata/CFNS/", package = "brainvr.reader")
TIMESTAMP = "17-41-52-03-12-2017"

test_that("Loading helpers", {
  expect_silent(header <- load_headers(file.path(HEADER)))
})
 
test_that("Loading from a folder", {
  dir_loaded <- load_experiments(DIR, save = FALSE)
  expect_length(dir_loaded, 2)
})

test_that("Loading player log works", {
  # Multiple in the same dir
  expect_warning(obj <- open_player_log(DIR, save = FALSE))
  expect_message(obj <- open_player_log(DIR, exp_timestamp = TIMESTAMP,
                                              save = FALSE))
  expect_s3_class(obj$data, "data.frame")
  expect_gt(nrow(obj$data), 100)
  expect_message(obj2 <- open_player_log(DIR, exp_timestamp = TIMESTAMP,
                                        override = TRUE, remove = FALSE,
                                        save = FALSE))
})

test_that("loading custom logs work", {
  results_filepath <- file.path(DIR, "NEO_results_CFNSLearning_17-41-52-03-12-2017.txt")
  expect_silent(results <- open_brainvr_log(DIR, log_name = "results",
                                            exp_timestamp = "17-41-52-03-12-2017"))
  expect_silent(results2 <- load_brainvr_log(results_filepath))
  expect_equal(results, results2)
})

test_that("Loading all separately works", {
  results_filepath <- file.path(DIR, "NEO_results_CFNSLearning_17-41-52-03-12-2017.txt")
  expect_silent(load_brainvr_log(results_filepath))
  test_filepath <- file.path(DIR, "NEO_test_CFNSLearning_17-41-52-03-12-2017.txt")
  expect_silent(load_experiment_log(test_filepath))
})

test_that("Loaded data have expected structure", {
  exps <- load_experiments(DIR)
  exp <- exps[[1]]
  expect_equal(names(exp$data), c("experiment_info", "position", 
                                  "experiment_log", "results_log"))
  expect_true(all(names(exp$data$experiment_info) %in% c("header","Experiment")))
})

test_that("Cannot use single funtions if multiple logs are in the same folder", {
  ## Cannot load experiment if there are multiple exp infos
  expect_error(load_experiment(DIR))
  expect_warning(out <- open_brainvr_log(DIR, "results"))
  expect_null(out)
})