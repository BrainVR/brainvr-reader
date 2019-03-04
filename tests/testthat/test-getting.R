context('Getting elements')
obj <- brainvr_object

test_that("Can get parts of the object",{
  expect_type(get_experiment_log(obj)$Index, "integer")
  expect_type(get_experiment_settings(obj), "list")
  expect_type(get_experiment_settings(obj)$GoalNames, "character")
})

test_that("Can get trial parts",{
  expect_s3_class(get_trial_position(obj, 1), "navr")
  navr_1 <- get_trial_position(obj, 1)
  expect_identical(get_trial_log(obj, 1), navr_1$data)
})

test_that("Can get trial times", {
  expect_type(get_trial_times(obj, 1), "list")
})

test_that("Can get trial overviews stats",{
  expect_type(get_finished_trials(obj), "integer")
  expect_equal(max(get_finished_trials(obj)), 17)
})


test_that("Can get distance summary stats",{
  expect_type(get_trial_distance(obj, 2), "double")
})

test_that("Can get time summary stats", {
  expect_equal(round(get_trial_duration(obj, 2)), 94)
})

test_that("Can get trial times", {
  expect_equal(round(get_trial_event_times(brainvr_object, 1, "WaitingToStart")), 63713)
})