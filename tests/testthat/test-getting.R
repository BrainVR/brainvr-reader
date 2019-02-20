context('Getting elements')
obj <- brainvr_object

test_that("Can get parts of the object",{
  expect_type(get_experiment_log(obj)$Index, "integer")
})

test_that("Can get trial parts",{
  
})