test_that("Unity vector conversion works", {
  unity_vector <- "(1.0, 2.0, 1.0)"
  out <- unity_vector_to_numeric(unity_vector)
  expect_equal(out, c(1,2,1))
  expect_vector(out, size = 3) 
  
  unity_vector <- rep("(1.0, 2.0, 1.0)", 2)
  out <- unity_vector_to_numeric(unity_vector)
  
})