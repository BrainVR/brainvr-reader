context('Visualising')
obj <- brainvr_object

test_that("TEsts no errors in charts",{
  for(i in 1:18){
    expect_silent(plot_trial_path(obj, i))
  }
})