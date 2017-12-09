ui <- fluidPage(
  sliderInput(inputId = "sliderTrial", label = "Select trial to plot", 
              value = 1, min = 1, step = 1, max = n_trials),
  sliderInput(inputId = "sliderTime", label = "Slide to follow trial time", 
              value = 2, min = 2, max = nrow(dt_player), 
              animate = animationOptions(interval = 250, loop = FALSE)),
  plotOutput(outputId = "plotTrialPath"),
  plotOutput(outputId = "plotRotation")
)