ui <- fluidPage(
  selectInput(inputId = "selectTrial", label = "Select trial to plot", choices = c(1:3)),
  sliderInput(inputId = "sliderTime", label = "Slide to follow trial time", 
              value = 1, min = 1, max = nrow(dt_player)),
  plotOutput(outputId = "plotTrialPath"),
  plotOutput(outputId = "plotRotation")
)