server <- function(input, output, session){
  
  trialPlayer <- reactive({
    dt_trial <- get_player_log_trial(obj, input$sliderTrial)
    #set time slider to 0 (it will actually fall to min)
    updateSliderInput(session, "sliderTime", value = 0, max = nrow(dt_trial))
    #change slider
    return(dt_trial)
  })
  
  subPlayerPos <- reactive({
    dt_player <- trialPlayer()
    return(dt_player[1:input$sliderTime, ])
  })
  
  output$plotTrialPath <- renderPlot({
    ggplot(subPlayerPos(), aes(Position.x, Position.z)) + geom_path() + 
    xlim(-2, 110) + ylim(0, 100) + theme_void()
  })
}
