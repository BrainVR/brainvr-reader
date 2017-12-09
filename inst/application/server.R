server <- function(input, output, session){
  
  trialPlayer <- reactive({
    dt_trial <- get_player_log_trial(obj, input$sliderTrial)
    log_nrow <<- nrow(dt_trial)
    trial_end <<- dt_trial$Time[nrow(dt_trial)]
    #set time slider to 0 (it will actually fall to min)
    updateSliderInput(session, "sliderTime", value = 0, max = log_nrow)
    return()
  })
  
  subPlayerPos <- reactive({
    dt_player <- trialPlayer()
    return(dt_player[1:input$sliderTime, ])
  })
  
  output$plotTrialPath <- renderPlot({
    ggplot(subPlayerPos(), aes(Position.x, Position.z)) + geom_path() + 
    xlim(-5, 110) + ylim(0, 100) + theme_void()
  })
  
  output$plotRotation <- renderPlot({
    data <- subPlayerPos()
    end <- trial_end
    ggplot(data, aes(Time, Rotation.Virtualizer)) + geom_line() + 
      xlim(data$Time[1], end) + ylim(0, 360) + theme_bw()
  })
}
