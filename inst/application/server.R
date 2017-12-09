server <- function(input, output){
  
  subPlayerPos <- reactive({
    dt_player[1:input$sliderTime,]
  })
  
  output$plotTrialPath <- renderPlot({
    ggplot(subPlayerPos(), aes(Position.x, Position.z)) + geom_path() + 
    xlim(-2, 90) + ylim(0, 100)
  })
}
