library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$dens <- renderPlot({
    ggplot(mtcars,aes(x=wt))+
      geom_density(fill="burlywood",kernel=input$kernel,adjust=input$adjust)+
      labs(y="density",x="Weight (tons)",title="Weights of Some Vehicles")+geom_rug()
    
  })
})