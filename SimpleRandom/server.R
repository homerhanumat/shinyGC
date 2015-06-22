library(shiny)
library(tigerstats,quietly=TRUE)

source("plot.R")

shinyServer(function(input, output,session) {
  
  session$total <- 0
  session$totalPrev <- 0
  session$newVar <- TRUE
  session$sample <- NULL

  update1 <- observe({
    input$sample
    if (input$sample > 0) {
    session$newVar <- FALSE
    print(session$newVar)
    }
  })
  
  
  update2 <- observe({
    input$reset
    session$newVar <- TRUE
  })
  
  
  
  plotting <- eventReactive(input$sample,tryThis)
  output$plot <- renderPlot({
    
    if (input$sample > 0) {
      form <- as.formula(paste0("~",input$variable))
      print(histogram(form,data=imagpop))
    }
  })
})
