library(shiny)
library(tigerstats)


shinyUI(fluidPage(
  
  titlePanel("Simple Random Sampling from Imagpop"),
  
  sidebarLayout(
    
    sidebarPanel(width=3,
      conditionalPanel(
        condition="output.newVar == true",
        radioButtons("variable","Variable to study:",selected="income",
                     c(names(imagpop)))
        ),
      numericInput("n","Desired Sample Size:",value=100,min=2,max=1000),
      actionButton("sample",HTML("<strong>Take the Sample!</strong>")),
      conditionalPanel(
        condition="session.newVar == false",
        actionButton("reset",HTML("<strong>Start Over</strong>"))
      )
    ),
    
    mainPanel(width=9,
      
      plotOutput("plot"),
      tableOutput("table")
      
  )
  
  )
  
))
