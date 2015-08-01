library(shiny)
library(tigerstats)
library(shinythemes)

fluidPage(
  titlePanel("Simple Random Sampling from Imagpop"),
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      width=3,
      conditionalPanel(
        condition="input.sample == 0 || output.newVar == true",
        radioButtons("variable","Variable to study:",selected="income",
                     c(names(imagpop)))
        ),
      helpText("Enter desired sample size below:  anything from 4 to 1000."),
      numericInput("n", "Desired Sample Size:", value=25, min=4, max=1000),
      actionButton("sample", HTML("<strong>Take the Sample!</strong>")),
      conditionalPanel(
        condition="output.newVar == false",
        actionButton("reset", HTML("<strong>Start Over</strong>"))
      )
    ),
    mainPanel(
      width=9,
      conditionalPanel(
        condition = "output.newVar == true",
        plotOutput("initialPlot"),
        tableOutput("initialTable")
      ),
      conditionalPanel(
        condition = "output.newVar == false",
        plotOutput("plot"),
        tableOutput("table")
        )
      )
  
  )
)
