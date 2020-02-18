## (1)  Reactives do "return" values

## (2)  When their dependencies change, a reactive
## does not run right away.  It waits until something
## that depends on it needs to run.
#####

library(shiny)
library(ggplot2)


ui <- pageWithSidebar(
  headerPanel("With Reactives"),
  sidebarPanel(
    helpText(
      "Press both buttons quickly in succession.  Your wait should be only
      the time for the task in the active panel."
    ),
    actionButton("a", "Task A (2 sec)"),
    actionButton("b", "Task B (10 sec)")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "A Results", 
        wellPanel(plotOutput("a_results"))
      ),
      tabPanel(
        title = "B Results", 
        wellPanel(plotOutput("b_results"))
      )
    )
  )
)

server <- function(input, output, session) {
  a <- eventReactive(input$a, {
    Sys.sleep(2)
    data.frame(results = rnorm(1000))
  })
  b <- eventReactive(input$b, {
    Sys.sleep(10)
    data.frame(results = rnorm(1000))
  })
  output$a_results <- renderPlot({
    ggplot(a(), aes(x = results)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(title = paste0("This took 2 seconds to produce."))
  })
  output$b_results <- renderPlot({
    ggplot(b(), aes(x = results)) +
      geom_histogram(fill = "burlywood", color = "black") +
      labs(title = paste0("This took 10 seconds to produce."))
  })
}

shinyApp(ui, server)
