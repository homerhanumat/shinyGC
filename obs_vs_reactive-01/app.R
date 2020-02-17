## (1) Observers can produce side-effects.

## (2) Observers do not "return" values.

## (3) Observers will definitely re-run their
## code, whenever their dependency changes.

library(shiny)
library(ggplot2)

ui <- pageWithSidebar(
  headerPanel("With Observers and Reactive Values"),
  sidebarPanel(
    helpText(
      "Press both buttons quickly in succession.  You'll experience a
      12-second wait to see new results in the active panel."
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
  rv <- reactiveValues(
    a = NULL,
    b = NULL
  )
  observeEvent(input$a, {
    cat("Starting Task A ...\n")
    Sys.sleep(2)
    rv$a <- data.frame(results = rnorm(1000))
    cat("Completed Task A!\n")
  })
  observeEvent(input$a, {
    cat("Starting Task B ...\n")
    Sys.sleep(10)
    rv$b <-  data.frame(results = rnorm(1000))
    cat("Completed Task B!\n")
  })
  output$a_results <- renderPlot({
    req(rv$a)
    ggplot(rv$a, aes(x = results)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(title = paste0("This took 2 seconds to produce."))
  })
  output$b_results <- renderPlot({
    req(rv$b)
    ggplot(rv$b, aes(x = results)) +
      geom_histogram(fill = "burlywood", color = "black") +
      labs(title = paste0("This took 10 seconds to produce."))
  })
}

shinyApp(ui, server)