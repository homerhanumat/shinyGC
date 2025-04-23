library(shiny)


ui <- fluidPage(
  titlePanel("Guessing Game"),
  numericInput(
    inputId = "guess",
    label = "Guess my secret number (1-10)",
    step = 1,
    value = 5,
    min = 1,
    max = 10
  ),
  actionButton(
    inputId = "go",
    label = "Submit Guess"
  ),
  verbatimTextOutput("response")
)

server <- function(input, output, session) {
  secret_number <- sample(1:10, size = 1)

  guess <- eventReactive(input$go, {
    input$guess
  })

  output$response <- renderText({
    if (guess() < secret_number) {
      msg <- "Your guess is too low."
    } else if (guess() > secret_number) {
      msg <- "Your guess is too high."
    } else {
      msg <- "You are correct!  Congratulations!"
    }
  })
}

shinyApp(ui, server)
