library(shiny)
library(glue)

## globals
play_once <- function() {
  flips <- sample(c("Heads", "Tails"), size = 2, replace = TRUE)
  possible_winnings <- c(-1, 0, 2)
  number_heads <- sum(flips == "Heads")
  winnings <- possible_winnings[number_heads + 1]
  list(flips = flips, winnings = winnings)
}

## ui
ui <- pageWithSidebar(
  headerPanel = titlePanel("Coin-Flipping Game"),
  sidebarPanel = sidebarPanel(
    actionButton("go", "Play Game"),
    actionButton("restart", "Start Over")
  ),
  mainPanel = mainPanel(verbatimTextOutput("report"))
)


## server logic
server <- function(input, output) {
  
  rv <- reactiveValues(
    total = 0,
    plays = 0,
    msg = ""
  )
  
  observeEvent(input$restart, {
    rv$total <- 0
    rv$plays <- 0
    rv$msg <- ""
  })
  
  observeEvent(input$go, {
    res <- play_once()
    rv$total <- rv$total + res$winnings
    rv$plays <- rv$plays + 1
    avg <- round(rv$total/ rv$plays, digits = 3)
    msg <- glue(
      "You got {res$flips[1]} and then {res$flips[2]}.
       You receive {res$winnings} dollars.
      
       After {rv$plays} play(s), your net winnings are:  {rv$total} dollars.
       Your average winnings per play so far is:  {avg} dollars."
    )
    print(msg)
    rv$msg <- msg
  })

  output$report <- renderText({
    rv$msg
  })
}

# run it
shinyApp(ui = ui, server = server)