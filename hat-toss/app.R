library(shiny)
library(shinyjs)
library(glue)
library(DT)

## globals

## ui
ui <- pageWithSidebar(
  headerPanel = titlePanel("Hat Toss"),
  sidebarPanel = sidebarPanel(
    numericInput("n", "Number of People Involved", value = 10, min =1, step = 1),
    actionButton("go", "Toss the Hats!"),
    hidden(actionButton("restart", "Start Over"))
  ),
  mainPanel = mainPanel(
    verbatimTextOutput("report"),
    dataTableOutput("df"),
    useShinyjs()
  )
)


## server logic
server <- function(input, output) {
  
  rv <- reactiveValues(
    occurred = 0,
    plays = 0,
    msg = "",
    df = NULL
  )
  
  observeEvent(input$restart, {
    rv$occurred <- 0
    rv$plays <- 0
    rv$msg <- ""
    rv$df <- NULL
    hide("restart")
    show("n")
  })
  
  observeEvent(input$go, {
    hide("n")
    show("restart")
    people <- 1:input$n
    hats <- sample(people, size = input$n, replace = FALSE)
    rv$occurred <- rv$occurred + any(people == hats)
    rv$plays <- rv$plays + 1
    avg <- round(rv$occurred/rv$plays, digits = 3)
    rv$df <- data.frame(
      person = people,
      hat_picked_up = hats,
      response = ifelse(
        people == hats,
        "Woo-hoo!  I got my own hat!",
        "meh"
      )
    )
    # msg <- glue(
    #   "After {rv$plays} tosses, at least one person got his/her own hat:  {rv$occured} times.
    #    The proportion of times this has occurred is:  {avg}."
    # )
    msg <- glue::glue(
      "After {rv$plays} tosses, at least once person got his/her own hat {rv$plays} times.
      The proportion of times this happened is:    {avg}."
    )
    rv$msg <- msg
  })
  
  output$report <- renderText({
    rv$msg
  })
  
  output$df <- renderDT({
    req(!is.null(rv$df))
    rv$df
  }, rownames = FALSE)
}

# run it
shinyApp(ui = ui, server = server)