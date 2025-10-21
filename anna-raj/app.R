library(shiny)
library(glue)

## globals
play_once <- function() {
  list(
    anna = runif(1, min = 0, max = 60),
    raj = runif(1, min = 0, max = 60)
  )
}

meetup_plot <- function(anna, raj) {
  # Ensure that anna and raj are between 0 and 60
  if (any(c(anna, raj) < 0 | c(anna, raj) > 60)) {
    stop("Both anna and raj must be between 0 and 60.")
  }
  
  # Determine whether they meet
  meet <- abs(anna - raj) < 10
  
  # Set up the plot window
  plot(0, 0, type = "n",
       xlim = c(0, 60), ylim = c(0, 3),
       xlab = "Minutes after 10am", ylab = "",
       axes = FALSE)
  axis(1, at = seq(0, 60, by = 10))
  
  # Draw Anna’s segment (from anna to max(anna + 10, 60))
  segments(anna, 1.5, min(anna + 10, 60), 1.5, lwd = 3, col = "blue")
  points(anna, 1.5, pch = 19, col = "blue")
  text(anna, 1.7, labels = "Anna", col = "blue", cex = 0.8, pos = 3)
  
  # Draw Raj’s segment (from raj to max(raj + 10, 60))
  segments(raj, 2.5, min(raj + 10, 60), 2.5, lwd = 3, col = "red")
  points(raj, 2.5, pch = 19, col = "red")
  text(raj, 2.7, labels = "Raj", col = "red", cex = 0.8, pos = 3)
  
  # Add the title
  if (meet) {
    title(main = "They meet!", col.main = "darkgreen", font.main = 2)
  } else {
    title(main = "They do not meet: sad!", col.main = "darkred", font.main = 2)
  }
}

## ui
ui <- pageWithSidebar(
  headerPanel = titlePanel("Anna and Raj at the Cafe"),
  sidebarPanel = sidebarPanel(
    actionButton("go", "Try to Meet"),
    actionButton("restart", "Start Over")
  ),
  mainPanel = mainPanel(
    plotOutput("plot"),
    verbatimTextOutput("report")
  )
)


## server logic
server <- function(input, output) {
  
  rv <- reactiveValues(
    times = NA,
    total = 0,
    plays = 0,
    msg = ""
  )
  
  observeEvent(input$restart, {
    rv$times <- NA
    rv$total <- 0
    rv$plays <- 0
    rv$msg <- ""
  })
  
  observeEvent(input$go, {
    times <- play_once()
    rv$times <- times
    rv$total <- rv$total + (abs(times$anna - times$raj) < 10)
    rv$plays <- rv$plays + 1
    avg <- round(rv$total/ rv$plays, digits = 3)
    msg <- glue(
      "After {rv$plays} tries, they met:  {rv$total} times.
       The proportion of times they have met so far is:  {avg}."
    )
    print(msg)
    rv$msg <- msg
  })

  output$report <- renderText({
    rv$msg
  })
  
  output$plot <- renderPlot({
    req(!is.na(rv$times))
    meetup_plot(anna = rv$times[[1]], raj = rv$times[[2]])
  })
}

# run it
shinyApp(ui = ui, server = server)