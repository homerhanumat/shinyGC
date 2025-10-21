library(shiny)
library(glue)

## globals

numbers_needed_plot <- function(numbs) {
  
  # Determine numbers sum to more than 1
  exceed <- sum(numbs) > 1
  
  # Set up the plot window
  plot(0, 0, type = "n",
       xlim = c(0, 2), ylim = c(0, 2),
       xlab = "Sum", ylab = "",
       axes = FALSE)
  axis(1, at = seq(0, 2, by = 0.2))
  
  segments(1, 0, 1, 3, lwd = 1, col = "black")
  
  total <- 0
  for (number in numbs) {
    segments(total, 1.5, total + number, 1.5, lwd = 3, col = "blue")
    points(total + number, 1.5, pch = 19, col = "red")
    total <- total + number
  }
  
  
  # Add the title
  if (exceed) {
    title(main = "The sum exceeds 1!", col.main = "darkgreen", font.main = 2)
  } else {
    title(main = "Get another number", col.main = "darkred", font.main = 2)
  }
}

## ui
ui <- pageWithSidebar(
  headerPanel = titlePanel("The Number of Numbers Needed"),
  sidebarPanel = sidebarPanel(
    actionButton("go", "Get a number"),
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
    numbers = numeric(),
    total = 0,
    plays = 0,
    msg = "",
    trying = TRUE
  )
  
  observeEvent(input$restart, {
    rv$numbers <- numeric()
    rv$msg <- ""
    rv$trying <- TRUE
  })
  
  observeEvent(input$go, {
    req(rv$trying)
    new_number <- runif(1)
    rv$numbers <- c(rv$numbers, new_number)
    so_far <- length(rv$numbers)
    #avg <- round(rv$total/ rv$plays, digits = 3)
    msg <- glue(
      "{so_far} numbers so far.  They are: 
        {paste(round(rv$numbers, digits = 3), collapse =', ')}."
    )
    if (sum(rv$numbers) > 1) {
      rv$trying <- FALSE
      rv$plays <- rv$plays + 1
      rv$total <- rv$total + length(rv$numbers)
      avg <- round(rv$total / rv$plays, digits = 3)
      more_msg <- glue(
        "
        
        The sum exceeds 1.  You are done with this try.
        You have exceeded 1 {rv$plays} times.
        The mean number of numbers needed so far is {avg}."
      )
      msg <- c(msg, more_msg)
    }
    rv$msg <- msg
  })

  output$report <- renderText({
    rv$msg
  })
  
  output$plot <- renderPlot({
    req(length(rv$numbers) > 0)
    numbers_needed_plot(numbs = rv$numbers)
  })
}

# run it
shinyApp(ui = ui, server = server)