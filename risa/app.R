library(shiny)

## globals
range <- 20
risa_plot <- function(picard, laris, dist, attempts) {
  # Set up the plotting field
  plot(1, type = "n", xlim = c(0, 100), ylim = c(0, 100),
       xlab = "", ylab = "", axes = FALSE, asp = 1)
  # add surrounding box back
  box()
  # Draw transparent beige circles
  symbols(picard[1], picard[2], circles = range,
          inches = FALSE, add = TRUE,
          bg = adjustcolor("beige", alpha.f = 0.6),
          fg = "black")
  # symbols(laris[1], laris[2], circles = 10,
  #         inches = FALSE, add = TRUE,
  #         bg = adjustcolor("beige", alpha.f = 0.6),
  #         fg = "black")
  # Add the centers:
  points(picard[1], picard[2], pch = 19, cex = 0.2)
  points(laris[1], laris[2], pch = 19, cex = 0.2)
  # Add labels for clarity
  text(picard[1], picard[2] + 2, labels = "Picard", col = "red", cex = 0.8)
  text(laris[1], laris[2] + 2, labels = "Laris", col = "blue", cex = 0.8)
  plot_title <- ifelse(
    dist < range,
    paste0("Attempt #", attempts, ": they meet!"),
    paste0("Attempt #", attempts, ": they miss each other.  Sad!")
  )
  title(main = plot_title)
}

## ui
ui <- pageWithSidebar(
  headerPanel = titlePanel("Risa Rendevous"),
  sidebarPanel = sidebarPanel(
    actionButton("go", "Try to Meet!")
  ),
  mainPanel = mainPanel(plotOutput("risaPlot"))
)


## server logic
server <- function(input, output) {
  rv <- reactiveValues(
    attempts = 0,
    distance = Inf,
    picard = NULL,
    laris = NULL,
    trying = FALSE
  )
  
  observeEvent(input$go, {
    rv$distance <- Inf
    rv$attempts <- 0
    rv$picard <- NULL
    rv$laris <- NULL
    rv$trying <- TRUE
  })
  
  observe({
    invalidateLater(1000)
    isolate({
      req(rv$trying)
      rv$picard <- runif(2, min = 0, max = 100)
      rv$laris <- runif(2, min = 0, max = 100)
      rv$attempts <- rv$attempts + 1
      px <- rv$picard[1]
      py <- rv$picard[2]
      lx <- rv$laris[1]
      ly <- rv$laris[2]
      dist <- sqrt((px - lx)^2 + (py - ly)^2)
      rv$distance <- dist
      print(rv$attempts)
      print(rv$distance)
      if (dist < range) {
        rv$trying <- FALSE
      }
    })
  })
  
  output$risaPlot <- renderPlot(
    {
      if (rv$attempts > 0) {
        isolate({
          risa_plot(
            picard = rv$picard, 
            laris = rv$laris, 
            dist = rv$distance,
            attempts = rv$attempts
          )
        })
      }
    },
    width = 400,
    height = 400
  )
}

# run it
shinyApp(ui = ui, server = server)