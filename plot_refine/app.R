library(shiny)
library(tidyverse)


ui <- pageWithSidebar(
  headerPanel = headerPanel(
    "Polish This Plot!"
  ),
  sidebarPanel = sidebarPanel(
    helpText(HTML(glue::glue("
                             This is a plot of eruption duration (x-axis)
                             vs. waiting time until the next eruption (y-axis)
                             for eruptions of the  Old Faithful geyser.
                             Run <code>?faithful</code> for details."))),
    selectInput("color", "Point Color", choices = colors(),
          selected = "black"),
    numericInput("size", "Point Size", value = 1, 
                 min = 0, max = 10, step = 0.1),
    textInput("x_axis", "x-axis Label", placeholder = "your label here"),
    textInput("y_axis", "y-axis Label", placeholder = "your label here"),
    textInput("title", "Plot Title", placeholder = "your title here")
  ),
  mainPanel = mainPanel(
    plotOutput("plot")
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    faithful %>% 
      ggplot(aes(x = eruptions, y = waiting)) +
      geom_point(color = input$color, size = input$size) +
      labs(
        x = input$x_axis,
        y = input$y_axis,
        title = input$title
      )
  })
}

shinyApp(ui, server)