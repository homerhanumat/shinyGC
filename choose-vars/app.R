library(shiny)
library(bslib)
library(ggplot2)


cat_vars <- c("species", "island", "sex")
num_vars <- setdiff(names(penguins), cat_vars)

ui <- page_sidebar(
  title = "Penguin Violins",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    width = 280,
    selectInput("numvar", "Numerical variable (y-axis)", choices = num_vars),
    selectInput("catvar", "Categorical variable (x-axis)", choices = cat_vars),
  ),
  card(
    full_screen = TRUE,
    card_header("Violin Plot"),
    plotOutput("violin")
  )
)

server <- function(input, output, session) {

output$violin <- renderPlot({
  ggplot(penguins, aes(x = .data[[input$catvar]], y = .data[[input$numvar]])) +
    geom_violin() +
    geom_jitter(width = 0.15, alpha = 0.4) +
    labs(x = input$cat_var, y = input$num_var, title = input$dataset)
  })
}

shinyApp(ui, server)
