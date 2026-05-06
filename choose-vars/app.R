library(shiny)
library(bslib)
library(ggplot2)

qualifying_datasets <- c("ChickWeight", "CO2", "esoph", "penguins", "penguins_raw")

get_dataset <- function(nm) get(nm, envir = as.environment("package:datasets"))

ui <- page_sidebar(
  title = "Dataset Explorer",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    width = 280,
    selectInput("dataset", "Dataset", choices = c("-- select --" = "", qualifying_datasets)),
    conditionalPanel(
      condition = "input.dataset != ''",
      selectInput("num_var", "Numerical variable (y-axis)", choices = NULL),
      selectInput("cat_var", "Categorical variable (x-axis)", choices = NULL)
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Violin Plot"),
    plotOutput("violin")
  )
)

server <- function(input, output, session) {
  current_data <- reactive({
    req(input$dataset)
    get_dataset(input$dataset)
  })

  observeEvent(current_data(), {
    df <- current_data()
    num_vars <- names(Filter(is.numeric, df))
    cat_vars <- names(Filter(function(x) is.factor(x) || is.character(x), df))
    updateSelectInput(session, "num_var", choices = num_vars)
    updateSelectInput(session, "cat_var", choices = cat_vars)
  })

  output$violin <- renderPlot({
    req(input$dataset != "", input$num_var, input$cat_var)
    df <- current_data()
    ggplot(df, aes(x = .data[[input$cat_var]], y = .data[[input$num_var]])) +
      geom_violin() +
      geom_jitter(width = 0.15, alpha = 0.4) +
      labs(x = input$cat_var, y = input$num_var, title = input$dataset)
  })
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
