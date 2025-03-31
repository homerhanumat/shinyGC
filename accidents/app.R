##
## Stage 5
##
## For a more professional look
## apply styling from shinydashboard
## or similar package
##

library(tidyverse)
library(vroom)
library(shinydashboard)

## globals ----

table_bg_color <- "light-blue"
dashboard_skin <- "yellow"

if (!exists("injuries")) {
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  products <- vroom::vroom("neiss/products.tsv")
  population <- vroom::vroom("neiss/population.tsv")
}

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

## ui ----

ui <- dashboardPage(
  skin = dashboard_skin,
  dashboardHeader(title = "Injuries"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Tables/Stories", tabName = "tables", icon = icon("table")),
      menuItem(text = "Graph", tabName = "graph", icon = icon("chart-area"))
    ),
    br(),
    selectInput("code", "Product",
      choices = setNames(products$prod_code, products$title),
      width = "100%"
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tables",
        fluidRow(
          box(
            width = 4,
            title = "Diagnosis",
            status = "primary",
            solidHeader = TRUE,
            background = table_bg_color,
            tableOutput("diag")
          ),
          box(
            width = 4,
            title = "Body Part",
            status = "primary",
            solidHeader = TRUE,
            background = table_bg_color,
            tableOutput("body_part")
          ),
          box(
            width = 4,
            title = "Location of Injury",
            status = "primary",
            solidHeader = TRUE,
            background = table_bg_color,
            tableOutput("location")
          )
        ), # <<row
        br(),
        fluidRow(
          column(2, actionButton("story", "Tell me a story")),
          column(10, textOutput("narrative"))
        ) # <<row
      ), # <<tabItem,
      tabItem(
        tabName = "graph",
        fluidRow(
          box(
            width = 12,
            title = "Occurrences by Age and Gender",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("age_sex")
          ),
          br(),
          div(
            style = "margin-left: 20px;",
            tagList(
              selectInput("y", "Y axis", c("rate", "count"))
            )
          )
        ) # <<row
      ) # <<tabItem
    ) # <<tabItems
  ) # <<body
) # <<page

## server ----

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))

  # << tables
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  # >>

  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  # << plot
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_grey(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_grey(15)
    }
  })
  # >>

  # << narrative
  output$narrative <- renderText({
    input$story
    selected() %>%
      pull(narrative) %>%
      sample(1)
  })
  # <<
}

## make app ----

shinyApp(ui, server)
