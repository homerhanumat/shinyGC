library(shiny)
library(shinyjs)

fieldsMandatory <- c("name")

ui <- navbarPage(
  #shinyjs::useShinyjs(),
  title = "A Little Survey",
  tabPanel(
    #shinyjs::useShinyjs(),
    title = "Survey",
    fluidPage(
      # shinyjs::useShinyjs(),
      conditionalPanel(
        condition = "input.submit == 0",
        textInput("name", "Name", ""),
        actionButton("submit", "Submit", class = "btn-primary")
      ),
      conditionalPanel(
        condition = "input.submit > 0",
        h3("Thanks for your submission!")
      )
    )
  ),
  tabPanel(
    title = "Map"
    # BLANK
  )
)

server <- function(input, output, session) {
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
}

shinyApp(ui = ui, server = server)