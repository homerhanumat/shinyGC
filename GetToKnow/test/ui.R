library(shinyjs)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

navbarPage(
  shinyjs::useShinyjs(),
  title = "A Little Survey",
  tabPanel(
    title = "Survey",
    fluidPage(
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
    ),
  tabPanel(
    title = "Summary"
    # BLANK
    ),
  tabPanel(
    title = "Responses"
    # BLANK
    )
  )