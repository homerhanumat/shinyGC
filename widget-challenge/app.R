library(shiny)

ui <- fluidPage(
  headerPanel("Various Widgets"),
  fluidRow(
    column(
      width = 6,
      helpText("This text-input widget has a placeholder where the name should go."),
      textInput("name", "Name", placeholder = "Your name here"),
      helpText("Here is a slider-input widget for dates!"),
      sliderInput("date", "When should we deliver the item?",
                value = as.Date("2019-08-10"), 
                min = as.Date( "2019-08-09"),
                max = as.Date("2019-08-16"),
                timeFormat = "%F"),
      helpText("This select-input groups the choices!"),
      selectInput("creature", "Pick your favorite creature",
                  choices = list(
                    mammal = list("horse", "pig", "dog"),
                    insect = list("ant", "beetle", "moth", "long-nosed weevil"),
                    reptile = list("sea turtle", "rattlesnake", "crocodile")
                  ))
    ),
    column(
      width = 6,
      helpText(glue::glue("This slider-input goes in steps of 5.
                          Push the arrow-button underneath to animate it!")),
      sliderInput("number", "Pick a Number!",
                  value = 10,
                  min = 0, max = 100,
                  step = 5,
                  animate = TRUE),
      helpText(glue::glue("If you use up/down arrows 
                          when the focus is in the numeric-input below,
                          the selected value increases/decreases by 50, but
                          will not go above 1000 or below 0.")),
      numericInput("number", "Select a value", value = 150, 
                   min = 0, max = 1000, step = 50),
      helpText(glue::glue("Finally, here is a textarea-input widget. 
                          You can enlarge it to write a whole lot!")),
      textAreaInput("bio", "Tell us a bit (or a lot) about yourself")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)