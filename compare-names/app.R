library(shiny)
library(babynames)
library(tidyverse)

cleanup <- function(strs) {
  strs |> 
    str_trim() |> 
    str_to_lower()
}

## user interface:
ui <- fluidPage(
  titlePanel("Compare Two Names"),
  sidebarLayout(
    sidebarPanel(
      textInput("name1", label = "First Name:"),
      textInput("name2", label = "Second Name:"),
      radioButtons(
        "babysex",
        label = "Sex of Baby:",
        choices = c("Female" = "F", "Male" = "M")
      ),
      actionButton("go", "Make the Line Graph")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

## server logic:
server <- function(input, output) {
  
  ## this reactive makes only the data, not the plot:
  cleaned_names <- eventReactive(input$go, {
    cleanup(c(input$name1, input$name2))
  })
  
  output$plot <- renderPlot({
    ## this element will run when the reactive changes:
    cleaned <- cleaned_names()
    ## get the pre-cleaned names and the sex, 
    ## but isolate them so the element won't
    ## run when the user is still working on names and sex:
    isolate({
      name1 <- input$name1
      name2 <- input$name2
      baby_sex <- input$babysex
    })
    ## find how many babies there are for each name:
    n1 <- babynames |> filter(str_to_lower(name) == cleaned[1]) |> nrow()
    n2 <- babynames |> filter(str_to_lower(name) == cleaned[2]) |> nrow()
    ## if there are no babies for one or both names, stop, with
    ## a helpful message to the user:
    full_sex <- ifelse(
      baby_sex == "M",
      "male",
      "female"
    )
    validate(
      need(
        n1 > 0 & n2 > 0,
        message = case_when(
          n1 == 0 & n2 > 0 ~ paste0("There are no ", full_sex, " babies with a name like \"", name1, "\"."),
          n1 > 0 & n2 == 0 ~ paste0("There are no ", full_sex, " babies with a name like \"", name2, "\"."),
          n1 == 0 & n2 == 0 ~ paste0("There are no ", full_sex, " babies with names like these!")
        )
      )
    )
    ## if we got this far, it's OK to make the dataset and graph it:
    babynames |> 
      filter(
        sex == baby_sex & str_to_lower(name) %in% cleaned
      ) |> 
      mutate(perc = prop * 100) |> 
      ggplot(aes(x = year, y = perc)) +
      geom_line(aes(color = name)) +
      labs(x = NULL, y = "Percentage")
  })
}


## run the app:
shinyApp(ui, server)

