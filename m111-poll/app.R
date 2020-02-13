library(shiny)
library(glue)
library(shinyjs)
library(ggplot2)
library(plotly)
library(dplyr)



# which fields get saved
fieldsAll <- c("game_once", "reason_once", "game_lots", "reason_lots")

# which fields are mandatory
fieldsMandatory <- c("game_once", "game_lots")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf(
    "%s_%s.csv",
    humanTime(),
    digest::digest(data)
  )

  write.csv(
    x = data, file = file.path(responsesDir, fileName),
    row.names = FALSE, quote = TRUE
  )
}

# load all recent responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- do.call(rbind, data)
  data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
  df <- data %>% 
    filter(difftime(Sys.time(), timestamp, units = "secs") < 600)
  df
}

# directory where responses get stored

responsesDir <- file.path("responses")

if (!dir.exists("responses")) {
  dir.create("responses")
}

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "



ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  title = "MAT 111 Poll",
  div(
    id = "header",
    h1("A MAT 111 Poll"),
    fluidRow(
      column(
        4,
        div(
          id = "form",
          helpText(glue::glue(
            "You can play a game where you toss a fair coin. 
                 If the coin lands Heads, you win $12.  If it lands Tails 
                 then you lose $10."
          )),
          radioButtons("game_once", labelMandatory("Would you play the game once?"),
            choices = c("Yes", "No", "I cannot decide!"),
            selected = ""
          ),
          textAreaInput("reason_once", "Briefly explain your choice"),
          helpText(glue::glue(
            "Suppose that you could play the game 2000 times, not paying
                 and receiving money as you go but instead
                 settling up AFTER all 200 tosses."
          )),
          radioButtons("game_lots", labelMandatory("Would you play the game 2000 tmes?"),
            choices = c("Yes", "No", "I cannot decide!"),
            selected = ""
          ),
          textAreaInput("reason_lots", "Briefly explain your choice"),
          actionButton("submit", "Submit", class = "btn-primary"),

          shinyjs::hidden(
            span(id = "submit_msg", "Submitting..."),
            div(
              id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
            )
          )
        ),

        shinyjs::hidden(
          div(
            id = "thankyou_msg",
            h3("Thanks, your response was submitted successfully!"),
            actionButton("show", "Show responses")
          )
        )
      ),
      column(
        8,
        column(
          6,
          shinyjs::hidden(div(
            id = "results1",
            plotlyOutput("plot1"),
            HTML("<h2>Reasons Offered</h2>"),
            htmlOutput("reasons1")
          ))
          
        ),
        column(
          6,
          shinyjs::hidden(div(
            id = "results2",
            plotlyOutput("plot2"),
            HTML("<h2>Reasons Offered</h2>"),
            tableOutput("reasons2")
          )
        ))
      )
    )
  )
)


server <- function(input, output, session) {

  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(
        fieldsMandatory,
        function(x) {
          !is.null(input[[x]]) && input[[x]] != ""
        },
        logical(1)
      )
    mandatoryFilled <- all(mandatoryFilled)

    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })

  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {

    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")

    # Save the data (show an error message in case of error)
    tryCatch(
      {
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
        shinyjs::show("show")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      }
    )
  })

  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
  observeEvent(input$show, {
    shinyjs::show("results1")
    shinyjs::show("results2")
  })


 # Show the responses in the admin table
output$plot1 <- renderPlotly({
    req(input$show)
    data <- loadData()
    dataSummary <-
      data %>% 
      group_by(game_once) %>% 
      summarize(count = n())
    p<-
      ggplot(dataSummary, aes(x = game_once, y = count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Play once?",
           x = NULL)
    ggplotly(p, tooltip = "count") %>% config(displayModeBar = FALSE)
  })
  
  output$plot2 <- renderPlotly({
      req(input$show)
      data <- loadData()
      p<-
        ggplot(data, aes(x = game_lots)) +
        geom_bar(fill = "skyblue") +
        labs(title = "Play 2000 times?",
             x = NULL)
      ggplotly(p) %>% config(displayModeBar = FALSE)
      })
  
  output$reasons1 <- renderTable({
    req(input$show)
    data <- loadData()
    data %>% 
      select(game_once, reason_once) %>% 
      rename(response = game_once, reason = reason_once)
    # responses <- data$reason_once[!is.na(data$reason_once)]
    # stuff <- paste(responses, collapse = "</p><p>")
    # paste0("<p>", stuff, "</p>", collapse = "")
  }, rownames = FALSE)
  
  output$reasons2 <- renderTable({
    req(input$show)
    data <- loadData()
    data %>% 
      select(game_lots, reason_lots) %>% 
      rename(response = game_lots, reason = reason_lots)
    # responses <- data$reason_lots[!is.na(data$reason_lots)]
    # stuff <- paste(responses, collapse = "</p><p>")
    # paste0("<p>", stuff, "</p>", collpase = "")
  }, rownames = FALSE)
  
}

shinyApp(ui, server)
