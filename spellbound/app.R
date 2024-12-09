## spellbound (primitive version)

library(shiny)
library(stringr)
library(shinyjs)
library(words)
library(glue)

## Globals ----

## time allowed (sec):
time_allowed <- 60
lexicon <- words::words$word
## number of letters provided:
n <- 12
# weight on chance of vowel:
vowel_weight <- 5
## function to determine if a word can be formed from letters
check_entry <- function(entry, letters) {
  cl <- str_c(letters, collapse = "")
  entry_letters <- str_split(entry, pattern = "") |> unlist() |> unique()
  formable <- TRUE
  for (letter in entry_letters) {
    if (str_count(entry, letter) > str_count(cl, letter)) {
      formable <- FALSE
    }
  }
  list(formable = formable, is_word = entry %in% lexicon)
}

## ui ----
ui <- pageWithSidebar(
  headerPanel("Spellbound"),
  sidebarPanel(
    useShinyjs(),
    actionButton("play", "Play Spellbound"),
    hidden(
      div(
        id = "controls",
        textInput("entry", "Make a word:"),
        actionButton("submit", "Submit Entry")
      )
    )
  ),
  mainPanel(
    hidden(
      div(
        id = "info",
        verbatimTextOutput("letters"),
        verbatimTextOutput("response")
      )
    )
  )
)

## server logic ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    letters = NULL,
    words = character(),
    playing = FALSE,
    time = Sys.time(),
    score = 0,
    attempts = 0,
    results = NULL
  )
  
  observeEvent(input$play, {
    vowels <- c("a", "e", "i", "o", "u")
    cons <- setdiff(letters, vowels)
    probs <- c(rep(vowel_weight, length(vowels)), rep(1, length(cons)))
    rv$letters <- sample(letters, size = n, replace = TRUE, prob = probs)
    rv$words <- character()
    rv$playing <- TRUE
    rv$time <- Sys.time()
    rv$score <- 0
    rv$attempts <- 0
    updateTextInput(session, "entry", value = "")
    hide("play")
    show("controls")
    show("info")
  })
  
  
  observe({
    req(input$play)
    invalidateLater(100, session)
    d <- as.numeric(difftime(Sys.time(), rv$time, units = "secs"))
    print(d)
    if (d > time_allowed) {
      rv$playing <- FALSE
      hide("controls")
      show("play")
    }
  })

  
  
  observeEvent(input$submit, {
    entry <- input$entry
    rv$attempts <- rv$attempts + 1
    updateTextInput(session, "entry", value = "")
    check <- check_entry(entry = entry, letters = rv$letters)
    repeated <- entry %in% rv$words
    if (check$formable & check$is_word & !(repeated)) {
      rv$words <- c(rv$words, entry)
      rv$score <- rv$score + str_length(entry)
    }
    rv$results <- c(list(entry = entry, dup = repeated), check)
    rv$attempts <- rv$attempts + 1
  })
  
  
  output$letters <- renderText({
    letter_list <- str_c(rv$letters, collapse = ", ")
    glue::glue("
      Letters are:
      
      {letter_list}.
                 
      Submit a word made from these letters!")
  })
  
  output$response <- renderText({
    rv$attempts
    isolate({
      res <- rv$results
      prev <- str_c(rv$words, collapse = "\n")
      score <- rv$score
      attempts <- rv$attempts
    })
    if (attempts == 0 & rv$playing) {
      return(NULL)
    }
    if (attempts == 0 & !rv$playing) {
      return(glue::glue("
                        Time's up!  Words made:
                        
                        {prev}"))
    }
    if (!rv$playing) {
      initial <- "Time's up! "
    }  else if (res$dup) {
      initial <- glue::glue("\"{res$entry}\" has already been submitted. ")
    } else if (res$formable & !(res$is_word)) {
      initial <- glue::glue("\"{res$entry}\" is not a word. ")
    } else if (!res$formable & !(res$is_word)) {
      initial <- glue::glue(
        "\"{res$entry}\" isn't a word and can't be made from the letters. ")
    } else if (!res$formable & (res$is_word)) {
      initial <- glue::glue(
        "\"{res$entry}\" cannot be made from the given letters. ")
    } else {
      initial <- "Good entry! "
    }
    msg <- glue::glue(
      "{initial} Your score is {score}.  Words made so far:
      
      {prev}"
    )
    msg
  })
  
}


## run app ----
shinyApp(ui, server)