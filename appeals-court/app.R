# app.R
library(shiny)

ui <- fluidPage(
  titlePanel("Five-Judge Court Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      div(
        id = "prob_inputs",
        numericInput("pA", "Judge A Probability:", value = 0.95, min = 0, max = 1, step = 0.01),
        numericInput("pB", "Judge B Probability:", value = 0.94, min = 0, max = 1, step = 0.01),
        numericInput("pC", "Judge C Probability:", value = 0.9, min = 0, max = 1, step = 0.01),
        numericInput("pD", "Judge D Probability:", value = 0.9, min = 0, max = 1, step = 0.01),
        numericInput("pE", "Judge E Probability:", value = 0.8, min = 0, max = 1, step = 0.01),
        br(),
        checkboxInput("retire", "Retire Worst Judge", value = FALSE)
      ),
      
      br(),
      actionButton("simulate", "Simulate One Case"),
      actionButton("simulate1000", "Simulate 1000 Cases"),
      br(), br(),
      
      div(
        id = "reset_div",
        actionButton("reset", "Start Over")
      )
    ),
    
    mainPanel(
      h4("Simulation Results"),
      uiOutput("table_ui"),
      br(),
      verbatimTextOutput("summaryText")
    )
  )
)

server <- function(input, output, session) {
  
  vals <- reactiveValues(
    total_cases = 0,
    correct_cases = 0,
    last_results = NULL,
    show_table = FALSE,
    initialized = FALSE,
    retired_info = NULL
  )
  
  # JS helper to fade UI in/out
  toggle_elements <- function(show_inputs, show_reset) {
    session$sendCustomMessage(
      "fadeElements",
      list(show_inputs = show_inputs, show_reset = show_reset)
    )
  }
  
  # compute retired/doubled judges when checkbox is checked
  observe({
    if (!input$retire) {
      vals$retired_info <- NULL
    } else {
      probs <- c(input$pA, input$pB, input$pC, input$pD, input$pE)
      print(probs)
      max_idx <- which(probs == max(probs))
      print(max_idx)
      top <- ifelse(length(max_idx) == 1, max_idx, sample(max_idx, size = 1))
      min_idx <- which(probs == min(probs))
      print(min_idx)
      bottom <- ifelse(length(min_idx) == 1, min_idx, sample(min_idx, size = 1))
      print(bottom)
      res <- list(
        top = top,
        bottom = bottom
      )
      print(res)
      vals$retired_info <- res
    }
    
  })
  
  # Core simulation function
  simulate_case <- function(probs, retired_info) {
    votes <- rbinom(5, 1, probs) == 1
    weights <- rep(1, 5)
    if (!is.null(retired_info)) {
      weights[retired_info$top] <- 2
      weights[retired_info$bottom] <- 0
    }
    correct_votes <- sum(votes * weights)
    total_possible_votes <- sum(weights)
    court_correct <- correct_votes >= (total_possible_votes / 2)
    
    list(votes = votes, correct_votes = correct_votes, court_correct = court_correct)
  }
  
  # --- Simulate one case ---
  observeEvent(input$simulate, {
    probs <- c(input$pA, input$pB, input$pC, input$pD, input$pE)
    judges <- c("A", "B", "C", "D", "E")
    retired_info <- vals$retired_info
    
    result <- simulate_case(probs, retired_info)
    
    vals$total_cases <- vals$total_cases + 1
    if (result$court_correct) vals$correct_cases <- vals$correct_cases + 1
    
    vals$last_results <- data.frame(
      Judge = judges,
      Correct = ifelse(result$votes, "Yes", "No"),
      stringsAsFactors = FALSE
    )
    vals$show_table <- TRUE
    
    if (!vals$initialized) {
      toggle_elements(show_inputs = FALSE, show_reset = TRUE)
      vals$initialized <- TRUE
    }
  })
  
  # --- Simulate 1000 cases ---
  observeEvent(input$simulate1000, {
    probs <- c(input$pA, input$pB, input$pC, input$pD, input$pE)
    retired_info <- vals$retired_info
    
    court_correct <- 0
    for (i in 1:1000) {
      result <- simulate_case(probs, retired_info)
      if (result$court_correct) court_correct <- court_correct + 1
    }
    
    vals$total_cases <- vals$total_cases + 1000
    vals$correct_cases <- vals$correct_cases + court_correct
    vals$last_results <- NULL
    vals$show_table <- FALSE
    
    if (!vals$initialized) {
      toggle_elements(show_inputs = FALSE, show_reset = TRUE)
      vals$initialized <- TRUE
    }
  })
  
  # --- Reset everything ---
  observeEvent(input$reset, {
    vals$total_cases <- 0
    vals$correct_cases <- 0
    vals$last_results <- NULL
    vals$show_table <- FALSE
    vals$initialized <- FALSE
    vals$retired_info <- NULL
    updateCheckboxInput(session, "retire", value = FALSE)
    toggle_elements(show_inputs = TRUE, show_reset = FALSE)
  })
  
  # --- Table output ---
  output$table_ui <- renderUI({
    if (vals$show_table && !is.null(vals$last_results)) {
      tableOutput("resultsTable")
    }
  })
  
  output$resultsTable <- renderTable({
    vals$last_results
  }, bordered = TRUE, align = "lc")
  
  # --- Summary text ---
  output$summaryText <- renderText({
    judges <- c("A", "B", "C", "D", "E")
    
    if (vals$total_cases == 0) {
      msg <- "Press 'Simulate One Case' or 'Simulate 1000 Cases' to begin."
      ret_judge <- judges[vals$retired_info$bottom]
      if (input$retire) msg <- paste(msg, "\n(Judge ", ret_judge, "has retired.)")
      return(msg)
    }
    
    prop_correct <- round(vals$correct_cases / vals$total_cases, 3)
    base_summary <- paste0(
      "Simulations so far: ", vals$total_cases, "\n",
      "Court decided correctly in ", vals$correct_cases,
      " of these (proportion = ", prop_correct, ")."
    )
    
    # Show which judges were doubled/retired if applicable
    retire_note <- ""
    if (!is.null(vals$retired_info)) {
      retire_note <- paste0(
        "\n\nRetirement rule active:\n",
        "  Judge ", judges[vals$retired_info$top], " gets 2 votes.\n",
        "  Judge ", judges[vals$retired_info$bottom], " is retired (0 votes)."
      )
    }
    
    if (vals$show_table && !is.null(vals$last_results)) {
      correct_votes <- sum(vals$last_results$Correct == "Yes")
      court_correct <- correct_votes >= 3
      case_summary <- paste0(
        "This case: ", correct_votes, " judges voted correctly.\n",
        if (court_correct) "The court decided correctly." else "The court decided incorrectly.", "\n\n"
      )
      paste0(case_summary, base_summary, retire_note)
    } else {
      paste0(base_summary, retire_note)
    }
  })
}

# --- JavaScript for fade effects ---
js <- "
Shiny.addCustomMessageHandler('fadeElements', function(message) {
  if(message.show_inputs){
    $('#prob_inputs').fadeIn(500);
  } else {
    $('#prob_inputs').fadeOut(500);
  }
  if(message.show_reset){
    $('#reset_div').fadeIn(500);
  } else {
    $('#reset_div').fadeOut(500);
  }
});
$(document).ready(function(){
  $('#reset_div').hide();
});
"

ui <- tagList(
  tags$head(tags$script(HTML(js))),
  ui
)

shinyApp(ui = ui, server = server)
