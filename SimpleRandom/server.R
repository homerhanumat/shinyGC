library(shiny)
library(scales)
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tigerstats, quietly=TRUE)

source("plot.R")

function(input, output) {
  
  rv <- reactiveValues(
    newVar = TRUE,
    variable = "income",
    factor = FALSE,
    psData = NULL
  )
  
  observeEvent(input$variable,
               {
                 rv$variable <- input$variable
                 var <- imagpop[,rv$variable]
                 if (is.factor(var)) {rv$factor <- TRUE
                 } else rv$factor <- FALSE
               })

  observeEvent(input$sample,
              {
              rv$newVar <- FALSE
              pop <- imagpop[,rv$variable]
              samp <- sample(pop, size = input$n, replace = FALSE)
              popDF <- data.frame(values = pop, graph = rep("Population",length(pop)))
              sampDF <- data.frame(values = samp, graph = rep("Sample",input$n))
              rv$psData <- rbind(popDF, sampDF)
              })
  
  
  observeEvent(input$reset,
          {
          rv$newVar <- TRUE
          rv$variable <- "income"
          rv$factor <- FALSE
          rv$psData <- NULL
          })

  output$newVar <- reactive({
    rv$newVar
  })
  
  outputOptions(output, "newVar", suspendWhenHidden = FALSE)
  
  output$initialPlot <- renderPlot({
    if ( ! rv$factor ) {
      ggplot(data = imagpop, aes_string(x = rv$variable), alpha = 0.5) +
        geom_density(fill = "red") +
        labs(title = "Density Plot of the Population")
    } else {
      ggplot(data = imagpop, aes_string(x = rv$variable)) +
        geom_bar(fill = "red") +
        labs(title = "Bar Graph of the Population")
    }
    })
  
  output$plot <- renderPlot({
    if ( ! rv$newVar ) {
      if ( ! rv$factor ) {
        ggplot(data = rv$psData, aes(x = values, fill = graph)) +
          geom_density(alpha = 0.5) +
          labs(title = "Density Plot of the Population, with Sample",
              x = rv$variable)
      } else {
        df <- rv$psData %>% group_by(graph, values) %>%
          summarize(n = n()) %>%
          mutate(relFreq = n/sum(n)) %>%
          mutate(pos = 0.5 * relFreq) %>%
          mutate(label = paste0(sprintf("%.1f", relFreq*100), "%"))
        ggplot(data = df, aes(x = values, fill = graph)) +
          geom_bar(aes(y = relFreq), stat = "identity", position = "dodge") +
          scale_y_continuous(labels = percent) +
          geom_text(aes(x = values, y = pos, label = label, ymax = relFreq),
                    position = position_dodge(width = 1),size = 5) +
          labs(title = "Density Plot of the Population, with Sample",
              x = rv$variable)
      }
    }
  })
  
}
