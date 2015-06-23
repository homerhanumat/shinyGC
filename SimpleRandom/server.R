library(shiny, quietly = TRUE)
library(scales, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tigerstats, quietly=TRUE)

function(input, output) {
  
  rv <- reactiveValues(
    newVar = TRUE,
    variable = "income",
    factor = FALSE,
    psData = NULL,
    yMax = NULL
  )
  
  observeEvent(input$variable,
               {
                 rv$variable <- input$variable
                 var <- imagpop[,rv$variable]
                 if (is.factor(var)) {
                   rv$factor <- TRUE
                 } else {
                   rv$factor <- FALSE
                   den <- density(var)
                   rv$yMax <- 1.5 * max(den$y)
                 }
               })

  observeEvent(input$sample,
              {
              rv$newVar <- FALSE
              pop <- imagpop[,rv$variable]
              samp <- sample(pop, size = input$n, replace = FALSE)
              popDF <- data.frame(values = pop, type = rep("Population",length(pop)))
              sampDF <- data.frame(values = samp, type = rep("Sample",input$n))
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
    validate(
      need(4 <= input$n && input$n <= 1000,
           message = "Sample size must be between 4 and 1000.")
    )
    if ( ! rv$factor ) {
      ggplot(data = imagpop, aes_string(x = rv$variable), alpha = 0.5) +
        geom_density(fill = "red") +
        scale_y_continuous(limits = c(0,rv$yMax)) +
        labs(title = "Density Plot of the Population")
    } else {
      ggplot(data = imagpop, aes_string(x = rv$variable)) +
        geom_bar(aes(y = ..count../sum(..count..)),fill = "red") +
        scale_y_continuous(labels = percent) +
        labs(title = "Bar Graph of the Population",
             y = "Percent")
    }
    })
  
  output$initialTable <- renderTable({
    validate(
      need(4 <= input$n && input$n <= 1000,
           message = "Sample size must be between 4 and 1000.")
    )
    form <- as.formula(paste0("~",rv$variable))
    if ( ! rv$factor ) {
      favstats(form, data = imagpop)[1:8]
    } else {
      tab <- (rowPerc(xtabs(form, data = imagpop)))
      rownames(tab) <- "Population Percentages"
      tab
    }
  })
  
  output$plot <- renderPlot({
    validate(
      need(4 <= input$n && input$n <= 1000,
           message = "Sample size must be between 4 and 1000.")
    )
    if ( ! rv$newVar ) {
      if ( ! rv$factor ) {
        dfSamp <- subset(rv$psData, type == "Sample")
        ggplot(data = rv$psData, aes(x = values, fill = type)) +
          geom_density(alpha = 0.5) +
          geom_rug(data = dfSamp, aes(x = values)) +
          labs(title = "Density Plot of the Population, with Sample",
              x = rv$variable) +
          scale_y_continuous(limits = c(0, rv$yMax)) +
          guides(fill = guide_legend(title = "Plot for:"))
      } else {
        df <- rv$psData %>% group_by(type, values) %>%
          summarize(n = n()) %>%
          mutate(relFreq = n/sum(n)) %>%
          mutate(pos = 0.5 * relFreq) %>%
          mutate(label = paste0(sprintf("%.1f", relFreq*100), "%"))
        ggplot(data = df, aes(x = values, fill = type)) +
          geom_bar(aes(y = relFreq), stat = "identity", position = "dodge") +
          scale_y_continuous(labels = percent) +
          geom_text(aes(x = values, y = pos, label = label, ymax = relFreq),
                    position = position_dodge(width = 1),size = 5) +
          labs(title = "Density Plot of the Population, with Sample",
              x = rv$variable) +
          guides(fill = guide_legend(title = "Bars for:"))
      }
    }
  })
  
  output$table <- renderTable({
    validate(
      need(4 <= input$n && input$n <= 1000,
           message = "Sample size must be between 4 and 1000.")
    )
    if (! is.null(rv$psData) ) {
      df2 <- rv$psData
      names(df2)[1] <- rv$variable
      if ( ! rv$factor ) {
        form <- as.formula(paste0(rv$variable," ~ type"))
        favstats(form, data = df2)[1:8]
      } else {
        form <- as.formula(paste0("~ type + ",rv$variable))
        tab <- rowPerc(xtabs(form, data = df2))
        tab
      }
    }
  })
  
}
