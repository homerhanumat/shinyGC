library(shiny)
library(rhandsontable)

simLimit <- 10000

source("globals.R")

shinyServer(function(input, output, session) {
  
  numberSims <- 0 # persistant variable
  
  rv <- reactiveValues(
    rowNames = NULL,
    colNames = NULL,
    DF = NULL,       # data frame of obs
    df = NULL,       # degrees of freedom
    xmax = NULL,
    chisqSims = NULL,
    chisqDensities = NULL,
    latestTable = NULL,
    latestSim = NULL,
    state = 'tableSetup'
  )
  
  observeEvent(input$rowNames,{
    rv$rowNames <- unlist(strsplit(input$rowNames, split = ","))
  })
  
  observeEvent(input$colNames,{
    rv$colNames <- unlist(strsplit(input$colNames, split = ","))
  })
  
  observeEvent(input$cross,{
    rv$DF <- hot_to_r(input$cross)
  })
  
  observeEvent(input$submitTable,{
    if ( input$submitTable > 0 ) {
      rv$state <- 'simSetup'
      rv$df <- ( nrow(rv$DF) - 1 ) * ( ncol(rv$DF) - 1 )
      rv$xmax <- qchisq(0.999, df = rv$df)
      }
  })
  
  observeEvent(input$sim,{
    rv$state <- 'simulating'
    reps <- min(simLimit, input$numberSims)
    simType <- input$simMethod
    observed <- rv$DF
    if (simType=="rcFix") newSims <- DoubleFixedResampler(observed,reps)
    if (simType=="rFix") newSims <- RandFixedResampler(observed,reps,effects="fixed")
    if (simType=="nFix") newSims <- RandFixedResampler(observed,reps,effects="random")
    rv$chisqSims <- c(rv$chisqSims, newSims$sims)
    tempTab <- data.frame(newSims$last_table)
    row.names(tempTab) <- row.names(rv$DF)
    names(tempTab) <- names(rv$DF)
    rv$latestTable <- tempTab
    rv$latestSim <- newSims$sims[reps]
    numberSims <<- numberSims + reps
  })
  
  observe({
    chisqSims <- rv$chisqSims
    n <- length(chisqSims)
    if ( n == 1 ) band <- 1 else band <- "nrd0"
    if ( n >= 1 ) {
      rv$chisqDensities <- density(chisqSims,n=500,
                                   from=0,to=rv$xmax,bw=band)
    }
  })
  
  observeEvent(input$reset,{
    rv$state <- 'simSetup'
    rv$chisqSims <- NULL
    rv$chisqDensities <- NULL
    rv$latestTable <- NULL
    rv$latestSim <- NULL
    numberSims <- 0
  })
  
  observeEvent(input$newTable,{
    rv$DF <- NULL
    rv$state <- 'tableSetup'
    rv$chisqSims <- NULL
    rv$chisqDensities <- NULL
    rv$latestTable <- NULL
    rv$latestSim <- NULL
    numberSims <- 0
    rv$df <- NULL
    rv$xmax <- NULL
  })
  
  observe({
    if ( input$numberSims == 1 && rv$state == 'simSetup' ) {
      updateTabsetPanel(session, inputId = "tabs", selected = "Latest Simulation")
    }
  })
  
  output$state <- reactive({
    rv$state
  })
  
  outputOptions(output, "state", suspendWhenHidden = FALSE)
  
  output$cross <- renderRHandsontable({
    input$newTable # for the dependency
    rows <- input$rows
    cols <- input$cols
    rowNames <- rv$rowNames
    colNames <- rv$colNames
    if (rows > length(rowNames)) {
      rowNames <- c(rowNames, letters[(length(rowNames)+1):rows])
    } else {
      rowNames <- rowNames[1:rows]
    }
    if (cols > length(colNames)) {
      colNames <- c(colNames, LETTERS[(length(colNames)+1):cols])
    } else {
      colNames <- colNames[1:cols]
    }
    if (is.null(input$cross)) {
        entries <- rows * cols
        mat <- matrix(rep(1,times = entries), nrow = rows)
        mat[1,1] <- 2; mat[1,2] <- 7; mat[2,1] <- 8; mat[2,2] <- 4
        df <- data.frame(mat)
        df <- rowColAdjust(df, rows, cols)
    } else {
        df <- rv$DF
        df <- rowColAdjust(df, rows, cols)
    }
    names(df) <- colNames
    row.names(df) <- rowNames
    rv$DF <- df
    rhandsontable(df)
  })
  
  output$remarksInitial <- renderText({
    paste("Observed chi-square statistic =  ",
          as.character(round(chisqStat(rv$DF),2)),sep="")
  })
  
  output$obsTable <- renderTable({
    rv$DF
  }, include.rownames = TRUE)
  
  output$expTable <- renderTable({
    expCounts(rv$DF)
  }, include.rownames = TRUE)
  
  output$mosaicInitial <- renderPlot({
    observed <- rv$DF
    obsMat <- as.matrix(rv$DF)
    expected <- expCounts(observed)
    par(mfrow=c(1,2))
    if (input$barmosInit == "mosaic") {
      mosaicplot(t(observed),col="orange",main="Observed Table",cex.axis=1.3)
      mosaicplot(t(expected),col="grey",main="Expected Table",cex.axis=1.3)
    } else {
        barplot(t(obsMat), beside = TRUE,
                legend.text = names(observed), cex.axis = 1.3,
                main = "Observed Table")
        barplot(t(expected), beside = TRUE,
                legend.text = names(observed), cex.axis = 1.3,
                main = "Expected Table")
      }
    par(mfrow=c(1,1))
  })
  
  output$contrTable <- renderTable({
    observed <- rv$DF
    (observed-expCounts(observed))^2/expCounts(observed)
  })
  
  output$remarksLatest1 <- renderText({
    chisqSims <- rv$chisqSims
    obschisq <- chisqStat(rv$DF)
    rounded1 <- round(obschisq,2)
    rounded2 <- round(chisqSims[length(chisqSims)],2)
    paste("Observed chi-square =  ",as.character(rounded1),
          ",\n Last resampled chi-square = ",as.character(rounded2),sep="")
  })
  
  output$mosaicLatest <- renderPlot({
    expTitle <- ifelse(input$simMethod == "nFix",
                        "Expected Table",
                        "Expected Table\n(from simulation)")
    if( ! is.null(rv$latestTable) ) { # for the dependency
      latest_table <- rv$latestTable
      latest_table <- as.matrix(latest_table)
      expected <- expCounts(latest_table)
      par(mfrow=c(1,2))
      if (input$barmosLatest == "mosaic") {
        mosaicplot(t(latest_table),col="blue",main="Simulated Table",
                  cex.axis=1.3)
        mosaicplot(t(expected),col="grey",main = expTitle,
                  cex.axis=1.3)
      } else {
        barplot(t(latest_table), beside = TRUE,
                legend.text = names(rv$DF), cex.axis = 1.3,
                main = "Simulated Table")
        barplot(t(expected), beside = TRUE,
                legend.text = names(rv$DF), cex.axis = 1.3,
                main = expTitle)
        }
      par(mfrow=c(1,1))
    }
  })
  
  output$latestTable <- renderTable({
    latest_table <- rv$latestTable
    if (!is.null(latest_table)) {
      return(latest_table)
    }
  }, include.rownames = TRUE)
  
  output$latestExpTable <- renderTable({
    latest_table <- rv$latestTable
    if (! is.null(latest_table) ) {
      exp <- expCounts(latest_table)
      return(exp)
    }
  }, include.rownames = TRUE)
  
  output$summary1 <- renderTable({
    obs <- chisqStat(rv$DF)
    chisqSims <- rv$chisqSims
    if (length(chisqSims) >0) {
      n <- length(chisqSims)
      latest <- chisqSims[n]
      p.value <- length(chisqSims[chisqSims>=obs])/n
      percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
      df <- data.frame(round(latest,2),n,percentage)
      names(df) <- c("Last Resampled Chi-Square",
                     "Number of Resamples So Far",
                     paste("Percent Above ",round(obs,2),sep="")
      )
      df
    }
  }, include.rownames = FALSE)
  
  output$remarksProbBar <- renderText({
    obs <- chisqStat(rv$DF)
    paste0("The percentage in the table gives the approximate probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationship between the two factor",
           " variables under study) is true. The more resamples you take the better these",
           "approximations will be!")
  })
  
  output$summary2 <- renderTable({
    chisqSims <- rv$chisqSims
    obs <- chisqStat(rv$DF)
    n <- length(chisqSims)
    latest <- chisqSims[n]
    p.value <- length(chisqSims[chisqSims>=obs])/n
    percentage <- paste(as.character(round(p.value*100,2)),"%",sep="")
    df <- data.frame(round(latest,2),n,percentage)
    names(df) <- c("Last Resampled Chi-Square",
                   "Number of Resamples So Far",
                   paste("Percent Above ",round(obs,2),sep="")
    )
    df
  }, include.rownames = FALSE)
  
  output$densityplot <-renderPlot({
      dchisq <- rv$chisqDensities
      chisqSims <- rv$chisqSims
      plot(dchisq$x,dchisq$y,type="l",col="blue",
           xlab="Chi-Square Value",ylab="Estimated Density",
           main="Distribution of Resampled Chi-Square Statistics")
      if (length(chisqSims) <= 200) rug(chisqSims)
      latest <- chisqSims[length(chisqSims)]
      points(latest,0,col="blue",pch=19)
      abline(v=chisqStat(rv$DF))
      
    })
  
  output$remarksProbDensity <- renderText({
    obs <- chisqStat(rv$DF)
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " It is based on our resamples so far.  The percentage in the table gives the approximate ",
           "probability, based on our resamples so far, of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationship between the two factor",
           " variables under study) is true. The more resamples you take the better these",
           "approximations will be!")
  })
  
  
  output$chisqCurve <- renderPlot({
    obs <- chisqStat(rv$DF)
    degFreedom <- rv$df
    if (input$yates) {
      chisqGraph(bound=yatesCorrection(rv$DF),region="above",
                 df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=yatesCorrection(rv$DF))
    } else {
      chisqGraph(bound=obs,region="above",
                 df=degFreedom,xlab="Chi-Square Values",
                 graph=TRUE)
      abline(v=obs)
    }
    if (input$compareDen) {
      lines(rv$chisqDensities,col="blue",lwd=4)
    }
  })
  
  output$remarksProb <- renderText({
    obs <- chisqStat(rv$DF)
    paste0("The curve above approximates the true probability distribution of the chi-square statistic.",
           " The shaded area gives the approximate probability of getting a chi-square statistic of ",
           round(obs,2)," or more, if the Null Hypothesis (no relationshoip between the two factor",
           " variables under study) is true.")
  })
  
})