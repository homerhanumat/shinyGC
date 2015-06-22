library(shiny)
library(shinydashboard)
library(ggplot2)

playerNumbA <- 15
playerNumbB <- 10
playerNumbC <- 10


rollDie <- function() sample(1:6,size=1)

A1rule <- function(x) (x %in% 2:5)
A2rule <- function(x) (x %in% 6:8)
A3rule <- function(x) (x %in% 9:12)

gameARoll <- function(rule) {
  a <- rollDie()
  b <- rollDie()
  if (rule(a+b)) {
    return(list(a=a,b=b,point=TRUE))
  } else return(list(a=a,b=b,point=FALSE))
}

B1rule <- function(x) (x %% 2 == 0)
B2rule <- function(x) (x %% 2 == 1)

gameBRoll <- function(rule) {
  a <- rollDie()
  b <- rollDie()
  if (rule(a+b)) {
    return(list(a=a,b=b,point=TRUE))
  } else return(list(a=a,b=b,point=FALSE))
}

C1rule <- function(x) (x %% 2 == 0)
C2rule <- function(x) (x %% 2 == 1)

gameCRoll <- function(rule) {
  a <- rollDie()
  b <- rollDie()
  if (rule(a*b)) {
    return(list(a=a,b=b,point=TRUE))
  } else return(list(a=a,b=b,point=FALSE))
}

shinyServer(function(input, output) {

  
  resultsAinit <- data.frame(player=LETTERS[1:playerNumbA],
                        GetPointWhen=c(rep("2,3,4 or 5",playerNumbA/3),
                                   rep("6,7 or 8",playerNumbA/3),
                                   rep("9,10,11 or 12",playerNumbA/3)),
                        rule=c(rep("A1rule",playerNumbA/3),
                               rep("A2rule",playerNumbA/3),
                               rep("A3rule",playerNumbA/3)),
                        a=rep(NA,playerNumbA),
                        b=rep(NA,playerNumbA),
                        points=rep(0,playerNumbA),
                        done=rep(FALSE,playerNumbA)
  )
  
  resultsA <- resultsAinit
  
  #we also want the ability to refresh the "set-up
  totalA <- 0 #total number of sims over all set-ups including current one
  totalAPrev <- 0 #total number of sims over all set-ups excluding current one
  
  rollAUpdate <- reactive({
    if(input$rollA > 0) {
      for (i in 1:playerNumbA) {
        if (resultsA$points[i] == 10  && resultsA$done[i]==FALSE) {
          resultsA$done[i] <<- TRUE
        }
        if (resultsA$done[i]==FALSE) {
          roll <- gameARoll(get(as.character(resultsA$rule[i])))
          resultsA$a[i] <<- roll$a
          resultsA$b[i] <<- roll$b
          resultsA$points[i] <<- resultsA$points[i] + roll$point
        }
        totalA <<- totalA+ 1
      } #end for
    } # end if input$roll
  })
  
  
  
  #this erases the simulation history and puts user back to initial graph
  resetA <- reactive({
    input$resetA
    totalAPrev <<- totalAPrev + totalA
    totalA <<- 0
    resultsA <<- resultsAinit
  })
  
  
  output$bargraphA <- renderPlot({
    rollAUpdate()
    resetA()
    print(ggplot(resultsA, aes(x=player, y=points,fill=GetPointWhen)) +
            geom_bar(stat="identity", position="dodge") + 
            scale_y_continuous(limits = c(0,10),breaks=0:10,expand=c(0,0)) +
            coord_flip())
  })
  
  resultsBinit <- data.frame(player=LETTERS[1:playerNumbB],
                             GetPointWhen=c(rep("Sum even",playerNumbB/2),
                                        rep("Sum odd",playerNumbB/2)),
                             rule=c(rep("B1rule",playerNumbB/2),
                                    rep("B2rule",playerNumbB/2)),
                             a=rep(NA,playerNumbB),
                             b=rep(NA,playerNumbB),
                             points=rep(0,playerNumbB),
                             done=rep(FALSE,playerNumbB)
  )
  
  resultsB <- resultsBinit
  
  #we also want the ability to refresh the "set-up
  totalB <- 0 #total number of sims over all set-ups including current one
  totalBPrev <- 0 #total number of sims over all set-ups excluding current one
  
  rollBUpdate <- reactive({
    if(input$rollB > 0) {
      for (i in 1:playerNumbB) {
        if (resultsB$points[i] == 10  && resultsB$done[i]==FALSE) {
          resultsB$done[i] <<- TRUE
        }
        if (resultsB$done[i]==FALSE) {
          roll <- gameBRoll(get(as.character(resultsB$rule[i])))
          resultsB$a[i] <<- roll$a
          resultsB$b[i] <<- roll$b
          resultsB$points[i] <<- resultsB$points[i] + roll$point
        }
        totalB <<- totalB+ 1
      } #end for
    } # end if input$roll
  })

  
  
  #this erases the simulation history and puts user back to initial graph
  resetB <- reactive({
    input$resetB
    totalBPrev <<- totalBPrev + totalB
    totalB <<- 0
    resultsB <<- resultsBinit
  })
  
  
  output$bargraphB <- renderPlot({
    rollBUpdate()
    resetB()
    print(ggplot(resultsB, aes(x=player, y=points,fill=GetPointWhen)) +
            geom_bar(stat="identity", position="dodge") + 
            scale_y_continuous(limits = c(0,10),breaks=0:10,expand=c(0,0)) +
            coord_flip())
  })
  
  
  resultsCinit <- data.frame(player=LETTERS[1:playerNumbC],
                             GetPointWhen=c(rep("Product even",playerNumbC/2),
                                        rep("Product odd",playerNumbC/2)),
                             rule=c(rep("C1rule",playerNumbC/2),
                                    rep("C2rule",playerNumbC/2)),
                             a=rep(NA,playerNumbC),
                             b=rep(NA,playerNumbC),
                             points=rep(0,playerNumbC),
                             done=rep(FALSE,playerNumbC)
  )
  
  resultsC <- resultsCinit
  
  #we also want the ability to refresh the "set-up
  totalC <- 0 #total number of sims over all set-ups including current one
  totalCPrev <- 0 #total number of sims over all set-ups excluding current one
  
  rollCUpdate <- reactive({
    if(input$rollC > 0) {
      for (i in 1:playerNumbC) {
        if (resultsC$points[i] == 10  && resultsC$done[i]==FALSE) {
          resultsC$done[i] <<- TRUE
        }
        if (resultsC$done[i]==FALSE) {
          roll <- gameCRoll(get(as.character(resultsC$rule[i])))
          resultsC$a[i] <<- roll$a
          resultsC$b[i] <<- roll$b
          resultsC$points[i] <<- resultsC$points[i] + roll$point
        }
        totalC <<- totalC+ 1
      } #end for
    } # end if input$roll
  })
  
  
  
  #this erases the simulation history and puts user back to initial graph
  resetC <- reactive({
    input$resetC
    totalCPrev <<- totalCPrev + totalC
    totalC <<- 0
    resultsC <<- resultsCinit
  })
  
  
  output$bargraphC <- renderPlot({
    rollCUpdate()
    resetC()
    print(ggplot(resultsC, aes(x=player, y=points,fill=GetPointWhen)) +
            geom_bar(stat="identity", position="dodge") + 
            scale_y_continuous(limits = c(0,10),breaks=0:10,expand=c(0,0)) +
            coord_flip())
  })
  
}) #end shinyServer