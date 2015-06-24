library(shiny)

# bounds for intercept
lowa <- -5
higha <- 5
# bounds for slope
lowb <- -2
highb <- 2
# error sd
sigma <- 3
n <- 10 # number of points in in cloud
x <- 1:10 # x-values

# Define server logic for FindRegLine
function(input, output, session) {
  
  # read in players records
  leaders <- read.csv(file = "leaders.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # users should start in different places
  set.seed(as.numeric(Sys.time()))
  
  #initiate status values
  rv <- reactiveValues(
    beginning = TRUE,
    playing = FALSE,
    reporting = FALSE
  )
  
  #set up:
  player_rank <- NULL
  ta <- round(runif(1, min = lowa, max = higha), 2)
  tb <- round(runif(1, min = lowb, max = highb), 2)
  y <- ta+tb*x+rnorm(n,mean=0,sd=sigma)
  #SS for the regression line:
  mod <- lm(y~x)
  ess <- sum((resid(mod))^2)
  #determine nice limits for plot (and a slider):
  reg.slope <- coef(mod)[2]
  reg.int <- coef(mod)[1]
  #Find range of y-intercepts of lines through
  #points on scatterplot having slope = reg.slope
  int.min <- min(y-reg.slope*x)
  int.max <- max(y-reg.slope*x)
  int.band <- (int.max-int.min)/2
  #Expand this range, and make sure it includes 0:
  int.mid <- (int.max+int.min)/2
  lowa.slider <- floor(min(c(int.mid-1.2*int.band,-1,min(y)-1)))
  higha.slider <- ceiling(max(c(int.mid+1.2*int.band,1,max(y)+1)))
  #plot limits reflect this range, too:
  ymin <- lowa.slider
  ymax <- higha.slider
  y.mean <- mean(y)
  your.y <- rep(y.mean,n)
  #SS for the line initially placed (a= mean(y),b=0):
  total.ss <- sum((y-mean(y))^2)
  your.ss <- total.ss
  turns <- 0
  close <- 100
  score <- close + turns

  # initiate a slider info:
  rvSlider <- reactiveValues(
    lowa.slider = lowa.slider,
    higha.slider = higha.slider,
    y.mean = y.mean
  )
  
  #initiate bslider info
  rvbSlider <- reactiveValues(
    value = 0
  )
  
  rvGraph <- reactiveValues(
    ymin = ymin,
    ymax = ymax
  )

  #make the a and b sliders
  output$aslider <- renderUI({
        sliderInput("a",min=rvSlider$lowa.slider,max=rvSlider$higha.slider,
                label="y-Intercept",step=0.01,value=rvSlider$y.mean)
        })
  
  output$bslider <- renderUI({
        input$reset
        sliderInput("b",min=2*lowb,max=2*highb,label="Slope",
                    step=0.01,value=0)
          })
  
  observeEvent(input$submit,
               {
                 rv$beginning <- FALSE
                 rv$playing <- TRUE
                 turns <<- turns + 1
                 your.y <<- input$a+input$b*x
                 your.ss <<- sum((y-your.y)^2)
                 close <<- 100*(your.ss-ess)/(total.ss-ess)
                 score <<- turns+close
               })
  
  observeEvent(input$enditall,
               {
                 rv$reporting <- TRUE
                 rv$playing <- FALSE
                 if (input$player != "") {
                   game <- data.frame(name = input$player,
                                      score = score,
                                      time = as.character(Sys.time()))
                   rank <- max(which(score >= leaders$score))
                   if (!is.infinite(rank)) {
                     player_rank <<- rank + 1
                   } else player_rank <<- 1
                   leaders <<- rbind(leaders,game)
                   ordered_scores <- order(leaders$score)
                   leaders <<- leaders[ordered_scores,]
                   write.csv(leaders, file = "leaders.csv", row.names = FALSE)
                 }
               })
  
  observeEvent(input$reset,
              {
                rv$beginning <- TRUE
                rv$reporting <- FALSE
                rv$playing <- FALSE
                #set up again:
                ta <<- round(runif(1, min = lowa, max = higha), 2)
                tb <<- round(runif(1, min = lowb, max = highb), 2)
                y <<- ta+tb*x+rnorm(n,mean=0,sd=sigma)
                #SS for the regression line:
                mod <<- lm(y~x)
                ess <<- sum((resid(mod))^2)
                #determine nice limits for plot (and a slider):
                reg.slope <<- coef(mod)[2]
                reg.int <<- coef(mod)[1]
                #Find range of y-intercepts of lines through
                #points on scatterplot having slope = reg.slope
                int.min <<- min(y-reg.slope*x)
                int.max <<- max(y-reg.slope*x)
                int.band <<- (int.max-int.min)/2
                #Expand this range, and make sure it includes 0:
                int.mid <<- (int.max+int.min)/2
                rvSlider$lowa.slider <- floor(min(c(int.mid-1.2*int.band,-1,min(y)-1)))
                rvSlider$higha.slider <- ceiling(max(c(int.mid+1.2*int.band,1,max(y)+1)))
                #plot limits reflect this range, too:
                rvGraph$ymin <- rvSlider$lowa.slider
                rvGraph$ymax <- rvSlider$higha.slider
                y.mean <<- mean(y)
                rvSlider$y.mean <- y.mean # so a slider sets correctly
                your.y <<- rep(y.mean,n)
                #SS for the line initially placed (a= mean(y),b=0):
                total.ss <<- sum((y-mean(y))^2)
                your.ss <<- total.ss
                turns <<- 0
                close <<- 100
                score <<- close + turns
              })
  
  output$beginning <- reactive({
    rv$beginning
  })
  
  output$playing <- reactive({
    rv$playing
  })
  
  output$reporting <- reactive({
    rv$reporting
  })
  
  outputOptions(output,"beginning", suspendWhenHidden = FALSE)
  outputOptions(output,"playing", suspendWhenHidden = FALSE)
  outputOptions(output,"reporting", suspendWhenHidden = FALSE)
  
  
 output$gamecloud <- renderPlot({
   input$submit #just in case user decides not to change line
   plot(x,y,pch=16,col="blue",ylim=c(rvGraph$ymin,rvGraph$ymax),
        xlim=c(0,n))
   points(0,0,cex=0.8,pch=16,col="green")
   abline(input$a,input$b)
   abline(0,0,lty=2,col="green")
   lines(x=c(0,0),y=c(rvGraph$ymin,rvGraph$ymax),lty=2,col="green")
   current.y <- input$a + input$b * x
   for(i in 1:n)  {
     lines(x=c(x[i],x[i]),y=c(current.y[i],y[i]))
   }
 })
 
 output$finalcloud <- renderPlot({
   input$enditall
   plot(x,y,pch=16,col="blue",ylim=c(ymin,ymax),
        xlim=c(0,n))
   points(0,0,cex=0.8,pch=16,col="green")
   isolate(abline(input$a,input$b))
   abline(0,0,lty=2,col="green")
   lines(x=c(0,0),y=c(ymin,ymax),lty=2,col="green")
   coefs <- coef(mod)
   abline(coefs,col="red",lwd=3)
 })
 
 output$score <- renderTable({
   input$submit
   input$reset
   tab <- rbind(your.ss,ess,close,turns,round(score,3))
   colnames(tab) <- "Score Report"
   rownames(tab) <- c("Your Error Sum of Squares",
              "Regression Line's Error Sum of Squares",
              "Closeness Measure",
              "Turns So Far","Score (Turns + Closeness)")
   tab
 })
 
 output$rank <- reactive({
   input$enditall
   paste0("Your rank for this game is: ", player_rank,".")
 })
 
 output$leaders <- renderDataTable({
   input$enditall
   leaders
 })
 
 output$revelation <- renderTable({
   if (input$enditall > 0) {
     coefs <- round(coef(mod),2)
     tab <- rbind(c(input$a,input$b),coefs)
     rownames(tab) <- c("Your Final Guesses","Regression Line")
     colnames(tab) <- c("y-Intercept","Slope")
     tab
     }
 })
  
}

