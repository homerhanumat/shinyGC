library(shiny)

# repeated info:
max.n_0 <- 1000

# function to compute field color:
field.color <- function(m, n) {
  prop <- n/m
  if(prop > 1.2){
    prop <- 1.2
  }
  loss <- prop * 130
  grn <- 255 - loss
  gain <- prop * 165
  rd <- gain
  color <- rgb(red = rd, blue = 0, green = grn, maxColorValue = 555)
  color
}


growthRate <- function( popSizes, b, d, m) {
  rate = NULL
  if(b > d) {
    rate = (popSizes*(b - d)/m)*(m - popSizes)
  }
  else if(b == d) {
    rate = 0
  }
  else {
    rate = popSizes * (b - d)
  }
}

# function to compute growth rate as a function of population size
growthRatePops <- function( n0, b, d, m) {
  # n0 = initial population
  # first we compute a reasonable range of pop values for the x-axis
  if (b > d) {
    maxPop <- max(1.1*m, 1.3*n0)
  } else maxPop <- 1.3*n0
  popSizes <- 1:maxPop
  rate = NULL
  if(b > d) {
    rate <- (popSizes*(b - d)/m)*(m - popSizes)
  }
  else if(b == d) {
    rate  <- rep(0,popSizes)
  }
  else {
    rate <- popSizes * (b - d)
  }
  results <- list(pops = popSizes, rates = rate)
  results
}

## begin server function:

function(input, output, session) {
  generationsTillMaturity <- 1 # how many generations it takes for a generation
  # to mature


  # compute probability for different death causes
  deathProbs <- function(n, b, d, m) {
    # n = current population size
    # b = max birth rate
    # d = min death rate
    # m = carrying capacity (may not be relevant)
    if (b > d) {
      lm <- 0.01^(n/(1.2*m))
      fox <- 0.5*(1- lm)
      mal <- 0.5*(1 - lm)
    } else {
      mal <- 0
      lm <- 0.10^(n/(1.2*max.n_0))
      fox <- 1 - lm
    }
    c(lm, fox, mal)
  }

  # REACTIVE VALUES
  rv <- reactiveValues(sim = 0
                       , littersizes = list(integer(0))
                       , dead = list(integer(0))
                       , daframe = data.frame(time = NULL
                                    , numLitters = NULL
                                    , population = NULL
                                    , birthrate = NULL
                                    , born = NULL
                                    , deathrate = NULL
                                    , dead = NULL)
                      , beginning = TRUE
                      , currentPopField = NULL
                      , currentPopGY = NULL)
  # END REACTIVE VALUES

  daframe <- data.frame(time = NULL
             , numLitters = NULL
             , population = NULL
             , birthrate = NULL
             , born = NULL
             , deathrate = NULL
             , dead = NULL)
  t <- 0
  n_0 <- 0
  n_t <- 0
  b <- 0
  d <- 0
  m <- 0
  littersizes <- list(integer(0))
  dead <- list(integer(0))
  deathTypes <- matrix()
  st <- 0
  initial <- numeric(length = 7)

  observeEvent(input$reset,{
    rv$beginning <- TRUE
    # tabsetpanel should always begin on the population graph:
    updateTabsetPanel(session, "tabset", selected = "Population Graphs")
    updateRadioButtons(session, "initialGraphType", selected = "pop")
    updateRadioButtons(session, "initialGraphX", selected = "time")
    updateRadioButtons(session, "graphX", selected = "time")
    updateSliderInput(session, "mom", value = 0)
    updateSliderInput(session, "momG", value = 0)
  })

  observeEvent(input$goButton,{
    if (input$goButton > 0) {
      rv$beginning <- FALSE
      updateTabsetPanel(session, "tabset", selected = "Population Graphs")
      updateRadioButtons(session, "initialGraphType", selected = "pop")
      updateRadioButtons(session, "initialGraphX", selected = "time")
      updateRadioButtons(session, "graphX", selected = "time")
    }
  })

  output$beginning <- reactive({
    rv$beginning
  })

  outputOptions(output, "beginning", suspendWhenHidden = FALSE)
  
  observeEvent(input$initialGraphType,{
    updateRadioButtons(session, "initialGraphX", selected = "time")
  })
  
  observeEvent(input$graphType,{
    updateRadioButtons(session, "graphX", selected = "time")
  }) 
  
observe({
  time <- input$mom
  if (is.null(time) || time == 0) {
    rv$currentPopField <- input$n_0
  } else{
    rv$currentPopField <- rv$daframe$population[time]
  }
})

output$currentPopField <- reactive({
  rv$currentPopField
})

outputOptions(output, "currentPopField", suspendWhenHidden = FALSE)

observe({
  time <- input$momG
  if (is.null(time) || time == 0) {
    rv$currentPopGY <- input$n_0
  } else{
    rv$currentPopGY <- rv$daframe$population[time]
  }
})

output$currentPopGY <- reactive({
  rv$currentPopGY
})

outputOptions(output, "currentPopGY", suspendWhenHidden = FALSE)

  advanceTime <- function() {
    dfrow = numeric(length = 7)

    t <<- t + 1

    if(d <= b){
      bstar =  b + (d - b)/2*(n_t/m) # Important factor in birth rate at time t
      dstar =  d + (b - d)/2*(n_t/m) # Important factor in death rate at time t
      bt <- max(0, bstar)            # birth rate at time t
      dt <- dstar + max(0, -bstar)   # death rate at time t
    }
    else{
      bt <- b
      dt <- d
    }

    st <<- 8*bt/b  # expected value of litter-size
    lt <<- b/8  # per-capita litter rate
    Lt <<- rpois(n = 1, lambda = lt*n_t) # number of litters based on litter
    # rate
    littersizes[[t]] <<- rpois(Lt, lambda = st) # produce sizes of each litter

    if (is.null(dead)) {
    }
    else {
      dead[[t]] <<- rpois(1, lambda = dt*n_t) # produce dead rabbits
    }
    if (dead[[t]] > n_t){
      dead[[t]] <<- n_t
    }

    # now simulate causes of death:  lawnmover, fox, malnutrition
    deadNow <- dead[[t]]
    probs <- deathProbs(n_t, b, d, m)
    deathCauses[t-1,] <<- rmultinom(1, size = deadNow, prob = probs)

    # keep track of the living rabbits and when they were born
    birthsThisGeneration <- sum(littersizes[[t]])

    # population sizes = previous population + sum of all litters - dead rabbits
    n_t <<- n_t + sum(littersizes[[t]]) - sum(dead[[t]])

    dfrow[1] = t
    dfrow[2] = Lt
    dfrow[3] = n_t
    dfrow[4] = bt
    dfrow[5] = sum(littersizes[[t]])
    dfrow[6] = dt
    dfrow[7] = sum(dead[[t]])
    daframe[t,] <<- dfrow
  }

  popGraph <- function(m, b, d, n_0, totalTime, display) {

    plot(0, type='n', ylab = "Population (n)"
         , xlab = "Time (Insert Units)", xlim = c(0,(totalTime*1.05)),
         ylim = c(0,(max(rv$daframe$population)*1.05)))
    if(display == 1 | display == 2){
      lines(x=rv$daframe$time, rv$daframe$population, type = 'l')
    }
    if((display == 1) | (display == 3)){
      lines(x=rv$daframe$time, theoretical(0:input$totalTime), col = "red")
    }
    if(b > d)
      lines(x=rv$daframe$time, rep(m, length(rv$daframe$time)), col = "blue")

  }

  makeCenters <- function(numblitters){
    car <- ceiling(sqrt(numblitters))
    x <- (1:car)/(car+1)
    y <- (1:car)/(car+1)
    df <- expand.grid(x,y)
    names(df) <- c("x", "y")
    distance <- 1/(car+1)
    bools <- c(rep(TRUE, numblitters),
               rep(FALSE, car^2-numblitters))
    selected <- sample(bools, size = car^2, replace = FALSE)
    results <- list(centers = df[selected,],distance = distance)
    return(results)
  }


  observeEvent(input$goButton, {
    if(input$seed){
      set.seed(input$setter)
    }
    else{
      set.seed(as.numeric(Sys.time()))
    }

    n_0 <<- input$n_0
    m <<- input$m
    t <<- 1
    n_t <<- input$n_0
    littersizes <<- list(integer(0))
    dead <<- list(integer(0))
    deathCauses <<- matrix(0, (input$totalTime +1)*3, ncol = 3)
    b <<- input$b
    d <<- input$d
    # reset dataframe
    daframe <<- data.frame(time = rep(0, input$totalTime)
                          , numLitters = rep(0, input$totalTime)
                          , population = rep(n_0, input$totalTime)
                          , birthrate = rep(0, input$totalTime)
                          , born = rep(0, input$totalTime)
                          , deathrate = rep(0, input$totalTime)
                          , dead = rep(0, input$totalTime))

    # initialize initial state of population
    init_bt <- max(0, b + (d - b)/2*(n_t/m))
    init_dt <- d + (b - d)/2*(n_t/m) + max(0, -(b + (d - b)/2*(n_t/m)))
    initial <<- c(0, 0, n_t, init_bt, 0, init_dt, 0)
    daframe <<- rbind(initial, daframe)
    for(i in 1:input$totalTime + 1) {
      advanceTime()
    }
    rv$littersizes <- littersizes
    rv$dead <- dead
    rv$daframe <- daframe
    rv$sim <- rv$sim + 1
    output$pop <- renderPlot({
      rv$sim
      type <- input$graphType
      typeX <- input$graphX
      disp <- 1 #this used to vary with input$display
      if(type == "pop"  && typeX == "time") {
        popGraph(m
                , b
                , d
                , input$n_0
                , input$totalTime
                , disp)
      }
      else if(type == "rate" && typeX == "time") {
        plot(x = 1:input$totalTime
             , y = growthRate(theoretical(1:input$totalTime)
                              , b = b
                              , d = d
                              , m = m)
             , type = "l"
             , ylab = "Growth Rate"
             , xlab = "Time (Insert Units here)")
      }
      else if(type == "relRate"  && typeX == "time") {
        # TODO: make a function for computing per-capita growth rate
        plot(x = 1:input$totalTime
             , y = growthRate(theoretical(1:input$totalTime)
                              , b = b
                              , d = d
                              , m = m) / theoretical(1:input$totalTime)
             , type = "l"
             , ylab = "Per-Capita Growth Rate"
             , xlab = "Time (Insert Units here)")
      }
      else if (type == "rate" && typeX == "pop") {
        results <- growthRatePops(input$n_0, b, d, m)
        plot(x = results$pops
             , y = results$rates
             , type = "l"
             , ylab = "Growth Rate"
             , xlab = "Population Size")
      } else {
        results <- growthRatePops(input$n_0, b, d, m)
        plot(x = results$pops
             , y = results$rates/results$pops
             , type = "l"
             , ylab = "Per-Capita Growth Rate"
             , xlab = "Population Size")
      }
    })
  })

  theoretical <- function(times) {
    b = input$b
    d = input$d
    n_0 = input$n_0
    m = input$m
    if(b >= d){
      if(n_0 < m){
        pop = m / (1 + ((m-n_0)/n_0)*exp(-1*(b-d)*times))
      }
      if(m <= n_0){
        pop = m / (1 - ((n_0 - m)/n_0)*exp((d-b)*times))
      }
    }
    if(d > b){
      pop = n_0*exp((b-d)*times)
    }
    return(pop)
  }
  observe({
    output$initialGraph <- renderPlot({
      endTime <- input$totalTime
      b <- input$b
      d <- input$d
      m <- input$m
      times <- 1:endTime
      if(input$initialGraphType == "pop"  && input$initialGraphX == "time") {
        plot(times, theoretical(times), type = "l"
             , col = "red"
             , ylab = "Population Size"
             , xlab = "Time (Insert units here)")
        if (b > d) {
          lines(x = c(0,endTime), y = c(m,m), col = "blue")
        }
      }
      else if(input$initialGraphType == "rate" && input$initialGraphX == "time")  {
        plot(x = 1:input$totalTime
             , y = growthRate(theoretical(1:input$totalTime)
                              , b = b
                              , d = d
                              , m = m)
             , type = "l"
             , ylab = "Growth Rate"
             , xlab = "Time (Insert Units here)")
      }
      else if(input$initialGraphType == "relRate" && input$initialGraphX == "time") {
        plot(x = 1:input$totalTime
             , y = growthRate(theoretical(1:input$totalTime)
                              , b = b
                              , d = d
                              , m = m) / theoretical(1:input$totalTime)
             , type = "l"
             , ylab = "Growth Rate"
             , xlab = "Time (Insert Units here)")
      }
      else if (input$initialGraphType == "rate"  && input$initialGraphX == "pop") {
        results <- growthRatePops(input$n_0, b, d, m)
        plot(x = results$pops
             , y = results$rates
             , type = "l"
             , ylab = "Growth Rate"
             , xlab = "Population Size")
      } else if (input$initialGraphType == "relRate" && input$initialGraphX == "pop") {
        results <- growthRatePops(input$n_0, b, d, m)
        plot(x = results$pops
             , y = results$rates/results$pops
             , type = "l"
             , ylab = "Per-Capita Growth Rate"
             , xlab = "Population Size")
      }
    })
  })

  output$initialDiscuss <- renderText({
    type <- input$initialGraphType
    typeX <- input$initialGraphX
    if (type == "pop" && typeX == "time")  {
      HTML(
      "<h2>Explanation</h2> <div><p>There can be up to two lines on the graph. </p>
            <ol><li>The <font color='red'>red</font> line represents the population
                  based on the selected parameters, as determined by a differential
                  equation.</li>
                <li>The <font color='blue'>blue</font> line will only appear if the carrying
                  capacity is
                  relevant. It is not relevant when the minimum death rate is
                  at least as big as the maximum birth rate.</li>
            </ol></div>")
    } else if (type == "rate" && typeX == "time") {
      HTML(
        "<h2>Explanation</h2> <div><p>The curve above represents the population 
        growth-rate over time, based on the selected parameters.</p></div>")
    } else if (type == "relRate" && typeX == "time") {
      HTML(
        "<h2>Explanation</h2> <div><p>The curve above represents the <em>per-capita</em>
        population growth-rate.  It's the growth rate at a given time,
        divided by the population at that time.</p></div>")
    } else if (type == "rate" && typeX == "pop") {
      HTML(
        "<h2>Explanation</h2> <div><p>The curve above represents the population 
        growth-rate as a function of population size, based on the selected parameters.</p></div>")
    } else {
      HTML(
        "<h2>Explanation</h2> <div><p>The curve above represents the per-cpita population 
        growth-rate as a function of population size, based on the selected parameters.</p></div>")
    }
      })

output$discuss <- renderText({
  type <- input$graphType
  typeX <- input$graphX
  if (type == "pop" && typeX == "time") {
    HTML("
    <h2>Explanation</h2>
    <div><p>There can be up to three lines on the graph. </p>
  <ol>
    <li>The <font color='red'>red</font> line represents the theoretical
    population determined by the selected parameters. </li>
    <li>The <strong>black</strong> line represents a simulated population, using
    the given parameters. </li>
    <li>The <font color='blue'>blue</font> line will only appear if the carrying
    capacity is relevant. It is not relevant when the minimum death rate is at
    least as big as the maximum birth rate.</li>
  </ol>
</div>")
  } else if ( type == "rate" && typeX == "time") {
    HTML(
      "<h2>Explanation</h2> <div><p>The curve above represents the population 
      growth-rate over time, based on the selected parameters.</p></div>")
  } else if (type == "relRate" && typeX == "time") {
    HTML(
      "<h2>Explanation</h2> <div><p>The curve above represents the <em>per-capita</em>
        population growth-rate.  It's the growth rate at a given time,
        divided by the population at that time.</p></div>")
  } else if (type == "rate" && typeX == "pop") {
    HTML(
      "<h2>Explanation</h2> <div><p>The curve above represents the population growth-rate.
        It's plotted as a function of population size.</p></div>") 
  } else {
    HTML(
      "<h2>Explanation</h2> <div><p>The curve above represents the <em>per-capita</em>
      population growth-rate:  the growth rate at a given population,
      divided by the population.  It's plotted s a function of population size.</p></div>")
    }
  
    
    })

  output$momentF <- renderUI({
    sliderInput (  label = "Time To View"
                 , inputId = "mom"
                 , step = 1
                 , min = 0
                 , max = input$totalTime
                 , value = 0
                 , animate = animationOptions(loop = TRUE))
  })

  output$momentG <- renderUI({
    sliderInput (  label = "Time To View"
                   , inputId = "momG"
                   , step = 1
                   , min = 0
                   , max = input$totalTime
                   , value = 0
                   , animate = animationOptions(loop = TRUE))
  })

  observeEvent(rv$sim, {
    updateRadioButtons(session, "graphType", selected = "pop")
    output$gy <- renderPlot({
      time <- input$momG
      if (is.null(time) || time == 0 ) {
        deaths <- c(0L,0L,0L)
      } else {
        deaths <- deathCauses[time, ]
      }
      lm <- deaths[1]
      fox <- deaths[2]
      mal <- deaths[3]
      n <- sum(deaths)

      if ( n == 0) {
#         if (is.null(time) || time == 0) {
#           pop <- initial[3]
#         } else {
#           pop <- rv$daframe$population[time]
#         }
#         sub <- ifelse(pop == 0,
#                       "Sorry, all of the rabbits have died!",
#                       "Only recently-deceased unburied rabbits are shown.")
        plot(0, 0, col = "transparent"
             , axes = FALSE
             , main = "Graveyard"
             , sub = "Only recently-deceased unburied rabbits are shown."
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
#         if ( pop == 0 ) {
#           rasterImage(img, xleft = 0.25, ybottom = 0, xright = 0.75, ytop = 1)
#         }
      } else {
        xd <- runif(n)
        yd <- runif(n)
        cause <- c(rep("red", lm), rep("black", fox), rep("blue", mal))
        plot(xd, yd, col = cause
             , pch = 19
             , cex = 1.5
             , axes = FALSE
             , main = "Graveyard"
             , sub = "Only recently-deceased unburied rabbits are shown."
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
      }
    })
  })


  observeEvent(rv$sim, {
    output$field <- renderPlot({
      time <- input$mom
      if (is.null(time) || time == 0) {
        na <- initial[3]
        xa <- runif(na)
        ya <- runif(na)
      }
      else {
        # number of adults
        na <- rv$daframe$population[time] - sum(rv$littersizes[[time]])
        xa <- runif(na)
        ya <- runif(na)
        nl <- rv$daframe$numLitters[time]
        myCens <- makeCenters(nl)
        cents <- myCens$centers
        d <- myCens$distance
        xb <- NULL
        yb <- NULL
        if(nl != 0) {
          for(i in 1:nl){
            cent <- cents[i,]
            sd <- d/6
            xbbit <- rnorm(rv$littersizes[[time]][i],mean = cent$x, sd = sd)
            ybbit <- rnorm(rv$littersizes[[time]][i],mean = cent$y, sd = sd)
            xb <- c(xb, xbbit)
            yb <- c(yb, ybbit)
          }
        }
      }
      # determine field color
      b <- isolate(input$b)
      d <- isolate(input$d)
      pop <- rv$currentPopField
      if ( b > d ) {
        top <- isolate(input$m)
        color <- field.color(top, pop)
      } else {
        top <- 100 * rv$daframe$population[1]
        color <- field.color(top,pop)
      }
      

      par(mfrow = c(1,2))
      plot(NULL,NULL, axes = FALSE, cex = 0.5
           , pch = 19
           , main = paste0("Field:  ",na, " Adult(s) Foraging")
           , xlab = ""
           , ylab = ""
           , xlim = c(0,1)
           , ylim = c(0,1))
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = color)
      points(
        xa, ya, cex = 0.5, pch = 19
      )
      if (is.null(time) || time == 0) {
        plot(0, 0, col = "transparent"
             , cex = 0.5
             , pch = 19
             , axes = FALSE
             , main = "Warren:  No Babies Here Yet!"
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4])
      }
      else {
        plot(xb,yb, axes = FALSE, cex = 0.5
             , pch = 19
             , main = paste0("Warren:  ",length(xb)," Newborn Rabbits Here!")
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4])
      }
      par(mfrow = c(1,1))
    }, height = 375)
  })


  output$babies <- renderTable({
    if (is.null(input$mom) || input$mom == 0) {
      out <- NULL
    }
    else {
      num <- rv$littersizes[[input$mom]]
      num <- num[num > 0]
      if (length(num) > 0) {
        df <- data.frame(length(num), mean(num))
        names(df) <- c("Number of Litters", "Mean Litter Size")
        out <- df
      } else {
        out <- NULL
        }
    }
    out
  }, include.rownames = FALSE)
  
  # if relevant, create a table in graveyard tab to show where we stand with respect 
  # to carrying capacity
  output$gyPopCapRep <- renderTable({
    time <- input$momG - 1
    num <- as.integer(rv$daframe$population[time + 1])
    cap <- as.integer(input$m)
    if (input$b > input$d) {
      mat <- matrix(c(num, cap), ncol = 2)
      colnames(mat) <- c("Population", "Capacity")
    } else {
      mat <- matrix(num, ncol = 1)
      colnames(mat) <- "Population"
    }
    mat
  }, include.rownames = F)


  output$deathTallies <- renderTable({
    time <- input$momG
    if (is.null(time) || time == 0) {
      deaths <- c(0L,0L,0L)
    } else {
      deaths <- as.integer(deathCauses[input$momG, ])
    }
    mat <- matrix(deaths, ncol = 3)
    colnames(mat) <- c("Lawnmower (red)", "Fox (black)", "Malnutrition (blue)")
    rownames(mat) <- "Cause"
    mat
  })
  

  observe({
    if (input$helpDeathTallies > 0) {
    info(paste0("A rabbit can die in any one of three ways:  it can be run",
                " over by a lawnmower, killed by a fox, or die from malnutrition.",
                "  When carrying capacity is relevant and population is low,",
                " there is plenty of grass and few foxes patrol the field,",
                " so foxes and malnutrion will be unlikely as causes of death.",
                " As the population",
                " rises, fox-death and malnutrition-death become more likely",
                " death by lawnmower becomes less likely.",
                "  When carrying capcity is not relevant then malnutrition",
                " is not a cause of death  and death by fox is unlikely at",
                " low population size and more likely at high population size."))
    }
    })
  
  observe({
    if ( input$helpField > 0) {
      info(paste0(
        "When carrying capacity is relevant, you will see that the field is",
        " mostly green in color when the population is low (lots of grass)",
        " and closer to brown when the poplulation is high (mostly bare ground)",
        "  When carrying capacity is not relevant, the color of the field will",
        " not change as population changes."
      ))
    }
  })
  
  observe({
    if ( input$sliderTip > 0) {
      info(paste0(
        "To change a slider value by one unit per click, click on the",
        " slider and then use left and right arrow-keys."
      ))
    }
  })

}