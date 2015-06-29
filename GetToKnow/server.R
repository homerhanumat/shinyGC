library(ggplot2)
library(mosaic)
library(shiny)
library(magrittr)
library(DT)
library(leaflet)
library(XML)
library(dismo)

# for markers
iconURL <- "Tiger_head_solo.png"

labelFinder <- function(varName) {
  switch(varName,
         "age" = "Age (in years)",
         "major" = "Intended Major",
         "height" = "Height (inches)",
         "ideal_ht" = "Ideal Height (inches)",
         "fastest" = "Fastest speed ever driven (mph)",
         "sleep" = "Average amount of sleep per night (hours)",
         "seat" = "Preferred classroom seating location",
         "love_first" = "Belief in love at first sight",
         "extra_life" = "Belief in extraterrestrial life",
         "sex" = "Sex",
         "random" = "Random Number choice"
         )
}

denVars <- c("age","height","ideal_ht","fastest","sleep")
barVars <- c("major","seat","love_first","extra_life","sex","random")

processVariables <- function(data) {
  data$time <- as.POSIXct(data$time)
  data$major <- factor(data$major)
  data$seat <- factor(data$seat,levels = c("front","middle","back"))
  data$love_first <- factor(data$love_first,levels = c("yes","no"))
  data$extra_life <- factor(data$extra_life,levels = c("yes","no"))
  data$sex <- factor(data$sex,levels = c("female","male"))
  data$random <- factor(data$random, levels = as.character(1:10))
  return(data)
}

# read in previous responses
if (file.exists("responses.csv")) {
  responses <- read.csv(file = "responses.csv", header = TRUE, stringsAsFactors = FALSE)
  responses <- processVariables(responses)
} else {
  address <- "198 Hiawatha Trail, Georgetown KY 40324, USA"
  location <- geocode(address, oneRecord = TRUE)
  responses <- data.frame(name = "Homer",
                          class = NA,
                          semester = NA,
                          year = NA,
                          age = 52,
                          address = address,
                          major = "Philosophy",
                          height = 74.8,
                          ideal_ht = 74.8,
                          fastest = 85,
                          sleep = 7,
                          seat = "front",
                          love_first = "yes",
                          extra_life = "yes",
                          sex = "male",
                          random = 4,
                          surprise = "I read Sanskrit.",
                          link = "http://statistics.georgetowncollege.edu",
                          latitude = location$lat,
                          longitude = location$lon,
                          time = as.character(Sys.time())
                        )
  write.csv(responses, file = "responses.csv", row.names = FALSE)
}

function(input, output, session) {
  
  rv <- reactiveValues(
    responses = responses
  )
  
  observeEvent(input$get_started,{
               shinyjs::toggleState("intro_message")
               shinyjs::toggleState("form")
               })
  
  observeEvent(input$submit,{
    location <- geocode(input$address, oneRecord = TRUE)
    # create response.  Jitter locations a bit in case two respondent slive at the 
    # same address
    response <- data.frame(name = input$name,
                           class = input$class,
                           semester = input$semester,
                           year = input$year,
                           age = input$age,
                           address = input$address,
                           major = input$major,
                           height = input$height,
                           ideal_ht = input$ideal_ht,
                           fastest = input$fastest,
                           sleep = input$sleep,
                           seat = input$seat,
                           love_first = input$love_first,
                           extra_life = input$extra_life,
                           sex = input$sex,
                           random = input$random,
                           surprise = input$surprise,
                           link = input$link,
                           time = as.character(Sys.time()),
                           latitude = location$lat + runif(1, min = -0.0001,0.0001),
                           longitude = location$lon + runif(1, min = -0.0001,0.0001)
    )
    #update rv:
    rv$responses <- rbind(rv$responses, response)
    # write to file:
    write.table(rv$responses, file = "responses.csv", row.names = FALSE, sep = ",")
    # show thank-you:
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  })
  
  observeEvent(input$sumbit_another,{
    shinyjs::hide("thankyou_msg")
    shinyjs::show("form")
  })
  
  observe({
    input$update1
    input$update2
    input$update3
    temp <- read.csv(file = "responses.csv", header = TRUE, stringsAsFactors = FALSE)
    rv$responses <- processVariables(temp)
  })
  
  output$map <- renderLeaflet({
    responses <- rv$responses
    var <- input$varMap
    n <- nrow(responses)
    latitude <- responses$latitude
    longitude <- responses$longitude
    info <- paste(responses$name,":  ",responses[,var])
    m <- leaflet(responses) %>%
      addTiles() %>%
      addMarkers(lat = latitude, lng = longitude, popup = info, 
                 icon = list(iconUrl = iconURL, iconSize = c(40,40)))
    m
  })
  
  output$graph <- renderPlot({
    responses <- rv$responses
    n <- nrow(responses)
    varName <- input$varSummary
    variable <- responses[,varName]
    if (is.numeric(variable) && n  >= 3 ) {
      title <- paste0("Density Plot of ",varName)
      return(ggplot(responses, aes_string(x = varName)) +
        geom_density() + geom_rug() +
        labs(title = title, x = labelFinder(varName)))
    } 
    if ( is.factor(variable) ) {
      title <- paste0("Bar Graph of ",varName)
      return(ggplot(responses, aes_string(x = varName)) +
        geom_bar(fill = "burlywood") + 
        scale_x_discrete(drop = FALSE) +
        labs(title = title, x = labelFinder(varName)))
    }
  })
  
  output$summary <- renderTable({
    responses <- rv$responses
    n <- nrow(responses)
    varName <- input$varSummary
    variable <- responses[,varName]
    form <- as.formula(paste0("~ ",varName))
    if (is.numeric(variable)) {
      if (n >= 2) {
        return(favstats(form, data = responses)[c(1,3,5,6,7,8)])
      } else {
        return(favstats(form, data = responses)[c(1,3,5,6,8)])
      }
    } 
    if (is.factor(variable)) {
      return(t(xtabs(form, data = responses)))
    }
    if (varName == "surprise") {
      data.frame(name = responses$name, surprise = responses$surprise)
    }
    
  }, include.rownames = FALSE)
  
  output$responses <- DT::renderDataTable({
    responses2 <- rv$responses
    responses2$address <- NULL
    responses2$latitude <- NULL
    responses2$longitude <- NULL
    responses2$time <- NULL
    responses2$surprise <- NULL
    responses2$link <- NULL
    responses2$year <- factor(responses2$year)
    responses2$semester <- factor(responses2$semester)
    responses2$class <- factor(responses2$class)
    responses2$random <- factor(responses2$random)
    datatable(responses2,rownames = FALSE, 
              caption = "Here are all the responses:",
              filter = "bottom") %>%
      formatStyle(
        'name',
        backgroundColor = styleEqual(input$name, 'lightblue'))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "responses.csv"
    },
    content = function(file) {
      sep <- ","
      # Write to a file specified by the 'file' argument
      write.table(responses, file, sep = sep,
                  row.names = FALSE)
    }
  )
}