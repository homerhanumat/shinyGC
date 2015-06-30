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

fieldsMandatory <- c("name", "class", "semester","year","address")

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
    responses = responses,
    mapFiltered = rep(TRUE, nrow(responses))
  )
  
  # Enable the Submit button when all mandatory fields are filled out
#   observe({
#     mandatoryFilled <-
#       vapply(fieldsMandatory,
#              function(x) {
#                !is.null(input[[x]]) && input[[x]] != ""
#              },
#              logical(1))
#     mandatoryFilled <- all(mandatoryFilled)
#     
#     shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
#   })
  
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
  })
  
  observe({
    input$update1
    input$update2
    input$update3
    temp <- read.csv(file = "responses.csv", header = TRUE, stringsAsFactors = FALSE)
    rv$responses <- processVariables(temp)
  })
  
  output$classMap <- renderUI({
    var <- rv$responses$class
    uni <- unique(var)
    vals <- uni[!is.na(uni)]
    selectInput(inputId = "classMap", label = "Class", multiple = TRUE,
                   choices = c("",vals), selected = "")
  })
  
  output$semesterMap <- renderUI({
    var <- rv$responses$semester
    uni <- unique(var)
    vals <- uni[!is.na(uni)]
    selectInput(inputId = "semesterMap", label = "Semester", multiple = TRUE,
                   choices = c("",vals), selected = "")
  })
  
  output$yearMap <- renderUI({
    var <- as.character(rv$responses$year)
    uni <- unique(var)
    vals <- uni[!is.na(uni)]
    selectInput(inputId = "yearMap", label = "Year", multiple = TRUE,
                   choices = c("",vals), selected = "")
  })
  
  output$sexMap <- renderUI({
    var <- as.character(rv$responses$sex)
    uni <- unique(var)
    vals <- uni[!is.na(uni)]
    selectInput(inputId = "sexMap", label = "Sex",
                   choices = c("",vals), selected = "")
  })
  
  output$fastestMap <- renderUI({
    speeds <- rv$responses$fastest
    sliderInput(inputId = "fastestMap", label = "Fastest speed driven:",
                min = min(speeds), max = max(speeds),
                value = c(min(speeds), max(speeds)))
  })
  
  observe({
    resp <- rv$responses
    all <- rep(TRUE, nrow(resp))
    if (!is.null(input$classMap)) {
      classBool <- with(resp, class %in% input$classMap)
    } else {
        classBool <- all
    }
    if (!is.null(input$semesterMap)) {
      semesterBool <- with(resp, semester %in% input$semesterMap)
    } else {
      semesterBool <- all
    }
    if (!is.null(input$yearMap)) {
      yearBool <- with(resp, year %in% input$yearMap)
    } else {
      yearBool <- all
    }
    if ( !is.null(input$sexMap) && input$sexMap != "") {
      sexBool <- with(resp, sex %in% input$sexMap)
    } else {
      sexBool <- all
    }
    sr <- input$fastestMap
    if (!is.null(sr)) {
      fastestBool <- with(resp, sr[1] <= fastest & fastest <= sr[2])
    } else {
      fastestBool <- all
    }
    rv$mapFiltered <- classBool & semesterBool & yearBool & fastestBool & sexBool
  })
  
  output$map <- renderLeaflet({
    responses <- rv$responses
    filt <- rv$mapFiltered
    var <- input$varMap
    marked <- responses[filt, ]
    latitude <- marked$latitude
    longitude <- marked$longitude
    info <- paste(marked$name,":  ",marked[,var])
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