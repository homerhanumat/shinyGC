library(DT)
library(leaflet)
library(shinyjs)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

surveyVarChoices <- c("Age" = "age",
                      "Address" = "address",
                      "Major" = "major",
                      "Height" = "height",
                      "Ideal height" = "ideal_ht",
                      "Fastest speed ever driven" = "fastest",
                      "average amount of sleep" = "sleep",
                      "Seating preference" = "seat",
                      "Belief in love at first sight" = "love_first",
                      "Belief in extraterrestrial life" = "extra_life",
                      "Sex" = "sex",
                      "Favorite website" = "link",
                      "Random choice" = "random",
                      "Surprising Fact" = "surprise"
                      )

surveyVarChoicesSummary <- c("Age" = "age",
                             "Major" = "major",
                             "Height" = "height",
                             "Ideal height" = "ideal_ht",
                             "Fastest speed ever driven" = "fastest",
                             "average amount of sleep" = "sleep",
                             "Seating preference" = "seat",
                             "Belief in love at first sight" = "love_first",
                             "Belief in extraterrestrial life" = "extra_life",
                             "Sex" = "sex",
                             "Random choice" = "random",
                             "Surprising fact" = "surprise"
                             )

navbarPage(
  # shinyjs::useShinyjs(),
  title = "Ice-Breaker Survey",
  tabPanel(
    title = "Survey",
    fluidPage(
      conditionalPanel(
        condition = "input.submit == 0",
        column(width = 6,
        textInput("name", "Name", ""),
        textInput("class", "Name of this course", ""),
      selectInput("semester","Semester", 
                  choices = c("Fall","Spring","Summer1","Summer2"), selected = ""),
      selectInput("year", "Year", 
                  choices = c("",as.character(2015:2020)), selected = ""),
      sliderInput("age", "Your Age", 15, 70, 18, step = 1,ticks = FALSE),
      textInput("address", "Your Address (type, do not use autofill):", ""),
      selectInput("major", "Intended Major",
                choices = c("","Accounting","Athletic Training","Art","Biology",
                            "Business",
                            "Chemistry",
                            "Communication",
                            "Criminal Justice",
                            "Economics",
                            "English",
                            "Environmental Science",
                            "History",
                            "Kinesiology","Math",
                            "Philosophy",
                            "Political Science",
                            "Psychology",
                            "Physics","Religion",
                            "Sociology","Spanish",
                            "Sports Administration",
                            "Theater",
                            "Other Major",
                            "I Have No Idea!"),
                selected = ""),
      sliderInput("height","Your height (in inches)",53,84,66,
                step = 0.5,ticks = FALSE),
      sliderInput("ideal_ht","Your ideal height (in inches)",53,84,66,
                step = 0.5,ticks = FALSE)
    ),
    column(width = 6,
      selectInput("seat","Where you prefer to sit in a classroom:",
                  choices = c("","front","middle","back"),
                  selected = ""),
      selectInput("love_first","Do you believe in love at first sight?",
                  choices = c("","yes","no"),selected = ""),
      selectInput("extra_life","Do you believe in extraterrestrial life?",
                  choices = c("","yes","no"), selected = ""),
      selectInput("sex","I am a:",choices = c("","female","male"), selected = ""),
      selectInput("random", "Pick one of these numbers at random:",
                  choices = c("",as.character(1:10)), selected = ""),
      textInput("link","URL of a favorite website:","http://google.com"),
      textInput("surprise","A Surprising fact about me:","I have no surprises"),
      sliderInput("fastest","Fastest speed you ever drove a car (in mph)",0,200,70,
                  step = 1,ticks = FALSE),
      sliderInput("sleep","Average amount of sleep at night (hours)",0,20,7,
                  step = 0.5,ticks = FALSE),
      actionButton("submit", "Submit", class = "btn-primary")
    )
  )),
    conditionalPanel(
      condition = "input.submit > 0",
      h3("Thanks for your submission!")
    )
  ),
  tabPanel(
    title = "Map",
    fluidPage(
      sidebarPanel(
        selectInput("varMap", "Variable to Display",
                    choices = surveyVarChoices,selected = "age"),
        helpText("Here are some controls to select who shows up on the map:"),
        # Render the filter controls:
        uiOutput("classMap"),
        uiOutput("semesterMap"),
        uiOutput("yearMap"),
        uiOutput("sexMap"),
        uiOutput("fastestMap"),
        actionButton("update1","Update (as others enter data)")
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  tabPanel(
    title = "Summary",
    fluidPage(
      sidebarPanel(
        selectInput("varSummary", "Variable to Display",
                    choices = surveyVarChoicesSummary,selected = "age"),
        actionButton("update2","Update (as others enter data)")
      ),
      mainPanel(
        plotOutput("graph"),
        tableOutput("summary")
      )
    )
  ),
  tabPanel(
    title = "Responses",
    fluidRow(
      column(width = 3,
             actionButton("update3", "Update (as others enter data)")),
      column(width = 3,
             downloadButton('downloadData', 'Download'))
    ),
    HTML("<p> </p>"),
    DT::dataTableOutput("responses")
  )
)