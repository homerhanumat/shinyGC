library(shiny)
library(shinythemes)
library(shinyjs)

max.time<-1000
max.b <- 1
min.d <- 1
max.m <-1000
max.n_0 <- 1000

navbarPage(
  title = "Logistic Growth for a Population",
  theme = shinytheme("spacelab"),
  tabPanel(
  useShinyjs(),
  title = "The App",
  sidebarPanel(
    conditionalPanel(condition = "input.goButton == 0 || output.beginning == true",
      sliderInput(inputId = "totalTime", label = "Time",
                min = max.time*.1, max = max.time, step = 50, value = max.time*0.5),
      sliderInput(inputId = "n_0", label = "Initial Population",
                min = 2, max = max.n_0, value = 20, step = 1),
      sliderInput(inputId = "b", label = "Birth Rate",
                min = 0, max = max.b, value = .05, step = 0.01),
      sliderInput(inputId = "d", label = "Death Rate",
                min = 0, max = min.d, value = .02, step = 0.01),
      conditionalPanel(condition = "input.b > input.d",
        sliderInput(inputId = "m", label = "Carrying Capacity",
                  min = 10, max = max.m, step = 10, value = max.m*0.8)),
      conditionalPanel(condition = "input.b <= input.d",
        helpText("Carrying Capacity doesn't affect population when death rate",
               "is greater than birth rate.")),
      actionLink(inputId = "sliderTip", "Tip for Using Sliders",
                 icon = icon("info-sign", lib = "glyphicon")),
      checkboxInput(inputId = 'seed', label = "Do you want to set the seed?"),
      conditionalPanel(condition = "input.seed == true",
        numericInput(inputId = 'setter', label = "Set your seed", value = 2200,
                    min = 1, step = 1))
    ),
    helpText("Click the button below for simulation results:"),
    actionButton("goButton", "Simulate"),
    conditionalPanel(
      condition = "output.beginning == false",
      br(),
      actionButton("reset", "Start Over")
    )
  ),
  mainPanel(
    conditionalPanel(
      condition = "input.goButton == 0 || output.beginning == true",
      div(style = "display:inline-block; width: 60%",
        radioButtons("initialGraphType", "Y-Axis shows Population: ",
                   choices = c("Size" = "pop",
                               "Growth Rate" = "rate",
                               "Per-capita Growth Rate" = "relRate"),
                   inline = TRUE)),
      div(style = "display:inline-block; width: 30%",
          conditionalPanel(condition = 'input.initialGraphType != "pop"',
                        radioButtons("initialGraphX", "X-Axis shows: ",
                          choices = c("Time" = "time",
                                    "Population" = "pop"),
                          inline = TRUE))
                       ),
      plotOutput("initialGraph"),
      htmlOutput("initialDiscuss")
    ),
    conditionalPanel(
      condition = "input.goButton > 0 && output.beginning == false",
      tabsetPanel(
      tabPanel(
        title = "Population Graphs",
        div(style = "display:inline-block; width: 60%",
            radioButtons("graphType", "Y-Axis shows Population: ",
                         choices = c("Size" = "pop",
                                     "Growth Rate" = "rate",
                                     "Per-capita Growth Rate" = "relRate"),
                         inline = TRUE)),
        div(style = "display:inline-block; width: 30%",
            conditionalPanel(condition = 'input.graphType != "pop"',
                             radioButtons("graphX", "X-Axis shows: ",
                                          choices = c("Time" = "time",
                                                      "Population" = "pop"),
                                          inline = TRUE))
        ),
        plotOutput("pop"),
        htmlOutput('discuss')
      ),
      tabPanel(
        title = "Field"
        , fluidRow(
          column(width = 5, uiOutput("momentF")),
          column(width = 4, br(),br(),
                 conditionalPanel(
                   condition = "output.currentPopField > 0",
                   actionLink("helpField","Explain Field Color",
                              icon = icon("question-sign", lib = "glyphicon")))))
        , conditionalPanel(
            condition = "output.currentPopField > 0"
            , plotOutput("field")
            , tableOutput("babies")
        )
        , conditionalPanel(
            condition = "output.currentPopField == 0"
            , HTML('<figure>
                     <img src="images/dead_bunny.png" width="400" height="400">
                     <figcaption>All the bunnies are dead!</figcaption>
                     </figure>')
        )
      ),
      tabPanel(
        title = "Graveyard"
        , fluidRow(
          column(width = 5, uiOutput("momentG")),
          column(width = 4, br(),br(),
                 actionLink("helpDeathTallies","Explain Death Causes",
                            icon = icon("question-sign", lib = "glyphicon"))))
        , conditionalPanel(
            condition = "output.currentPopGY > 0"
            , plotOutput("gy")
            , div(style = "display:inline-block; width: 60%",
              tableOutput("deathTallies"))
            , div(style = "display:inline-block; width: 30%",
              tableOutput("gyPopCapRep"))
        )
        , conditionalPanel(
          condition = "output.currentPopGY == 0"
          , HTML('<figure>
                 <img src="images/dead_bunny.png" width="400" height="400">
                 <figcaption>All the bunnies are dead!</figcaption>
                 </figure>')
          )
      ),
      id = "tabset"
    )
    ) # end 2nd cond panel in main panel
  )
  ),
  tabPanel(
    title = "About"
    , includeHTML("./info.files/about.html")
  )
)