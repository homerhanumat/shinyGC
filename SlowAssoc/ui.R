library(shiny)
library(rhandsontable)

# no more than this many sims at any one time
simLimit <- 10000

navbarPage(
  title = "Chi-Square Test for Association",
  tabPanel(
    title = "The Test",
fluidPage(
  sidebarPanel(
    conditionalPanel(
      condition = "output.state == 'tableSetup'",
    numericInput(inputId ="rows", "Number of rows", min = 2, max = 5, 
                 step = 1, value = 2),
    helpText("Enter row names (separated by commas):"),
    textInput(inputId = "rowNames","Row Names",
              value = "cool,warm"),
    numericInput(inputId ="cols", "Number of columns", min = 2, max = 5, 
                 step = 1, value = 2),
    helpText("Enter column names (separated by commas):"),
    textInput(inputId = "colNames","Column Names",
              value = "baiting,polite"),
    actionButton(inputId = "submitTable", "Submit the Table")
    ),
    conditionalPanel(
      condition = "output.state == 'simSetup'",
      helpText("Select a method for simulation (see About Tab for details)."),
      radioButtons(inputId = "simMethod", "Simulation Method",
                   choices = c("row/col sums fixed" = "rcFix",
                               "row sums fixed" = "rFix",
                               "neither fixed" = "nFix"))
    ),
    conditionalPanel(
      condition = "output.state != 'tableSetup'",
      helpText("How many simulations do you want the machine to perform at once?","
               (Upper limit is 10000)"),
      numericInput(inputId = "numberSims", "Number of Simulations",
                   min = 1, max = simLimit, value = 1),
      actionButton(inputId = "sim", "Simulate Now"),
      actionButton(inputId = "reset", "Reset (Same Table)")
    ),
    conditionalPanel(
      condition = "output.state != 'tableSetup'",
      actionButton(inputId = "newTable", "Make New Table")
    ),
  width = 2),
  mainPanel(
    conditionalPanel(
      condition = "input.submitTable == 0 || output.state == 'tableSetup'",
      HTML(paste0("<h3>Enter your two-way table</h3>",
                   "<p>You use the table below (see the About Tab for a description),",
                   " or you can enter your own table usng the input widgets on the ",
                   "sideboard.</p><hr>")),
      rHandsontableOutput("cross", height = 160),
      HTML("<hr><p>When you are ready, press the button to submit your table.</p>")
    ),
    conditionalPanel(
      condition = "output.state == 'simSetup'",
      fluidRow(
        column(4,
               radioButtons(inputId = "barmosInit", 
                   label = "Graph Type:",
                   choices = c("Bar Chart" = "bar",
                                "Mosaic Plot" = "mosaic"),
                   selected = "bar", inline = TRUE)
               ),
        column(6,
               h5(textOutput("remarksInitial"))
               )
      ),
      plotOutput("mosaicInitial", height = 350),
      fluidRow(
        column(3,
               h5("Observed"),
               tableOutput("obsTable")
        ),
        column(3, offset = 1,
               h5("Expected by Null"),
               tableOutput("expTable")
        ),
        column(4,offset=1,
               h5("Contributions"),
               tableOutput("contrTable")
        )
      )
    ),
    conditionalPanel(
      condition = "output.state == 'simulating'",
      tabsetPanel(
        tabPanel(
          title = "Latest Simulation",
          fluidRow(
          column(4,
                 radioButtons(inputId = "barmosLatest", 
                       label = "Graph Type:",
                       choices = c("Bar Chart" = "bar",
                                   "Mosaic Plot" = "mosaic"),
                       selected = "bar", inline = TRUE)
                 ),
          column(6,
                 p(textOutput("remarksLatest1"))
                 )
          ),
          plotOutput("mosaicLatest", height = 350),
          fluidRow(
            column(4,
                  h5("Simulated Table"),
                  tableOutput("latestTable")
            ),
            column(4,offset=2,
                  h5("Expected Table"),
                  tableOutput("latestExpTable")
            )
          ),
          tableOutput("summary1"),
          p(textOutput("remarksProbBar"))
        ),
        tabPanel(
          title = "Density Plot",
          plotOutput("densityplot"),
          p(textOutput("remarksLatest2")),
          tableOutput("summary2"),
          p(textOutput("remarksProbDensity"))
        ),
        tabPanel(
          title = "Curve",
          plotOutput("chisqCurve"),
          br(),
          splitLayout(
            checkboxInput("compareDen",
                          HTML("Compare with simulated <br>chi-square distribution")),
            checkboxInput("yates","Use Yates correction")
          ),
          p(textOutput("remarksProb"))
        ),
        id = "tabs"
      )
    )
  )
)
),# end tabPanel "The Test"
tabPanel(
  title = "About",
  includeHTML("informationFiles/about.html")
)

) #nd navbarPage