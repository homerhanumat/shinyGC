library(shiny)
library(shinythemes)

# Define UI for SamplingMethods application
fluidPage(
  
  #  Application title
  titlePanel("Three Random Sampling Methods"),
  theme = shinytheme("cerulean"),
  
  # Sidebar
  sidebarPanel(
      
      helpText("Pick a sample size from the options below."),
    
      selectInput(inputId="n", label="Sample Size:",
                  choices=c(16,32,64,160,640)),
      
      helpText("What sampling method would you like to use?"),

      selectInput(inputId="method", label="Sampling Method:",
                  choices=c("Simple Random Sampling"="srs",
                            "Stratified Sampling"="strat",
                            "Cluster Sampling"="cluster")),
      checkboxInput("hide","Hide District Boundaries",value=FALSE),
      actionButton("sample","Sample Now"),
      actionButton("reset","Start Over")
      
    ), # end sidebar panel


  
  # Here comes the main panel
  
  mainPanel(
    
    conditionalPanel(
      condition="input.sample == 0 || output.beginning == true",
      plotOutput("population"),
      h3("The population has 2704 members, evenly divided into 16 districts."),
      h3("Each dot is a member of the population.")
    ),
    
    conditionalPanel(
      condition="output.beginning == false",
      plotOutput("sampleplot"),
      h3(textOutput("explanation"))
    )
    
    
  ) #end main panel
  
)