library(shiny)


shinyUI(pageWithSidebar(
  
  headerPanel("Kernel Density Estimators"),
  sidebarPanel(
    sliderInput(inputId="adjust", label="Adjust Default Bandwidth", 
                min=0.01,max=2,value=1,step=0.01),
    selectInput(inputId="kernel",label="Kernel",
            choices=c("Gaussian" = "gaussian","Epanechnikov" = "epanechnikov","Rectangular" = "rectangular",
                "Triangular" = "triangular","Biweight" = "biweight","Cosine" = "cosine","Optcosine" = "optcosine"))
  ),
  mainPanel(
    plotOutput("dens")
  )
))