library(shiny)
library(shinydashboard)

header <- dashboardHeader(title="Dice Races")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sum-in-Range Race",tabName="gameA"),
    menuItem("Even/Odd Sum Race",tabName="gameB"),
    menuItem("Even/Odd Product Race",tabName="gameC")
    )
)

docA <- paste0("<p>There are three teams:  Red, Green and Blue.  ",
               "Each player rolls a pair of fair dice.</p>",
               "<ul>",
               "<li>A red player gets a point if her sum is 2,3,4 or 5.</li>",
               "<li>A green player gets a point if her sum is 6,7 or 8.</li>",
               "<li>A blue player gets a point if her sum is 9, 10, 11 or 12.</li>",
               "</ul>",
               "<p>Each player keeps rolling until she gets 10 points.  ",
               "The first team to have all its players finish is the winner.</p>"
)

docB <- paste0("<p>There are two teams:  Red and Blue.  ",
               "Each player rolls a pair of fair dice.</p>",
               "<ul>",
               "<li>A red player gets a point if her sum is even.</li>",
               "<li>A blue player gets a point if her sum is odd.</li>",
               "</ul>",
               "<p>Each player keeps rolling until she gets 10 points.  ",
               "The first team to have all its players finish is the winner.</p>"
)

docC <- paste0("<p>There are two teams:  Red and Blue.  ",
               "Each player rolls a pair of fair dice.</p>",
               "<ul>",
               "<li>A red player gets a point if her product is even.</li>",
               "<li>A blue player gets a point if her product is odd.</li>",
               "</ul>",
               "<p>Each player keeps rolling until she gets 10 points.  ",
               "The first team to have all its players finish is the winner.</p>"
)

body <- dashboardBody(
  tabItems(
    tabItem("gameA",
            box(width=12,title="Sum-in-Range Race",
              plotOutput("bargraphA"),
              actionButton("rollA","Players, roll your dice!",
                           style="font-size: 200%;"),
              actionButton("resetA","Start Over",
                           style="font-size:  150%")
                ),
            box(width=12,title=NULL,
                HTML(docA)
            )
            ),
    tabItem("gameB",
            box(width=12,title="Even/Odd Sum Race",
                plotOutput("bargraphB"),
                actionButton("rollB","Players, roll your dice!",
                             style="font-size: 200%;"),
                actionButton("resetB","Start Over",
                             style="font-size: 150%;")
                ),
            box(width=12,title=NULL,
                HTML(docB)
            )
            ),
    tabItem("gameC",
            box(width=12,title="Even/Odd Product Race",
                plotOutput("bargraphC"),
                actionButton("rollC","Players, roll your dice!",
                             style="font-size: 200%;"),
                actionButton("resetC","Start Over",
                             style="font-size: 150%;")
                ),
            box(width=12,title=NULL,
                HTML(docC)
            )
            )
    
    ) #end tabItems
  
  ) #end dashboardBody

dashboardPage(
  header,
  sidebar,
  body
)