## Shiny app to play Star Trek


## attach necessary packages:
library(shinydashboard)
library(shinyjs)

## globals ----

klingon_step <- function(dist) {
  15 - dist / 5
}
klingon_angle_fuzz <- pi / 4
ship <- c(x = 50, y = 50)
dim <- 100
interval <- 2000
shields_initial <- 100
torpedo_radius <- function(d) {
  5 - d / 15
}
initial_torpedoes <- 10

get_initial_location <- function() {
  angle <- runif(1, 0, 2 * pi)
  ship + dim / 2 * c(x = cos(angle), y = sin(angle))
}

move <- function(len, pos, target) {
  angle_to_target <-
    atan2(
      y = target["y"] - pos["y"],
      x = target["x"] - pos["x"]
    )
  movement_angle <- runif(
    1,
    angle_to_target - klingon_angle_fuzz,
    angle_to_target + klingon_angle_fuzz
  )
  len * c(x = cos(movement_angle), y = sin(movement_angle))
}

klingon_shot <- function(dist) {
  upper <- min(dim / 2 - dist, 15)
  round(runif(1, min = 0, max = upper), digits = 1)
}

distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}


## user interface ----
ui <- shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Star Trek"),
    dashboardSidebar(
      sidebarMenu(
        id = "menu",
        menuItem(
          "About the Game",
          icon = icon("circle-info"),
          tabName = "about"
        ),
        menuItem(
          "Star Trek",
          tabName = "game",
          icon = icon("star")
        )
      )
    ),
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "game",
          fluidRow(
            column(
              width = 8,
              htmlOutput("scotty", style = "margin: auto")
            ),
            column(
              width = 2,
              actionButton(
                inputId = "start",
                label = "Start Game"
              )
            ),
            column(
              width = 2,
              hidden(
                actionButton(
                  inputId = "restart",
                  label = "Play Again"
                )
              )
            ),
            br(), br(),
            plotOutput("plot", height = "auto", click = "expl")
          )
        ),
        tabItem(
          tabName = "about",
          markdown(
            "
# Welcome to Star Trek!

## The Situation

Your star ship, the *The Enterprise*, is the blue circle in the center of the field.  A Klingon ship (the red dot) is approaching.  Every two seconds its moves, bobbing and weaving somewhat randomly, but generally making its way toward the *Enterprise*.

When it moves the Klingon ship also fires at the *Enterprise*:  the closer it gets, the more damage it can do. It also moves faster as it gets closer.

## Shields

The *Enterprise* shields are initially rated at 100 units, but the rating decreases as the *Enterprise* sustains fires from the Klingon ship.  If the shield-rating falls below zero, the *Enterprise* will be destroyed.

## Photon Torpedoes

The *Enterprise* is equipped with ten photon torpedoes.  You can fire a torpedo by clicking on a point in the field:  the torpedo will be launched from the *Enterprise* and will proceed to the click-point, exploding when it gets there.  If the Klingon ship is within the the lethal radius of the blast, it will be destroyed.  The further the blast-point is from the Enterprise, however, the smaller the lethal radius will be.

You can fire torpedoes as often as you like.  Every two seconds the screen will reveal the path and blast-radius of the torpedoes you have fired during that time, and your Chief Engineer Mr. Scot will report on the current shield-level and the number of torpedoes left to fire.
            "
          )
        )
      )
    ) # end dashboard body
  ) # end dashboard page
) # end shinyUI


## server logic ----
server <- function(input, output, session) {
  rv <- reactiveValues(
    klingon = get_initial_location(),
    shields = shields_initial,
    shot = NULL,
    klingon_alive = TRUE,
    torpedo_supply = initial_torpedoes,
    step = 0
  )


  observeEvent(input$restart, {
    rv$klingon <- get_initial_location()
    rv$shields <- shields_initial
    rv$shot <- NULL
    rv$klingon_alive <- TRUE
    rv$torpedo_supply <- initial_torpedoes
    rv$step <- 0
  })

  observeEvent(input$start, {
    hide("start")
    show("restart")
  })

  observeEvent(input$expl, {
    if (rv$torpedo_supply > 0) {
      rv$shot <- c(rv$shot, list(input$expl))
      rv$torpedo_supply <- rv$torpedo_supply - 1
    }
  })

  observe({
    req(input$start)
    req(input$menu == "game")
    invalidateLater(interval, session)

    isolate({
      if (rv$step > 0) {
        k <- rv$klingon
        s <- rv$shields
        if (s > 0 & rv$klingon_alive) {
          d <- distance(ship[1], ship[2], k[1], k[2])
          nl <- k + move(
            len = klingon_step(d),
            pos = k,
            target = ship
          )
          rv$klingon <- nl
          d <- distance(ship[1], ship[2], nl[1], nl[2])
          rv$shields <- s - klingon_shot(d)
        }
        if (!is.null(rv$shot)) {
          for (i in 1:length(rv$shot)) {
            expl <- rv$shot[[i]]
            range <- distance(expl$x, expl$y, ship[1], ship[2])
            k <- rv$klingon
            d <- distance(expl$x, expl$y, k[1], k[2])
            if (d < torpedo_radius(range)) {
              rv$klingon_alive <- FALSE
            }
          }
        }
      }
    })
    isolate(rv$step <- rv$step + 1)
  })


  output$plot <- renderPlot(
    {
      rv$step
      isolate({
        s <- rv$shields
        ks <- rv$klingon_alive
        klingon_color <- ifelse(ks, "red", "black")
        if (s > 0) {
          plot(
            x = ship["x"], y = ship["y"],
            col = "blue", cex = 2, pch = 19,
            xlim = c(0, dim), ylim = c(0, dim),
            axes = FALSE, xlab = "", ylab = "", asp = 1
          )
          graphics::box(lwd = 3)
          if (!is.null(rv$shot)) {
            for (i in 1:length(rv$shot)) {
              expl <- rv$shot[[i]]
              lines(
                x = c(ship["x"], expl$x),
                y = c(ship["y"], expl$y),
                col = "yellow",
                lwd = 2
              )
              range <- distance(expl$x, expl$y, ship[1], ship[2])
              symbols(
                expl$x, expl$y,
                circles = torpedo_radius(range), add = TRUE,
                inches = FALSE,
                fg = "orange",
                lwd = 2
              )
            }
            rv$shot <- NULL
          }
          points(
            x = rv$klingon["x"],
            y = rv$klingon["y"],
            col = klingon_color, cex = 1, pch = 19
          )
        }
      })
    },
    width = 600,
    height = 600
  )

  output$scotty <- renderText({
    rv$step
    isolate({
      s <- rv$shields
      t <- rv$torpedo_supply
      if (s > 0 & rv$klingon_alive) {
        scotty_message <-
          paste0("<p>Shields are down to ", round(s, digits = 2),
            " Cap'n!  We have ",
            t, " torpedoes left.</p>",
            sep = ""
          )
      } else if (s > 0 & !rv$klingon_alive) {
        scotty_message <- "Ya got 'em, Cap'n!"
      } else {
        scotty_message <- "We're done for, Cap'n!"
      }
      scotty_message
    })
  })
}

## run the app ----
shinyApp(ui, server)
