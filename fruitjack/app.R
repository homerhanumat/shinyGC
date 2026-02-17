## Claude chat link:
## https://claude.ai/share/2db795ff-6a68-4591-b911-02e244423332

library(shiny)
library(shinyjs)
library(stringr)

# ── Scoring logic ─────────────────────────────────────────────────────────────

score_fruit <- function(name) {
  # Special rule: jackfruit triggers 21 for whole hand (handled separately)
  pts <- 0
  
  # 5 pts: two-word name
  if (str_detect(name, "^\\S+ \\S+$")) pts <- pts + 5
  
  # 1 pt: ends in "berry"
  if (str_ends(name, "berry")) pts <- pts + 1
  
  # Count vowels and consonants (letters only)
  letters_only <- str_to_lower(str_replace_all(name, "[^a-z]", ""))
  vowel_count <- str_count(letters_only, "[aeiou]")
  consonant_count <- nchar(letters_only) - vowel_count
  
  # 4 pts: vowels >= consonants
  if (vowel_count >= consonant_count) pts <- pts + 4
  
  # 2 pts per distinct run of 2+ vowels or 2+ consonants
  runs <- str_extract_all(letters_only, "[aeiou]{2,}|[^aeiou]{2,}")[[1]]
  pts <- pts + 2 * length(runs)
  
  pts
}

hand_value <- function(hand) {
  if ("jackfruit" %in% hand) return(21)
  score <- 0
  for (fruit in hand) {
    score <- score + score_fruit(name = fruit)
  }
  score
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;900&family=DM+Mono:wght@400;500&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      :root {
        --cream:  #fdf6e3;
        --bark:   #3b2a1a;
        --grove:  #2d6a4f;
        --leaf:   #52b788;
        --citrus: #f4a261;
        --berry:  #c1121f;
        --mist:   #e9f5ee;
      }

      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background-color: var(--cream);
        background-image:
          radial-gradient(ellipse at 10% 20%, rgba(82,183,136,.15) 0%, transparent 55%),
          radial-gradient(ellipse at 90% 80%, rgba(244,162,97,.12) 0%, transparent 50%);
        min-height: 100vh;
        font-family: 'DM Mono', monospace;
        color: var(--bark);
        display: flex;
        align-items: flex-start;
        justify-content: center;
        padding: 2rem 1rem 4rem;
      }

      .container-fluid { max-width: 680px; width: 100%; }

      /* ── Header ── */
      .game-header {
        text-align: center;
        margin-bottom: 2.5rem;
      }
      .game-header h1 {
        font-family: 'Playfair Display', serif;
        font-size: clamp(2.8rem, 8vw, 4.5rem);
        font-weight: 900;
        line-height: 1;
        color: var(--grove);
        letter-spacing: -1px;
      }
      .game-header h1 span { color: var(--citrus); }
      .game-header p {
        margin-top: .5rem;
        font-size: .8rem;
        letter-spacing: .12em;
        text-transform: uppercase;
        color: #7a6550;
      }

      /* ── Score bar ── */
      .score-bar {
        display: flex;
        justify-content: center;
        gap: 3rem;
        background: var(--bark);
        color: var(--cream);
        border-radius: 12px;
        padding: .9rem 2rem;
        margin-bottom: 2rem;
        font-size: .78rem;
        letter-spacing: .1em;
        text-transform: uppercase;
      }
      .score-bar .val {
        font-family: 'Playfair Display', serif;
        font-size: 1.7rem;
        display: block;
        letter-spacing: 0;
        text-transform: none;
        color: var(--leaf);
      }

      /* ── Card area ── */
      .hand-area {
        display: flex;
        flex-direction: column;
        gap: 1rem;
        margin-bottom: 1.5rem;
      }

      .fruit-card {
        background: #fff;
        border: 2px solid #e0d5c5;
        border-radius: 14px;
        padding: 1rem 1.4rem;
        display: flex;
        justify-content: space-between;
        align-items: center;
        animation: slideIn .35s ease;
        box-shadow: 0 2px 12px rgba(59,42,26,.06);
        transition: border-color .2s;
      }
      .fruit-card:hover { border-color: var(--leaf); }
      .fruit-card .fname {
        font-family: 'Playfair Display', serif;
        font-size: 1.25rem;
        font-weight: 700;
        color: var(--grove);
        text-transform: capitalize;
      }
      .fruit-card .fpts {
        font-size: .8rem;
        background: var(--mist);
        color: var(--grove);
        border-radius: 8px;
        padding: .25rem .7rem;
        white-space: nowrap;
      }
      .fruit-card.jackfruit-card {
        border-color: var(--citrus);
        background: linear-gradient(135deg, #fffbf2, #fff8e7);
      }
      .fruit-card.jackfruit-card .fname { color: #c07020; }

      @keyframes slideIn {
        from { opacity: 0; transform: translateY(14px); }
        to   { opacity: 1; transform: translateY(0); }
      }

      /* ── Running total ── */
      .running-total {
        text-align: center;
        font-size: .82rem;
        letter-spacing: .08em;
        text-transform: uppercase;
        color: #7a6550;
        margin-bottom: 1.6rem;
      }
      .running-total .big {
        font-family: 'Playfair Display', serif;
        font-size: 2.8rem;
        display: block;
        color: var(--bark);
        letter-spacing: -1px;
      }
      .running-total .big.bust { color: var(--berry); }
      .running-total .big.blackjack { color: var(--citrus); }

      /* ── Message ── */
      .game-message {
        text-align: center;
        min-height: 1.8rem;
        font-size: .88rem;
        margin-bottom: 1.2rem;
        font-style: italic;
        color: #7a6550;
      }
      .game-message.bust-msg   { color: var(--berry); font-weight: 600; font-style: normal; letter-spacing: .05em; }
      .game-message.hold-msg   { color: var(--grove); font-weight: 600; font-style: normal; }
      .game-message.jack-msg   { color: #c07020;     font-weight: 600; font-style: normal; }

      /* ── Buttons ── */
      .btn-row {
        display: flex;
        gap: .9rem;
        justify-content: center;
        flex-wrap: wrap;
        margin-bottom: 1.5rem;
      }
      .btn-fj {
        font-family: 'DM Mono', monospace;
        font-size: .82rem;
        letter-spacing: .1em;
        text-transform: uppercase;
        border: 2px solid currentColor;
        border-radius: 10px;
        padding: .65rem 1.6rem;
        cursor: pointer;
        background: transparent;
        transition: background .18s, color .18s, transform .1s;
      }
      .btn-fj:active { transform: scale(.96); }
      .btn-deal  { color: var(--grove);  }
      .btn-deal:hover  { background: var(--grove);  color: #fff; }
      .btn-draw  { color: var(--bark);   }
      .btn-draw:hover  { background: var(--bark);   color: #fff; }
      .btn-hold  { color: var(--berry);  }
      .btn-hold:hover  { background: var(--berry);  color: #fff; }
      .btn-again { color: var(--citrus); }
      .btn-again:hover { background: var(--citrus); color: #fff; }

      /* ── End screen ── */
      .end-screen {
        text-align: center;
        padding: 2rem 1rem;
        border: 2px dashed var(--leaf);
        border-radius: 16px;
        background: var(--mist);
        animation: slideIn .4s ease;
      }
      .end-screen h2 {
        font-family: 'Playfair Display', serif;
        font-size: 2rem;
        color: var(--grove);
        margin-bottom: .5rem;
      }
      .end-screen p { font-size: .88rem; color: #5a4535; line-height: 1.7; }
      .end-screen .final-score {
        font-family: 'Playfair Display', serif;
        font-size: 3.5rem;
        color: var(--citrus);
        display: block;
        margin: .8rem 0;
      }

      /* ── Scoring legend toggle ── */
      details {
        font-size: .75rem;
        color: #8a7560;
        border-top: 1px solid #ddd0bb;
        padding-top: 1rem;
        margin-top: 1rem;
      }
      details summary {
        cursor: pointer;
        letter-spacing: .08em;
        text-transform: uppercase;
        margin-bottom: .5rem;
      }
      details ul { padding-left: 1.4rem; line-height: 2; }
    "))
  ),
  
  div(class = "game-header",
      h1(HTML("Fruit<span>jack</span>")),
      p("A blackjack-style card game played with fruit")
  ),
  
  div(class = "score-bar",
      div(
        tags$span(class = "val", textOutput("ui_games_played", inline = TRUE)),
        "Games played"
      ),
      div(
        tags$span(class = "val", textOutput("ui_total_score", inline = TRUE)),
        "Total score"
      )
  ),
  
  # Game panel (hidden when game over)
  div(id = "game_panel",
      div(class = "hand-area", uiOutput("hand_cards")),
      div(class = "running-total",
          "Hand value",
          uiOutput("ui_hand_value")
      ),
      div(class = "game-message", id = "game_msg", uiOutput("ui_message")),
      div(class = "btn-row",
          actionButton("btn_deal",  "Deal",       class = "btn-fj btn-deal"),
          hidden(actionButton("btn_draw",  "Draw card",  class = "btn-fj btn-draw")),
          hidden(actionButton("btn_hold",  "Hold",       class = "btn-fj btn-hold")),
          hidden(actionButton("btn_again", "Play again", class = "btn-fj btn-again"))
      ),
      tags$details(
        tags$summary("Scoring rules"),
        tags$ul(
          tags$li("5 pts — two-word fruit name"),
          tags$li("1 pt  — name ends in 'berry'"),
          tags$li("4 pts — vowels ≥ consonants"),
          tags$li("2 pts per run of 2+ vowels or 2+ consonants in a row"),
          tags$li("🌟 Jackfruit = entire hand worth 21 pts"),
          tags$li("Exceed 21 → bust → 0 pts for that round")
        )
      )
  ),
  
  # End-of-session panel
  hidden(div(id = "end_panel",
             div(class = "end-screen",
                 h2("Session Complete"),
                 p("You played all 10 games."),
                 tags$span(class = "final-score", textOutput("ui_final_score")),
                 p("points across 10 games")
             )
  ))
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  MAX_PLAYS <- 10
  
  rv <- reactiveValues(
    pool        = NULL,   # remaining fruits to draw from
    hand        = NULL,   # current hand (character vector)
    hand_pts    = NULL,   # per-fruit scores
    busted      = FALSE,
    held        = FALSE,
    jackfruit   = FALSE,
    games_played = 0,
    total_score  = 0,
    phase        = "pre"  # pre | playing | ended | session_over
  )
  
  # ── Deal ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_deal, {
    if (rv$games_played >= MAX_PLAYS) return()
    
    pool <- sample(stringr::fruit)
    rv$hand   <- pool[1:2]
    rv$pool   <- pool[-(1:2)]
    rv$hand_pts <- sapply(rv$hand, score_fruit)
    rv$busted   <- FALSE
    rv$held     <- FALSE
    rv$jackfruit <- "jackfruit" %in% rv$hand
    rv$phase    <- "playing"
    
    shinyjs::hide("btn_deal")
    shinyjs::show("btn_draw")
    shinyjs::show("btn_hold")
    shinyjs::hide("btn_again")
    
    check_bust(rv)
  })
  
  # ── Draw ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_draw, {
    if (length(rv$pool) == 0) return()
    
    new_fruit   <- rv$pool[1]
    rv$pool     <- rv$pool[-1]
    rv$hand     <- c(rv$hand, new_fruit)
    rv$hand_pts <- c(rv$hand_pts, score_fruit(new_fruit))
    if (new_fruit == "jackfruit") rv$jackfruit <- TRUE
    
    check_bust(rv)
  })
  
  # ── Hold ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_hold, {
    end_round(rv, held = TRUE)
  })
  
  # ── Play again ────────────────────────────────────────────────────────────
  observeEvent(input$btn_again, {
    if (rv$games_played >= MAX_PLAYS) {
      shinyjs::hide("game_panel")
      shinyjs::show("end_panel")
      return()
    }
    rv$hand   <- NULL
    rv$hand_pts <- NULL
    rv$busted <- FALSE
    rv$held   <- FALSE
    rv$jackfruit <- FALSE
    rv$phase  <- "pre"
    
    shinyjs::show("btn_deal")
    shinyjs::hide("btn_draw")
    shinyjs::hide("btn_hold")
    shinyjs::hide("btn_again")
  })
  
  # ── Helpers ───────────────────────────────────────────────────────────────
  
  check_bust <- function(rv) {
    val <- hand_value(rv$hand)
    if (val > 21) {
      rv$busted <- TRUE
      end_round(rv, held = FALSE)
    }
  }
  
  end_round <- function(rv, held) {
    rv$held  <- held
    rv$phase <- "round_over"
    rv$games_played <- rv$games_played + 1
    
    round_score <- if (rv$busted) 0 else hand_value(rv$hand)
    rv$total_score <- rv$total_score + round_score
    
    shinyjs::hide("btn_draw")
    shinyjs::hide("btn_hold")
    
    if (rv$games_played >= MAX_PLAYS) {
      shinyjs::html("btn_again", "Wrap Up")
    }
    shinyjs::show("btn_again")
  }
  
  # ── Outputs ───────────────────────────────────────────────────────────────
  
  output$hand_cards <- renderUI({
    req(rv$hand)
    lapply(seq_along(rv$hand), function(i) {
      fname <- rv$hand[i]
      pts   <- rv$hand_pts[i]
      is_jack <- fname == "jackfruit"
      card_class <- if (is_jack) "fruit-card jackfruit-card" else "fruit-card"
      div(class = card_class,
          span(class = "fname", fname),
          span(class = "fpts",
               if (is_jack) "🌟 jackfruit!" else paste(pts, "pts"))
      )
    })
  })
  
  output$ui_hand_value <- renderUI({
    req(rv$hand)
    val <- hand_value(rv$hand)
    cls <- if (rv$busted) "big bust" else if (val == 21) "big blackjack" else "big"
    tags$span(class = cls, val)
  })
  
  output$ui_message <- renderUI({
    if (is.null(rv$hand)) {
      div("Press Deal to start a round.")
    } else if (rv$jackfruit && !rv$busted) {
      div(class = "game-message jack-msg",
          "🌟 Jackfruit! Your hand is worth 21 points.")
    } else if (rv$busted) {
      div(class = "game-message bust-msg",
          "BUST! You exceeded 21 — this round scores 0.")
    } else if (rv$held) {
      val <- hand_value(rv$hand)
      div(class = "game-message hold-msg",
          paste0("You held with ", val, " points."))
    } else {
      div(paste0("Hand total: ", hand_value(rv$hand),
                 ". Draw another fruit or hold?"))
    }
  })
  
  output$ui_games_played <- renderText({ rv$games_played })
  output$ui_total_score  <- renderText({ rv$total_score  })
  output$ui_final_score  <- renderText({ rv$total_score  })
}

shinyApp(ui, server)