library(shiny)
library(bslib)

# ─────────────────────────────────────────────────────────────────────────────
# Instrumented memoized Fibonacci
#
# Produces a closure that:
#   - persists a cache (memo) across calls
#   - records every call, hit, and return event with a cache snapshot
#   - can be reset (cache wiped) to start fresh
# ─────────────────────────────────────────────────────────────────────────────

make_memo_fib <- function() {
  cache  <- list()
  log    <- list()
  active <- integer(0)   # n-values currently being computed (not yet returned)

  self <- function(n) {
    key <- as.character(n)

    if (!is.null(cache[[key]])) {
      log[[length(log) + 1]] <<- list(
        type  = "hit",
        n     = n,
        value = cache[[key]],
        cache = cache,
        stack = active
      )
      return(cache[[key]])
    }

    active <<- c(active, n)
    log[[length(log) + 1]] <<- list(
      type  = "call",
      n     = n,
      value = NULL,
      cache = cache,
      stack = active
    )

    result <- if (n <= 2L) 1L else self(n - 1L) + self(n - 2L)

    cache[[key]] <<- result
    active <<- active[-length(active)]
    log[[length(log) + 1]] <<- list(
      type  = "return",
      n     = n,
      value = result,
      cache = cache,
      stack = active
    )

    result
  }

  list(
    run       = function(n) { log <<- list(); active <<- integer(0); self(n) },
    get_log   = function() log,
    get_cache = function() cache,
    reset     = function() { cache <<- list(); log <<- list(); active <<- integer(0) }
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Source code display
# ─────────────────────────────────────────────────────────────────────────────

fib_lines <- c(
  "memo <- list()",
  "",
  "fib <- function(n) {",
  "  key <- as.character(n)",
  "  if (!is.null(memo[[key]])) {",
  "    return(memo[[key]])     # cache hit!",
  "  }",
  "  result <- if (n <= 2) 1",
  "             else fib(n-1) + fib(n-2)",
  "  memo[[key]] <<- result   # store in memo",
  "  result",
  "}"
)

active_line_for <- function(event_type) {
  switch(event_type,
    call   = 8L,
    hit    = 6L,
    return = 10L,
    NULL
  )
}

line_bg_for <- function(event_type) {
  switch(event_type,
    call   = "#fff3cd",   # amber  — new computation
    hit    = "#cff4fc",   # cyan   — cache hit
    return = "#d1e7dd",   # green  — stored in memo
    "transparent"
  )
}

render_code <- function(active_line = NULL, event_type = NULL) {
  line_tags <- lapply(seq_along(fib_lines), function(i) {
    line      <- fib_lines[[i]]
    is_active <- !is.null(active_line) && i == active_line

    if (nchar(line) == 0) return(tags$div(style = "height:1.5em;"))

    bg      <- if (is_active) line_bg_for(event_type) else "transparent"
    pointer <- if (is_active) "\u25b6" else "\u00a0"

    tags$div(
      style = paste0(
        "display:flex; align-items:baseline; background-color:", bg, ";",
        if (is_active) " border-radius:3px; margin:0 -4px; padding:1px 4px;" else ""
      ),
      tags$span(
        style = paste0("color:#adb5bd; user-select:none; min-width:1.6rem;",
                       " text-align:right; margin-right:0.5rem;",
                       " font-size:0.8em; flex-shrink:0;"),
        i
      ),
      tags$span(
        style = paste0("min-width:1rem; margin-right:0.3rem; flex-shrink:0;",
                       " color:#0d6efd; font-size:0.75em;"),
        pointer
      ),
      tags$span(style = "white-space:pre;", line)
    )
  })

  tags$pre(
    style = paste0("background:#f8f9fa; padding:12px 12px 12px 8px;",
                   " border-radius:6px; font-size:0.82em; line-height:1.8;",
                   " margin:0; overflow-x:auto;"),
    tagList(line_tags)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Active call-stack display
# ─────────────────────────────────────────────────────────────────────────────

render_active_stack <- function(stack, event_type = NULL, current_n = NULL, current_value = NULL) {
  # For return/hit events the frame is absent from `stack` (popped or never
  # pushed), so temporarily add it back at the top for display.
  display_stack <- if (event_type %in% c("return", "hit") && !is.null(current_n)) {
    c(stack, current_n)
  } else {
    stack
  }

  if (length(display_stack) == 0) {
    return(tags$p(class = "text-muted fst-italic small mt-2",
                  "No active computations."))
  }

  frame_tags <- lapply(rev(seq_along(display_stack)), function(i) {
    n      <- display_stack[[i]]
    is_top <- i == length(display_stack)

    status <- if (is_top && identical(event_type, "call")) {
      "active"
    } else if (is_top && identical(event_type, "return")) {
      "returning"
    } else if (is_top && identical(event_type, "hit")) {
      "hit"
    } else {
      "waiting"
    }

    border_color <- switch(status,
      active    = "#0d6efd",
      returning = "#198754",
      hit       = "#0dcaf0",
      waiting   = "#adb5bd"
    )
    status_badge <- switch(status,
      active    = tags$span(class = "badge bg-primary ms-2",            "computing"),
      returning = tags$span(class = "badge bg-success ms-2",            "returning"),
      hit       = tags$span(class = "badge bg-info ms-2 text-dark",     "cache hit!"),
      waiting   = tags$span(class = "badge bg-secondary ms-2",          "waiting")
    )
    ret_tag <- if (status %in% c("returning", "hit") && !is.null(current_value)) {
      tags$span(
        class = "badge bg-success-subtle text-success-emphasis border border-success-subtle",
        paste("\u21d2", current_value)
      )
    } else {
      tags$span(class = "text-muted small fst-italic", "pending\u2026")
    }

    tags$div(
      class = "card mb-2",
      style = paste0("border-left: 4px solid ", border_color, ";"),
      tags$div(
        class = "card-body py-2 px-3",
        tags$div(
          class = "d-flex align-items-center justify-content-between flex-wrap gap-1",
          tags$div(
            class = "d-flex align-items-center",
            tags$span(class = "text-muted me-2 small", paste0("#", i)),
            tags$code(paste0("fib(", n, ")")),
            status_badge
          ),
          ret_tag
        )
      )
    )
  })

  tagList(
    tags$p(class = "text-muted small mb-2",
           paste0("Depth: ", length(display_stack), " frame",
                  if (length(display_stack) != 1) "s" else "")),
    tagList(frame_tags)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Memo-cache table
# ─────────────────────────────────────────────────────────────────────────────

render_memo_table <- function(cache, highlight_n = NULL) {
  if (length(cache) == 0) {
    return(tags$div(
      class = "text-center py-4",
      tags$span(class = "text-muted fst-italic",
                "Memo is empty \u2014 run fib(n) to begin.")
    ))
  }

  ns   <- sort(as.integer(names(cache)))
  rows <- lapply(ns, function(n) {
    key       <- as.character(n)
    is_active <- !is.null(highlight_n) && identical(n, highlight_n)
    tags$tr(
      class = if (is_active) "table-success fw-semibold" else "",
      tags$td(class = "font-monospace text-center", n),
      tags$td(class = "font-monospace text-center", cache[[key]])
    )
  })

  tags$div(
    tags$table(
      class = "table table-sm table-bordered table-hover",
      style = "width:auto; margin:0 auto;",
      tags$thead(
        class = "table-light",
        tags$tr(
          tags$th(class = "text-center", style = "width:80px;", "n"),
          tags$th(class = "text-center", style = "width:90px;", "fib(n)")
        )
      ),
      tags$tbody(rows)
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Fibonacci Memoization",
  theme = bs_theme(bootswatch = "flatly"),

  # Keyboard navigation (arrow keys step through trace)
  tags$script(HTML("
    $(document).on('keydown', function(e) {
      var tag = document.activeElement.tagName;
      if (tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA') return;
      if (e.key === 'ArrowRight' || e.key === 'ArrowDown') {
        e.preventDefault(); $('#btn_next').click();
      } else if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') {
        e.preventDefault(); $('#btn_prev').click();
      }
    });
  ")),

  sidebar = sidebar(
    width = 285,

    numericInput("n_input", "Run fib(n)", value = 6, min = 1, max = 25, step = 1),
    tags$small(class = "text-muted d-block mb-2",
               "n \u2264 25 recommended. First call is slow; repeats are instant."),

    actionButton("btn_run", "\u25b6\ufe0e  Run fib(n)",
                 class = "btn-primary w-100"),

    hr(),

    actionButton("btn_reset", "\u21ba  Reset Memo",
                 class = "btn-outline-danger w-100"),
    tags$small(class = "text-muted d-block mt-1 mb-0",
               "Clears the cache and redefines the function."),

    hr(),

    # Run history table
    uiOutput("run_history"),

    hr(),

    # Step counter + slider
    uiOutput("step_counter"),

    sliderInput("step_slider", NULL,
                min = 1, max = 2, value = 1, step = 1,
                ticks = FALSE, width = "100%"),

    div(
      class = "d-flex gap-2 mt-1",
      actionButton("btn_first", "\u23ee",
                   class = "btn-outline-secondary flex-fill", title = "First step"),
      actionButton("btn_prev",  "\u25c4",
                   class = "btn-outline-secondary flex-fill",
                   title = "Previous step (\u2190 key)"),
      actionButton("btn_next",  "\u25ba",
                   class = "btn-outline-primary  flex-fill",
                   title = "Next step (\u2192 key)"),
      actionButton("btn_last",  "\u23ed",
                   class = "btn-outline-secondary flex-fill", title = "Last step")
    ),

    hr(),

    # Current-step description
    uiOutput("description_box"),

    # Legend
    tags$div(
      class = "small mt-3",
      tags$p(class = "fw-semibold mb-1", "Legend"),
      tags$div(
        class = "d-flex flex-column gap-1",
        tags$div(
          tags$span(
            style = "background:#fff3cd; border:1px solid #ffc107;",
            class = "badge text-dark me-1",
            "call"
          ),
          "New computation"
        ),
        tags$div(
          tags$span(
            style = "background:#cff4fc; border:1px solid #0dcaf0;",
            class = "badge text-dark me-1",
            "hit"
          ),
          "Cache hit \u2014 instant!"
        ),
        tags$div(
          tags$span(
            style = "background:#d1e7dd; border:1px solid #198754;",
            class = "badge text-dark me-1",
            "store"
          ),
          "Result stored in memo"
        )
      )
    )
  ),

  # ── Main panel ─────────────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(4, 3, 5),

    card(
      card_header("Source Code"),
      card_body(padding = "0.75rem", uiOutput("code_display"))
    ),

    card(
      card_header("Active Computations"),
      card_body(
        style = "max-height:68vh; overflow-y:auto;",
        uiOutput("stack_display")
      )
    ),

    card(
      full_screen = TRUE,
      card_header("Memo Cache"),
      card_body(
        style = "max-height:68vh; overflow-y:auto;",
        uiOutput("memo_display")
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  rv_fib     <- reactiveVal(make_memo_fib())   # persists across runs
  rv_steps   <- reactiveVal(list())            # trace for the current run
  rv_idx     <- reactiveVal(0L)
  rv_history <- reactiveVal(list())            # summary of all runs

  # ── Run ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_run, {
    n   <- as.integer(input$n_input)
    fib <- rv_fib()

    result <- tryCatch(
      fib$run(n),
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        NULL
      }
    )
    if (is.null(result)) return()

    steps <- fib$get_log()
    hits  <- sum(vapply(steps, function(s) s$type == "hit",  logical(1)))
    calls <- sum(vapply(steps, function(s) s$type == "call", logical(1)))

    rv_steps(steps)
    rv_idx(1L)

    n_steps <- length(steps)
    updateSliderInput(session, "step_slider",
                      max = max(n_steps, 1L), value = 1L)

    history <- rv_history()
    history <- c(history, list(list(
      run    = length(history) + 1L,
      n      = n,
      result = result,
      total  = n_steps,
      hits   = hits,
      calls  = calls
    )))
    rv_history(history)

    showNotification(
      paste0(
        "fib(", n, ") = ", result,
        "  \u2014  ", n_steps, " step", if (n_steps != 1) "s" else "",
        " (", hits, " cache hit", if (hits != 1) "s" else "", ")"
      ),
      type = "message", duration = 5
    )
  })

  # ── Reset ─────────────────────────────────────────────────────────────────
  observeEvent(input$btn_reset, {
    rv_fib(make_memo_fib())
    rv_steps(list())
    rv_idx(0L)
    rv_history(list())
    updateSliderInput(session, "step_slider", max = 2L, value = 1L)
    showNotification("Memo cleared \u2014 function redefined.",
                     type = "warning", duration = 3)
  })

  # ── Step navigation ───────────────────────────────────────────────────────
  set_idx <- function(new_idx) {
    n <- length(rv_steps())
    if (n == 0) return()
    new_idx <- max(1L, min(as.integer(new_idx), n))
    rv_idx(new_idx)
    updateSliderInput(session, "step_slider", value = new_idx)
  }

  observeEvent(input$btn_next,  set_idx(rv_idx() + 1L))
  observeEvent(input$btn_prev,  set_idx(rv_idx() - 1L))
  observeEvent(input$btn_first, set_idx(1L))
  observeEvent(input$btn_last,  set_idx(length(rv_steps())))

  observeEvent(input$step_slider, {
    if (rv_idx() != input$step_slider)
      rv_idx(as.integer(input$step_slider))
  }, ignoreInit = TRUE)

  # ── Run history ───────────────────────────────────────────────────────────
  output$run_history <- renderUI({
    history <- rv_history()
    if (length(history) == 0)
      return(tags$p(class = "text-muted small", "No runs yet."))

    rows <- lapply(rev(seq_along(history)), function(i) {
      h      <- history[[i]]
      is_cur <- i == length(history)
      tags$tr(
        class = if (is_cur) "table-primary" else "",
        tags$td(class = "text-muted", h$run),
        tags$td(class = "font-monospace", paste0("fib(", h$n, ")")),
        tags$td(class = "text-center", h$total),
        tags$td(class = "text-center text-success fw-semibold", h$hits)
      )
    })

    tagList(
      tags$p(class = "fw-semibold small mb-1", "Run History"),
      tags$div(
        style = "max-height:150px; overflow-y:auto;",
        tags$table(
          class = "table table-sm table-bordered small mb-0",
          tags$thead(
            class = "table-light",
            tags$tr(
              tags$th("#"),
              tags$th("call"),
              tags$th(class = "text-center", "steps"),
              tags$th(class = "text-center text-success", "hits")
            )
          ),
          tags$tbody(rows)
        )
      )
    )
  })

  # ── Step counter ──────────────────────────────────────────────────────────
  output$step_counter <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()
    if (length(steps) == 0 || idx == 0L)
      return(tags$p(class = "text-muted small", "Set n and press Run."))

    div(
      class = "text-center mb-1",
      tags$span(
        class = "badge bg-light text-dark border",
        style = "font-size:0.95rem;",
        paste("Step", idx, "/", length(steps))
      )
    )
  })

  # ── Description box ───────────────────────────────────────────────────────
  output$description_box <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()
    if (length(steps) == 0 || idx == 0L) return(NULL)

    step <- steps[[idx]]
    cls  <- switch(step$type,
      call   = "alert-warning",
      hit    = "alert-info",
      return = "alert-success"
    )
    desc <- switch(step$type,
      call   = paste0("\u2192 Computing fib(", step$n, ")..."),
      hit    = paste0("\u26a1 fib(", step$n, ") = ", step$value,
                      "  [found in memo]"),
      return = paste0("\u2190 fib(", step$n, ") = ", step$value,
                      "  [stored in memo]")
    )
    div(class = paste("alert", cls, "p-2 small mb-0 font-monospace"), desc)
  })

  # ── Source code panel ─────────────────────────────────────────────────────
  output$code_display <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()
    if (length(steps) == 0 || idx == 0L) return(render_code())

    step <- steps[[idx]]
    render_code(active_line_for(step$type), step$type)
  })

  # ── Active computations (call stack) ─────────────────────────────────────
  output$stack_display <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()
    if (length(steps) == 0 || idx == 0L)
      return(tags$p(class = "text-muted fst-italic small mt-2",
                    "Press Run to begin."))
    step <- steps[[idx]]
    render_active_stack(step$stack,
                        event_type    = step$type,
                        current_n     = step$n,
                        current_value = step$value)
  })

  # ── Memo cache ────────────────────────────────────────────────────────────
  output$memo_display <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()
    if (length(steps) == 0 || idx == 0L)
      return(render_memo_table(list()))

    step      <- steps[[idx]]
    highlight <- if (step$type %in% c("hit", "return")) step$n else NULL
    render_memo_table(step$cache, highlight_n = highlight)
  })
}

shinyApp(ui, server)
