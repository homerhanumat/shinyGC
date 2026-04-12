library(shiny)
library(bslib)
library(DiagrammeR)

# ─────────────────────────────────────────────────────────────────────────────
# Tracer — collects call/return events during execution
# ─────────────────────────────────────────────────────────────────────────────

make_tracer <- function() {
  log <- list()
  list(
    push  = function(e) { log[[length(log) + 1]] <<- e },
    get   = function() log,
    reset = function() { log <<- list() }
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Instrumented recursive functions
# Each uses a closure so self-calls go through the traced version
# ─────────────────────────────────────────────────────────────────────────────

make_factorial <- function(tr) {
  self <- function(n) {
    tr$push(list(type = "call",   fn = "factorial", args = list(n = n)))
    result <- if (n <= 1L) 1L else n * self(n - 1L)
    tr$push(list(type = "return", fn = "factorial", args = list(n = n), value = result))
    result
  }
  self
}

make_fib <- function(tr) {
  self <- function(n) {
    tr$push(list(type = "call",   fn = "fib", args = list(n = n)))
    result <- if (n <= 2L) 1L else self(n - 1L) + self(n - 2L)
    tr$push(list(type = "return", fn = "fib", args = list(n = n), value = result))
    result
  }
  self
}

make_power <- function(tr) {
  self <- function(base, exp) {
    tr$push(list(type = "call",   fn = "power", args = list(base = base, exp = exp)))
    result <- if (exp == 0L) 1L else base * self(base, exp - 1L)
    tr$push(list(type = "return", fn = "power", args = list(base = base, exp = exp), value = result))
    result
  }
  self
}

make_sum_list <- function(tr) {
  self <- function(lst) {
    tr$push(list(type = "call",   fn = "sum_list", args = list(len = length(lst))))
    result <- if (length(lst) == 0L) 0L else lst[[1L]] + self(lst[-1L])
    tr$push(list(type = "return", fn = "sum_list", args = list(len = length(lst)), value = result))
    result
  }
  self
}

# ─────────────────────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────────────────────

fmt_args <- function(args) {
  paste(names(args), "=", unlist(args), collapse = ", ")
}

# Map an event to the 1-based line in fn_sources that should be highlighted.
# All four functions share the same 7-line skeleton:
#   1  fn <- function(...) {
#   2    if (<base condition>) {
#   3      return(<base value>)
#   4    } else {
#   5      <recursive expression>
#   6    }
#   7  }
get_active_line <- function(event) {
  if (event$type == "call") return(2L)  # about to evaluate the if-condition

  is_base <- switch(event$fn,
    factorial = event$args$n   <= 1L,
    fib       = event$args$n   <= 2L,
    power     = event$args$exp == 0L,
    sum_list  = event$args$len == 0L,
    FALSE
  )
  if (is_base) 3L else 5L  # base-case return vs recursive expression
}

# ─────────────────────────────────────────────────────────────────────────────
# build_trace: single pass over events that builds both
#   • steps  — per-step snapshots for the stack/code panels
#   • nodes  — tree nodes (one per unique call)
#   • edges  — parent → child relationships
#
# Every step carries current_node_id so the tree panel knows what to highlight.
# ─────────────────────────────────────────────────────────────────────────────

build_trace <- function(events) {
  steps    <- list()
  stack    <- list()          # step-level call stack
  nodes    <- list()          # tree nodes, indexed by node_id
  edges    <- list()          # list of list(from, to)
  id_stack <- integer(0)      # parallel stack of node IDs
  node_id  <- 0L

  for (event in events) {
    if (event$type == "call") {
      # ── Tree bookkeeping ───────────────────────────────────────────────────
      node_id  <- node_id + 1L
      parent   <- if (length(id_stack) > 0) id_stack[length(id_stack)] else NA_integer_
      nodes[[node_id]] <- list(
        id        = node_id,
        fn        = event$fn,
        args      = event$args,
        value     = NULL,
        parent_id = parent
      )
      if (!is.na(parent)) edges <- c(edges, list(list(from = parent, to = node_id)))
      id_stack    <- c(id_stack, node_id)
      current_nid <- node_id

      # ── Step stack ─────────────────────────────────────────────────────────
      stack <- lapply(stack, function(f) { f$status <- "waiting"; f })
      stack <- c(stack, list(list(
        fn = event$fn, args = event$args, value = NULL, status = "active"
      )))
      desc <- paste0("\u2192 Calling ", event$fn, "(", fmt_args(event$args), ")")

    } else {
      # ── Tree bookkeeping ───────────────────────────────────────────────────
      current_nid              <- id_stack[length(id_stack)]
      nodes[[current_nid]]$value <- event$value
      id_stack                 <- id_stack[-length(id_stack)]

      # ── Step stack ─────────────────────────────────────────────────────────
      n              <- length(stack)
      stack[[n]]$value  <- event$value
      stack[[n]]$status <- "returning"
      desc <- paste0(
        "\u2190 Returning ", event$value,
        " from ", event$fn, "(", fmt_args(event$args), ")"
      )
    }

    steps <- c(steps, list(list(
      stack           = stack,
      description     = desc,
      event_type      = event$type,
      active_line     = get_active_line(event),
      current_node_id = current_nid
    )))

    if (event$type == "return") stack <- stack[-length(stack)]
  }

  list(steps = steps, nodes = nodes, edges = edges)
}

# ─────────────────────────────────────────────────────────────────────────────
# generate_dot: produce Graphviz DOT for the full tree with one node highlighted
# ─────────────────────────────────────────────────────────────────────────────

generate_dot <- function(nodes, edges, current_node_id, event_type) {
  node_defs <- vapply(nodes, function(nd) {
    arg_str <- fmt_args(nd$args)
    label   <- paste0(nd$fn, "(", arg_str, ")")
    if (!is.null(nd$value)) label <- paste0(label, "\\n= ", nd$value)

    is_curr <- nd$id == current_node_id
    done    <- !is.null(nd$value)

    fillcolor <- if (is_curr) {
      if (event_type == "call") "#fff3cd" else "#d1e7dd"
    } else if (done) {
      "#f0fff4"   # faint green: call has returned
    } else {
      "#ffffff"   # white: call is pending
    }

    border <- if (is_curr) {
      if (event_type == "call") "#d97706" else "#198754"
    } else {
      "#ced4da"
    }

    pw <- if (is_curr) 2.5 else 1

    paste0(
      nd$id,
      ' [label="', label, '"',
      ', fillcolor="', fillcolor, '"',
      ', color="',     border,    '"',
      ', penwidth=',   pw, ']'
    )
  }, character(1))

  edge_defs <- if (length(edges) > 0) {
    vapply(edges, function(e) paste0(e$from, " -> ", e$to), character(1))
  } else {
    character(0)
  }

  paste(c(
    "digraph recurse {",
    '  graph [rankdir=TB bgcolor=transparent nodesep=0.35 ranksep=0.5]',
    '  node [shape=box style=filled fontname=monospace fontsize=10]',
    '  edge [color="#ced4da" arrowsize=0.7]',
    paste0("  ", node_defs),
    if (length(edge_defs) > 0) paste0("  ", edge_defs) else NULL,
    "}"
  ), collapse = "\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# Source code strings and render_code
# ─────────────────────────────────────────────────────────────────────────────

fn_sources <- list(
  factorial = c(
    "factorial <- function(n) {",
    "  if (n <= 1) {",
    "    return(1)",
    "  } else {",
    "    n * factorial(n - 1)",
    "  }",
    "}"
  ),
  fib = c(
    "fib <- function(n) {",
    "  if (n <= 2) {",
    "    return(1)",
    "  } else {",
    "    fib(n - 1) + fib(n - 2)",
    "  }",
    "}"
  ),
  power = c(
    "power <- function(base, exp) {",
    "  if (exp == 0) {",
    "    return(1)",
    "  } else {",
    "    base * power(base, exp - 1)",
    "  }",
    "}"
  ),
  sum_list = c(
    "sum_list <- function(lst) {",
    "  if (length(lst) == 0) {",
    "    return(0)",
    "  } else {",
    "    lst[[1]] + sum_list(lst[-1])",
    "  }",
    "}"
  )
)

fn_labels <- c(
  "Factorial"   = "factorial",
  "Fibonacci"   = "fib",
  "Power"       = "power",
  "Sum of list" = "sum_list"
)

render_code <- function(fn_name, active_line = NULL, event_type = NULL) {
  lines <- fn_sources[[fn_name]]

  line_tags <- lapply(seq_along(lines), function(i) {
    is_active <- !is.null(active_line) && i == active_line

    bg <- if (is_active) {
      if (identical(event_type, "call")) "#fff3cd" else "#d1e7dd"
    } else {
      "transparent"
    }

    pointer <- if (is_active) "\u25b6" else "\u00a0"

    tags$div(
      style = paste0(
        "display: flex; align-items: baseline;",
        " background-color: ", bg, ";",
        if (is_active) " border-radius: 3px; margin: 0 -4px; padding: 1px 4px;" else ""
      ),
      tags$span(
        style = "color: #adb5bd; user-select: none; min-width: 1.4rem;
                 text-align: right; margin-right: 0.6rem; font-size: 0.8em; flex-shrink: 0;",
        i
      ),
      tags$span(
        style = "min-width: 1rem; margin-right: 0.3rem; flex-shrink: 0;
                 color: #0d6efd; font-size: 0.75em;",
        pointer
      ),
      tags$span(style = "white-space: pre;", lines[[i]])
    )
  })

  tags$pre(
    style = "background: #f8f9fa; padding: 12px 12px 12px 8px; border-radius: 6px;
             font-size: 0.82em; line-height: 1.8; margin: 0; overflow-x: auto;",
    tagList(line_tags)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Render the call stack as HTML
# ─────────────────────────────────────────────────────────────────────────────

render_stack <- function(stack) {
  if (length(stack) == 0) {
    return(tags$p(class = "text-muted fst-italic mt-2",
                  "Stack is empty \u2014 all calls have returned."))
  }

  frame_tags <- lapply(rev(seq_along(stack)), function(i) {
    fr <- stack[[i]]

    call_str     <- paste0(fr$fn, "(", fmt_args(fr$args), ")")
    border_color <- switch(fr$status,
      active    = "#0d6efd", returning = "#198754", waiting = "#adb5bd"
    )
    status_badge <- switch(fr$status,
      active    = tags$span(class = "badge bg-primary ms-2",   "active"),
      returning = tags$span(class = "badge bg-success ms-2",   "returning"),
      waiting   = tags$span(class = "badge bg-secondary ms-2", "waiting")
    )
    ret_tag <- if (!is.null(fr$value)) {
      tags$span(
        class = "badge bg-success-subtle text-success-emphasis border border-success-subtle",
        paste("\u21d2", fr$value)
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
            tags$code(call_str),
            status_badge
          ),
          ret_tag
        )
      )
    )
  })

  tagList(
    tags$p(class = "text-muted small mb-2",
           paste0("Depth: ", length(stack), " frame",
                  if (length(stack) != 1) "s" else "")),
    tagList(frame_tags)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Recursive Call Stack Stepper",
  theme = bs_theme(bootswatch = "flatly"),

  tags$head(tags$style(HTML("
    code { font-size: 0.9em; }
    pre  { background: #f8f9fa; padding: 12px; border-radius: 6px;
           font-size: 0.82em; line-height: 1.6; }
    /* keep DiagrammeR SVG filling its container */
    .grViz { width: 100% !important; height: 100% !important; }
    .grViz svg { width: 100% !important; height: 100% !important; }
  "))),

  # Arrow-key navigation
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
    width = 270,

    selectInput("fn_choice", "Function", choices = fn_labels),

    conditionalPanel("input.fn_choice === 'factorial'",
      numericInput("n_fact", "n", value = 4, min = 1, max = 12, step = 1)
    ),
    conditionalPanel("input.fn_choice === 'fib'",
      numericInput("n_fib", "n", value = 5, min = 1, max = 8, step = 1),
      tags$small(class = "text-muted",
                 "Keep n \u2264 8 \u2014 Fibonacci grows exponentially.")
    ),
    conditionalPanel("input.fn_choice === 'power'",
      numericInput("base_pow", "base", value = 2, min = 1, max = 10, step = 1),
      numericInput("exp_pow",  "exp",  value = 4, min = 0, max = 10, step = 1)
    ),
    conditionalPanel("input.fn_choice === 'sum_list'",
      textInput("list_vals", "List values (comma-separated)", value = "3, 1, 4, 1, 5")
    ),

    actionButton("btn_trace", "\u25b6 Run Trace", class = "btn-primary w-100 mt-1"),

    hr(),

    uiOutput("step_counter"),

    sliderInput("step_slider", NULL,
                min = 1, max = 2, value = 1, step = 1,
                ticks = FALSE, width = "100%"),

    div(
      class = "d-flex gap-2 mt-1",
      actionButton("btn_first", "\u23ee", class = "btn-outline-secondary flex-fill",
                   title = "First step"),
      actionButton("btn_prev",  "\u25c4",  class = "btn-outline-secondary flex-fill",
                   title = "Previous step (or \u2190 key)"),
      actionButton("btn_next",  "\u25ba",  class = "btn-outline-primary  flex-fill",
                   title = "Next step (or \u2192 key)"),
      actionButton("btn_last",  "\u23ed", class = "btn-outline-secondary flex-fill",
                   title = "Last step")
    ),

    hr(),

    uiOutput("description_box")
  ),

  layout_columns(
    col_widths = c(4, 8),

    # ── Left: source code ────────────────────────────────────────────────────
    card(
      card_header("Source Code"),
      card_body(uiOutput("code_display"))
    ),

    # ── Right: stack + tree tabs ─────────────────────────────────────────────
    navset_card_underline(
      full_screen = TRUE,

      nav_panel(
        "Call Stack",
        div(
          style = "max-height: 74vh; overflow-y: auto;",
          uiOutput("stack_display")
        )
      ),

      nav_panel(
        "Call Tree",
        grVizOutput("tree_diagram", height = "68vh")
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  rv_steps <- reactiveVal(list())
  rv_tree  <- reactiveVal(list(nodes = list(), edges = list()))
  rv_idx   <- reactiveVal(0L)

  # ── Run trace ──────────────────────────────────────────────────────────────
  observeEvent(input$btn_trace, {
    tr <- make_tracer()

    result <- tryCatch({
      fn <- input$fn_choice
      if (fn == "factorial") {
        make_factorial(tr)(as.integer(input$n_fact))
      } else if (fn == "fib") {
        make_fib(tr)(as.integer(input$n_fib))
      } else if (fn == "power") {
        make_power(tr)(as.integer(input$base_pow), as.integer(input$exp_pow))
      } else if (fn == "sum_list") {
        vals <- as.integer(trimws(strsplit(input$list_vals, ",")[[1]]))
        if (any(is.na(vals))) stop("Please enter comma-separated integers.")
        make_sum_list(tr)(as.list(vals))
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })

    if (!is.null(result)) {
      traced <- build_trace(tr$get())
      rv_steps(traced$steps)
      rv_tree(list(nodes = traced$nodes, edges = traced$edges))
      rv_idx(1L)

      n <- length(traced$steps)
      updateSliderInput(session, "step_slider", max = n, value = 1L)
      showNotification(
        paste0("Traced ", n, " steps \u2014 final result: ", result),
        type = "message", duration = 4
      )
    }
  })

  # ── Navigation ─────────────────────────────────────────────────────────────
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
    if (rv_idx() != input$step_slider) rv_idx(as.integer(input$step_slider))
  }, ignoreInit = TRUE)

  # Reset when switching functions
  observeEvent(input$fn_choice, {
    rv_steps(list())
    rv_tree(list(nodes = list(), edges = list()))
    rv_idx(0L)
  })

  # ── Step counter ───────────────────────────────────────────────────────────
  output$step_counter <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()
    if (length(steps) == 0 || idx == 0L) {
      return(p(class = "text-muted small", "Configure and press Run Trace."))
    }
    div(
      class = "text-center mb-1",
      tags$span(
        class = "badge bg-light text-dark border",
        style = "font-size: 0.95rem;",
        paste("Step", idx, "/", length(steps))
      )
    )
  })

  # ── Description box ────────────────────────────────────────────────────────
  output$description_box <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()
    if (length(steps) == 0 || idx == 0L) return(NULL)

    step <- steps[[idx]]
    cls  <- if (step$event_type == "call") "alert-info" else "alert-success"
    div(class = paste("alert", cls, "p-2 small mb-0 font-monospace"), step$description)
  })

  # ── Source code panel ──────────────────────────────────────────────────────
  output$code_display <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()

    if (length(steps) == 0 || idx == 0L) {
      return(render_code(input$fn_choice))
    }

    step <- steps[[idx]]
    render_code(input$fn_choice, step$active_line, step$event_type)
  })

  # ── Call stack panel ───────────────────────────────────────────────────────
  output$stack_display <- renderUI({
    steps <- rv_steps()
    idx   <- rv_idx()

    if (length(steps) == 0 || idx == 0L) {
      return(p(class = "text-muted fst-italic mt-2", "Press Run Trace to begin."))
    }

    render_stack(steps[[idx]]$stack)
  })

  # ── Call tree panel ────────────────────────────────────────────────────────
  output$tree_diagram <- renderDiagrammeR({
    steps <- rv_steps()
    tree  <- rv_tree()
    idx   <- rv_idx()

    if (length(steps) == 0 || idx == 0L || length(tree$nodes) == 0) {
      # Render an empty placeholder graph
      return(grViz('digraph { graph [bgcolor=transparent]
                    msg [label="Press Run Trace to begin." shape=plaintext
                         fontcolor="#6c757d" fontsize=12] }'))
    }

    step <- steps[[idx]]
    dot  <- generate_dot(tree$nodes, tree$edges,
                         step$current_node_id, step$event_type)
    grViz(dot)
  })
}

shinyApp(ui, server)
