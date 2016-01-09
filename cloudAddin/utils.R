## My utils -------

checkDF <- function(dfName) {
  df <- tryCatch(get(dfName, envir = .GlobalEnv, inherits = TRUE), 
                 error = function(e) FALSE)
  # if object if not found, we get FALSE, so return false
  # snce user can't get to the object anyway:
  if (is.logical(df) && !df) {
    return(FALSE)
  }
  is.data.frame(df)
}

getDataFrameChoices <- function(packages) {
  dfList <- character()
  for (package in packages) {
    # from the data() function, pull out just the data names:
    packList <- data(package = package)$results[,3]
    # for each one, check that it's a data frame:
    isDF <- sapply(packList, checkDF)
    # add the ones that are data frames:
    dfList <- c(dfList, packList[isDF])
  }
  dfList <- sort(unique(dfList))
  #note:  if there are name conflicts, user gets the df in the
  #most recently-loaded package, as usual
  dfList
}

exists_as_numeric <- function(var) {
  !is.null(var) && !is.na(var)
}

find_numeric_vars <- function(data) {
  isNum <- function(name, data) {
    is.numeric(get(name, envir = as.environment(data)))
  }
  numNames <- sapply(names(data), isNum, data = data)
  names(data)[numNames]
}

find_factor_vars <- function(data) {
  isFac <- function(name, data) {
    is.factor(get(name, envir = as.environment(data)))
  }
  facNames <- sapply(names(data), isFac, data = data)
  names(data)[facNames]
}

find_facnum_vars <- function(data) {
  isFacNum <- function(name, data) {
    var <- get(name, envir = as.environment(data))
    is.factor(var) || is.numeric(var)
  }
  facNumNames <- sapply(names(data), isFacNum, data = data)
  names(data)[facNumNames]
}

entered <- function(string) {
  !is.null(string) && nzchar(string)
}

suggestedName <- function(varName) {
  if (tolower(varName) != varName) {
    suggestion <- tolower(varName)
  } else {
    suggestion <- Hmisc::capitalize(varName)
  }
}

# make limited-reactivity text input
# lrTextInput <- function(inputId, label, value = "") {
#   tagList(tags$head(tags$script(src = "js/custom.js")),
#           tags$label(label, `for` = inputId),
#           tags$input(id = inputId,
#                      type = "text", value = value,
#                      class="lrTextInput form-control shiny-bound-input"))
# }



## code highlighting ----------
injectHighlightHandler <- function() {

  code <- "
  Shiny.addCustomMessageHandler('highlight-code', function(message) {
    var id = message['id'];
    setTimeout(function() {
      var el = document.getElementById(id);
      hljs.highlightBlock(el);
    }, 100);
  });
  "

  tags$script(code)
}

includeHighlightJs <- function() {
  resources <- system.file("www/shared/highlight", package = "shiny")
  list(
    includeScript(file.path(resources, "highlight.pack.js")),
    includeCSS(file.path(resources, "rstudio.css")),
    injectHighlightHandler()
  )
}

highlightCode <- function(session, id) {
  session$sendCustomMessage("highlight-code", list(id = id))
}

rCodeContainer <- function(...) {
  code <- as.character(tags$code(class = "language-r", ...))
  div(pre(code))
}

renderCode <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- NULL
  installExprFunction(expr, "func", env, quoted)
  markRenderFunction(textOutput, function() {
    paste(func(), collapse = "\n")
  })
}

stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  fluidRow(
    lapply(dots, function(el) {
      div(class = class, el)
    })
  )
}

isErrorMessage <- function(object) {
  inherits(object, "error_message")
}

errorMessage <- function(type, message) {
  structure(
    list(type = type, message = message),
    class = "error_message"
  )
}