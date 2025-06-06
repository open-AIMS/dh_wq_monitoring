ansi2html <- function(ansi){
  HTML(sprintf(
    "<pre style = 'line-height: 11.2pt;'>%s</pre>",
    gsub("\n", "<br/>", as.character(sgr_to_html(ansi)))
  ))
}

## Create a reactive object that updates when status_$status changes
status_obj_reactive <- reactiveVal(list(data = status_$status))

## Render the tree output
output$tree <- renderTree({
  #  status_list_to_tree(status_obj_reactive()$data)
  log_file_reactive()
  status_list_to_tree(status_$status)
})

## Render the log_file to the verbatimTextOutput box
output$log_output <- renderText({
    status_obj_reactive()
    log_content <- readLines(log_file)
    ## print(log_content)
    log_content <- paste(log_content, collapse = "\n")
    ## print(log_content)
    log_content
  })
## Create a reactive object that updates every second.
## Each second, it reads the log_file and:
## - triggers a re-display of the terminal like verbatim box
## - triggers a redraw of the status tree
log_file_reactive <- reactivePoll(1000, session,
  # This function returns the time that log_file was last modified
  checkFunc = function() {
    llog_file <- paste0(
      status_$settings$status_dir$item, "/",
      status_$settings$log_file$item
    )
    if (file.exists(llog_file))
      file.info(llog_file)$mtime[1]
    else
      ""
  },
  # This function returns the content of the status terminal output
  valueFunc = function() {
    status_obj_reactive(list(data = status_$status))
    status::display_status_terminal(dest = "shiny")
  }
)

## Render the terminal-like output - triggered by change in log_file
output$status_output <- renderUI({
  ansi2html(paste(log_file_reactive(), collapse = ""))
})

## Progress bar =======================================================================
progress_vals <- reactiveVal(list(count = 0, total = 10, label = ""))
## progress <- shiny::Progress$new()

# Update the progress bar and text
observe({
  progress_log_reactive()
  updateProgressBar(session,
    id = "progress_bar", value = progress_vals()$count,
    total = progress_vals()$total,
    title = progress_vals()$label
  )
})

progress_log_reactive <- reactivePoll(1000, session,
  # This function returns the time that log_file was last modified
  checkFunc = function() {
    ## progress_file <- paste0(tempdir(), "/progress.log")
    progress_file <- paste0(data_path, "/progress.log")
    if (file.exists(progress_file))
      file.info(progress_file)$mtime[1]
    else
      ""
  },
  # This function returns the content of the status terminal output
  valueFunc = function() {
    ## progress_file <- paste0(tempdir(), "/progress.log")
    progress_file <- paste0(data_path, "/progress.log")
    if (file.exists(progress_file)) {
      rl <- readLines(progress_file, warn = FALSE)

      progress_data <- strsplit(rl[length(rl)], ",")[[1]]
      current <- as.numeric(progress_data[3])
      total <- as.numeric(progress_data[4])
      label <- paste0(progress_data[1], ":<br> ", progress_data[2])
        progress_vals(list(count = current, total = total, label = label)) # Update progress as a percentage
      rl[length(rl)]
    } else {
      progress_vals(list(count = 0, total = 10, label = ""))
      ""
    }
  }
)





## output$model_log_output <- renderText({
##   model_log_reactive()
##   ## if (exists(model_log_file)) {
##   model_log_content <- readLines(paste0(data_path, "modelled/log_models.log"),
##     warn = FALSE
##   )
##     ## print(log_content)
##     model_log_content <- paste(model_log_content, collapse = "\n")
##     ## print(log_content)
##     model_log_content
##   ## }
## })

## model_log_reactive <- reactivePoll(1000, session,
##   # This function returns the time that log_file was last modified
##   checkFunc = function() {
##     llog_file <- paste0(data_path, "modelled/log_models.log")
##     if (file.exists(llog_file))
##       file.info(llog_file)$mtime[1]
##     else
##       ""
##   },
##   # This function returns the content of the status terminal output
##   valueFunc = function() {
##     ## if (exists(model_log_file)) {
##       readLines(paste0(data_path, "modelled/log_models.log"), warn = FALSE)
##     ## } else {
##       ## ""
##     ## }
##   }
## )

## Helper functions =============================================================================

## The following function converts a status_$status list into
## a tree structure 
status_list_to_tree <- function(lst) {
  ll <- lapply(seq_along(lst), function(l) {
    ll <- lapply(seq_along(lst[[l]]$names), function(x) {
      ic <- switch(lst[[l]]$status[[x]],
        "success" = "circle-check",
        "failure" = "circle-xmark",
        "warning" = "circle-exclamation",
        "pending" = "clock"
      )
      structure(list(), sttype = "element", sticon = ic)
    })
    ll <- setNames(ll, lst[[l]]$names)
    attr(ll, "errors") <- any(lst[[l]]$status == "failure")
    attr(ll, "complete") <- all(lst[[l]]$status == "success")
    attr(ll, "warnings") <- any(lst[[l]]$status == "warning")
    attr(ll, "pendings") <- any(lst[[l]]$status == "pending")
    ll
  })

  ll <- setNames(ll, sapply(lst, function(x) x$title))
  attr(ll, "sttype") <- "default"
  attr(ll, "sticon") <- "signal"
  attr(ll, "stopened") <- FALSE
  attr(ll[[status_$settings$current_stage$item]], "stopened") <- TRUE

  wch <- which(sapply(ll, function(x) attr(x, "errors")))
  if (length(wch) > 0) attr(ll[wch], "stopened") <- TRUE
  if (length(wch) > 0) {
          ll[wch] <- lapply(ll[wch], function(x) {
                  attr(x, "sticon") <- "circle-xmark"
                  x
          })
  }

  wch <- which(sapply(ll, function(x) attr(x, "warnings")))
  if (length(wch) > 0) attr(ll[wch], "stopened") <- TRUE
  if (length(wch) > 0) {
          ll[wch] <- lapply(ll[wch], function(x) {
                  attr(x, "sticon") <- "circle-exclamation"
                  x
          })
  }
  wch <- which(sapply(ll, function(x) attr(x, "complete")))
  if (length(wch) > 0) {
          ll[wch] <- lapply(ll[wch], function(x) {
                  attr(x, "sticon") <- "circle-check"
                  x
          })
  }
  wch <- which(sapply(ll, function(x) attr(x, "pendings")))
  if (length(wch) > 0) {
          ll[wch] <- lapply(ll[wch], function(x) {
                  attr(x, "sticon") <- "clock"
                  x
          })
  }
  ll
}
