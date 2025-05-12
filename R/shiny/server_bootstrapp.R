source("50_bootstrapp.R")


## progress_vals <- reactiveVal(list(count = 0, total = 10, label = ""))
## progress <- shiny::Progress$new()

## # Update the progress bar and text
## observe({
##   progress_log_reactive()
##   updateProgressBar(session,
##     id = "progress_bar", value = progress_vals()$count,
##     total = progress_vals()$total,
##     title = progress_vals()$label
##   )
## })

## progress_log_reactive <- reactivePoll(1000, session,
##   # This function returns the time that log_file was last modified
##   checkFunc = function() {
##     ## progress_file <- paste0(tempdir(), "/progress.log")
##     progress_file <- paste0(data_path, "/progress.log")
##     if (file.exists(progress_file))
##       file.info(progress_file)$mtime[1]
##     else
##       ""
##   },
##   # This function returns the content of the status terminal output
##   valueFunc = function() {
##     ## progress_file <- paste0(tempdir(), "/progress.log")
##     progress_file <- paste0(data_path, "/progress.log")
##     if (file.exists(progress_file)) {
##       rl <- readLines(progress_file, warn = FALSE)

##       progress_data <- strsplit(rl[length(rl)], ",")[[1]]
##       current <- as.numeric(progress_data[3])
##       total <- as.numeric(progress_data[4])
##       label <- paste0(progress_data[1], ":<br> ", progress_data[2])
##         progress_vals(list(count = current, total = total, label = label)) # Update progress as a percentage
##       rl[length(rl)]
##     } else {
##       progress_vals(list(count = 0, total = 10, label = ""))
##       ""
##     }
##   }
## )


promise_boot <- ExtendedTask$new(function() {
  future_promise({
    print(status_file)
    print(log_file)
    print(box_width)
    print(project_name)
    module_boot()
  }) |>
    then(\(result) {
      toggle_buttons(status_$status, stage = 6, bttn1 = "runBootstrappCode", bttn2 = "runSummariesCode")
      shinyjs::enable(selector = "a[data-value='bootstrapp']")
      addCssClass(selector = "a[data-value='bootstrapp']", class = "activeLink")
      shinyjs::toggle(id = paste0("overlay_div"))
      result
    })
}) |>
  bslib::bind_task_button("runBootstrappCode")

observeEvent(input$runBootstrappCode, {
  shinyjs::toggle(id = paste0("overlay_div"))
  addCssClass(selector = "div[class='progress-group']", class = "hidden")
  promise_boot$invoke()
})

## ## Trigger to run 50_bootstrapp.R
## observeEvent(input$runBootstrappCode, {
##   module_boot()

##   toggle_buttons(status_$status, stage =  6, bttn1 = "runBootstrappCode", bttn2 = "runSummariesCode")
##   shinyjs::enable(selector = "a[data-value='bootstrapp']")
##   addCssClass(selector = "a[data-value='bootstrapp']", class = "activeLink")
## })
