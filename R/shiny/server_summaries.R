source("60_summaries.R")

promise_summaries <- ExtendedTask$new(function() {
  future_promise({
    print(status_file)
    print(log_file)
    print(box_width)
    print(project_name)
    ## module_summaries()
    ## data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))
    ## data_spatial <- data$spatial
    ## list(data = data, data_spatial = data_spatial)
    list("done")
  }) |>
    then(\(result) {
      toggle_buttons(status_$status, stage =  6, bttn1 = "runSummariesCode", bttn2 = NULL)
      shinyjs::enable(selector = "a[data-value='summaries']")
      addCssClass(selector = "a[data-value='summaries']", class = "activeLink")
      shinyjs::toggle(id = paste0("overlay_div"))
      ## progress_log_reactive(NULL)  # kill the reactive poll
      result
    })
}) |>
  bslib::bind_task_button("runSummariesCode")

## Trigger to run 60_summaries.R
observeEvent(input$runSummariesCode, {
  progress_file <- paste0(progress_path, "/progress.log")
  if (file.exists(progress_file)) {
    file.remove(progress_file)
  }
  shinyjs::toggle(id = paste0("overlay_div"))
  removeCssClass(selector = "div[class='progress-group hidden']", class = "hidden")
  promise_summaries$invoke()
})
