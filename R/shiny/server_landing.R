updateTextInput(session, "settings_project_path_input",
  value = dirname(getwd())
)
updateTextInput(session, "settings_status_path_input", value = status_$settings$status_dir$item)
updateTextInput(session, "settings_status_file_input", value = status_$settings$status_file$item)
updateTextInput(session, "settings_log_file_input", value = status_$settings$log_file$item)
updateTextInput(session, "settings_input_dir_input", value = status_$setting$input_path$item)
updateTextInput(session, "settings_data_dir_input", value = status_$setting$data_path$item)
updateTextInput(session, "settings_params_dir_input", value = status_$setting$params_path$item)
updateTextInput(session, "settings_output_dir_input", value = status_$setting$output_path$item)
updateTextInput(session, "settings_docs_dir_input", value = status_$setting$docs_path$item)
updateTextInput(session, "settings_run_stages_input", value = names(status_$status))

## output$settings_input_dir_files <- renderText({
##   files <- list.files(path = status_$settings$input_path$item, pattern = ".*xlsx|.*csv")
##   stringr::str_wrap(paste0(files, collapse = "\t"), 45)
## })
output$settings_input_dir_files <- renderReactable({
  files <- list.files(path = status_$settings$input_path$item, pattern = ".*csv")
  ## stringr::str_wrap(paste0(files, collapse = "\t"), 45)
  files |>
    as.data.frame() |> 
    reactable(
      compact = TRUE, bordered = TRUE,
      pagination = FALSE,
      height = 200,
      theme = reactableTheme(
        headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
        borderWidth = "1pt",
        borderColor = "rgb(85, 85, 85)",
        style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "10px")
      )) 
})

## When hit the Clear previous data button
observeEvent(input$runClearPreviousDataCode, {
  print("Cleaning")
  cleanse_paths(
    paths = c("data_path", "output_path"),
    files = list.files(
      path = docs_path,
      pattern = ".*(.qmd|.html|.md)",
      full.names = TRUE
    )
  )
  prepare_paths()                                                    ## prepare file structure
}
)

## Triggered from the settings_run_in_sequence switch
observeEvent(input$settings_run_in_sequence, {
  if (input$settings_run_in_sequence) {
    ## shinyjs::enable("runProcessCode")
    ## shinyjs::enable("runIndicesCode")
    ## shinyjs::enable("runQAQCCode")
    for (id in c("data", "qaqc", "summaries")) {
      shinyjs::disable(selector = paste0("a[data-value='", id, "']"))
      removeCssClass(selector = paste0("a[data-value='", id, "']"), class = "activeLink")
    }
    for (id in c("runProcessCode", "runIndicesCode", "runQAQCCode",
      "runBootstrappCode", "runSummariesCode")) {
      shinyjs::disable(id)
      shinyjs::removeClass(id = id, class = "btn-default")
      shinyjs::addClass(id = id, class = "btn-disabled")
    }
  } else {
    for (id in c("data", "qaqc", "summaries")) {
      shinyjs::enable(selector = paste0("a[data-value='", id, "']"))
      addCssClass(selector = paste0("a[data-value='", id, "']"), class = "activeLink")
    }
    ## shinyjs::enable(selector = "a[data-value='qaqc']")
    ## addCssClass(selector = "a[data-value='qaqc']", class = "activeLink")
    ## shinyjs::enable(selector = "a[data-value='summaries']")
    ## addCssClass(selector = "a[data-value='summaries']", class = "activeLink")
    ## shinyjs::disable("runProcessCode")
    ## shinyjs::disable("runIndicesCode")
    ## shinyjs::disable("runQAQCCode")
    for (id in c("runProcessCode", "runIndicesCode", "runQAQCCode",
      "runBootstrappCode", "runSummariesCode")) {
      shinyjs::enable(id)
      shinyjs::removeClass(id = id, class = "btn-disabled")
      shinyjs::addClass(id = id, class = "btn-default")
    }
  }
})
