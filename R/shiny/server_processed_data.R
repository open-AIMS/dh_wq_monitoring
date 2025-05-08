source("15_prepare_spatial.R")
source("20_process_data.R")

output$Processed_data <- reactable::renderReactable({
  promise_process$result()$wq_long |> 
    slice(1:2000) |> 
    reactable(
      pagination = TRUE,
      ## defaultPageSize = 50, # Show 50 rows per page
      compact = TRUE, bordered = TRUE, resizable = TRUE,
      highlight = TRUE,
      wrap = FALSE,
      filterable = TRUE,
      defaultColDef = colDef(filterMethod = JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId].indexOf(filterValue) !== -1
        })
      }")),
      ##pagination = FALSE, height = 600,
      ## defaultColDef = colDef(style = "white-space: nowrap;"),
      theme = reactableTheme(
        headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
        borderWidth = "1pt",
        borderColor = "rgb(85, 85, 85)",
        style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "10px")
      )
    )
})

promise_process <- ExtendedTask$new(function() {
  future_promise({
    print(status_file)
    print(log_file)
    print(box_width)
    print(project_name)
    module_prepare_spatial()
    module_process_data()
    data <- readRDS(file = paste0(data_path, "processed/data.rds"))
    data
  }) |>
    then(\(result) {
      toggle_buttons(status_$status, stage =  3, bttn1 = "runProcessCode", bttn2 = "runIndicesCode")
      result
    })
}) |>
  bslib::bind_task_button("runProcessCode")

observeEvent(input$runProcessCode, {
  promise_process$invoke()
})

## ## Trigger to run 20_process_data.R
## observeEvent(input$runProcessCode, {
##   ## sedMod::module_process_data()
##   module_prepare_spatial()
##   module_process_data()
  
##   data <- readRDS(file = paste0(data_path, "processed/data.rds"))
##   output$Processed_data <- reactable::renderReactable({
##     ## dv <-
##       data$wq_long |>
##       slice(1:2000) |> 
##       reactable(
##         pagination = TRUE,
##         ## defaultPageSize = 50, # Show 50 rows per page
##         compact = TRUE, bordered = TRUE, resizable = TRUE,
##         highlight = TRUE,
##         wrap = FALSE,
##         filterable = TRUE,
##         defaultColDef = colDef(filterMethod = JS("function(rows, columnId, filterValue) {
##         return rows.filter(function(row) {
##           return row.values[columnId].indexOf(filterValue) !== -1
##         })
##       }")),
##           ##pagination = FALSE, height = 600,
##         ## defaultColDef = colDef(style = "white-space: nowrap;"),
##         theme = reactableTheme(
##           headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
##           borderWidth = "1pt",
##           borderColor = "rgb(85, 85, 85)",
##           style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "10px")
##         )
##       )
##   })

##   toggle_buttons(status_$status, stage =  3, bttn1 = "runProcessCode", bttn2 = "runIndicesCode")
##   ## toggle_buttons(status_$status, stage =  3, bttn1 = "runProcessCode", bttn2 = "runEDACode")
##   ## addCssClass(selector = "a[data-value='eda']", class = "activeLink")
## })


output$download_processed_data <- downloadHandler(
  filename = function() {
    # Use the selected dataset as the suggested file name
    paste0("data", ".csv")
  },
  content = function(file) {
    data <- readRDS(file = paste0(data_path, "processed/data.rds"))
    data <- data$wq_long
    # Write the dataset to the `file` that will be downloaded
    write_csv(data, file)
  }
)

