source("30_indices.R")

output$Indices_data <- reactable::renderReactable({
  promise_indices$result() |> 
    group_by(across(-Index)) |>
    ## mutate(boot = 1:n()) |>
    ## ungroup() |>
    ## dplyr::select(-Index, everything(), Index) |>
    summarise(
      lower = quantile(Index, 0.025, na.rm = TRUE)[[1]],
      upper = quantile(Index, 0.975, na.rm = TRUE)[[1]],
      Index = mean(Index, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(Index = sprintf("%0.3f [%0.3f, %0.3f]", Index, lower, upper)) |>
    dplyr::select(-lower, -upper) |> 
    ## convert all numeric columns into characters so that they can be filtered
    mutate(across(c(Region, Zone), function(x) sprintf("%0.0f", x))) |> 
    mutate(across(where(is.numeric), function(x) sprintf("%0.3f", x))) |> 
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

promise_indices <- ExtendedTask$new(function() {
  future_promise({
    print(status_file)
    print(log_file)
    print(box_width)
    print(project_name)
    module_indices()
    data_boot_zone <- readRDS(file = paste0(data_path, "/processed/indices/data_boot_zone.rds"))
    data_boot_zone
  }) |>
    then(\(result) {
      toggle_buttons(status_$status, stage =  5, bttn1 = "runIndicesCode", bttn2 = "runQAQCCode")
      result
    })
}) |>
  bslib::bind_task_button("runIndicesCode")

observeEvent(input$runIndicesCode, {
  promise_indices$invoke()
})

## ## Trigger to run 30_indices.R
## observeEvent(input$runIndicesCode, {
##   ## sedMod::module_eda()
##   module_indices()

##   data_boot_zone <- readRDS(file = paste0(data_path, "/processed/indices/data_boot_zone.rds"))

##   output$Indices_data <- reactable::renderReactable({
##     ## dv <-
##     data_boot_zone |>
##       group_by(across(-Index)) |>
##       ## mutate(boot = 1:n()) |>
##       ## ungroup() |>
##       ## dplyr::select(-Index, everything(), Index) |>
##       summarise(
##         lower = quantile(Index, 0.025, na.rm = TRUE)[[1]],
##         upper = quantile(Index, 0.975, na.rm = TRUE)[[1]],
##         Index = mean(Index, na.rm = TRUE)
##       ) |>
##       ungroup() |>
##       mutate(Index = sprintf("%0.3f [%0.3f, %0.3f]", Index, lower, upper)) |>
##       dplyr::select(-lower, -upper) |> 
##       ## convert all numeric columns into characters so that they can be filtered
##       mutate(across(c(Region, Zone), function(x) sprintf("%0.0f", x))) |> 
##       mutate(across(where(is.numeric), function(x) sprintf("%0.3f", x))) |> 
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
##   toggle_buttons(status_$status, stage =  5, bttn1 = "runIndicesCode", bttn2 = "runQAQCCode")
## })

output$download_indices_data <- downloadHandler(
  filename = function() {
    # Use the selected dataset as the suggested file name
    paste0("data", ".csv")
  },
  content = function(file) {

    data_boot_zone <- readRDS(file = paste0(data_path, "/processed/indices/data_boot_zone.rds"))
    # Write the dataset to the `file` that will be downloaded
    write_csv(data_boot_zone, file)
  }
)
