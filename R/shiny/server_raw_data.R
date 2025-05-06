source("10_load_data.R")

uploaded_files <- reactiveVal(list())
output$uploaded_files_table <- reactable::renderReactable(uploaded_files()$data)
## Trigger to run 10_load_data.R
observeEvent(input$runLoadCode, {
  ## Run the load data module
  ## - reads in the data
  ## - validates the data
  module_load_data()
  
  raw_data <- readRDS(file = paste0(data_path, "primary/data.rds"))
  raw_data_validation <- readRDS(file = paste0(data_path, "primary/data_validation.RData"))
  filename_lookup <- readRDS(file = paste0(data_path, "primary/filename_lookup.rds"))
  ## uploaded_files(list(data = raw_data_to_reactable(raw_data, raw_data_validation)))
  uploaded_files(list(data = raw_data_to_reactable(raw_data, raw_data_validation, filename_lookup)))

  toggle_buttons(status_$status, stage =  1, bttn1 = "runLoadCode", bttn2 = "runProcessCode")
  
  shinyjs::enable(selector = "a[data-value='data']")
  addCssClass(selector = "a[data-value='data']", class = "activeLink")
})

## Make a trigger associated with clicking rows of the uploaded data table
observeEvent(input[["rowClicked"]], {
  raw_data <- readRDS(file = paste0(data_path, "primary/data.rds"))
  raw_data <- c(raw_data$wq, raw_data[names(raw_data) != "wq"])
  filename_lookup <- readRDS(file = paste0(data_path, "primary/filename_lookup.rds"))
  raw_data_validation <- readRDS(file = paste0(data_path, "primary/data_validation.RData"))
  raw_data_validation <- c(raw_data_validation$wq, raw_data_validation[names(raw_data_validation) != "wq"])
  ## cat("Run the function for row ", input[["rowClicked"]], "\n")
  output$Sheet_data <- reactable::renderReactable({
    dv <- raw_data_validation |>
      enframe(name = "File") |>
      bind_cols(path = basename(filename_lookup$path)) |>
      dplyr::rename(name = File) |>
      dplyr::select(File = path, name, data = value) |>
      ## mutate(data = map(.x = value, .f = ~ enframe(.x, name = "Sheet"))) |>
      ## dplyr::select(-value) |>
      ## unnest(data) |>
      ## dplyr::select(-value) |>
      slice(input[["rowClicked"]])
    ## Add a dynamic downloadHandler()
    output$download_raw_data <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("raw_data", "_", str_remove(dv$name, ".csv"), ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write_csv(raw_data[[dv$name]], file)
      }
    )
    ## Generate the table
    raw_data[[dv$name]] |>
      ## mutate(across(contains("(mg/kg)"), function(x) {
      ##   ifelse(str_detect(x, "<|^$"), x, round(as.numeric(x),3))
      ## })) |>
      reactable(
        compact = TRUE, bordered = TRUE,
        defaultColDef = colDef(format = colFormat(digits = 3)),
        theme = reactableTheme(
          headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
          borderWidth = "1pt",
          borderColor = "rgb(85, 85, 85)",
          style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "12px")
        )
      )
  })
  
  output$Sheet_issues <- reactable::renderReactable({
    dv <-
      raw_data_validation |>
      enframe(name = "File") |>
      bind_cols(path = basename(filename_lookup$path)) |>
      dplyr::rename(name = File) |>
      dplyr::select(File = path, name, data = value) |>
      slice(input[["rowClicked"]]) 

    issues_data <-
      dv |>
      dplyr::select(data) |>
      unnest(cols = c(data)) |>
      dplyr::select(df) |>
      unnest(cols = c(df)) |>
      dplyr::select(description, severity, everything(), -name, -value, -expression) 
    ## Add a dynamic downloadHandler()
    output$download_issues_data <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("issues_data", "_", str_remove(dv$name, ".csv"), ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write_csv(issues_data, file)
      }
    )
    ## Generate the table
    ## dv |>
    ##   dplyr::select(data) |>
    ##   unnest(cols = c(data)) |>
    ##   dplyr::select(df) |>
    ##   unnest(cols = c(df)) |>
    ##   dplyr::select(description, severity, everything(), -name, -value, -expression) |>
    issues_data |> 
      reactable(
        compact = TRUE, bordered = TRUE,
        theme = reactableTheme(
          headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
          borderWidth = "1pt",
          borderColor = "rgb(85, 85, 85)",
          style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "12px")
        )
      )
  })
})



raw_data_to_reactable <- function(lst, lst.valid, filename_lookup) {
  ## raw_data_to_info_dataframe(lst, lst.valid) |>
  files_to_info_dataframe(lst, lst.valid, filename_lookup) |>
    data_frame_to_reactable()
}

files_to_info_dataframe <- function(lst, lst.valid, filename_lookup) {
  ## start with the input and parameter files
  filenames <- filename_lookup$path 
  ## get the parent files
  file_info <- file.info(filenames) |>
    as.data.frame() |>
    rownames_to_column(var = "File") |>
    mutate(File = basename(File)) |>
    dplyr::select(File, Size = size, Time = mtime) |>
    cbind(name = filename_lookup$name)
  ## unlist the wq items into their own items
  lst.valid <- c(lst.valid$wq, lst.valid[names(lst.valid) != "wq"])
  # Append ".csv" to names that don't match the regex
  ## names(lst.valid) <- sapply(names(lst.valid), function(name) {
  ##   if (!grepl("^[0-9]*_wq\\.csv", name)) {
  ##     paste0(name, ".csv")
  ##   } else {
  ##     name
  ##   }
  ## })
  info <-
    file_info |>
    as_tibble() |>
    full_join(get_validation_info(lst.valid) |>
      dplyr::rename("name" = "File"), by = "name") |>
    arrange(File) |>
    dplyr::select(-name)
  return(info)
}

data_frame_to_reactable <- function(df) {
  df |>
    dplyr::select(-value) |>
    reactable(
      ## groupBy = "File",
      resizable = TRUE,
      compact =  TRUE,
      selection = "single",
      ## selection = "multiple",
      pagination = FALSE,
      highlight = TRUE,
      columns = list(
        File = colDef(minWidth = 1),
        Size = colDef(aggregate = "max", minWidth = 1),
        Time = colDef(aggregate = "max", minWidth = 1),
        ## Sheet = colDef(aggregate = "unique", minWidth = 1),
        Status = colDef(
          minWidth = 1,
          html =  TRUE,
          cell = function(value) status_symbol(value),
          aggregate =  aggregate_table(Status)#,
        )
      ),
      bordered =  TRUE,
      theme = reactableTheme(
        headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
        borderWidth = "1pt",
        borderColor =  "rgb(85, 85, 85)",
        rowSelectedStyle = list(backgroundColor = "#c0d6e4", color = "#000"),
        style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "14px")
      ),
      onClick = JS("function(rowInfo, column) {
          Shiny.setInputValue('rowClicked', rowInfo.index + 1, { priority: 'event' })
          // Trigger the 'select' action
          rowInfo.toggleRowSelected()
        }"
      )
      ## onClick = "select"
    )
}


status_symbol <- function(status) {
  print(status)
  if (status) {
    tagAppendAttributes(shiny::icon("circle-check"), style = paste("color: green; font-weight:900;"))
  } else {
    tagAppendAttributes(shiny::icon("circle-xmark"), style = paste("color: red; font-weight:900;"))
  }
}

aggregate_table <- function(x) {
  JS(paste0(
    "function(values) {
//console.log(values.toString())
s = values.toString().includes('false')
//console.log(s)
//console.log(s == true)
//console.log(s == 'true')
//if any of the values are false (e.g. failed)
if (s == true) {  // at least one failure
//console.log('At least one fail')
return('<i class=\"jstree-icon jstree-themeicon far fa-circle-xmark jstree-themeicon-custom\" role = \"presentation\", style = \"color: red;\"></>')
} else {  // no failures
//console.log('No fails')
return('<i class=\"jstree-icon jstree-themeicon far fa-circle-check jstree-themeicon-custom\" role = \"presentation\", style = \"color: green;\"></>')
}
}"
))

}
