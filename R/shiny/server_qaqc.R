source("40_qaqc.R")

progress_vals <- reactiveVal(list(count = 0, total = 10, label = ""))
progress <- shiny::Progress$new()

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


## Observations
output$qaqc_obs <- renderUI({
  data <- promise_qaqc$result()$data 
  qaqctabs <- lapply(
    c("all", "CFM", "Discrete"),
    function(x) {
      nm <- paste0("waterQAQC_", x)
      verticalTabPanel(
        box_height = "80px",
        title = HTML(paste0("QAQC<br>", x)),
        fillRow(
          flex = NA,
          tags$div(
            class = "dynamic-image",
            imageOutput(outputId = nm, height = "750px", width = "1000px"),
            ),
          box(
            class = "caption-box",
            status = "info",
            width = 1,
            solidHeader = TRUE,
            textOutput(paste0(nm, "_caption"))
          )
        )
      )
    }
  )
  ## do.call(verticalTabsetPanel, qaqctabs)
  # Wrap the vertical tabset panel and add the input box to the menu area
  tags$div(
    class = "vertical-tab-container",
    style = "display: flex; flex-direction: column;",
    # Add the input box at the top of the menu
    tags$div(
      class = "vertical-tab-menu",
      style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd;",
      selectInput("year_selector", "Sampling year:",
        choices = sort(unique(data$wq_long$Year)),
        selected = status::get_setting("focal_year")),
      ),
    # Add the vertical tabset panel below the input box
    do.call(verticalTabsetPanel, qaqctabs)
  )
})
observe({
  if (!is.null(input$year_selector)) {
    lapply(c("all", "CFM", "Discrete"), function(x) {
      output[[paste0("waterQAQC_", x)]] <- renderImage(
      {
        list(
          src = paste0(output_path, "figures/QAQC/waterQAQC_", x, "_", input$year_selector, ".png"),
          contentType = "image/png",
          ## height = "750px",
          alt = paste0("water QAQC ", x)
        )
      },
      deleteFile = FALSE
      )
      output[[paste0("waterQAQC_", x, "_caption")]] <- renderText(
        paste0(
          "The figure to the left depicts the observed Measures
(columns) of ", x, " Water Quality data from each of eleven Zones (rows) for
the ", status::get_setting("focal_year"), " sampling year.
The red vertical line indicates associated Water Quality Guideline value.
The transparent red band indicates a range of values represented by half
and twice the guideline value (equivalent to the Scaled Modified Amplitude
index capping domain). The blue band represents the Guideline range for
Dissolved Oxygen. Note, the y-axis only represents jittered and unordered
space. temporal sampling design."
)
)
    })
  }
})

## boxplots
output$qaqc_boxplots <- renderUI({
  data <- promise_qaqc$result()$data
  data_spatial <- promise_qaqc$result()$data_spatial
  boxplotstabs <- lapply(
    c("all", "timeseries", "zone"),
    function(x) {
      nm <- paste0("waterBOXPLOTS_", x)
      verticalTabPanel(
        box_height = "80px",
        title = HTML(paste0("Boxplots<br>", x)),
        ## contentWidth = 11,
        fillRow(
          flex = NA,
          ## style = "width: 90%;", # Adjust the width as needed
          if (x != "zone") {
            tags$div(
              imageOutput(outputId = nm, height = "750px", width = "1000px"),
              style = "margin-right:50px"
            )
          } else {
            fls <- list.files(paste0(output_path, "figures/QAQC/"), pattern = "Zone_")
            zones <- fls |> str_replace(".*_Zone_([0-9]*).*png", "\\1")
            zone_names <- data_spatial |>
              filter(Zone %in% zones) |>
              pull(ZoneName)
            yrs <- fls |>
              str_replace(".*_[0-9]*_(.*).png", "\\1") |> unique() |> sort()
            tags$div(
              fluidRow(
                box(
                  class = "qaqac-zone-box",
                  status = "info",
                  width = 9,
                  solidHeader = TRUE,
                  column(
                    width = 9,
                    selectInput("zone_selector", "Select Zone:", choices = unique(zone_names))
                  ),
                  ),
                ),
              fluidRow(
                imageOutput(outputId = nm, height = "600px", width = "1000px"),
                ),
              ## ),
              style = "margin-right:50px"
            )
          },
          box(
            class = "caption-box",
            status = "info",
            width = 2,
            solidHeader = TRUE,
            textOutput(paste0(nm, "_caption"))
          )
        )
      )
    }
  )
  ## do.call(verticalTabsetPanel, boxplotstabs)
  # Wrap the vertical tabset panel and add the input box to the menu area
  tags$div(
    class = "vertical-tab-container",
    style = "display: flex; flex-direction: column;",
    # Add the input box at the top of the menu
    tags$div(
      class = "vertical-tab-menu",
      style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd;",
      selectInput("year_selector", "Sampling year:",
        choices = sort(unique(data$wq_long$Year)),
        selected = status::get_setting("focal_year")),
      ),
    # Add the vertical tabset panel below the input box
    do.call(verticalTabsetPanel, boxplotstabs)
  )
})
observe({
  data_spatial <- promise_qaqc$result()$data_spatial
  if (!is.null(input$zone_selector) & !is.null(input$year_selector)) {
    lapply(c("all", "timeseries", "zone"), function(x) {
      ## alert(input$zone_selector)
      zone_selector <- data_spatial |>
        filter(ZoneName == input$zone_selector) |>
        pull(Zone)
      output[[paste0("waterBOXPLOTS_", x)]] <- renderImage({
        extr <- case_when(
          x == "all" ~ paste0("_", input$year_selector),
          x == "timeseries" ~ "_timeseries",
          x == "zone" ~ paste0("_Zone_", zone_selector, "_", input$year_selector)
        )
        ht <- case_when(
          x == "all" ~ "750px",
          x == "timeseries" ~ "750px",
          x == "zone" ~ "600px"
        )
        list(src = paste0(output_path, "figures/QAQC/wq_boxplot", extr, ".png"),
          contentType = "image/png",
          height = ht,
          alt = paste0("water BOXPLOTS ", extr))
      }, deleteFile = FALSE)
      txt <- case_when(
        x == "all" ~ paste0("from each of eleven Zones (rows) for
the ", input$year_selector, " sampling year for each source"),
x == "timeseries" ~ paste0("from each of the eleven Zones (rows) across all sampling years"),
x == "zone" ~ paste0("from the ", input$zone_selector, " zone for the ", input$year_selector, " sampling year")
)
      output[[paste0("waterBOXPLOTS_", x, "_caption")]] <- renderText(
        paste0(
          "The figure to the left depicts the boxplots of the observed Measures
(panels) of Water Quality data ", txt, ".
The horizontal dashed lines indicate the associated Water Quality Guideline values."
)
)
    })
  }
})

promise_qaqc <- ExtendedTask$new(function() {
  future_promise({
    print(status_file)
    print(log_file)
    print(box_width)
    print(project_name)

    module_qaqc()
    data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))
    data_spatial <- data$spatial
    list(data = data, data_spatial = data_spatial)
  }) |>
    then(\(result) {
      toggle_buttons(status_$status, stage =  6, bttn1 = "runQAQCCode", bttn2 = "runBootstrappCode")
      shinyjs::enable(selector = "a[data-value='qaqc']")
      addCssClass(selector = "a[data-value='qaqc']", class = "activeLink")
      shinyjs::toggle(id = paste0("overlay_div"))
      result
    })
}) |>
  bslib::bind_task_button("runQAQCCode")

observeEvent(input$runQAQCCode, {
  progress_file <- paste0(progress_path, "/progress.log")
  if (file.exists(progress_file)) {
    file.remove(progress_file)
  }
  shinyjs::toggle(id = paste0("overlay_div"))
  promise_qaqc$invoke()
})

## ## Trigger to run 40_qaqc.R
## observeEvent(input$runQAQCCode, {
##   #module_qaqc()

##   data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))
##   data_spatial <- data$spatial
  
##   ## Observations
##   output$qaqc_obs <- renderUI({
##     qaqctabs <- lapply(
##       c("all", "CFM", "Discrete"),
##       function(x) {
##         nm <- paste0("waterQAQC_", x)
##         verticalTabPanel(
##           box_height = "80px",
##           title = HTML(paste0("QAQC<br>", x)),
##           fillRow(
##             flex = NA,
##             tags$div(
##               class = "dynamic-image",
##               imageOutput(outputId = nm, height = "750px", width = "1000px"),
##             ),
##             box(
##               class = "caption-box",
##               status = "info",
##               width = 1,
##               solidHeader = TRUE,
##               textOutput(paste0(nm, "_caption"))
##             )
##           )
##         )
##       }
##     )
##     ## do.call(verticalTabsetPanel, qaqctabs)
##     # Wrap the vertical tabset panel and add the input box to the menu area
##     tags$div(
##       class = "vertical-tab-container",
##       style = "display: flex; flex-direction: column;",
##       # Add the input box at the top of the menu
##       tags$div(
##         class = "vertical-tab-menu",
##         style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd;",
##         selectInput("year_selector", "Sampling year:",
##           choices = sort(unique(data$wq_long$Year)),
##           selected = status::get_setting("focal_year")),
##       ),
##       # Add the vertical tabset panel below the input box
##       do.call(verticalTabsetPanel, qaqctabs)
##     )
##   })
##   observe({
##     if (!is.null(input$year_selector)) {
##       lapply(c("all", "CFM", "Discrete"), function(x) {
##         output[[paste0("waterQAQC_", x)]] <- renderImage(
##           {
##             list(
##               src = paste0(output_path, "figures/QAQC/waterQAQC_", x, "_", input$year_selector, ".png"),
##               contentType = "image/png",
##               ## height = "750px",
##               alt = paste0("water QAQC ", x)
##             )
##           },
##           deleteFile = FALSE
##         )
##         output[[paste0("waterQAQC_", x, "_caption")]] <- renderText(
##           paste0(
##             "The figure to the left depicts the observed Measures
## (columns) of ", x, " Water Quality data from each of eleven Zones (rows) for
## the ", status::get_setting("focal_year"), " sampling year.
## The red vertical line indicates associated Water Quality Guideline value.
## The transparent red band indicates a range of values represented by half
## and twice the guideline value (equivalent to the Scaled Modified Amplitude
## index capping domain). The blue band represents the Guideline range for
## Dissolved Oxygen. Note, the y-axis only represents jittered and unordered
## space. temporal sampling design."
##           )
##         )
##       })
##     }
##   })

##   ## boxplots
##   output$qaqc_boxplots <- renderUI({
##     boxplotstabs <- lapply(
##       c("all", "timeseries", "zone"),
##       function(x) {
##         nm <- paste0("waterBOXPLOTS_", x)
##         verticalTabPanel(
##           box_height = "80px",
##           title = HTML(paste0("Boxplots<br>", x)),
##           ## contentWidth = 11,
##           fillRow(
##             flex = NA,
##             ## style = "width: 90%;", # Adjust the width as needed
##             if (x != "zone") {
##               tags$div(
##                 imageOutput(outputId = nm, height = "750px", width = "1000px"),
##                 style = "margin-right:50px"
##               )
##             } else {
##               fls <- list.files(paste0(output_path, "figures/QAQC/"), pattern = "Zone_")
##               zones <- fls |> str_replace(".*_Zone_([0-9]*).*png", "\\1")
##               zone_names <- data_spatial |>
##                 filter(Zone %in% zones) |>
##                 pull(ZoneName)
##               yrs <- fls |>
##                 str_replace(".*_[0-9]*_(.*).png", "\\1") |> unique() |> sort()
##               tags$div(
##                 fluidRow(
##                   box(
##                     class = "qaqac-zone-box",
##                     status = "info",
##                     width = 9,
##                     solidHeader = TRUE,
##                     column(
##                       width = 9,
##                       selectInput("zone_selector", "Select Zone:", choices = unique(zone_names))
##                     ),
##                   ),
##                 ),
##                 fluidRow(
##                   imageOutput(outputId = nm, height = "600px", width = "1000px"),
##                 ),
##                 ## ),
##                 style = "margin-right:50px"
##               )
##             },
##             box(
##               class = "caption-box",
##               status = "info",
##               width = 2,
##               solidHeader = TRUE,
##               textOutput(paste0(nm, "_caption"))
##             )
##           )
##         )
##       }
##     )
##     ## do.call(verticalTabsetPanel, boxplotstabs)
##     # Wrap the vertical tabset panel and add the input box to the menu area
##     tags$div(
##       class = "vertical-tab-container",
##       style = "display: flex; flex-direction: column;",
##       # Add the input box at the top of the menu
##       tags$div(
##         class = "vertical-tab-menu",
##         style = "padding: 10px; background-color: #f8f9fa; border-bottom: 1px solid #ddd;",
##         selectInput("year_selector", "Sampling year:",
##           choices = sort(unique(data$wq_long$Year)),
##           selected = status::get_setting("focal_year")),
##       ),
##       # Add the vertical tabset panel below the input box
##       do.call(verticalTabsetPanel, boxplotstabs)
##     )

##   })
##   observeEvent(input$zone_selector, {
##     ## alert(input$zone_selector)
##     ## zone_selector <- data_spatial |>
##     ##   filter(ZoneName == input$zone_selector) |>
##     ##   pull(Zone)
##   }, ignoreInit = TRUE)
##   observe({
##     if (!is.null(input$zone_selector) & !is.null(input$year_selector)) {
##       lapply(c("all", "timeseries", "zone"), function(x) {
##         ## alert(input$zone_selector)
##         zone_selector <- data_spatial |>
##           filter(ZoneName == input$zone_selector) |>
##           pull(Zone)
##         output[[paste0("waterBOXPLOTS_", x)]] <- renderImage({
##           extr <- case_when(
##             x == "all" ~ paste0("_", input$year_selector),
##             x == "timeseries" ~ "_timeseries",
##             x == "zone" ~ paste0("_Zone_", zone_selector, "_", input$year_selector)
##           )
##           ht <- case_when(
##             x == "all" ~ "750px",
##             x == "timeseries" ~ "750px",
##             x == "zone" ~ "600px"
##           )
##           list(src = paste0(output_path, "figures/QAQC/wq_boxplot", extr, ".png"),
##             contentType = "image/png",
##             height = ht,
##             alt = paste0("water BOXPLOTS ", extr))
##         }, deleteFile = FALSE)
##           txt <- case_when(
##             x == "all" ~ paste0("from each of eleven Zones (rows) for
## the ", input$year_selector, " sampling year for each source"),
##             x == "timeseries" ~ paste0("from each of the eleven Zones (rows) across all sampling years"),
##             x == "zone" ~ paste0("from the ", input$zone_selector, " zone for the ", input$year_selector, " sampling year")
##           )
##         output[[paste0("waterBOXPLOTS_", x, "_caption")]] <- renderText(
##           paste0(
##             "The figure to the left depicts the boxplots of the observed Measures
## (panels) of Water Quality data ", txt, ".
## The horizontal dashed lines indicate the associated Water Quality Guideline values."
## )
## )
##       })
##     }
##   })
  
##   toggle_buttons(status_$status, stage =  6, bttn1 = "runQAQCCode", bttn2 = "runBootstrappCode")
##   shinyjs::enable(selector = "a[data-value='qaqc']")
##   addCssClass(selector = "a[data-value='qaqc']", class = "activeLink")

## })


