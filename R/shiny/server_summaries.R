source("60_summaries.R")

## Trends
## output$summaries_trends <- renderUI({
output$summaries_page <- renderUI({
  trends_meta <- readRDS(file = paste0(data_path, "/summaries/report_card_trend_plots.rds"))
  trends_meta <- trends_meta |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "All", .x)))
  fluidPage(
    fluidRow(
      box(
        ## title = span(icon("chart-line", style = "margin-right: 10px;"), "Trends"),
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        ## column(width = 2,
        ##   selectInput("summaries_indicator_selector", "Select Indicator:",
        ##     choices = unique(trends_meta$Indicator))
        ## ),
        column(
          width = 2,
          selectInput("summaries_subindicator_selector", "Select Subindicator:",
            choices = unique(trends_meta$Subindicator),
            selected = "All"
          )
        ),
        column(
          width = 2,
          selectInput("summaries_measure_selector", "Select Measures:",
            choices = unique(trends_meta$Measure),
            selected = "All"
          )
        ),
        column(
          width = 2,
          selectInput("summaries_region_selector", "Select Region:",
            choices = unique(trends_meta$RegionName),
            selected = "All"
          )
        ),
        column(
          width = 2,
          selectInput("summaries_zone_selector", "Select Zone:",
            choices = unique(trends_meta$ZoneName),
            selected = "All"
          )
        ),
        column(
          width = 2,
          selectInput("summaries_source_selector", "Select Source:",
            choices = unique(trends_meta$Source),
            selected = "All"
          )
        ),
        ## column(width = 2,
        ##   selectInput("summaries_site_selector", "Select Site:",
        ##     choices = unique(trends_meta$Site))
        ## ),
      ),
      ## fillRow(
      ##   flex = NA,
      ##   tags$div(
      ##     class = "dynamic-image",
      ##     imageOutput(outputId = "summaries_trends_plot", width = "1000px", height = "750px"),
      ##   ),
      ##   box(
      ##     class = "caption-box",
      ##     status = "info",
      ##     width = 2,
      ##     solidHeader = TRUE,
      ##     textOutput(outputId = "summaries_trends_plot_caption")
      ##   )
      ## ),
      ## textOutput(outputId = "summaries_trends_plot_filename")
    ),
    fluidRow(
      tabsetPanel(
        tabPanel(
          title = "Trends",
          icon = icon("chart-line"),
          id = "summaries_trends_tab",
          fillRow(
            flex = NA,
            tags$div(
              class = "dynamic-image",
              imageOutput(outputId = "summaries_trends_plot", width = "1000px", height = "750px"),
              ),
            box(
              class = "caption-box",
              status = "info",
              width = 2,
              solidHeader = TRUE,
              textOutput(outputId = "summaries_trends_plot_caption")
            )
          ),
          textOutput(outputId = "summaries_trends_plot_filename")
        ),
        tabPanel(
          title = "Annual effects",
          icon = icon("chart-column"),
          id = "summaries_annual_tab",
          fillRow(
            flex = NA,
            tags$div(
              class = "dynamic-image",
              imageOutput(outputId = "summaries_annual_plot", width = "1000px", height = "750px"),
              ),
            box(
              class = "caption-box",
              status = "info",
              width = 2,
              solidHeader = TRUE,
              textOutput(outputId = "summaries_annual_plot_caption")
            )
          ),
          textOutput(outputId = "summaries_annual_plot_filename")
        ),
        tabPanel(
          title = "Contrast effects",
          icon = icon("chart-column"),
          id = "summaries_contrast_tab",
          fillRow(
            flex = NA,
            tags$div(
              class = "dynamic-image",
              imageOutput(outputId = "summaries_contrast_plot", width = "1000px", height = "750px"),
              ),
            box(
              class = "caption-box",
              status = "info",
              width = 2,
              solidHeader = TRUE,
              textOutput(outputId = "summaries_contrast_plot_caption")
            )
          ),
          textOutput(outputId = "summaries_contrast_plot_filename")
        ),
        )
    )
  )
})

observe({
  Subindicator <- case_when(
    input$summaries_subindicator_selector == "All" ~ " ",
    .default = input$summaries_subindicator_selector
  )
  Measure <- case_when(
    input$summaries_measure_selector == "All" ~ " ",
    .default = input$summaries_measure_selector
  )
  Region <- case_when(
    input$summaries_region_selector == "All" ~ " ",
    .default = input$summaries_region_selector
  )
  Zone <- case_when(
    input$summaries_zone_selector == "All" ~ " ",
    .default = input$summaries_zone_selector
  )
  Source <- case_when(
    input$summaries_source_selector == "All" ~ " ",
    input$summaries_source_selector == "Discrete" ~ "Discrete",
    input$summaries_source_selector == "CFM" ~ "CFM")

  ## Trends
  fnm <- paste0(
    output_path, "figures/summaries/trend___Environmental__Water Quality__",
    Subindicator, "__", Measure,
    "__", Region, "__", Zone,
    "__", Source, ".png"
  )
  output$summaries_trends_plot <- renderImage(
    {
      list(
        src = fnm,
        contentType = "image/png",
        ## height = "750px",
        alt = paste0("Trend plots")
      )
    },
    deleteFile = FALSE
  )
  txt <- paste0("Temporal trend in health index scores for ",
    Subindicator, " (", Measure, ") in ",
    Region, " (", Zone, ") for ",
    Source, " data.  The trend is shown as a line with 95% confidence intervals.",
    "  The symbol colours reflect the letter grades and horizontal dashed lines represent the grade boundaries."
  )
  output$summaries_trends_plot_caption <- renderText(txt)
  output$summaries_trends_plot_filename <- renderText(fnm)

  ## Annual effects
  fnm1 <- str_replace(fnm, "trend", "annual_effects")
  output$summaries_annual_plot <- renderImage(
  {
    list(
      src = fnm1,
      contentType = "image/png",
      ## height = "750px",
      alt = paste0("Annual effects plots")
    )
  },
  deleteFile = FALSE
  )
  txt <- paste0("Annual effects in health index scores for ",
    Subindicator, " (", Measure, ") in ",
    Region, " (", Zone, ") for ",
    Source, " data.  The trend is shown as a line with 95% confidence intervals.",
    "  The symbol colours reflect the letter grades and horizontal dashed lines represent the grade boundaries."
  )
  output$summaries_annual_plot_caption <- renderText(txt)
  output$summaries_annual_plot_filename <- renderText(fnm1)

  ## Contrast (period) effects
  fnm2 <- str_replace(fnm, "trend", "period_effects")
  output$summaries_contrast_plot <- renderImage(
  {
    list(
      src = fnm2,
      contentType = "image/png",
      ## height = "750px",
      alt = paste0("Contrast effects plots")
    )
  },
  deleteFile = FALSE
  )
  txt <- paste0("Contrast effects in health index scores for ",
    Subindicator, " (", Measure, ") in ",
    Region, " (", Zone, ") for ",
    Source, " data.  The trend is shown as a line with 95% confidence intervals.",
    "  The symbol colours reflect the letter grades and horizontal dashed lines represent the grade boundaries."
  )
  output$summaries_contrast_plot_caption <- renderText(txt)
  output$summaries_contrast_plot_filename <- renderText(fnm2)
})

select_detail <- function(focal = "Subindicator") {
  trends_meta <- readRDS(file = paste0(data_path, "/summaries/report_card_trend_plots.rds"))
  trends_meta <- trends_meta |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "All", .x)))
  if (focal == "Subindicator") {
    newfocal <- "Measure"
    selector <- input[["summaries_subindicator_selector"]]
    flt <- \(x) x |> dplyr::filter(!!sym(focal) == selector) 
    ## if (selector == "All") selector <- " "
  } else if (focal == "RegionName") {
    newfocal <- "ZoneName"
    selector <- input[["summaries_region_selector"]]
    flt <- \(x) x |> dplyr::filter(!!sym(focal) == selector) 
    ## selector <- "summaries_region_selector"
    ## if (selector == "All") selector <- " "
  } else if (focal == "Measure" | focal == "ZoneName") {
    newfocal <- "Source"
    selector <- input[["summaries_measure_selector"]]
    flt <- \(x) {
      x |>
        dplyr::filter(
          Measure == input[["summaries_measure_selector"]],
          ZoneName == input[["summaries_zone_selector"]]
        )
    }
  }
  trends_meta |>
    ## dplyr::filter(!!sym(focal) == selector) |>
    flt() |> 
    dplyr::pull({{newfocal}}) |> 
    unique() |>
    sort()
}
  
observeEvent(input$summaries_subindicator_selector, {
  updateSelectInput(session,
    "summaries_measure_selector",
    choices = select_detail(focal = "Subindicator")
  )
})
observeEvent(input$summaries_region_selector, {
  updateSelectInput(session,
    "summaries_zone_selector",
    choices = select_detail(focal = "RegionName")
  )
})

observeEvent(input$summaries_measure_selector, {
  updateSelectInput(session,
    "summaries_source_selector",
    choices = select_detail(focal = "Measure")
  )
})

observeEvent(input$summaries_zone_selector, {
  updateSelectInput(session,
    "summaries_source_selector",
    choices = select_detail(focal = "ZoneName")
  )
})

promise_summaries <- ExtendedTask$new(function() {
  future_promise({
    print(status_file)
    print(log_file)
    print(box_width)
    print(project_name)
    module_summaries()
    ## data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))
    ## data_spatial <- data$spatial
    ## list(data = data, data_spatial = data_spatial)
    list("done")
  }) |>
    then(\(result) {
      toggle_buttons(status_$status, stage =  8, bttn1 = "runSummariesCode", bttn2 = NULL)
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
