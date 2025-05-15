data_tab <- tabItem(
  tabName = "data",
  fluidPage(  ## This is necessary to allow content that is longer than the screen to stretch the size of the container
    ## verticalTabsetPanel(
    tabsetPanel(
      id = "data_tabs",
      tabPanel(
        title = "Raw data",
        icon = icon("database"),
        id = "raw_data_tab",
        reactableOutput("uploaded_files_table"),
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Instructions",
            box(
              title = span(icon("info", style = "margin-right: 10px;"), "Instructions"),
              width = 6,
              solidHeader = TRUE,
              status = "info",
              htmltools::includeMarkdown("../md/data_raw_instructions.md")
            ),
            box(
              title = span(icon("info", style = "margin-right: 10px;"), "Data requirements"),
              width = 6,
              solidHeader = TRUE,
              status = "info",
              htmltools::includeMarkdown("../md/raw_data.md")
            ),
            ),
          tabPanel(
            title = "Data",
            reactableOutput("Sheet_data"),
            downloadButton("download_raw_data", "Download as csv")
          ),
          tabPanel(
            title = "Validation issues",
            reactableOutput("Sheet_issues"),
            downloadButton("download_issues_data", "Download as csv")
          )
        )
      ),

      tabPanel(
        title = "Processed data",
        icon = icon("table"),
        id = "processed_data_tab",
        box(
          width =  12,
          reactableOutput("Processed_data"),
          p("The following button allows you to download all the processed data as a single csv file.  Please be aware that preparing the data for download may take some time, depending on the size of the data.  Please be patient and refrane from clicking the download button repeatedly - this will not speed up the process.  When the data are available for download, a file picker dialog box will apear."),
          downloadButton("download_processed_data", "Download as csv"),
          ),
        ## csvDownloadButton("cars_table", filename = "cars.csv")
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "Instructions"),
          width = 6,
          solidHeader = TRUE,
          status = "info",
          htmltools::includeMarkdown("../md/processed_data_instructions.md")
        ),
        ),
      
      tabPanel(
        title = "Indices data",
        icon = icon("table"),
        id = "indices_data_tab",
        box(
          width =  12,
          p("The following table contains summaries of the Zone/Measure/Source level indices."),
          reactableOutput("Indices_data"),
          p("The following button allows you to download all the indices data as a single csv file.  Please be aware that preparing the data for download may take some time, depending on the size of the data.  Please be patient and refrane from clicking the download button repeatedly - this will not speed up the process.  When the data are available for download, a file picker dialog box will apear."),
          downloadButton("download_indices_data", "Download as csv"),
          ),
        ## csvDownloadButton("cars_table", filename = "cars.csv")
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "Instructions"),
          width = 6,
          solidHeader = TRUE,
          status = "info",
          htmltools::includeMarkdown("../md/indices_data_instructions.md")
        ),
        )
    )
  )
)

