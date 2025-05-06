sidebar <- dashboardSidebar(
  sidebarMenu(
    tags$head(tags$style(".inactiveLink {
                           pointer-events: none;
                           color: grey !important;
                           cursor: default;
                           }
                      .activeLink {
                           pointer-events: auto;
                           color: orange !important;
                           cursor: pointer;
                           }
                      .btn:not(.btn-tool-box){
                           text-align:left !important;
                           width: 130px;
                           }
    ")),
    menuItem("Landing", tabName = "landing", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    ## menuItem("Settings", tabName = "settings", icon = icon("sliders")),
    menuItem("Data", tabName = "data", icon = icon("file-excel")),
    menuItem("QAQC", tabName = "qaqc", icon = icon("chart-column")),
    ## menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-column")),
    menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
    menuItem("Manual", tabName = "manual", icon=icon("mortar-board")),
    hr(),
    actionButton("runLoadCode", "Run Stage 2",
      icon = icon("play")),
    actionButton("runProcessCode", "Run Stages 3 & 4",
      icon = icon("play"),
      class = "btn-disabled"),
    actionButton("runIndicesCode", "Run Stage 5",
      icon = icon("play"),
      class = "btn-disabled"),
    actionButton("runQAQCCode", "Run Stage 6",
      icon = icon("play"),
      class = "btn-disabled"),
    actionButton("runBootstrappCode", "Run Stage 7",
      icon = icon("play"),
      class = "btn-disabled"),
    actionButton("runSummariesCode", "Run Stage 8",
      icon = icon("play"),
      class = "btn-disabled")

    ## actionButton("runEDACode", "Run Stage 4", icon = icon("play"), class = "btn-disabled"),
    ## actionButton("runAnalysisCode", "Run Stage 5", icon = icon("play"))
    ## actionButton("runAnalysisCode", "Run Stage 5", icon = icon("play"), class = "btn-disabled")
    ## actionButton("runTestCode", "Run Test", icon = icon("play"), class = "btn-enabled")
  )
)
