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
.sidebar-menu {
overflow: visible;
}

.overlay-disabled {
  display: none;
  position: fixed;
  top: 0; left: 0; width: 100%; height: 100%;
  background-color: rgba(0, 0, 0, 0.7); 
  color: white;
  text-align: left;
  font-size: 24px;
  z-index:9999;
}

.overlay {
position: fixed;
top: 0; left: 0; width: 10%; height: 10%;
             background-color: rgba(0, 0, 0, 0.7); 
             color: white;
text-align: left;
font-size: 24px;
z-index:9999;
}

.overlay > div {
left:50%;
top:50%;
transform: translate(-50%, -50%);
position:relative;
}

.overlay #log_out {
display:flex;
flex-direction: column-reverse;
height: 500px;
line-height:1.5rem;
}

#wait_message {
//display:none;
}

.overlay #wait_message {
display:inline;
right:20px;
position:absolute;
}

.box-header {
 text-align:left;
}

.progress-text {
 color:black;
 //float:left;
 display: block;
 text-align:left;
 font-weight: normal !important;
}
.progress-number {
 color:black;
}

.hidden {
 display:none;
}
    ")),
    menuItem("Landing", tabName = "landing", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    ## menuItem("Settings", tabName = "settings", icon = icon("sliders")),
    menuItem("Data", tabName = "data", icon = icon("file-excel")),
    menuItem("QAQC", tabName = "qaqc", icon = icon("chart-column")),
    ## menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("chart-column")),
    menuItem("Summaries", tabName = "summaries", icon = icon("calculator")),
    menuItem("Manual", tabName = "manual", icon=icon("mortar-board")),
hr(),
div(style = "position: relative;",
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
    class = "btn-disabled"),
    ## class = "btn-enabled"),

  ## actionButton("runEDACode", "Run Stage 4", icon = icon("play"), class = "btn-disabled"),
  ## actionButton("runAnalysisCode", "Run Stage 5", icon = icon("play"))
  ## actionButton("runAnalysisCode", "Run Stage 5", icon = icon("play"), class = "btn-disabled")
  ## actionButton("runTestCode", "Run Test", icon = icon("play"), class = "btn-enabled"),
  ## textOutput("current_time", inline=TRUE),
  ## progressBar(id = "progress_bar", value = 0, total = 10),  # Progress bar
  ## textOutput("progress_text"),  # Display progress as text
  ## textOutput("sum"),

  div(id = "overlay_div", class = "overlay-disabled1",
    ## style = "margin-top: auto; margin-bottom:0px;",
    style = "
            position: absolute;
color: black;
            top: 0;
            left: 0;
            width: 220px;
            height: 350px;
            background-color: rgba(0, 0, 0, 0.5);
            color: white;
            display: none;  /* Initially hidden */
            justify-content: center;
            align-items: center;
            text-align: center;
            z-index: 10;
          ",
    box(
      title = span(#icon("info", style = "margin-right: 5px; text-align:left;"),
        #"Status",
        div(id = "wait_message",
          icon("spinner", class = "fa-spin"),
          "Processing...", style = "text-align:left;"
        )),
      width = 12,
      solidHeader = TRUE,
      status = "info",
      p("Some tasks are relatively slow. This box will remain until the task is complete.  When relevant, a progress bar will track the progression of a task.", style = "color:black; text-wrap:auto; text-align:left;"),
      progressBar(id = "progress_bar", value = 0, total = 10),  # Progress bar
      #verbatimTextOutput("log_output")  ## note, this has a style defined in styles.R
    )
  ) 
)
)
)
