
tag_styles <- tags$style(HTML(
  "
.vrtc-tab-panel-menu {
width: 10%;
}

div.vrtc-tab-panel-container {
margin-top: 0px;
}

.list-group-item {
 padding: 0px 15px;
}

.caption-box {
border: 1px solid black;
padding: 10px;
}

.dynamic-image > div > img {
max-height: 750px;
max-width: 900px;
height: auto;
width: auto;
"
)) 


summaries_tab <- tabItem(
  tabName = "summaries",
  tag_styles,
  ## tabsetPanel(
  ##   id =  "summaries_tabs",
  ##   tabPanel(
  ##       title = "Trends",
  ##     icon = icon("chart-line"),
  ##     id = "summaries_trends_tab",
  ##     uiOutput("summaries_trends")
  ##   ),
  ##   tabPanel(
  ##     title = "Annual effects",
  ##     icon = icon("chart-column"),
  ##     id = "summaries_annual_tab",
  ##     uiOutput("summaries_annual")
  ##   ),
  ##   tabPanel(
  ##     title = "Contrast effects",
  ##     icon = icon("chart-gantt"),
  ##     id = "summaries_contrast_tab",
  ##     uiOutput("summaries_contrast")
  ##   )
  ## )
  uiOutput("summaries_page")
)
