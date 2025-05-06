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


qaqc_tab <- tabItem(
  tabName = "qaqc",
  tag_styles,
  tabsetPanel(
    id =  "qaqc_tabs",
    tabPanel(
      title = "Observations",
      icon = icon("eye"),
      id = "qaqc_obs_tab",
      uiOutput("qaqc_obs")
    ),
    tabPanel(
      title = "Boxplots",
      icon = icon("chart-simple"),
      id = "qaqc_boxplots_tab",
      uiOutput("qaqc_boxplots")
    )
  )
)
