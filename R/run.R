library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyTree)
library(bslib)
library(fansi)
library(DT)
library(reactable)
library(leaflet)
library(shinyjs)

if (options()$browser == "") options(browser = "chromium")
#options(browser = "webmacs")
#setwd('~/Work/AIMS/Projects/Darwin Harbour Sediment Quality Shiny/dh_sediment_monitoring/R/')

#unloadNamespace("sedMod")
#detach(package:sedMod)
#library(sedMod)
unloadNamespace("status")
#detach(package:status)
library(status)
library(reportcards)

source("shiny/ui.R")
source("shiny/server.R")

runApp(
        shinyApp(ui, server),
        port = 3838,
        host = "0.0.0.0",
        launch.browser = TRUE
)
