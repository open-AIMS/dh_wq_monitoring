source("50_bootstrapp.R")

## Trigger to run 50_bootstrapp.R
observeEvent(input$runBootstrappCode, {
  module_boot()

  toggle_buttons(status_$status, stage =  6, bttn1 = "runBootstrappCode", bttn2 = "runSummariesCode")
  shinyjs::enable(selector = "a[data-value='bootstrapp']")
  addCssClass(selector = "a[data-value='bootstrapp']", class = "activeLink")
})
