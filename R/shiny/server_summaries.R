source("60_summaries.R")

## Trigger to run 60_summaries.R
observeEvent(input$runSummariesCode, {
  module_summaries()
})
