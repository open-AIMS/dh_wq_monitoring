source("startup_functions.R")

server <- function(input, output, session) {
  ## sedMod::start_matter()
  start_matter()
  source("shiny/global.R", local = TRUE)
  source("shiny/server_sidebar.R", local = TRUE)
  source("shiny/server_landing.R", local = TRUE)
  source("shiny/server_status.R", local = TRUE)
  source("shiny/server_raw_data.R", local = TRUE)
  source("shiny/server_processed_data.R", local = TRUE)
  source("shiny/server_indices.R", local = TRUE)
  source("shiny/server_qaqc.R", local = TRUE)
  source("shiny/server_bootstrapp.R", local = TRUE)
  source("shiny/server_summaries.R", local = TRUE)

  ## source("shiny/server_eda.R", local = TRUE)
  ## source("shiny/server_analysis.R", local = TRUE)
  ## source("shiny/server_analysis_overview.R", local = TRUE)
  ## source("shiny/server_analysis_diagnostics.R", local = TRUE)
  ## source("shiny/server_analysis_details.R", local = TRUE)
  source("shiny/server_test.R", local = TRUE)
}
