# =============================================================================
# server_main.R
# Sweep Width Calibration App
#
# Purpose:
#   Top-level server function. Initialises the shared reactive values object
#   and delegates to each module server. This file is sourced by app.R and
#   its contents are passed directly to shinyServer() / the server argument
#   of shinyApp().
#
# Module delegation:
#   server_data_input.R      -> data upload, validation, preview
#   server_classification.R  -> tolerance zones, detection classification
#   server_esw_results.R     -> curve fitting, ESW calculation, plots
# =============================================================================

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # Shared reactive values
  # All inter-module state lives here. Modules read and write to rv.
  # ---------------------------------------------------------------------------
  
  rv <- reactiveValues(
    # Data input
    master            = NULL,
    records           = NULL,
    master_error      = NULL,
    records_error     = NULL,
    n_runs            = 1,
    
    # Classification
    records_classified    = NULL,
    classification_summary = NULL,
    classification_error  = NULL
  )
  
  # ---------------------------------------------------------------------------
  # Source and call module servers
  # ---------------------------------------------------------------------------
  
  server_data_input(input, output, session, rv)
  server_classification(input, output, session, rv)
  server_esw_results(input, output, session, rv)
}