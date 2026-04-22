# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: UI layout for displaying cleaned daily field results
#               after ingestion and preprocessing
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Shows the standardized results table used for posterior updates.
#   - Provides a dedicated interface for reviewing field observations.
# =====================================================================

ingested_results_tab_ui <- function() {
  tabPanel(
    "Data",
    br(),
    
    tabsetPanel(
      tabPanel(
        "Inputs (Current Probabilities)",
        helpText("Displays the uploaded or selected input file containing current prior probabilities and related parameters."),
        DTOutput("inputs_tbl")
      ),
      tabPanel(
        "Field Results",
        helpText("Displays the uploaded field results file used for posterior computation."),
        DTOutput("results_tbl")
      )
    )
  )
}
