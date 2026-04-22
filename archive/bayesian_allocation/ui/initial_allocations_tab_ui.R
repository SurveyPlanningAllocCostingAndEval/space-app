# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: UI elements for displaying daily allocation outputs,
#               including final allocations and dropped-unit logs
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Presents results of the iterative allocation process.
#   - Used for reviewing initial polygons, effort assignments, and filtering.
# =====================================================================

initial_allocations_tab_ui <- function() {
  tabPanel(
    "Allocations",
    br(),
    
    tabsetPanel(
      tabPanel(
        "Recommended Allocations",
        helpText(HTML("<strong>Interpretation:</strong> Allocation values (<em>allocation</em>) are expressed in meters of walking effort (m/day) and represent the portion of Total Daily Effort assigned to each polygon for the current day.")),
helpText("Each row represents a survey unit (unit_id) with computed allocation parameters such as area, probability, sweep_width, visibility, and recommended transect length (allocation)."),
DTOutput("day1_alloc_tbl")
      ),
      tabPanel(
        "Dropped Units",
        helpText("Lists any units that were dropped during the iterative allocation process 
                  (e.g., negative recommended transect lengths)."),
        DTOutput("day1_dropped_tbl")
      )
    )
  )
}
