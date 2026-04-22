# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: UI layout for displaying update tables and posterior
#               probability results after field data is processed
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Shows both the update table and resulting posterior probabilities.
#   - Supports the transition from Day 1 results to subsequent allocation inputs.
# =====================================================================

update_posteriors_tab_ui <- function() {
  tabPanel(
    "Posteriors",
    br(),
    
    # Posterior Table Header
    strong("Posterior Probabilities:"),
    helpText("Displays the posterior probability for each unit after Bayesian updating. 
              Use the dropdown to filter for updated units only."),
    
    # Filter control
    fluidRow(
      column(
        width = 4,
        selectInput(
          "posterior_filter",
          label = NULL,
          choices = c("Show all units", "Show only updated units"),
          selected = "Show all units",
          width = "100%"
        )
      )
    ),
    
    # Posterior Table
    DTOutput("posteriors_tbl")
  )
}
