# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Defines the main tabset structure for navigating the
#               SPACE workflow, integrating all major UI components
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Serves as the central navigation hub for the application.
#   - Connects the Intro, Instructions, Allocations, Results, and Update tabs.
# =====================================================================

main_tabs_ui <- function() {
  tabsetPanel(
    id = "mainTabs",
    selected = "Introduction",
    
    # General Information Tabs
    intro_tab_ui(),               # Overview of the module
    instructions_tab_ui(),        # Step-by-step guidance for users
    
    # Data and Allocation Workflow Tabs
    ingested_results_tab_ui(),    # "Data" - displays uploaded input and field result tables
    initial_allocations_tab_ui(), # "Allocations" - results from Generate Allocations step
    
    # Posterior Update Workflow Tabs
    update_posteriors_tab_ui(),   # Posterior computation results (simplified summary table)

    # Spatial Map Tab
    map_tab_ui()                  # Interactive leaflet map (enabled when spatial data loaded)
  )
}
