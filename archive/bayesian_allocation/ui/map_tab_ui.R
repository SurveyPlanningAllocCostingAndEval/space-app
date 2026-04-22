# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Map tab UI — displays uploaded survey polygons on an
#               interactive leaflet map with optional choropleth styling
#               when posterior probabilities are available
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Visible only when a spatial file (GeoPackage, GeoJSON, Shapefile)
#     has been uploaded and rv$is_spatial is TRUE.
#   - Basemap toggled via selectInput; choropleth applied by map_server.R.
# =====================================================================

map_tab_ui <- function() {
  tabPanel(
    "Map",
    div(
      style = "padding: 10px 15px;",

      # Shown when no spatial data is loaded
      conditionalPanel(
        condition = "output.is_spatial_flag == false",
        div(
          style = "margin-top: 30px; color: #666; font-size: 15px;",
          "Upload a GeoPackage, GeoJSON, or Shapefile to enable the map."
        )
      ),

      # Shown when spatial data is loaded
      conditionalPanel(
        condition = "output.is_spatial_flag == true",
        div(
          style = "margin-bottom: 8px;",
          selectInput(
            inputId  = "basemap_choice",
            label    = "Basemap",
            choices  = c("OpenStreetMap", "CartoDB Positron"),
            selected = "OpenStreetMap",
            width    = "250px"
          )
        ),
        leafletOutput("survey_map", height = "600px")
      )
    )
  )
}
