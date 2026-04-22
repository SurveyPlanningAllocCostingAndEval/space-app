# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Map server module — renders survey polygons on a leaflet
#               map, updates choropleth fill from posterior probabilities,
#               and handles basemap toggling
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - rv$sf_pre_in is an sf object already in EPSG:4326 (transformed by
#     read_uploaded_spatial in 00_setup.R). No further st_transform needed.
#   - rv$posteriors must contain a `unit_id` column and a `probability` column.
#   - st_make_valid() is applied before rendering to repair any invalid
#     geometries.
# =====================================================================

map_server <- function(rv, input, output, session) {

  # ------------------------------------------------------------------
  # Bridge rv$is_spatial (server-side reactive) to JS for conditionalPanel
  # ------------------------------------------------------------------
  output$is_spatial_flag <- reactive({
    isTRUE(rv$is_spatial)
  })
  outputOptions(output, "is_spatial_flag", suspendWhenHidden = FALSE)

  # ------------------------------------------------------------------
  # Helper: prepare sf object for leaflet rendering
  # ------------------------------------------------------------------
  prepare_sf <- function(sf_in) {
    sf_obj <- sf::st_make_valid(sf_in)
    sf_obj
  }

  # ------------------------------------------------------------------
  # Reactive map: rebuilds when rv$sf_pre_in changes
  # Includes polygons directly so there is no timing race with proxy
  # ------------------------------------------------------------------
  output$survey_map <- renderLeaflet({
    m <- leaflet() |>
      addTiles(group = "OpenStreetMap") |>
      addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") |>
      setView(lng = 0, lat = 20, zoom = 2)

    sf_data <- rv$sf_pre_in
    if (!is.null(sf_data)) {
      tryCatch({
        sf_obj <- prepare_sf(sf_data)
        bbox   <- sf::st_bbox(sf_obj)

        popup_text <- paste0(
          "<b>Unit ID:</b> ", sf_obj$unit_id, "<br>",
          "<b>Area:</b> ",    sf_obj$area
        )

        m <- m |>
          addPolygons(
            data        = sf_obj,
            fillColor   = "#4a7fb5",
            fillOpacity = 0.4,
            color       = "#1E3765",
            weight      = 1,
            popup       = popup_text
          ) |>
          fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
      }, error = function(e) {
        showNotification(
          paste("Map render error:", e$message),
          type = "error"
        )
      })
    }

    m
  })
  outputOptions(output, "survey_map", suspendWhenHidden = FALSE)

  # ------------------------------------------------------------------
  # Basemap toggle
  # ------------------------------------------------------------------
  observeEvent(input$basemap_choice, {
    proxy <- leafletProxy("survey_map")
    if (input$basemap_choice == "CartoDB Positron") {
      proxy |>
        clearTiles() |>
        addProviderTiles("CartoDB.Positron")
    } else {
      proxy |>
        clearTiles() |>
        addTiles()
    }
  }, ignoreInit = TRUE)

  # ------------------------------------------------------------------
  # Choropleth update when posteriors become available
  # ------------------------------------------------------------------
  observeEvent(rv$posteriors, {
    req(rv$posteriors, rv$sf_pre_in)
    tryCatch({
      post_df <- rv$posteriors[, c("unit_id", "probability")]
      sf_obj  <- prepare_sf(rv$sf_pre_in)

      sf_joined <- merge(sf_obj, post_df, by = "unit_id", all.x = TRUE)

      pal <- leaflet::colorNumeric(
        palette  = "YlOrRd",
        domain   = sf_joined$probability,
        na.color = "#cccccc"
      )

      popup_text <- paste0(
        "<b>Unit ID:</b> ",    sf_joined$unit_id, "<br>",
        "<b>Area:</b> ",       sf_joined$area,    "<br>",
        "<b>Probability:</b> ", round(sf_joined$probability, 4)
      )

      leafletProxy("survey_map") |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          data        = sf_joined,
          fillColor   = ~pal(probability),
          fillOpacity = 0.7,
          color       = "#555555",
          weight      = 1,
          popup       = popup_text
        ) |>
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = sf_joined$probability,
          title    = "Posterior<br>Probability",
          opacity  = 0.8
        )
    }, error = function(e) {
      showNotification(
        paste("Choropleth update error:", e$message),
        type = "error"
      )
    })
  }, ignoreNULL = TRUE)
}
