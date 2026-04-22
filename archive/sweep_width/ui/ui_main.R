# =============================================================================
# ui_main.R
# Sweep Width Calibration App
# =============================================================================

ui <- fluidPage(
  
  # ---------------------------------------------------------------------------
  # Header
  # ---------------------------------------------------------------------------
  header_ui(),
  
  # ---------------------------------------------------------------------------
  # Main content — tabsetPanel matches Optimal Allocation app layout
  # ---------------------------------------------------------------------------
  tags$div(
    style = "padding: 15px;",
    
    tabsetPanel(
      id   = "main_tabs",
      type = "tabs",
      
      tab_data_input,
      tab_classification,
      tab_esw_results,
      tab_about
    ),
    
    # Footer
    tags$div(
      class = "app-footer",
      tags$p(
        "Sweep Width Calibration Module — part of the ",
        tags$span(class = "highlight", "SPACE"),
        " toolkit."
      )
    )
  )
)