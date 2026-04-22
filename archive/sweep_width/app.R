# =============================================================================
# app.R
# Sweep Width Calibration App
# Part of the SPACE (Survey Planning, Allocation, Costing and Evaluation)
# toolkit.
# =============================================================================

library(shiny)
library(readr)
library(ggplot2)
library(ggforce)
library(minpack.lm)

# patchwork is a soft dependency — used for multi-panel plots in ESW results
if (!requireNamespace("patchwork", quietly = TRUE)) {
  message("Note: 'patchwork' package not found. Install it for multi-panel ",
          "ESW plots: install.packages('patchwork')")
}

# ---------------------------------------------------------------------------
# Source functions
# ---------------------------------------------------------------------------
source("functions/distance_error.R")
source("functions/classify_detections.R")
source("functions/fit_detection_function.R")

# ---------------------------------------------------------------------------
# Source server modules
# ---------------------------------------------------------------------------
source("server/server_data_input.R")
source("server/server_classification.R")
source("server/server_esw_results.R")
source("server/server_main.R")

# ---------------------------------------------------------------------------
# Source UI modules
# ---------------------------------------------------------------------------
source("ui/header_ui.R")
source("ui/tab_data_input.R")
source("ui/tab_classification.R")
source("ui/tab_esw_results.R")
source("ui/tab_about.R")
source("ui/ui_main.R")

# ---------------------------------------------------------------------------
# Launch
# ---------------------------------------------------------------------------
shinyApp(ui = ui, server = server)