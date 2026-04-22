# =====================================================================
#  SPACE — Survey Planning, Allocation, Costing and Evaluation
#  Version: 1.0.0
#  Author: Steven Edwards, Centre of Geographic Sciences / NSCC
#  Description: Global setup — packages, helpers, module registry
#  License: MIT
# =====================================================================

# ============================================================
# Packages
# ============================================================

library(shiny)       # Core — both modules
library(shinyjs)     # Core — show/hide navigation

library(readr)       # Both — CSV I/O
library(DT)          # Allocation — interactive tables
library(readxl)      # Allocation — Excel reading
library(openxlsx)    # Allocation — Excel writing
library(zip)         # Allocation — ZIP download bundles
library(leaflet)     # Allocation — interactive maps
library(sf)          # Allocation — spatial data handling
library(dplyr)       # Allocation — data manipulation
library(purrr)       # Allocation — functional programming
library(rlang)       # Allocation — tidy evaluation

library(ggplot2)     # Sweep Width — plotting
library(ggforce)     # Sweep Width — geom_ellipse
library(minpack.lm)  # Sweep Width — Levenberg-Marquardt NLS

# patchwork is a soft dependency (Sweep Width multi-panel plots)
if (!requireNamespace("patchwork", quietly = TRUE)) {
  message(
    "Note: 'patchwork' package not found. Install it for multi-panel ",
    "ESW plots: install.packages('patchwork')"
  )
}

# ============================================================
# Global options
# ============================================================

options(
  scipen          = 999,
  stringsAsFactors = FALSE
)

# ============================================================
# Pure function scripts — Allocation module
# ============================================================

source(file.path("R", "allocation", "setup.R"))
source(file.path("R", "allocation", "read_inputs.R"))
source(file.path("R", "allocation", "functions.R"))
source(file.path("R", "allocation", "run_allocation.R"))
source(file.path("R", "allocation", "filter_and_rerun.R"))
source(file.path("R", "allocation", "ingest_results.R"))
source(file.path("R", "allocation", "read_results.R"))
source(file.path("R", "allocation", "build_update_table.R"))
source(file.path("R", "allocation", "compute_posteriors.R"))

# ============================================================
# Pure function scripts — Sweep Width module
# ============================================================

source(file.path("R", "sweep_width", "distance_error.R"))
source(file.path("R", "sweep_width", "classify_detections.R"))
source(file.path("R", "sweep_width", "fit_detection_function.R"))

# ============================================================
# Module UI and server files
# ============================================================

source(file.path("R", "mod_allocation_ui.R"))
source(file.path("R", "mod_allocation_server.R"))
source(file.path("R", "mod_sweep_width_ui.R"))
source(file.path("R", "mod_sweep_width_server.R"))

# ============================================================
# Shared theme helper
# ============================================================

#' Return the SPACE theme tags: CSS link + Bootstrap tooltip initializer.
#'
#' Include once in the top-level UI (app.R). Do not call from module UIs.
space_theme <- function() {
  tagList(
    includeCSS(file.path("www", "space_theme.css")),
    tags$head(
      tags$script(HTML(
        "$(function () { $('[data-toggle=\"tooltip\"]').tooltip({container: 'body'}); });"
      ))
    )
  )
}

# ============================================================
# Shared error modal (promoted from bayesian_allocation/scripts/00_setup.R)
# ============================================================

#' Display a user-friendly modal dialog for errors.
#'
#' @param session  The Shiny session object.
#' @param title    Modal title string (default "Error").
#' @param message  Error message string to display in a <pre> block.
show_error_modal <- function(session, title = "Error", message = "") {
  if (is.null(session)) return(invisible(FALSE))

  shiny::showModal(
    shiny::modalDialog(
      title     = title,
      easyClose = TRUE,
      footer    = shiny::modalButton("OK"),
      shiny::tags$p("Something went wrong. Details:"),
      shiny::tags$pre(
        style = "white-space: pre-wrap; font-size: 13px;",
        message
      )
    )
  )
  invisible(TRUE)
}

# ============================================================
# Module registry
# ============================================================

#' SPACE_MODULES — list of all registered modules.
#'
#' Each entry must contain:
#'   id          character  Unique module identifier (used for NS() and div IDs)
#'   title       character  Human-readable module name (shown on landing card)
#'   description character  One-sentence description (shown on landing card)
#'   ui_fn       function   Module UI function: ui_fn(id)
#'   server_fn   function   Module server function: server_fn(id)
#'
#' To add a new module: create R/mod_<name>_ui.R and R/mod_<name>_server.R,
#' source them above, then append an entry here. No other files need changing.

SPACE_MODULES <- list(

  list(
    id          = "allocation",
    title       = "Optimal Allocation Calculator",
    description = paste0(
      "Allocate survey effort across spatial units using Bayesian search theory, ",
      "then update posterior probabilities from field results."
    ),
    ui_fn       = allocationUI,
    server_fn   = allocationServer
  ),

  list(
    id          = "sweep_width",
    title       = "Sweep Width Estimator",
    description = paste0(
      "Estimate Effective Sweep Width from pedestrian survey calibration data ",
      "using a Gaussian detection function."
    ),
    ui_fn       = sweepWidthUI,
    server_fn   = sweepWidthServer
  )

)
