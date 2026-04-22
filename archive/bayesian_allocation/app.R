# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Main application integrating Bayesian allocation module and UI
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Part of the Survey Planning, Allocation, Costing & Evaluation (SPACE) toolkit.
# =====================================================================

# Required Packages
library(shiny)
library(DT)
library(readr)
library(readxl)
library(openxlsx)
library(zip)
library(leaflet)

# ============================================================
# Shared Output Helpers (for hosted app)
# ============================================================

# Create/return a temp output directory for each session
get_output_dir <- function() {
  dir <- file.path(tempdir(), "boa_outputs")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

# Generic writer for allocation/posterior results
write_alloc_xlsx <- function(final_alloc, dropped_log = NULL, params = NULL, path) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "final_alloc")
  openxlsx::writeData(wb, "final_alloc", final_alloc)
  if (!is.null(dropped_log)) {
    openxlsx::addWorksheet(wb, "dropped_log")
    openxlsx::writeData(wb, "dropped_log", dropped_log)
  }
  if (!is.null(params)) {
    openxlsx::addWorksheet(wb, "params")
    openxlsx::writeData(wb, "params", params)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

# ============================================================
# Directory setup
# ============================================================
APP_DIR     <- normalizePath(".", winslash = "/", mustWork = FALSE)
scripts_dir <- file.path(APP_DIR, "scripts")
ui_dir      <- file.path(APP_DIR, "ui")
server_dir  <- file.path(APP_DIR, "server")

# ---- Source backend scripts ----
source(file.path(scripts_dir, "00_setup.R"), local = TRUE)
source(file.path(scripts_dir, "01_read_inputs.R"), local = TRUE)
source(file.path(scripts_dir, "01_functions.R"), local = TRUE)
source(file.path(scripts_dir, "02_run_allocation.R"), local = TRUE)
source(file.path(scripts_dir, "03_filter_and_rerun.R"), local = TRUE)
source(file.path(scripts_dir, "04_ingest_results.R"), local = TRUE)
source(file.path(scripts_dir, "05_build_update_table.R"), local = TRUE)
source(file.path(scripts_dir, "06_compute_posteriors.R"), local = TRUE)

# ---- Source UI components ----
source(file.path(ui_dir, "header_ui.R"),                  local = TRUE)
source(file.path(ui_dir, "sidebar_ui.R"),                 local = TRUE)
source(file.path(ui_dir, "intro_tab_ui.R"),               local = TRUE)
source(file.path(ui_dir, "instructions_tab_ui.R"),        local = TRUE)
source(file.path(ui_dir, "initial_allocations_tab_ui.R"), local = TRUE)
source(file.path(ui_dir, "ingested_results_tab_ui.R"),    local = TRUE)
source(file.path(ui_dir, "update_posteriors_tab_ui.R"),   local = TRUE)
source(file.path(ui_dir, "map_tab_ui.R"),                 local = TRUE)
source(file.path(ui_dir, "main_tabs_ui.R"),               local = TRUE)

# ---- Source server helpers ----
source(file.path(server_dir, "upload_inputs_server.R"),       local = TRUE)
source(file.path(server_dir, "assign_sweep_widths_server.R"), local = TRUE)
source(file.path(server_dir, "run_allocation_server.R"),      local = TRUE)
source(file.path(server_dir, "posterior_workflow_server.R"),  local = TRUE)
source(file.path(server_dir, "outputs_server.R"),             local = TRUE)
source(file.path(server_dir, "upload_previous_allocations_server.R"), local = TRUE)
source(file.path(server_dir, "map_server.R"),                         local = TRUE)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  header_ui(),
  
  # ---- Global Styling ----
  tags$style(HTML("
    .instructions, .tab-content, .mainPanel { padding: 10px 15px; }
    .highlight { color: #1E3765; font-weight: 600; }
    .instructions ol { margin-left: 25px; margin-top: 10px; line-height: 1.6; font-size: 15px; }
    .instructions li { margin-bottom: 10px; }
    .instructions h3 { margin-top: 25px; margin-bottom: 10px; color: #1E3765; font-weight: 700; }
    .instructions p { font-size: 15px; color: #222; }
    .instructions ul { margin-left: 25px; font-size: 15px; }
    .dataTables_wrapper { margin-top: 10px; }
  ")),
  
  sidebarLayout(
    sidebarPanel(sidebar_ui(), width = 4),
    mainPanel(main_tabs_ui(), width = 8)
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ---- Reactive storage for all workflow data ----
  rv <- reactiveValues(
    # Allocation workflow
    df_pre_in     = NULL,
    day1_alloc    = NULL,
    day1_dropped  = NULL,
    day1_params   = NULL,

    # Spatial data (set by upload_inputs_server.R)
    sf_pre_in     = NULL,
    is_spatial    = FALSE,

    # Posterior update workflow
    posterior_inputs = NULL,
    clean_results    = NULL,
    update_table     = NULL,
    posteriors       = NULL,
    post_log         = NULL,

    # Full updated priors (used for saving updated probabilities)
    updated_priors   = NULL,

    # Track latest uploaded dataset (for unified Inputs tab)
    latest_inputs    = NULL,
    latest_source    = NULL
  )
  
  # ============================================================
  # Hosted app: Replace folder picker with tempdir()
  # ============================================================
  output_dir_reactive <- reactive({ get_output_dir() })
  
  # ============================================================
  # Track latest uploaded inputs (Posterior workflow only)
  # NOTE: input$pre_file upload is handled entirely by upload_inputs_server.R,
  # which sets rv$df_pre_in, rv$sf_pre_in, rv$is_spatial, and rv$latest_inputs.
  # rv$latest_source was removed — it was written but never read.
  # ============================================================
  observeEvent(input$posterior_inputs_file, {
    req(input$posterior_inputs_file)
    tryCatch({
      df <- read_inputs_from_upload(input$posterior_inputs_file)
      rv$posterior_inputs <- df
      rv$latest_inputs <- df
      showNotification("Inputs loaded and standardized successfully.", type = "message")
    }, error = function(e) {
      showNotification("Error loading inputs. Please check file format.", type = "error")
      show_error_modal(session, "Input Upload Error", e$message)
    })
  }, ignoreInit = TRUE)
  
  # ============================================================
  # Load remaining workflow server functions
  # ============================================================
  upload_inputs_server(rv, output_dir_reactive, input, session)
  assign_sweep_widths_server(rv, input, session)
  run_allocation_server(rv, output_dir_reactive, input, output, session)
  posterior_workflow_server(rv, output_dir_reactive, input, output, session)
  outputs_server(rv, input, output)
  map_server(rv, input, output, session)
  
  # ============================================================
  # Data tab preview of last loaded dataset
  # ============================================================
  output$inputs_tbl <- renderDT({
    req(rv$latest_inputs)
    datatable(rv$latest_inputs, options = list(pageLength = 15))
  })
  
  # ============================================================
  # Dynamic download buttons for Sample Data and Templates
  # ============================================================
  observe({
    output$sample_data_downloads <- renderUI({
      req(input$sample_data_select)
      if (input$sample_data_select == "Initial Inputs Sample") {
        tagList(
          downloadButton("download_inputs_sample_csv", "Download CSV", class = "btn-sm btn-primary"),
          downloadButton("download_inputs_sample_xlsx", "Download XLSX", class = "btn-sm btn-primary")
        )
      } else {
        tagList(
          downloadButton("download_field_results_sample_csv", "Download CSV", class = "btn-sm btn-primary"),
          downloadButton("download_field_results_sample_xlsx", "Download XLSX", class = "btn-sm btn-primary")
        )
      }
    })
    
    output$template_data_downloads <- renderUI({
      req(input$template_data_select)
      if (input$template_data_select == "Initial Inputs Template") {
        tagList(
          downloadButton("download_inputs_template_csv", "Download CSV", class = "btn-sm btn-success"),
          downloadButton("download_inputs_template_xlsx", "Download XLSX", class = "btn-sm btn-success")
        )
      } else {
        tagList(
          downloadButton("download_field_results_template_csv", "Download CSV", class = "btn-sm btn-success"),
          downloadButton("download_field_results_template_xlsx", "Download XLSX", class = "btn-sm btn-success")
        )
      }
    })
  })
  
  # ============================================================
  # File download handlers (for /www assets)
  # ============================================================
  output$download_inputs_sample_csv <- downloadHandler(
    filename = function() { "inputs_sample.csv" },
    content = function(file) { file.copy("www/inputs_sample.csv", file) }
  )
  output$download_inputs_sample_xlsx <- downloadHandler(
    filename = function() { "inputs_sample.xlsx" },
    content = function(file) { file.copy("www/inputs_sample.xlsx", file) }
  )
  output$download_field_results_sample_csv <- downloadHandler(
    filename = function() { "field_results_sample.csv" },
    content = function(file) { file.copy("www/field_results_sample.csv", file) }
  )
  output$download_field_results_sample_xlsx <- downloadHandler(
    filename = function() { "field_results_sample.xlsx" },
    content = function(file) { file.copy("www/field_results_sample.xlsx", file) }
  )
  output$download_inputs_template_csv <- downloadHandler(
    filename = function() { "inputs_template.csv" },
    content = function(file) { file.copy("www/inputs_template.csv", file) }
  )
  output$download_inputs_template_xlsx <- downloadHandler(
    filename = function() { "inputs_template.xlsx" },
    content = function(file) { file.copy("www/inputs_template.xlsx", file) }
  )
  output$download_field_results_template_csv <- downloadHandler(
    filename = function() { "field_results.csv" },
    content = function(file) { file.copy("www/field_results.csv", file) }
  )
  output$download_field_results_template_xlsx <- downloadHandler(
    filename = function() { "field_results.xlsx" },
    content = function(file) { file.copy("www/field_results.xlsx", file) }
  )
}

# ============================================================
# Launch the App
# ============================================================
shinyApp(ui, server)
