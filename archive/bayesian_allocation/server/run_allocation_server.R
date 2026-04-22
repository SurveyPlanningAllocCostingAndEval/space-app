# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Server module for running daily optimal
#               allocations based on user-specified total effort (L)
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Executes the iterative filtering and rerun process for daily runs.
#   - Produces final allocation tables and dropped-unit logs.
# =====================================================================

run_allocation_server <- function(rv, output_dir_reactive, input, output, session) {
  
  # ============================================================
  # Generate Allocations
  # ============================================================
  observeEvent(input$run_day1, {
    req(rv$df_pre_in, input$total_effort)
    L_val <- as.numeric(input$total_effort)
    
    validate(
      need(is.finite(L_val) && L_val >= 0,
           "Please enter a valid non-negative total effort (L).")
    )
    
    tryCatch({
      message("Generate Allocations button clicked")
      
      df_check <- rv$df_pre_in
      required <- c("unit_id", "area", "probability", "sweep_width")
      missing  <- setdiff(required, names(df_check))
      if (length(missing) > 0)
        stop("Input data is missing required column(s): ",
             paste(missing, collapse = ", "))
      
      # Run allocation loop
      res <- filter_and_rerun_allocation(
        input_df    = df_check,
        L           = L_val,
        output_dir  = output_dir_reactive(), # tempdir() on hosted app
        prefix      = "iteration",
        max_iters   = 10,
        write_steps = TRUE
      )
      
      rv$day1_alloc   <- res$final_alloc
      rv$day1_dropped <- res$dropped_log
      rv$day1_params  <- res$params %||% NULL
      
      showNotification(
        "Allocations successfully generated and stored in memory.",
        type = "message"
      )
      updateTabsetPanel(session, "mainTabs", selected = "Allocations")
      message("Allocation completed successfully — results stored in rv$day1_alloc and rv$day1_dropped.")
      
    }, error = function(e) {
      showNotification(paste("Error generating allocations:", e$message), type = "error")
      show_error_modal(session, "Allocation Error", e$message)
      message("Error generating allocations: ", e$message)
    })
  })
  
  
  # ============================================================
  # Hosted-safe Downloads instead of Save Dialog
  # ============================================================
  
  # Download single Excel workbook
  output$dl_alloc_xlsx <- downloadHandler(
    filename = function() paste0("allocations_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(rv$day1_alloc)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "allocations")
      openxlsx::writeData(wb, "allocations", rv$day1_alloc)
      if (!is.null(rv$day1_dropped)) {
        openxlsx::addWorksheet(wb, "dropped_log")
        openxlsx::writeData(wb, "dropped_log", rv$day1_dropped)
      }
      if (!is.null(rv$day1_params)) {
        openxlsx::addWorksheet(wb, "params")
        openxlsx::writeData(wb, "params", rv$day1_params)
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Download full bundle (zip)
  output$dl_alloc_zip <- downloadHandler(
    filename = function() paste0("allocation_bundle_", Sys.Date(), ".zip"),
    content  = function(file) {
      out_dir <- output_dir_reactive()
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      # write the individual CSVs into temp directory
      if (!is.null(rv$day1_alloc))
        readr::write_csv(rv$day1_alloc, file.path(out_dir, "allocations.csv"))
      if (!is.null(rv$day1_dropped))
        readr::write_csv(rv$day1_dropped, file.path(out_dir, "dropped_log.csv"))
      if (!is.null(rv$day1_params))
        readr::write_csv(rv$day1_params,  file.path(out_dir, "params.csv"))
      
      files <- list.files(out_dir, full.names = TRUE)
      zip::zipr(file, files)
    }
  )
}
