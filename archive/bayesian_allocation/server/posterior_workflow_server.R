# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Integrated server workflow for uploading results,
#               building the update table, and computing posteriors
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Wraps ingestion, update-table generation, and posterior steps into
#     a structured, user-friendly workflow.
# =====================================================================

posterior_workflow_server <- function(rv, output_dir_reactive, input, output, session)
{
  library(readr)
  library(readxl)
  library(dplyr)
  library(openxlsx)
  library(zip)
  
  # Helper: Read any input file type
  .read_any <- function(datapath, name) {
    ext <- tolower(tools::file_ext(name))
    switch(
      ext,
      "xlsx" = readxl::read_excel(datapath),
      "xls"  = readxl::read_excel(datapath),
      "csv"  = readr::read_csv(datapath, show_col_types = FALSE),
      "txt"  = readr::read_csv(datapath, show_col_types = FALSE),
      stop("Unsupported file type: ", ext)
    )
  }
  
  # Helper: Merge priors and results into update table
  .build_update_table <- function(priors_df, results_df) {
    pri <- std_names(priors_df)
    res <- std_names(results_df)
    
    names(pri) <- tolower(names(pri))
    names(res) <- tolower(names(res))
    
    rename_map <- list(
      "unit_id"        = c("unitid", "polygon", "polygons", "id", "unit"),
      "l_walked_today" = c("lwalkedtoday", "l_walked_today", "metres_walked",
                           "meters_walked", "distance_walked", "transect_length"),
      "success"        = c("success", "found", "detected", "result", "presence")
    )
    for (target in names(rename_map)) {
      for (alias in rename_map[[target]]) {
        if (alias %in% names(res)) names(res)[names(res) == alias] <- target
      }
    }
    
    req_priors  <- c("unit_id", "probability", "sweep_width", "area")
    req_results <- c("unit_id", "l_walked_today", "success")
    
    if (any(!req_priors %in% names(pri)))
      stop("Missing required priors columns: ", paste(setdiff(req_priors, names(pri)), collapse = ", "))
    if (any(!req_results %in% names(res)))
      stop("Missing required results columns: ", paste(setdiff(req_results, names(res)), collapse = ", "))
    
    update_tbl <- pri |>
      dplyr::left_join(res, by = "unit_id") |>
      dplyr::select(unit_id, l_walked_today, success, probability, sweep_width, area) |>
      dplyr::arrange(unit_id)
    
    update_tbl
  }
  
  # Helper: Apply posteriors back onto full priors table
  .apply_posteriors_to_priors <- function(priors_df, posterior_summary) {
    pri_std <- std_names(priors_df)
    names(pri_std) <- tolower(names(pri_std))
    
    post_tbl <- posterior_summary |>
      dplyr::transmute(unit_id = as.character(unit_id),
                       post_prob = as.numeric(post_prob))
    
    updated <- pri_std |>
      dplyr::mutate(unit_id = as.character(unit_id),
                    probability = suppressWarnings(as.numeric(probability))) |>
      dplyr::left_join(post_tbl, by = "unit_id") |>
      dplyr::mutate(probability = dplyr::coalesce(post_prob, probability)) |>
      dplyr::select(-post_prob)
    
    core <- c("unit_id", "area", "probability", "sweep_width", "visibility")
    ordered <- c(intersect(core, names(updated)),
                 setdiff(names(updated), core))
    updated[, ordered, drop = FALSE]
  }
  
  # ============================================================
  # Compute Posteriors
  # ============================================================
  observeEvent(input$compute_post_btn, {
    tryCatch({
      message("Computing posterior probabilities...")
      req(input$posterior_inputs_file, input$results_file)
      
      priors_df  <- .read_any(input$posterior_inputs_file$datapath, input$posterior_inputs_file$name)
      results_df <- .read_any(input$results_file$datapath,            input$results_file$name)
      
      rv$posterior_inputs <- priors_df
      
      update_tbl <- .build_update_table(priors_df, results_df)
      rv$update_table <- update_tbl
      
      post_summary <- compute_posteriors(update_tbl)
      
      surveyed_units <- unique(update_tbl$unit_id[!is.na(update_tbl$l_walked_today)])
      post_summary <- post_summary |>
        dplyr::mutate(updated = unit_id %in% surveyed_units)
      
      rv$posteriors <- post_summary
      rv$updated_priors <- .apply_posteriors_to_priors(priors_df, post_summary)
      
      showNotification("✅ Posterior probabilities computed successfully.", type = "message")
      updateTabsetPanel(session, "mainTabs", selected = "Posteriors")
      message("✅ Posterior computation complete.")
    }, error = function(e) {
      showNotification(paste("❌ Error computing posteriors:", e$message), type = "error")
      show_error_modal(session, "Posterior Update Error", e$message)
      message("❌ Error computing posteriors: ", e$message)
    })
  })
  
  # ============================================================
  # Single Download: Updated Priors / Posteriors (CSV)
  # ============================================================
  output$dl_post_csv <- downloadHandler(
    filename = function() paste0("posteriors_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$updated_priors)
      readr::write_csv(rv$updated_priors, file)
    }
  )
}
