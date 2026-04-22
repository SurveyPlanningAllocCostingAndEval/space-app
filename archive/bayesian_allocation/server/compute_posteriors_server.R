# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Server module for computing posterior probabilities
#               after field results are uploaded and validated
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Interfaces between the update table and the posterior computation engine.
# =====================================================================

compute_posteriors_server <- function(rv, output_dir_reactive, input) {
  observeEvent(input$compute_post_btn, {
    message("Compute Posteriors button clicked")
    req(input$results_file, rv$day1_alloc, rv$df_pre_in)
    
    tryCatch({
      message("Starting posterior computation pipeline (standardized schema)...")
      
      # Output directory handling
      out_dir <- tryCatch({
        if (is.null(output_dir_reactive) || !is.function(output_dir_reactive)) NULL else output_dir_reactive()
      }, error = function(e) NULL)
      
      if (is.null(out_dir) || out_dir == "") {
        message("No output directory selected. Files will not be written automatically.")
      } else {
        message("Output directory resolved: ", out_dir)
      }
      
      # Step 1: Ingest field results
      message("Ingesting and validating field results...")
      clean <- ingest_results(
        results_df   = read_inputs(input$results_file$datapath),
        alloc_ref_df = rv$day1_alloc,
        output_dir   = NULL  # no file writing here
      )
      rv$clean_results <- clean
      
      # Step 2: Build update table
      message("Building update table (priors + field results)...")
      upd <- build_update_table(
        clean_df         = clean,
        preconstraint_df = rv$day1_alloc,
        output_dir       = NULL
      )
      rv$update_table <- upd
      
      # Step 3: Compute posteriors
      message("Computing posteriors (using standardized fields)...")
      post_list <- compute_posteriors(
        update_df      = upd,
        full_inputs_df = rv$df_pre_in,
        output_dir     = NULL
      )
      
      rv$posteriors <- post_list$posterior_df
      rv$merged_posteriors <- post_list$merged_df  # this holds updated_priors_full equivalent
      
      # Step 4: Prepare next-day inputs (optional in-session operation)
      message("Preparing next-day inputs (merged priors + updated posteriors)...")
      next_in <- prepare_next_inputs(
        original_inputs_df = rv$df_pre_in,
        posteriors_df      = rv$posteriors,
        output_dir         = NULL,
        drop_success_1     = TRUE
      )
      rv$day2_inputs <- next_in
      
      # Completion message
      message("Posterior computation complete. Results stored in memory:")
      message("   • rv$clean_results")
      message("   • rv$update_table")
      message("   • rv$posteriors (surveyed subset)")
      message("   • rv$merged_posteriors (full updated priors)")
      message("   • rv$day2_inputs (next allocation inputs)")
      
      showNotification(
        "Posterior computation complete. Updated priors stored in memory.",
        type = "message"
      )
      
    }, error = function(e) {
      message("Posterior computation failed: ", e$message)
      showNotification(paste("Error computing posteriors:", e$message), type = "error")
    })
  })
}
