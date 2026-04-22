# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Filtering logic and allocation rerun routine used to
#               iteratively remove zero-allocation units and redistribute effort.
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Called by the main Day 1 allocation engine.
#   - Produces final allocation table and dropped-units log.
# =====================================================================

filter_and_rerun_allocation <- function(input_df,
                                        L,
                                        output_dir  = NULL,
                                        prefix      = "iteration",
                                        max_iters   = 10L,
                                        write_steps = TRUE) {
  if (missing(input_df) || is.null(input_df)) {
    stop("input_df must be provided.")
  }
  if (missing(L) || is.null(L) || !is.finite(as.numeric(L))) {
    stop("Total effort L must be a finite numeric value.")
  }
  
  # Standardize and validate input
  df_in_full <- input_df |> std_names() |> coerce_core_cols()
  req_cols   <- c("unit_id", "area", "probability", "sweep_width")
  check_required_cols(df_in_full, req_cols)
  
  out_dir <- resolve_output_dir(output_dir)
  
  iter          <- 1L
  dropped_log   <- dplyr::tibble()
  df_working_in <- df_in_full
  
  repeat {
    message(sprintf(">>> Iteration %d: %d units", iter, nrow(df_working_in)))
    
    # Core computation step
    df_alloc <- compute_preconstraint_columns(df_working_in, L = as.numeric(L))
    
    # Write interim per-iteration outputs
    if (!is.null(out_dir) && isTRUE(write_steps)) {
      readr::write_csv(
        df_alloc,
        file.path(out_dir, sprintf("%s%d_allocations_preconstraint.csv", prefix, iter))
      )
    }
    
    # Identify negative recommended transect lengths
    neg <- dplyr::filter(df_alloc, .data[["rec_L_of_transect"]] < 0)
    
    # Stop condition
    if (nrow(neg) == 0L) {
      message("No negatives; stopping.")
      break
    }
    
    # Log dropped units
    neg_aug <- neg |>
      dplyr::mutate(
        `_drop_reason` = paste0("Negative rec_L_of_transect (iteration ", iter, ")")
      ) |>
      dplyr::select(unit_id, `_drop_reason`, rec_L_of_transect, dplyr::everything())
    
    dropped_log <- dplyr::bind_rows(dropped_log, neg_aug)
    
    # Determine survivors
    survivors <- dplyr::filter(df_alloc, .data[["rec_L_of_transect"]] >= 0) |>
      dplyr::select(unit_id)
    
    if (nrow(survivors) == 0L) {
      stop("All units dropped after iteration ", iter, "; check inputs or constraints.")
    }
    
    df_working_in <- dplyr::semi_join(df_in_full, survivors, by = "unit_id")
    
    # Increment and termination guard
    iter <- iter + 1L
    if (iter > max_iters) {
      warning("Reached max_iters; stopping with remaining negatives possible.")
      break
    }
  }
  
  # Write only dropped-units log (optional interim info)
  if (!is.null(out_dir) && nrow(dropped_log) > 0L) {
    readr::write_csv(dropped_log, file.path(out_dir, "dropped_units_log.csv"))
  }
  
  # Return final allocation and drop log
  list(
    final_alloc = df_alloc,
    dropped_log = dropped_log
  )
}
