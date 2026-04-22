# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Runs the initial Bayesian allocation workflow for Day 1
#               using iterative filtering and effort redistribution.
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Implements the iterative “filter and rerun” allocation loop.
#   - Ensures no negative allocations and logs dropped units.
# =====================================================================

# Assumes you've already source()'d:
#   source("scripts/00_setup.R")
#   source("scripts/01_functions.R")

run_allocation_from_inputs <- function(input_df,
                                       L,
                                       output_dir   = NULL,
                                       out_basename = "iteration1_allocations_preconstraint",
                                       write_xlsx   = TRUE,
                                       write_csv    = TRUE) {
  if (missing(input_df) || is.null(input_df)) {
    stop("input_df must be provided.")
  }
  if (missing(L) || is.null(L) || !is.finite(as.numeric(L))) {
    stop("Total effort L must be a finite numeric value.")
  }
  
  # Standardize & validate
  df_in <- input_df |> std_names() |> coerce_core_cols()
  required <- c("unit_id", "area", "probability", "sweep_width")
  check_required_cols(df_in, required)
  
  # Core computation (pure math; no I/O)
  df_out <- compute_preconstraint_columns(df_in, L = as.numeric(L))
  
  # Optional writing (interim only)
  out_dir <- resolve_output_dir(output_dir)
  if (!is.null(out_dir) && nzchar(out_basename)) {
    if (isTRUE(write_csv)) {
      readr::write_csv(df_out, file.path(out_dir, paste0(out_basename, ".csv")))
    }
    if (isTRUE(write_xlsx)) {
      openxlsx::write.xlsx(
        x = list(Allocations = df_out),
        file = file.path(out_dir, paste0(out_basename, ".xlsx")),
        overwrite = TRUE
      )
    }
  }
  
  df_out
}
