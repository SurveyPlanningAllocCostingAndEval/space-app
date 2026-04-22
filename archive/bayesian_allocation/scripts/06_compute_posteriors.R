# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Computes Bayesian posterior probabilities for each unit
#               based on observed field outcomes and prior parameters.
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Applies the chosen Bayesian update formula to generate posteriors.
#   - Outputs updated probabilities for use in next day allocation inputs.
#   - Uses non-normalized Bayesian formula
# =====================================================================

compute_posteriors <- function(update_df) {
  if (missing(update_df) || is.null(update_df)) {
    stop("update_df must be provided.")
  }
  
  # Standardize and normalize column names
  upd <- update_df |> std_names()
  names(upd) <- tolower(names(upd))
  
  rename_map <- c(
    "unitid"         = "unit_id",
    "polygon"        = "unit_id",
    "lwalkedtoday"   = "l_walked_today",
    "l_walked_today" = "l_walked_today",
    "metres_walked"  = "l_walked_today",
    "meters_walked"  = "l_walked_today",
    "distance_walked"= "l_walked_today",
    "success"        = "success",
    "prob"           = "probability",
    "p"              = "probability",
    "sweepwidth"     = "sweep_width",
    "sw"             = "sweep_width",
    "width"          = "sweep_width"
  )
  
  for (k in names(rename_map)) {
    if (k %in% names(upd)) names(upd)[names(upd) == k] <- rename_map[[k]]
  }
  
  required <- c("unit_id", "l_walked_today", "success",
                "probability", "sweep_width", "area")
  missing <- setdiff(required, names(upd))
  if (length(missing)) {
    stop(
      "Missing required column(s): ", paste(missing, collapse = ", "),
      "\nColumns present: ", paste(names(upd), collapse = ", ")
    )
  }
  
  # Coerce numeric safely
  upd <- upd |>
    dplyr::mutate(
      unit_id        = as.character(unit_id),
      l_walked_today = suppressWarnings(as.numeric(l_walked_today)),
      success        = suppressWarnings(as.integer(success)),
      probability    = suppressWarnings(as.numeric(probability)),
      sweep_width    = suppressWarnings(as.numeric(sweep_width)),
      area           = suppressWarnings(as.numeric(area))
    )
  
  # Posterior computation
  post_df <- upd |>
    dplyr::mutate(
      coverage_raw = (sweep_width * l_walked_today) / area,
      coverage     = dplyr::if_else(is.na(coverage_raw), 0, clamp01(coverage_raw)),
      post_prob    = dplyr::case_when(
        success == 1L  ~ 1,
        success == 0L  ~ probability * (1 - coverage),
        is.na(success) ~ probability,
        TRUE           ~ probability
      ),
      prior_prob = probability
    ) |>
    dplyr::select(unit_id, prior_prob, post_prob) |>
    dplyr::arrange(unit_id)
  
  # Return simplified posterior table
  post_df
}
