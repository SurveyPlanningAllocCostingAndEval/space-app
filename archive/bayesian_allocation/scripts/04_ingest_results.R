# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Cleans and formats field results for Bayesian updating,
#               ensuring schema consistency with initial allocations.
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Validates field results and links them back to allocated units.
#   - Standardizes success/failure coding for posterior computation.
# =====================================================================

ingest_results <- function(results_df,
                           alloc_ref_df = NULL,
                           output_dir   = NULL,
                           out_name     = NULL) {
  if (missing(results_df) || is.null(results_df)) {
    stop("results_df must be provided.")
  }
  
  # Standardize names first
  df <- results_df |> std_names()
  # Robust column normalization
  nm <- trimws(names(df))
  nm <- gsub("[^A-Za-z0-9_]", "_", nm)
  nm <- gsub("_+", "_", nm)
  lower <- tolower(nm)
  
  # Handle synonyms & variants
  rename_map <- list(
    "unitid"           = "unit_id",
    "unit__id"         = "unit_id",
    "l_walked_today"   = "L_walked_today",
    "lwalkedtoday"     = "L_walked_today",
    "l_walked"         = "L_walked_today",
    "length_walked"    = "L_walked_today",
    "metres_walked"    = "L_walked_today",
    "distance_walked"  = "L_walked_today",
    "survey_length"    = "L_walked_today",
    "found"            = "success",
    "detected"         = "success",
    "result"           = "success",
    "presence"         = "success"
  )
  
  for (key in names(rename_map)) {
    hit <- which(lower == tolower(key))
    if (length(hit) == 1 && !(rename_map[[key]] %in% names(df))) {
      nm[hit] <- rename_map[[key]]
    }
  }
  
  names(df) <- nm
  message("📋 Cleaned result columns: ", paste(names(df), collapse = ", "))
  # Ensure required columns exist now
  required_cols <- c("unit_id", "L_walked_today", "success")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      "\nColumns present: ", paste(names(df), collapse = ", ")
    )
  }
  
  # Coerce types
  clean <- df |>
    dplyr::mutate(
      unit_id        = as.character(unit_id),
      L_walked_today = suppressWarnings(as.numeric(L_walked_today)),
      success        = suppressWarnings(as.integer(success))
    )
  
  # Validate content
  if (any(is.na(clean$unit_id) | clean$unit_id == "")) {
    stop("Blank or NA unit_id values found in results.")
  }
  
  if (any(is.na(clean$L_walked_today))) {
    bad <- unique(clean$unit_id[is.na(clean$L_walked_today)])
    stop("Non-numeric or missing L_walked_today for unit(s): ",
         paste(bad, collapse = ", "))
  }
  
  if (any(clean$L_walked_today < 0)) {
    bad <- unique(clean$unit_id[clean$L_walked_today < 0])
    stop("Negative L_walked_today for unit(s): ", paste(bad, collapse = ", "))
  }
  
  if (any(is.na(clean$success)) || !all(clean$success %in% c(0L, 1L))) {
    bad <- unique(clean$unit_id[is.na(clean$success) | !(clean$success %in% c(0L, 1L))])
    stop("success must be 0 or 1 for unit(s): ", paste(bad, collapse = ", "))
  }
  
  if (any(duplicated(clean$unit_id))) {
    dups <- unique(clean$unit_id[duplicated(clean$unit_id)])
    stop("Duplicate unit_id rows in results: ", paste(dups, collapse = ", "),
         ". Each unit_id should appear at most once per day.")
  }
  
  # Optional: cross-check against allocation reference
  if (!is.null(alloc_ref_df)) {
    ref <- alloc_ref_df |> std_names()
    if ("unit_id" %in% names(ref)) {
      ref_ids <- unique(as.character(ref$unit_id))
      unknown <- setdiff(clean$unit_id, ref_ids)
      if (length(unknown)) {
        warning("Unknown unit_id(s) in results (not present in reference allocation): ",
                paste(unknown, collapse = ", "))
      }
    }
  }
  
  # Optional write to user-chosen folder
  out_dir <- resolve_output_dir(output_dir)
  if (!is.null(out_dir)) {
    fname <- if (is.null(out_name) || !nzchar(out_name)) {
      paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_results_clean.csv")
    } else {
      out_name
    }
    readr::write_csv(clean, file.path(out_dir, fname))
    message("Wrote cleaned results: ", file.path(out_dir, fname))
  }
  
  clean
}
