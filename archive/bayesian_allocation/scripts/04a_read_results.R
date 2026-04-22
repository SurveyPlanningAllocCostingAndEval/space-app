# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Reads raw field results and prepares them for the
#               results ingestion and cleaning workflow
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Focused on safely loading results files.
#   - Ensures compatibility of result fields with the ingest_results module.
# =====================================================================

read_results_file <- function(path, sheet = NULL) {
  if (is.null(path) || !nzchar(path)) {
    stop("No results file path provided.")
  }
  
  ext <- tolower(tools::file_ext(path))
  df_raw <- switch(
    ext,
    "xlsx" = readxl::read_excel(path, sheet = sheet),
    "xls"  = readxl::read_excel(path, sheet = sheet),
    "csv"  = readr::read_csv(path, show_col_types = FALSE),
    "txt"  = readr::read_csv(path, show_col_types = FALSE),
    stop("Unsupported file type: ", ext)
  )
  
  # Clean and standardize column names
  names(df_raw) <- trimws(names(df_raw))
  names(df_raw) <- gsub("[^A-Za-z0-9_]", "_", names(df_raw))
  names(df_raw) <- gsub("_+", "_", names(df_raw))
  
  lower <- tolower(names(df_raw))
  names(df_raw) <- lower
  
  # Map to required canonical names
  rename_map <- c(
    "unitid" = "unit_id",
    "unit__id" = "unit_id",
    "lwalkedtoday" = "L_walked_today",
    "l_walked_today" = "L_walked_today",
    "length_walked" = "L_walked_today",
    "distance_walked" = "L_walked_today",
    "found" = "success",
    "detected" = "success",
    "result" = "success"
  )
  for (nm in names(rename_map)) {
    if (nm %in% names(df_raw)) {
      names(df_raw)[names(df_raw) == nm] <- rename_map[[nm]]
    }
  }
  
  # Validate required columns
  required <- c("unit_id", "L_walked_today", "success")
  missing_cols <- setdiff(required, names(df_raw))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required column(s):", paste(missing_cols, collapse = ", ")))
  }
  
  # Coerce numeric where needed
  df_raw$L_walked_today <- suppressWarnings(as.numeric(df_raw$L_walked_today))
  df_raw$success <- suppressWarnings(as.numeric(df_raw$success))
  df_raw$success[is.na(df_raw$success)] <- 0
  
  df_raw
}
