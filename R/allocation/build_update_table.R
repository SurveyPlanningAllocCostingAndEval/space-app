# =====================================================================
#  SPACE — Survey Planning, Allocation, Costing and Evaluation
#  Author: Steven Edwards, Centre of Geographic Sciences / NSCC
#  Description: Generates the update table that merges cleaned field
#               results with allocation information for posterior updates.
#  License: MIT
# =====================================================================

build_update_table <- function(clean_df,
                               preconstraint_df,
                               output_dir = NULL,
                               out_name   = "update_table.csv") {
  if (missing(clean_df) || is.null(clean_df)) {
    stop("clean_df must be provided.")
  }
  if (missing(preconstraint_df) || is.null(preconstraint_df)) {
    stop("preconstraint_df must be provided.")
  }

  # Standardize both input data frames
  clean <- clean_df        |> std_names()
  pre   <- preconstraint_df |> std_names()

  # Case-insensitive column validation + renaming
  normalize_cols <- function(df, required_cols) {
    # Clean column names (remove spaces/punct, compress underscores)
    nm <- trimws(names(df))
    nm <- gsub("[^A-Za-z0-9_]", "_", nm)
    nm <- gsub("_+", "_", nm)
    lower <- tolower(nm)

    # Try to rename lower/variant matches to canonical names
    for (req in required_cols) {
      hit <- which(lower == tolower(req))
      if (length(hit) == 1) {
        nm[hit] <- req
      }
    }
    names(df) <- nm

    # Re-check for any missing required columns
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(paste(
        "Missing required column(s):",
        paste(missing_cols, collapse = ", "),
        "\nColumns present:",
        paste(names(df), collapse = ", ")
      ))
    }
    df
  }

  # Validate required columns (case-insensitive)
  req_clean <- c("unit_id", "L_walked_today", "success")
  req_pre   <- c("unit_id", "probability", "sweep_width", "area")

  clean <- normalize_cols(clean, req_clean)
  pre   <- normalize_cols(pre,   req_pre)

  # Coerce to proper types
  clean <- clean |>
    dplyr::mutate(
      unit_id        = as.character(unit_id),
      L_walked_today = suppressWarnings(as.numeric(L_walked_today)),
      success        = suppressWarnings(as.integer(success))
    )

  pre_lookup <- pre |>
    dplyr::transmute(
      unit_id      = as.character(unit_id),
      probability  = suppressWarnings(as.numeric(probability)),
      sweep_width  = suppressWarnings(as.numeric(sweep_width)),
      area         = suppressWarnings(as.numeric(area))
    ) |>
    dplyr::distinct(unit_id, .keep_all = TRUE)

  # Merge results onto priors
  update_tbl <- pre_lookup |>
    dplyr::left_join(
      clean |> dplyr::select(unit_id, L_walked_today, success),
      by = "unit_id"
    ) |>
    dplyr::transmute(
      unit_id,
      L_walked_today,
      success,
      probability,    # prior
      sweep_width,
      area
    ) |>
    dplyr::arrange(unit_id)

  # Optional write
  out_dir <- resolve_output_dir(output_dir)
  if (!is.null(out_dir)) {
    readr::write_csv(update_tbl, file.path(out_dir, out_name))
    message("Wrote update table: ", file.path(out_dir, out_name))
  }

  update_tbl
}
