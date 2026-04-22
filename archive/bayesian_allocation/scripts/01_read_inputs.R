# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Reads and validates initial input tables for the
#               Bayesian allocation workflow
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Handles input formats (e.g., CSV/XLSX) and basic schema checks.
#   - Standardizes column names and types for downstream modules.
# =====================================================================

# Requires:
#   source("scripts/00_setup.R")  # std_names(), coerce_core_cols(), check_required_cols(), clamp01()

# =====================================================================
# read_inputs_file()
#   - Reads .csv/.txt/.xlsx/.xls
#   - Standardizes column names to:
#       unit_id, area, probability, sweep_width, visibility
#   - Validates schema and basic value constraints
#   - Returns a clean data frame for allocation workflow
# =====================================================================

read_inputs_file <- function(path, sheet = NULL) {
  if (is.null(path) || !nzchar(path)) {
    stop("No input file path provided.")
  }
  
  ext <- tolower(tools::file_ext(path))
  df_raw <- switch(
    ext,
    "xlsx"    = readxl::read_excel(path, sheet = sheet),
    "xls"     = readxl::read_excel(path, sheet = sheet),
    "csv"     = readr::read_csv(path, show_col_types = FALSE),
    "txt"     = readr::read_csv(path, show_col_types = FALSE),
    "gpkg"    = as.data.frame(sf::st_drop_geometry(sf::read_sf(path))),
    "geojson" = as.data.frame(sf::st_drop_geometry(sf::read_sf(path))),
    "json"    = as.data.frame(sf::st_drop_geometry(sf::read_sf(path))),
    "shp"     = as.data.frame(sf::st_drop_geometry(sf::read_sf(path))),
    stop("Unsupported file type: ", ext,
         ". Allowed: .csv, .txt, .xlsx, .xls, .gpkg, .geojson, .json, .shp")
  )
  
  # 1) Standardize names (lowercase + canonical mapping)
  df <- std_names(df_raw)
  
  # 2) Check required columns — visibility optional
  required <- c("unit_id", "area", "probability", "sweep_width")
  check_required_cols(df, required)
  
  # 3) Coerce numeric types safely
  df <- coerce_core_cols(df)
  
  # 4) Basic validations
  .bad <- list()
  
  # unit_id must be present, non-empty
  if (any(!nzchar(df$unit_id))) {
    .bad[["unit_id"]] <- "unit_id contains blank values."
  }
  
  # area must be > 0
  if (any(is.na(df$area) | df$area <= 0)) {
    .bad[["area"]] <- "area must be positive numeric for all rows."
  }
  
  # probability must be in [0,1]
  if (any(is.na(df$probability) | df$probability < 0 | df$probability > 1)) {
    .bad[["probability"]] <- "probability must be in [0,1] for all rows."
  }
  
  # sweep_width must be >= 0 (allow 0 = no detectability)
  if (any(is.na(df$sweep_width) | df$sweep_width < 0)) {
    .bad[["sweep_width"]] <- "sweep_width must be non-negative numeric for all rows."
  }
  
  if (length(.bad)) {
    msg <- paste(
      "Input validation failed:\n",
      paste(paste0(" - ", names(.bad), ": ", unlist(.bad)), collapse = "\n")
    )
    stop(msg)
  }
  
  # 5) Optional: visibility normalization (keep as-is if supplied)
  if ("visibility" %in% names(df)) {
    # strip leading/trailing spaces, keep user categories as-is
    df$visibility <- trimws(as.character(df$visibility))
  }
  
  # 6) De-duplicate unit_id if any duplicates exist (warn + keep first)
  if (anyDuplicated(df$unit_id) > 0) {
    dup_ids <- unique(df$unit_id[duplicated(df$unit_id)])
    warning("Duplicate unit_id values found: ",
            paste(dup_ids, collapse = ", "),
            ". Keeping the first occurrence for each duplicate.")
    df <- df |>
      dplyr::group_by(unit_id) |>
      dplyr::slice(1L) |>
      dplyr::ungroup()
  }
  
  # 7) Arrange for determinism
  df <- df |> dplyr::arrange(unit_id)
  
  # 8) Return ONLY standardized columns (keep visibility if present; pass-through any extras)
  #    We keep extras but ensure core columns are first and named correctly.
  core_order <- c("unit_id", "area", "probability", "sweep_width")
  col_order  <- c(core_order,
                  setdiff(names(df), c(core_order, "visibility")),
                  intersect("visibility", names(df)))
  df <- df[, col_order, drop = FALSE]
  
  df
}

# =====================================================================
# OPTIONAL: convenience wrapper used by server code
#   - Accepts a Shiny fileInput object (datapath + name)
#   - Returns standardized DF
# =====================================================================
read_inputs_from_upload <- function(file_input, sheet = NULL) {
  req(file_input)
  read_inputs_file(file_input$datapath, sheet = sheet)
}
