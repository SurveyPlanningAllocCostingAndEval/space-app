# =====================================================================
#  SPACE — Survey Planning, Allocation, Costing and Evaluation
#  Author: Steven Edwards, Centre of Geographic Sciences / NSCC
#  Description: Shiny-safe setup utilities and helper functions for
#               workspace configuration (no hard-coded paths or I/O)
#  License: MIT
# =====================================================================

# ============================================================
# Helpers for input validation and data cleaning
# ============================================================

# Normalize and standardize column names
std_names <- function(df) {
  if (is.null(df)) return(df)

  # Normalize case and trim whitespace
  names(df) <- tolower(trimws(names(df)))

  # Define mapping of known variants to standardized schema
  rename_map <- c(
    "polygon" = "unit_id",
    "polygons" = "unit_id",
    "unit id" = "unit_id",
    "area (m2)" = "area",
    "area_m2" = "area",
    "area (sq m)" = "area",
    "prior probability" = "probability",
    "probability" = "probability",
    "prior" = "probability",
    "sweep width" = "sweep_width",
    "sweepwidth" = "sweep_width",
    "sweep_width (m)" = "sweep_width",
    "visibility class" = "visibility",
    "visibility_class" = "visibility"
  )

  # Apply mapping wherever matches exist
  for (old_name in names(rename_map)) {
    if (old_name %in% names(df)) {
      names(df)[names(df) == old_name] <- rename_map[[old_name]]
    }
  }

  # Coerce ID column to character if present
  if ("unit_id" %in% names(df)) {
    df[["unit_id"]] <- as.character(df[["unit_id"]])
  }

  df
}

# ============================================================
# Coerce core numeric columns if present; leaves others untouched.
# ============================================================

coerce_core_cols <- function(df) {
  num_cols <- intersect(c("area", "probability", "sweep_width", "l"), names(df))
  for (nm in num_cols) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
  df
}

# ============================================================
# Simple required-column checker (useful in server & function layers)
# ============================================================
check_required_cols <- function(df, required) {
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

# ============================================================
# Value range helper (clamp function)
# ============================================================

clamp01 <- function(x) pmin(pmax(x, 0), 1)

# ============================================================
# Output directory handling
# ============================================================

# Resolve a user-chosen output directory. If NULL/empty, return NULL (no writing).
resolve_output_dir <- function(output_dir = NULL) {
  if (is.null(output_dir) || !nzchar(output_dir)) return(NULL)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(output_dir, winslash = "/", mustWork = FALSE)
}

# ============================================================
# Unified spatial/tabular file reader for Shiny fileInput data frames
# ============================================================

# Read an uploaded file (or set of files, for shapefiles) from a Shiny
# fileInput data frame and return a normalised list.
#
# Parameters:
#   file_df  — the data frame returned by Shiny's fileInput widget
#              (columns: name, datapath, size, type)
#
# Returns a named list:
#   $data        — plain data.frame (geometry stripped for spatial formats)
#   $sf          — sf object reprojected to EPSG:4326, or NULL for tabular formats
#   $is_spatial  — logical

read_uploaded_spatial <- function(file_df) {
  if (is.null(file_df) || nrow(file_df) == 0) {
    stop("No file provided to read_uploaded_spatial().")
  }

  # For shapefiles (multi-row), use the .shp row for extension detection
  shp_row <- which(grepl("\\.shp$", file_df$name, ignore.case = TRUE))
  if (length(shp_row) > 0) {
    ext      <- "shp"
    datapath <- file_df$datapath[shp_row[1]]
  } else {
    # Single-file upload — use the first row
    fname    <- file_df$name[1]
    ext      <- tolower(tools::file_ext(fname))
    datapath <- file_df$datapath[1]
  }

  # ---- Tabular formats ------------------------------------------------
  if (ext %in% c("csv", "txt")) {
    df <- readr::read_csv(datapath, show_col_types = FALSE)
    return(list(data = as.data.frame(df), sf = NULL, is_spatial = FALSE))
  }

  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(datapath)
    return(list(data = as.data.frame(df), sf = NULL, is_spatial = FALSE))
  }

  # ---- Spatial formats ------------------------------------------------
  if (ext == "gpkg") {
    layer_info <- sf::st_layers(datapath)
    poly_idx   <- grep("polygon", layer_info$geomtype, ignore.case = TRUE)
    layer_name <- if (length(poly_idx) > 0) layer_info$name[poly_idx[1]] else layer_info$name[1]
    sf_obj  <- sf::read_sf(datapath, layer = layer_name)
    sf_4326 <- sf::st_transform(sf_obj, 4326)
    data_df <- as.data.frame(sf::st_drop_geometry(sf_obj))
    return(list(data = data_df, sf = sf_4326, is_spatial = TRUE))
  }

  if (ext %in% c("geojson", "json", "shp")) {
    sf_obj  <- sf::read_sf(datapath)
    sf_4326 <- sf::st_transform(sf_obj, 4326)
    data_df <- as.data.frame(sf::st_drop_geometry(sf_obj))
    return(list(data = data_df, sf = sf_4326, is_spatial = TRUE))
  }

  # ---- Unsupported ----------------------------------------------------
  stop(
    "Unsupported file format: '.", ext, "'. ",
    "Supported formats: .csv, .txt, .xlsx, .xls, .gpkg, .geojson, .json, .shp"
  )
}
