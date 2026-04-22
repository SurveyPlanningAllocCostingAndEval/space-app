# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Core mathematical functions for Bayesian allocation
#               (pure functions; no file I/O)
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Contains all statistical and mathematical helpers used in the
#     allocation workflow (likelihoods, priors, posteriors, filtering).
#   - Designed as pure functions for predictable behavior and reusability.
# =====================================================================

# Assumes 00_setup.R has already loaded required packages and helper functions

# ============================================================
# Canonical column names used throughout the math
# ============================================================

COL <- list(
  id          = "unit_id",
  area        = "area",
  prob        = "probability",
  sweep_m     = "sweep_width",
  L           = "L",               # not required in df; prefer passing L as a function arg
  prob_dens   = "prob_density",
  logprobdens = "log_prob_density",
  alogp       = "alogp",
  sum_alogp   = "sum_alogp",
  sum_prob    = "sum_probability",
  sum_area    = "sum_area",
  sum_alogp_A = "sum_alogp_over_area",
  alogp_n     = "alogp_n",
  phi_over_A  = "phi_over_area",
  rec_cov     = "rec_cov",
  constraint  = "constraint",
  rec_L       = "rec_L_of_transect"
)

# ============================================================
# Internal utilities
# ============================================================

.as_num <- function(x) suppressWarnings(as.numeric(x))

.require_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("Missing required column(s): ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

# ============================================================
# Individual steps (kept granular for testing/parity with Excel)
# ============================================================

calc_prob_dens <- function(df) {
  .require_cols(df, c(COL$prob, COL$area))
  df[[COL$prob_dens]] <- .as_num(df[[COL$prob]]) / .as_num(df[[COL$area]])
  df
}

calc_logprobdens <- function(df) {
  .require_cols(df, c(COL$prob_dens))
  x <- .as_num(df[[COL$prob_dens]])
  df[[COL$logprobdens]] <- log10(x)  # Excel-style base-10 log
  df
}

calc_alogp <- function(df) {
  .require_cols(df, c(COL$area, COL$logprobdens))
  df[[COL$alogp]] <- .as_num(df[[COL$area]]) * .as_num(df[[COL$logprobdens]])
  df
}

calc_scalar_sums <- function(df) {
  .require_cols(df, c(COL$alogp, COL$prob, COL$area))
  df[[COL$sum_alogp]] <- sum(.as_num(df[[COL$alogp]]), na.rm = TRUE)
  df[[COL$sum_prob]]  <- sum(.as_num(df[[COL$prob]]),  na.rm = TRUE)
  df[[COL$sum_area]]  <- sum(.as_num(df[[COL$area]]),  na.rm = TRUE)
  df
}

calc_sum_alogp_over_area <- function(df) {
  .require_cols(df, c(COL$sum_alogp, COL$sum_area))
  df[[COL$sum_alogp_A]] <- .as_num(df[[COL$sum_alogp]]) / .as_num(df[[COL$sum_area]])
  df
}

calc_alogp_n <- function(df) {
  .require_cols(df, c(COL$logprobdens, COL$sum_alogp_A))
  df[[COL$alogp_n]] <- .as_num(df[[COL$logprobdens]]) - .as_num(df[[COL$sum_alogp_A]])
  df
}

calc_phi_over_A <- function(df, L = NULL) {
  .require_cols(df, c(COL$sweep_m, COL$sum_area))
  sum_area <- unique(.as_num(df[[COL$sum_area]]))
  if (length(sum_area) != 1L || is.na(sum_area)) stop("Sum of area not computed or ambiguous.")
  
  # Prefer function argument L; fallback to a single-valued L column if present
  L_val <- if (!is.null(L)) .as_num(L) else {
    if (!(COL$L %in% names(df))) stop("Provide total effort L as an argument, or include a single-valued 'L' column.")
    u <- unique(.as_num(df[[COL$L]]))
    if (length(u) != 1L || is.na(u)) stop("Ambiguous 'L' column; expected one repeated value.")
    u
  }
  
  df[[COL$phi_over_A]] <- (L_val * .as_num(df[[COL$sweep_m]])) / sum_area
  df
}

calc_rec_cov <- function(df) {
  .require_cols(df, c(COL$alogp_n, COL$phi_over_A))
  df[[COL$rec_cov]] <- .as_num(df[[COL$alogp_n]]) + .as_num(df[[COL$phi_over_A]])
  df
}

calc_constraint <- function(df, L = NULL) {
  .require_cols(df, c(COL$sum_prob, COL$sum_area, COL$prob, COL$area))
  sum_prob <- unique(.as_num(df[[COL$sum_prob]]))
  sum_area <- unique(.as_num(df[[COL$sum_area]]))
  if (length(sum_prob) != 1L || is.na(sum_prob)) stop("Sum of probability not computed or ambiguous.")
  if (length(sum_area) != 1L || is.na(sum_area)) stop("Sum of area not computed or ambiguous.")
  
  L_val <- if (!is.null(L)) .as_num(L) else {
    if (!(COL$L %in% names(df))) stop("Provide total effort L as an argument, or include a single-valued 'L' column.")
    u <- unique(.as_num(df[[COL$L]]))
    if (length(u) != 1L || is.na(u)) stop("Ambiguous 'L' column.")
    u
  }
  
  num   <- (sum_prob - .as_num(df[[COL$prob]]))
  denom <- (sum_area - .as_num(df[[COL$area]]))
  df[[COL$constraint]] <- (num / denom) * 10^(-L_val / denom)
  df
}

calc_rec_L_of_transect <- function(df) {
  .require_cols(df, c(COL$rec_cov, COL$area, COL$sweep_m))
  df[[COL$rec_L]] <- (.as_num(df[[COL$rec_cov]]) * .as_num(df[[COL$area]])) / .as_num(df[[COL$sweep_m]])
  df
}

# ============================================================
# Public API
# ============================================================

# Computes all derived columns from an input df (no mutation of inputs; probability left unchanged)
# Required input columns (standardized names):
#   "unit_id", "area", "probability", "sweep_width"
# Provide total effort via L (numeric scalar).
compute_preconstraint_columns <- function(df, L = NULL) {
  # Standardize columns
  d <- df |> std_names()
  
  # Ensure required columns exist
  .require_cols(d, c(COL$id, COL$area, COL$prob, COL$sweep_m))
  
  # Coerce core numerics used downstream
  d[[COL$area]]  <- .as_num(d[[COL$area]])
  d[[COL$prob]]  <- .as_num(d[[COL$prob]])
  d[[COL$sweep_m]] <- .as_num(d[[COL$sweep_m]])
  
  # Sequential pipeline (order matters)
  d |>
    calc_prob_dens()       |>
    calc_logprobdens()     |>
    calc_alogp()           |>
    calc_scalar_sums()     |>
    calc_sum_alogp_over_area() |>
    calc_alogp_n()         |>
    calc_phi_over_A(L = L) |>
    calc_rec_cov()         |>
    calc_constraint(L = L) |>
    calc_rec_L_of_transect()
}
