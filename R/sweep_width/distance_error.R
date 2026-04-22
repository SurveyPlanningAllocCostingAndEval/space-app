# =============================================================================
# distance_error.R
# Sweep Width Calibration App
#
# Purpose:
#   Computes elliptical tolerance zones for each detection in the calibration
#   records. These zones define the spatial envelope within which a reported
#   detection must fall relative to a known seeded artifact in order to be
#   classified as a true positive.
#
# Theoretical basis:
#   Loomis & Philbeck (2008) demonstrated that perceived egocentric distance
#   has a consistent linear relationship with actual distance, with a slope of
#   approximately 0.8 — meaning observers systematically underestimate distance
#   at a predictable, proportional rate. This implies that the acceptable error
#   in a surveyor's perpendicular distance estimate scales linearly with the
#   reported distance from the transect.
#
#   Critically, error is asymmetric:
#     - Perpendicular axis (away from transect): scales with reported distance,
#       since this is estimated by eye.
#     - Along-transect axis: remains small and approximately fixed, since
#       surveyors walk alongside a measuring tape and can report their position
#       along the transect with much greater accuracy.
#
#   This asymmetry justifies the use of ellipses rather than circles as
#   tolerance zones. The previous circular implementation was found to be
#   overly conservative, rejecting detections that were likely genuine.
#
# Parameters:
#   perp_scale  : Numeric. The scaling factor applied to the reported
#                 perpendicular distance (Dist) to compute the semi-major axis
#                 of the tolerance ellipse. Based on Loomis & Philbeck, a
#                 default of 0.2 (i.e., 20% of reported distance) is used,
#                 but this is exposed as a tunable parameter in the UI so that
#                 users can adjust it based on field conditions or prior
#                 calibration experiments. Banning et al. note that the
#                 appropriate tolerance may vary by field type and surveyor.
#
#   along_fixed : Numeric. The fixed semi-minor axis (in meters) applied along
#                 the transect direction. Reflects the relatively high accuracy
#                 of along-transect position estimates. Default is 1.0 m, which
#                 is consistent with the tolerances used in Banning et al.
#                 (2017) for distances up to 15 m.
#
# Output:
#   The function `compute_tolerance` returns the input data frame with two
#   additional columns appended:
#     - semi_major : the perpendicular (Dist-axis) semi-axis of the ellipse
#     - semi_minor : the along-transect (LDist-axis) semi-axis of the ellipse
#
# Usage:
#   records_with_tolerance <- compute_tolerance(
#     records,
#     perp_scale  = 0.2,
#     along_fixed = 1.0
#   )
#
# Dependencies: none (base R only)
# =============================================================================


#' Compute elliptical tolerance zones for calibration detections
#'
#' @param records     A data frame containing at minimum columns `Dist`
#'                    (perpendicular distance from transect, numeric) and
#'                    `LDist` (along-transect distance, numeric).
#' @param perp_scale  Numeric scalar. Scaling factor for the perpendicular
#'                    semi-axis. Defaults to 0.2 (20% of reported Dist),
#'                    following Loomis & Philbeck (2008).
#' @param along_fixed Numeric scalar. Fixed semi-minor axis length in meters
#'                    for the along-transect direction. Defaults to 1.0 m.
#'
#' @return The input data frame with two additional numeric columns:
#'         `semi_major` (perpendicular tolerance) and `semi_minor`
#'         (along-transect tolerance).
#'
#' @examples
#' records_with_tolerance <- compute_tolerance(records, perp_scale = 0.2,
#'                                             along_fixed = 1.0)

compute_tolerance <- function(records,
                              perp_scale  = 0.2,
                              along_fixed = 1.0) {
  
  # --- Input validation ------------------------------------------------------
  
  if (!is.data.frame(records)) {
    stop("'records' must be a data frame.")
  }
  
  required_cols <- c("Dist", "LDist")
  missing_cols  <- setdiff(required_cols, names(records))
  if (length(missing_cols) > 0) {
    stop(paste("'records' is missing required column(s):",
               paste(missing_cols, collapse = ", ")))
  }
  
  if (!is.numeric(records$Dist)) {
    stop("Column 'Dist' must be numeric.")
  }
  
  if (any(records$Dist < 0, na.rm = TRUE)) {
    stop("Column 'Dist' must contain non-negative values. ",
         "Ensure left-side distances have not been negated before calling ",
         "this function.")
  }
  
  if (!is.numeric(perp_scale) || length(perp_scale) != 1 ||
      perp_scale <= 0 || perp_scale >= 1) {
    stop("'perp_scale' must be a single numeric value between 0 and 1 ",
         "(exclusive). Typical values are in the range 0.1 to 0.3.")
  }
  
  if (!is.numeric(along_fixed) || length(along_fixed) != 1 ||
      along_fixed <= 0) {
    stop("'along_fixed' must be a single positive numeric value (in meters).")
  }
  
  # --- Compute tolerance axes ------------------------------------------------
  
  # Semi-major axis: perpendicular to transect, scales linearly with distance.
  # At Dist = 0 (artifact on the transect line) a minimum floor is applied so
  # that detections reported exactly on the transect are not given a zero-width
  # tolerance zone. The floor is set to along_fixed for consistency.
  records$semi_major <- pmax(perp_scale * records$Dist, along_fixed)
  
  # Semi-minor axis: along the transect, fixed regardless of distance.
  records$semi_minor <- along_fixed
  
  return(records)
}


# =============================================================================
# Helper: ellipse membership test
#
# Given a candidate point (px, py) and an ellipse centred at (cx, cy) with
# semi-axes (a, b) aligned with the coordinate axes, returns TRUE if the
# point lies within or on the ellipse boundary.
#
# This is used internally by classify_detections.R and is exported here so
# that both modules share a single, tested implementation.
#
# The standard ellipse membership formula is:
#   ((px - cx) / a)^2 + ((py - cy) / b)^2 <= 1
#
# where:
#   a = semi_major  (perpendicular / Dist axis)
#   b = semi_minor  (along-transect / LDist axis)
# =============================================================================

#' Test whether a point lies within an axis-aligned ellipse
#'
#' @param px Numeric. x-coordinate (Dist) of the candidate point.
#' @param py Numeric. y-coordinate (LDist) of the candidate point.
#' @param cx Numeric. x-coordinate (Dist) of the ellipse centre.
#' @param cy Numeric. y-coordinate (LDist) of the ellipse centre.
#' @param a  Numeric. Semi-major axis length (perpendicular / Dist direction).
#' @param b  Numeric. Semi-minor axis length (along-transect / LDist direction).
#'
#' @return Logical scalar. TRUE if (px, py) is inside or on the ellipse.

point_in_ellipse <- function(px, py, cx, cy, a, b) {
  ((px - cx) / a)^2 + ((py - cy) / b)^2 <= 1
}