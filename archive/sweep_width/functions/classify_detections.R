# =============================================================================
# classify_detections.R
# Sweep Width Calibration App
#
# Purpose:
#   Classifies each detection in the calibration records as a true positive
#   (Detected = TRUE) or false detection (Detected = FALSE) by testing whether
#   any seeded artifact from the master dataset falls within the elliptical
#   tolerance zone centred on the reported detection location.
#
# Approach:
#   Classification is performed on the combined dataset (left and right sides
#   together), with side (LorR) retained as an attribute. This follows the
#   approach described in Milwid (2017) and is consistent with Banning et al.
#   (2017), who match reported detections against the known master artifact
#   locations across the full transect before separating results by side for
#   ESW calculation.
#
#   For each row in `records`:
#     1. Retrieve the pre-computed elliptical tolerance axes (semi_major,
#        semi_minor) from compute_tolerance() in distance_error.R.
#     2. Test all artifacts in `master` using the ellipse membership formula:
#           ((Dist_master  - Dist_record)  / semi_major)^2 +
#           ((LDist_master - LDist_record) / semi_minor)^2  <= 1
#     3. If any master artifact satisfies this condition, the detection is
#        classified TRUE; otherwise FALSE.
#
#   Note on side handling: The master and records data both carry raw (positive)
#   Dist values and a LorR column. Matching is performed in the original
#   unsigned distance space, with LorR used as an additional filter — a
#   detection reported on the Right side is only matched against master
#   artifacts on the Right side, and vice versa. This prevents spurious
#   cross-side matches for transects where artifacts are densely seeded close
#   to the centreline.
#
# Dependencies:
#   - distance_error.R  (must be sourced before this file)
#     Provides: compute_tolerance(), point_in_ellipse()
#
# Usage:
#   # Step 1: compute tolerance zones
#   records_tol <- compute_tolerance(records, perp_scale = 0.2,
#                                    along_fixed = 1.0)
#   # Step 2: classify
#   records_classified <- classify_detections(records_tol, master)
# =============================================================================


#' Classify calibration detections as true or false positives
#'
#' @param records_tol A data frame of calibration detections that has already
#'                    been processed by `compute_tolerance()`. Must contain
#'                    columns: `Dist`, `LDist`, `LorR`, `semi_major`,
#'                    `semi_minor`.
#' @param master      A data frame of seeded artifact locations. Must contain
#'                    columns: `Dist`, `LDist`, `LorR`.
#'
#' @return `records_tol` with one additional logical column `Detected`:
#'         TRUE  = the detection matches a known seeded artifact within the
#'                 elliptical tolerance zone on the same side of the transect.
#'         FALSE = no matching artifact found; likely a false target or a
#'                 misidentified location.

classify_detections <- function(records_tol, master) {
  
  # --- Input validation ------------------------------------------------------
  
  if (!is.data.frame(records_tol)) {
    stop("'records_tol' must be a data frame.")
  }
  if (!is.data.frame(master)) {
    stop("'master' must be a data frame.")
  }
  
  required_records <- c("Dist", "LDist", "LorR", "semi_major", "semi_minor")
  missing_records  <- setdiff(required_records, names(records_tol))
  if (length(missing_records) > 0) {
    stop(paste(
      "'records_tol' is missing required column(s):",
      paste(missing_records, collapse = ", "),
      "\nEnsure compute_tolerance() has been called before classify_detections()."
    ))
  }
  
  required_master <- c("Dist", "LDist", "LorR")
  missing_master  <- setdiff(required_master, names(master))
  if (length(missing_master) > 0) {
    stop(paste("'master' is missing required column(s):",
               paste(missing_master, collapse = ", ")))
  }
  
  # Warn if semi_major contains NAs (e.g. from Dist = NA in records)
  if (any(is.na(records_tol$semi_major))) {
    warning("Some rows in 'records_tol' have NA semi_major values. ",
            "These detections will be classified as FALSE.")
  }
  
  # --- Classification --------------------------------------------------------
  
  records_tol$Detected <- mapply(
    FUN = function(r_dist, r_ldist, r_side, a, b) {
      
      # Guard against NA tolerance values
      if (is.na(a) || is.na(b)) return(FALSE)
      
      # Filter master to the same side of the transect as this detection
      master_side <- master[master$LorR == r_side, ]
      
      # If no master artifacts exist on this side, cannot be a true detection
      if (nrow(master_side) == 0) return(FALSE)
      
      # Test ellipse membership for all same-side master artifacts
      any(point_in_ellipse(
        px = master_side$Dist,
        py = master_side$LDist,
        cx = r_dist,
        cy = r_ldist,
        a  = a,
        b  = b
      ))
    },
    r_dist  = records_tol$Dist,
    r_ldist = records_tol$LDist,
    r_side  = records_tol$LorR,
    a       = records_tol$semi_major,
    b       = records_tol$semi_minor,
    SIMPLIFY = TRUE
  )
  
  return(records_tol)
}


# =============================================================================
# Helper: summarise classification results
#
# Returns a tidy summary data frame of detection counts broken down by side
# and detection outcome. Useful for displaying a results table in the UI.
# =============================================================================

#' Summarise true/false detection counts by transect side
#'
#' @param records_classified A data frame returned by `classify_detections()`,
#'                           containing columns `LorR` and `Detected`.
#'
#' @return A data frame with columns: Side, True_Detections,
#'         False_Detections, Total_Detections, Detection_Rate.

summarise_detections <- function(records_classified) {
  
  if (!all(c("LorR", "Detected") %in% names(records_classified))) {
    stop("'records_classified' must contain columns 'LorR' and 'Detected'.")
  }
  
  sides <- c("Left", "Right")
  
  summary_list <- lapply(sides, function(s) {
    sub  <- records_classified[records_classified$LorR == s, ]
    n    <- nrow(sub)
    tp   <- sum(sub$Detected, na.rm = TRUE)
    fp   <- n - tp
    rate <- if (n > 0) round(tp / n, 3) else NA
    data.frame(
      Side              = s,
      True_Detections   = tp,
      False_Detections  = fp,
      Total_Detections  = n,
      Detection_Rate    = rate,
      stringsAsFactors  = FALSE
    )
  })
  
  # Append combined row
  total_n  <- nrow(records_classified)
  total_tp <- sum(records_classified$Detected, na.rm = TRUE)
  summary_list[[3]] <- data.frame(
    Side             = "Combined",
    True_Detections  = total_tp,
    False_Detections = total_n - total_tp,
    Total_Detections = total_n,
    Detection_Rate   = if (total_n > 0) round(total_tp / total_n, 3) else NA,
    stringsAsFactors = FALSE
  )
  
  do.call(rbind, summary_list)
}