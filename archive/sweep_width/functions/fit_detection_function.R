# =============================================================================
# fit_detection_function.R
# Sweep Width Calibration App
#
# Purpose:
#   Fits a Gaussian detection function to classified calibration data and
#   computes Effective Sweep Width (ESW) for the left side, right side, or
#   combined total, depending on the user's selection.
#
# Detection function:
#   p(r) = b * exp(-k * r^2)
#
#   where:
#     r = perpendicular distance from the transect (metres)
#     b = detection probability at r = 0 (on the transect line); 0 <= b <= 1
#     k = decay constant (per metre^2); determines steepness of falloff
#
#   This is the standard exponential detection function described in
#   Banning et al. (2006, 2011, 2017) and used throughout the sweep width
#   literature following Koopman (1980).
#
# Sweep width calculation:
#   ESW is computed as the closed-form integral of p(r) from -Inf to +Inf,
#   which for the Gaussian function yields:
#
#     W = b * sqrt(pi / k)
#
#   This represents the full bilateral sweep width. For one-sided results
#   (Left or Right), W/2 is the half-width, but the function returns the
#   full W computed from the one-sided data as a bilateral equivalent,
#   consistent with the convention in Banning et al. (2017).
#
# Input preparation:
#   Before fitting, the classified detection data must be aggregated into
#   detection probabilities by distance bin. For each unique distance value
#   in the data, p(r) is estimated as:
#
#     p(r) = (number of TRUE detections at distance r) /
#            (number of artifacts seeded at distance r)
#
#   This aggregation is handled internally by `prepare_detection_probs()`.
#
# User toggle:
#   The `side` parameter controls which subset of the data is used:
#     "Left"  : only detections where LorR == "Left"
#     "Right" : only detections where LorR == "Right"
#     "Total" : all detections combined (left + right)
#
# Dependencies:
#   - minpack.lm (for nlsLM robust nonlinear least squares)
#
# Usage:
#   result <- fit_esw(records_classified, master, side = "Right")
#   result$W       # effective sweep width in metres
#   result$b       # fitted intercept parameter
#   result$k       # fitted decay parameter
#   result$fit_df  # data frame of distances and fitted probabilities for plot
# =============================================================================


#' Prepare detection probability data for curve fitting
#'
#' Aggregates classified detection records into observed detection
#' probabilities at each unique perpendicular distance, relative to the
#' number of seeded artifacts at that distance in the master data.
#'
#' @param records_classified Data frame from classify_detections(). Must
#'                           contain columns: Dist, LorR, Detected.
#' @param master             Data frame of seeded artifacts. Must contain
#'                           columns: Dist, LorR.
#' @param side               Character. One of "Left", "Right", or "Total".
#'
#' @return A data frame with columns:
#'         dist        - unique perpendicular distance values (master bins)
#'         n_seeded    - number of master artifacts at that distance
#'         n_detected  - number of true detections assigned to that bin
#'         prob        - detection probability (n_detected / n_seeded)

prepare_detection_probs <- function(records_classified, master, side, n_runs = 1) {
  
  # --- Validate side argument ------------------------------------------------
  valid_sides <- c("Left", "Right", "Total")
  if (!side %in% valid_sides) {
    stop(paste("'side' must be one of:", paste(valid_sides, collapse = ", ")))
  }
  
  # --- Subset by side --------------------------------------------------------
  if (side != "Total") {
    rec <- records_classified[records_classified$LorR == side, ]
    mas <- master[master$LorR == side, ]
  } else {
    rec <- records_classified
    mas <- master
  }
  
  if (nrow(rec) == 0) {
    stop(paste("No records found for side =", side))
  }
  if (nrow(mas) == 0) {
    stop(paste("No master artifacts found for side =", side))
  }
  
  # --- Aggregate detections by distance --------------------------------------
  # Use master distances as the canonical bins. Each true detection in the
  # records is assigned to the nearest master distance bin rather than
  # matched by rounded value. This is necessary because reported detection
  # distances carry realistic measurement error (scaling with true distance
  # per Loomis & Philbeck), so a detection of a 2.0 m artifact may be
  # reported at 1.83 m and would otherwise fall into the wrong bin.
  #
  # Only TRUE detections are assigned; FALSE detections (unmatched records)
  # are excluded from the probability calculation since they have no
  # corresponding master artifact to serve as the denominator.
  
  all_dists <- sort(unique(mas$Dist))
  
  # For each true detection, find the nearest master distance bin
  true_detections <- rec[rec$Detected == TRUE, ]
  
  if (nrow(true_detections) > 0) {
    nearest_bin <- all_dists[
      sapply(true_detections$Dist, function(d) which.min(abs(all_dists - d)))
    ]
    true_detections$dist_bin <- nearest_bin
  }
  
  prob_list <- lapply(all_dists, function(d) {
    n_seeded   <- sum(mas$Dist == d)
    n_seeded_effective <- n_seeded * n_runs
    n_detected <- if (nrow(true_detections) > 0) {
      sum(true_detections$dist_bin == d)
    } else {
      0L
    }
    # Cap n_detected at n_seeded_effective: each artifact can be detected once
    # per run, so the maximum possible detections is n_seeded * n_runs
    n_detected <- min(n_detected, n_seeded_effective)
    prob       <- ifelse(n_seeded_effective > 0, n_detected / n_seeded_effective, NA)
    data.frame(dist = d, n_seeded = n_seeded,
               n_seeded_effective = n_seeded_effective,
               n_detected = n_detected, prob = prob,
               stringsAsFactors = FALSE)
  })
  
  prob_df <- do.call(rbind, prob_list)
  
  # Remove rows where probability could not be computed
  prob_df <- prob_df[!is.na(prob_df$prob), ]
  
  if (nrow(prob_df) < 3) {
    stop(paste(
      "Insufficient data points to fit detection function for side =", side,
      "\nAt least 3 distance bins with observations are required.",
      "\nConsider using 'Total' or checking your input data."
    ))
  }
  
  return(prob_df)
}


#' Fit Gaussian detection function and compute Effective Sweep Width
#'
#' @param records_classified Data frame from classify_detections(). Must
#'                           contain columns: Dist, LorR, Detected.
#' @param master             Data frame of seeded artifacts. Must contain
#'                           columns: Dist, LorR.
#' @param side               Character. One of "Left", "Right", or "Total".
#'                           Controls which subset of the data is used for
#'                           fitting.
#' @param b_start            Numeric. Starting value for parameter b in the
#'                           nonlinear regression. Defaults to 0.5.
#' @param k_start            Numeric. Starting value for parameter k in the
#'                           nonlinear regression. Defaults to 0.05.
#'
#' @return A named list containing:
#'   \item{W}{Effective Sweep Width in metres (full bilateral equivalent)}
#'   \item{b}{Fitted detection probability at the transect (r = 0)}
#'   \item{b_se}{Standard error of b}
#'   \item{k}{Fitted decay constant}
#'   \item{k_se}{Standard error of k}
#'   \item{side}{The side used for fitting}
#'   \item{prob_df}{Aggregated detection probability data frame (for plotting)}
#'   \item{fit_df}{Data frame of fitted curve values over a fine distance grid}
#'   \item{converged}{Logical. Whether the nonlinear regression converged.}
#'   \item{message}{Character. Summary message for display in the UI.}

fit_esw <- function(records_classified,
                    master,
                    side    = "Right",
                    b_start = 0.5,
                    k_start = 0.05,
                    n_runs  = 1) {
  
  # --- Validate inputs -------------------------------------------------------
  required_rec <- c("Dist", "LorR", "Detected")
  missing_rec  <- setdiff(required_rec, names(records_classified))
  if (length(missing_rec) > 0) {
    stop(paste("'records_classified' is missing columns:",
               paste(missing_rec, collapse = ", ")))
  }
  
  required_mas <- c("Dist", "LorR")
  missing_mas  <- setdiff(required_mas, names(master))
  if (length(missing_mas) > 0) {
    stop(paste("'master' is missing columns:",
               paste(missing_mas, collapse = ", ")))
  }
  
  if (!requireNamespace("minpack.lm", quietly = TRUE)) {
    stop("Package 'minpack.lm' is required. ",
         "Install it with: install.packages('minpack.lm')")
  }
  
  # --- Prepare probability data ----------------------------------------------
  prob_df <- tryCatch(
    prepare_detection_probs(records_classified, master, side, n_runs = n_runs),
    error = function(e) {
      stop(paste("Error preparing detection probabilities:", e$message))
    }
  )
  
  # --- Fit detection function ------------------------------------------------
  fit <- tryCatch(
    minpack.lm::nlsLM(
      prob ~ b * exp(-k * dist^2),
      data    = prob_df,
      start   = list(b = b_start, k = k_start),
      lower   = c(b = 0,    k = 1e-6),
      upper   = c(b = 1,    k = Inf),
      control = minpack.lm::nls.lm.control(maxiter = 1000)
    ),
    error = function(e) {
      stop(paste(
        "Nonlinear regression failed to converge for side =", side, ".\n",
        "Try adjusting starting values or checking data quality.\n",
        "Original error:", e$message
      ))
    }
  )
  
  coefs  <- coef(fit)
  ses    <- sqrt(diag(vcov(fit)))
  b_hat  <- coefs["b"]
  k_hat  <- coefs["k"]
  b_se   <- ses["b"]
  k_se   <- ses["k"]
  
  # --- Compute ESW -----------------------------------------------------------
  # W = b * sqrt(pi / k)  [full bilateral sweep width in metres]
  W <- b_hat * sqrt(pi / k_hat)
  
  # --- Generate fitted curve for plotting ------------------------------------
  dist_range <- seq(0, max(prob_df$dist) + 2, length.out = 300)
  fit_df <- data.frame(
    dist   = dist_range,
    fitted = b_hat * exp(-k_hat * dist_range^2)
  )
  
  # --- Compose summary message -----------------------------------------------
  msg <- paste0(
    "Effective Sweep Width (", side, "): ", round(W, 2), " m\n",
    "  b = ", round(b_hat, 4), " (SE = ", round(b_se, 4), ")\n",
    "  k = ", round(k_hat, 4), " (SE = ", round(k_se, 4), ")\n",
    "  n distance bins used: ", nrow(prob_df), "\n",
    "  n_runs: ", n_runs
  )
  
  # --- Return results --------------------------------------------------------
  list(
    W         = W,
    b         = b_hat,
    b_se      = b_se,
    k         = k_hat,
    k_se      = k_se,
    side      = side,
    n_runs    = n_runs,
    prob_df   = prob_df,
    fit_df    = fit_df,
    converged = TRUE,
    message   = msg
  )
}


#' Run ESW fitting for all requested sides
#'
#' Convenience wrapper that calls fit_esw() for one or more sides and
#' returns a named list of results. Designed for use in the Shiny server
#' where the user selects which sides to compute via a checkbox group.
#'
#' @param records_classified Data frame from classify_detections().
#' @param master             Master artifact data frame.
#' @param sides              Character vector. Any combination of
#'                           "Left", "Right", "Total".
#' @param b_start            Starting value for b. Defaults to 0.5.
#' @param k_start            Starting value for k. Defaults to 0.05.
#'
#' @return A named list where each element corresponds to a requested side
#'         and contains the output of fit_esw() for that side.

fit_esw_multi <- function(records_classified,
                          master,
                          sides   = c("Left", "Right", "Total"),
                          b_start = 0.5,
                          k_start = 0.05,
                          n_runs  = 1) {
  
  valid_sides <- c("Left", "Right", "Total")
  invalid     <- setdiff(sides, valid_sides)
  if (length(invalid) > 0) {
    stop(paste("Invalid side(s):", paste(invalid, collapse = ", "),
               "\nMust be one or more of: Left, Right, Total"))
  }
  
  results <- lapply(sides, function(s) {
    tryCatch(
      fit_esw(records_classified, master, side = s,
              b_start = b_start, k_start = k_start, n_runs = n_runs),
      error = function(e) {
        list(
          W         = NA,
          b         = NA,
          b_se      = NA,
          k         = NA,
          k_se      = NA,
          side      = s,
          prob_df   = NULL,
          fit_df    = NULL,
          converged = FALSE,
          message   = paste("Could not compute ESW for side =", s,
                            ":", e$message)
        )
      }
    )
  })
  
  names(results) <- sides
  return(results)
}