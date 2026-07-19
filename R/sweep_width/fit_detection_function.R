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
#' @param se_method          Character. One of "Delta" or "Bootstrap".
#'                           Controls how W_se/W_ci_low/W_ci_high are derived.
#'                           "Delta" (default) uses a first-order Taylor
#'                           expansion around the fitted (b, k). "Bootstrap"
#'                           uses nonparametric case resampling (see
#'                           bootstrap_esw_ci()); slower, but does not rely
#'                           on asymptotic normality.
#' @param n_boot             Integer. Number of bootstrap resamples, used
#'                           only when se_method = "Bootstrap". Defaults to
#'                           1000.
#'
#' @return A named list containing:
#'   \item{W}{Effective Sweep Width in metres (full bilateral equivalent)}
#'   \item{W_se}{Standard error of W, via the selected se_method}
#'   \item{W_ci_low}{Lower bound of the 95% confidence interval for W}
#'   \item{W_ci_high}{Upper bound of the 95% confidence interval for W}
#'   \item{se_method}{Character. The method used: "Delta" or "Bootstrap"}
#'   \item{n_boot_success}{Bootstrap only: number of resamples that converged}
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
                    side       = "Right",
                    b_start    = 0.5,
                    k_start    = 0.05,
                    n_runs     = 1,
                    se_method  = "Delta",
                    n_boot     = 1000) {

  valid_se_methods <- c("Delta", "Bootstrap")
  if (!se_method %in% valid_se_methods) {
    stop(paste("'se_method' must be one of:", paste(valid_se_methods, collapse = ", ")))
  }
  
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
  vc     <- vcov(fit)
  ses    <- sqrt(diag(vc))
  b_hat  <- coefs["b"]
  k_hat  <- coefs["k"]
  b_se   <- ses["b"]
  k_se   <- ses["k"]

  # --- Compute ESW -----------------------------------------------------------
  # W = b * sqrt(pi / k)  [full bilateral sweep width in metres]
  W <- b_hat * sqrt(pi / k_hat)

  # --- SE and 95% CI for W -----------------------------------------------------
  n_boot_success <- NA_integer_
  boot_median    <- NA_real_
  skew_flag      <- NA
  boot_b         <- NULL
  boot_k         <- NULL

  if (se_method == "Delta") {
    # W is a nonlinear function of (b, k), so its variance is approximated via
    # a first-order Taylor expansion using the fitted covariance matrix of
    # (b, k):
    #   dW/db = sqrt(pi / k)
    #   dW/dk = -b * sqrt(pi) / (2 * k^1.5)
    #   Var(W) ~= dW^T %*% vcov(b, k) %*% dW
    # The b-k covariance term (off-diagonal of vc) is included, not just the
    # individual variances of b and k.
    dW_db <- sqrt(pi / k_hat)
    dW_dk <- -b_hat * sqrt(pi) / (2 * k_hat^1.5)
    grad  <- c(dW_db, dW_dk)

    W_var <- as.numeric(t(grad) %*% vc[c("b", "k"), c("b", "k")] %*% grad)
    W_se  <- if (is.finite(W_var) && W_var >= 0) sqrt(W_var) else NA_real_

    df_resid <- tryCatch(summary(fit)$df[2], error = function(e) NA_integer_)
    if (is.null(df_resid) || is.na(df_resid) || df_resid <= 0) {
      t_crit <- qnorm(0.975)
    } else {
      t_crit <- qt(0.975, df = df_resid)
    }

    if (!is.na(W_se)) {
      W_ci_low  <- W - t_crit * W_se
      W_ci_high <- W + t_crit * W_se
    } else {
      W_ci_low  <- NA_real_
      W_ci_high <- NA_real_
    }

  } else {
    # Bootstrap: nonparametric case resampling of records_classified and
    # master, refitting the full pipeline on each resample. See
    # bootstrap_esw_ci() for details.
    boot <- bootstrap_esw_ci(
      records_classified = records_classified,
      master              = master,
      side                = side,
      b_start             = b_start,
      k_start             = k_start,
      n_runs              = n_runs,
      n_boot              = n_boot,
      point_W             = W
    )
    W_se           <- boot$W_se
    W_ci_low       <- boot$W_ci_low
    W_ci_high      <- boot$W_ci_high
    n_boot_success <- boot$n_boot_success
    boot_median    <- boot$boot_median
    skew_flag      <- boot$skew_flag
    boot_b         <- boot$boot_b
    boot_k         <- boot$boot_k
  }

  # --- Generate fitted curve for plotting ------------------------------------
  dist_range <- seq(0, max(prob_df$dist) + 2, length.out = 300)
  fitted_curve <- b_hat * exp(-k_hat * dist_range^2)

  # --- Pointwise +/-1 SE envelope for the fitted curve ------------------------
  # p(r) = b * exp(-k * r^2) is a nonlinear function of (b, k) at every
  # distance r. The envelope is derived using whichever se_method was
  # selected, mirroring the treatment of W itself:
  #   Delta:     pointwise delta method using the fitted covariance matrix
  #              of (b, k):
  #                dp/db = exp(-k * r^2)
  #                dp/dk = -b * r^2 * exp(-k * r^2)
  #                Var(p(r)) ~= dp(r)^T %*% vcov(b, k) %*% dp(r)
  #   Bootstrap: the (b, k) pairs from each successful bootstrap resample are
  #              each used to compute a full resampled curve; the envelope is
  #              the pointwise standard deviation of those resampled curves
  #              (+/-1 SE, to match the delta-method envelope's scale).
  # In both cases the envelope is clipped to [0, 1] since p(r) is a probability.
  if (se_method == "Bootstrap" && !is.null(boot_b) && length(boot_b) >= 30) {
    boot_curves <- vapply(seq_along(boot_b), function(i) {
      boot_b[i] * exp(-boot_k[i] * dist_range^2)
    }, numeric(length(dist_range)))
    # boot_curves is [dist_range x n_boot_success]; take pointwise SD across resamples
    fitted_se <- apply(boot_curves, 1, stats::sd)
  } else {
    dp_db <- exp(-k_hat * dist_range^2)
    dp_dk <- -b_hat * dist_range^2 * exp(-k_hat * dist_range^2)
    vc_bk <- vc[c("b", "k"), c("b", "k")]

    fitted_se <- sqrt(pmax(
      0,
      vc_bk[1, 1] * dp_db^2 + vc_bk[2, 2] * dp_dk^2 +
        2 * vc_bk[1, 2] * dp_db * dp_dk
    ))
  }

  fit_df <- data.frame(
    dist       = dist_range,
    fitted     = fitted_curve,
    fitted_se  = fitted_se,
    fitted_lo  = pmax(0, fitted_curve - fitted_se),
    fitted_hi  = pmin(1, fitted_curve + fitted_se)
  )

  # --- Compose summary message -----------------------------------------------
  boot_note <- if (se_method == "Bootstrap") {
    paste0(" [", se_method, ", ",
           ifelse(is.na(n_boot_success), "NA", n_boot_success),
           "/", n_boot, " resamples converged]")
  } else {
    paste0(" [", se_method, "]")
  }
  skew_note <- if (isTRUE(skew_flag)) {
    paste0(
      "\n  \u26a0 Note: point estimate (", round(W, 2), " m) falls outside the ",
      "central 50% (IQR) of the bootstrap distribution (median = ",
      round(boot_median, 2), " m). This suggests a skewed sampling ",
      "distribution for W; consider this alongside the delta-method result ",
      "and check whether more calibration data would stabilise the estimate."
    )
  } else {
    ""
  }
  msg <- paste0(
    "Effective Sweep Width (", side, "): ", round(W, 2), " m",
    " (SE = ", ifelse(is.na(W_se), "NA", round(W_se, 2)),
    ", 95% CI = [",
    ifelse(is.na(W_ci_low),  "NA", round(W_ci_low,  2)), ", ",
    ifelse(is.na(W_ci_high), "NA", round(W_ci_high, 2)), "] m)", boot_note, "\n",
    "  b = ", round(b_hat, 4), " (SE = ", round(b_se, 4), ")\n",
    "  k = ", round(k_hat, 4), " (SE = ", round(k_se, 4), ")\n",
    "  n distance bins used: ", nrow(prob_df), "\n",
    "  n_runs: ", n_runs,
    skew_note
  )

  # --- Return results --------------------------------------------------------
  list(
    W              = W,
    W_se           = W_se,
    W_ci_low       = W_ci_low,
    W_ci_high      = W_ci_high,
    se_method      = se_method,
    n_boot_success = n_boot_success,
    boot_median    = boot_median,
    skew_flag      = skew_flag,
    b              = b_hat,
    b_se           = b_se,
    k              = k_hat,
    k_se           = k_se,
    side           = side,
    n_runs         = n_runs,
    prob_df        = prob_df,
    fit_df         = fit_df,
    converged      = TRUE,
    message        = msg
  )
}


#' Bootstrap SE and 95% CI for W via nonparametric case resampling
#'
#' Resamples both `records_classified` (surveyor detection events, already
#' TRUE/FALSE classified) and `master` (seeded artifacts) with replacement,
#' independently, refitting the full prepare_detection_probs() -> nlsLM
#' pipeline on each resample. This mirrors the resampling design of the
#' point estimate itself (which also treats the two tables independently)
#' and avoids the delta method's reliance on asymptotic normality and
#' linearization around the fitted (b, k).
#'
#' @param records_classified Data frame from classify_detections().
#' @param master             Master artifact data frame.
#' @param side               Character. One of "Left", "Right", "Total".
#' @param b_start            Starting value for b in each resampled fit.
#' @param k_start            Starting value for k in each resampled fit.
#' @param n_runs             Number of pooled surveyor passes (see fit_esw()).
#' @param n_boot             Number of bootstrap resamples. Default 1000.
#' @param point_W            Numeric. The point estimate of W from the full
#'                           (non-resampled) data, used only to flag whether
#'                           it falls outside the central 50% (interquartile
#'                           range) of the bootstrap distribution -- a sign
#'                           of a skewed sampling distribution for W. Optional;
#'                           if NULL, no flag is computed.
#'
#' @return A named list:
#'   \item{W_se}{Bootstrap standard error of W (SD of resampled W values)}
#'   \item{W_ci_low}{2.5th percentile of resampled W values}
#'   \item{W_ci_high}{97.5th percentile of resampled W values}
#'   \item{n_boot_success}{Number of resamples that converged and were used}
#'   \item{n_boot}{Number of resamples requested}
#'   \item{boot_W}{Numeric vector of resampled W values (successful fits only)}
#'   \item{boot_median}{Median of resampled W values}
#'   \item{skew_flag}{Logical. TRUE if point_W falls outside the IQR of the
#'                    bootstrap distribution (possible skew/bias); NA if
#'                    point_W was not supplied or too few resamples converged}
#'   \item{boot_b}{Numeric vector of resampled b values (successful fits only)}
#'   \item{boot_k}{Numeric vector of resampled k values (successful fits only)}

bootstrap_esw_ci <- function(records_classified,
                             master,
                             side     = "Right",
                             b_start  = 0.5,
                             k_start  = 0.05,
                             n_runs   = 1,
                             n_boot   = 1000,
                             point_W  = NULL) {

  n_rec <- nrow(records_classified)
  n_mas <- nrow(master)

  boot_mat <- vapply(seq_len(n_boot), function(i) {
    rec_i <- records_classified[sample.int(n_rec, n_rec, replace = TRUE), ]
    mas_i <- master[sample.int(n_mas, n_mas, replace = TRUE), ]

    res <- tryCatch(
      {
        prob_df_i <- prepare_detection_probs(rec_i, mas_i, side, n_runs = n_runs)
        fit_i <- minpack.lm::nlsLM(
          prob ~ b * exp(-k * dist^2),
          data    = prob_df_i,
          start   = list(b = b_start, k = k_start),
          lower   = c(b = 0,    k = 1e-6),
          upper   = c(b = 1,    k = Inf),
          control = minpack.lm::nls.lm.control(maxiter = 1000)
        )
        coefs_i <- coef(fit_i)
        c(W = unname(coefs_i["b"] * sqrt(pi / coefs_i["k"])),
          b = unname(coefs_i["b"]), k = unname(coefs_i["k"]))
      },
      error = function(e) c(W = NA_real_, b = NA_real_, k = NA_real_)
    )

    res
  }, numeric(3))

  ok <- is.finite(boot_mat["W", ])
  boot_W_ok <- boot_mat["W", ok]
  boot_b_ok <- boot_mat["b", ok]
  boot_k_ok <- boot_mat["k", ok]
  n_success <- length(boot_W_ok)

  if (n_success < 30) {
    return(list(
      W_se           = NA_real_,
      W_ci_low       = NA_real_,
      W_ci_high      = NA_real_,
      n_boot_success = n_success,
      n_boot         = n_boot,
      boot_W         = boot_W_ok,
      boot_median    = NA_real_,
      skew_flag      = NA,
      boot_b         = boot_b_ok,
      boot_k         = boot_k_ok
    ))
  }

  iqr <- stats::quantile(boot_W_ok, c(0.25, 0.75))
  skew_flag <- if (is.null(point_W) || !is.finite(point_W)) {
    NA
  } else {
    point_W < iqr[1] || point_W > iqr[2]
  }

  list(
    W_se           = stats::sd(boot_W_ok),
    W_ci_low       = unname(stats::quantile(boot_W_ok, 0.025)),
    W_ci_high      = unname(stats::quantile(boot_W_ok, 0.975)),
    n_boot_success = n_success,
    n_boot         = n_boot,
    boot_W         = boot_W_ok,
    boot_median    = stats::median(boot_W_ok),
    skew_flag      = skew_flag,
    boot_b         = boot_b_ok,
    boot_k         = boot_k_ok
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
#' @param se_method          Character. One of "Delta" or "Bootstrap".
#'                           Passed through to fit_esw(). Defaults to "Delta".
#' @param n_boot             Number of bootstrap resamples, used only when
#'                           se_method = "Bootstrap". Defaults to 1000.
#'
#' @return A named list where each element corresponds to a requested side
#'         and contains the output of fit_esw() for that side.

fit_esw_multi <- function(records_classified,
                          master,
                          sides     = c("Left", "Right", "Total"),
                          b_start   = 0.5,
                          k_start   = 0.05,
                          n_runs    = 1,
                          se_method = "Delta",
                          n_boot    = 1000) {

  valid_sides <- c("Left", "Right", "Total")
  invalid     <- setdiff(sides, valid_sides)
  if (length(invalid) > 0) {
    stop(paste("Invalid side(s):", paste(invalid, collapse = ", "),
               "\nMust be one or more of: Left, Right, Total"))
  }

  results <- lapply(sides, function(s) {
    tryCatch(
      fit_esw(records_classified, master, side = s,
              b_start = b_start, k_start = k_start, n_runs = n_runs,
              se_method = se_method, n_boot = n_boot),
      error = function(e) {
        list(
          W              = NA,
          W_se           = NA,
          W_ci_low       = NA,
          W_ci_high      = NA,
          se_method      = se_method,
          n_boot_success = NA,
          boot_median    = NA,
          skew_flag      = NA,
          b              = NA,
          b_se           = NA,
          k              = NA,
          k_se           = NA,
          side           = s,
          prob_df        = NULL,
          fit_df         = NULL,
          converged      = FALSE,
          message        = paste("Could not compute ESW for side =", s,
                            ":", e$message)
        )
      }
    )
  })

  names(results) <- sides
  return(results)
}
