# =============================================================================
# server_esw_results.R
# Sweep Width Calibration App
#
# Purpose:
#   Reactive logic for the ESW results tab. Takes the classified detection
#   data from rv$records_classified, fits the Gaussian detection function,
#   and renders ESW estimates, fitted curve plots, and a summary results
#   table for whichever sides the user has selected.
#
# Reactive objects read:
#   rv$master              : master artifact data frame
#   rv$records_classified  : classified records from server_classification.R
#
# Dependencies:
#   functions/fit_detection_function.R (fit_esw_multi)
#   minpack.lm
# =============================================================================

server_esw_results <- function(input, output, session, rv) {
  
  # ---------------------------------------------------------------------------
  # Core reactive: fit detection function when button is pressed
  # ---------------------------------------------------------------------------
  
  esw_results <- eventReactive(input$run_esw, {
    
    req(rv$master, rv$records_classified)
    
    sides <- input$esw_sides
    if (is.null(sides) || length(sides) == 0) {
      showNotification("Please select at least one side to compute.",
                       type = "warning")
      return(NULL)
    }
    
    # Read n_runs from rv with a safe fallback
    n_runs <- rv$n_runs
    if (is.null(n_runs) || !is.numeric(n_runs) || n_runs < 1) n_runs <- 1

    results <- tryCatch(
      fit_esw_multi(
        records_classified = rv$records_classified,
        master             = rv$master,
        sides              = sides,
        b_start            = input$b_start,
        k_start            = input$k_start,
        n_runs             = n_runs
      ),
      error = function(e) {
        showNotification(paste("ESW fitting error:", e$message), type = "error")
        NULL
      }
    )
    
    results
  })
  
  # ---------------------------------------------------------------------------
  # Output: ESW summary table
  # One row per requested side showing W, b, k and their standard errors.
  # ---------------------------------------------------------------------------
  
  output$esw_summary_table <- renderTable({
    req(esw_results())
    
    rows <- lapply(esw_results(), function(res) {
      data.frame(
        Side      = res$side,
        n_runs    = if (!is.null(res$n_runs)) res$n_runs else 1L,
        ESW_m     = ifelse(res$converged, round(res$W,    2), NA),
        b         = ifelse(res$converged, round(res$b,    4), NA),
        b_SE      = ifelse(res$converged, round(res$b_se, 4), NA),
        k         = ifelse(res$converged, round(res$k,    4), NA),
        k_SE      = ifelse(res$converged, round(res$k_se, 4), NA),
        Converged = res$converged,
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, rows)
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---------------------------------------------------------------------------
  # Output: ESW text messages
  # Displayed below the table; one message per side.
  # ---------------------------------------------------------------------------
  
  output$esw_messages <- renderUI({
    req(esw_results())
    msgs <- lapply(esw_results(), function(res) {
      cls <- if (res$converged) "alert alert-success" else "alert alert-warning"
      tags$div(class = cls, tags$pre(res$message))
    })
    do.call(tagList, msgs)
  })
  
  # ---------------------------------------------------------------------------
  # Output: detection function plot
  # One panel per selected side showing observed probabilities as points
  # and the fitted Gaussian curve. A vertical line marks W/2 (the one-sided
  # half sweep width) for reference.
  # ---------------------------------------------------------------------------
  
  output$detection_function_plot <- renderPlot({
    req(esw_results())
    
    plot_list <- lapply(esw_results(), function(res) {
      
      if (!res$converged || is.null(res$prob_df)) return(NULL)
      
      half_W <- res$W / 2
      
      ggplot2::ggplot() +
        # Shaded area under the fitted curve
        ggplot2::geom_ribbon(
          data = res$fit_df,
          ggplot2::aes(x = dist, ymin = 0, ymax = fitted),
          fill  = "steelblue",
          alpha = 0.2
        ) +
        # Fitted curve
        ggplot2::geom_line(
          data = res$fit_df,
          ggplot2::aes(x = dist, y = fitted),
          color = "steelblue",
          linewidth = 1.1
        ) +
        # Observed detection probabilities
        ggplot2::geom_point(
          data = res$prob_df,
          ggplot2::aes(x = dist, y = prob),
          size  = 2.5,
          color = "black"
        ) +
        # Half sweep width reference line
        ggplot2::geom_vline(
          xintercept = half_W,
          linetype   = "dashed",
          color      = "firebrick",
          linewidth  = 0.8
        ) +
        ggplot2::annotate(
          "text",
          x     = half_W,
          y     = max(res$fit_df$fitted) * 0.85,
          label = paste0("W/2 = ", round(half_W, 2), " m"),
          hjust = -0.1,
          color = "firebrick",
          size  = 3.5
        ) +
        ggplot2::labs(
          title    = paste0("Detection Function \u2014 ", res$side),
          subtitle = bquote(p(r) == b %.% e^{-k * r^2} ~
                              "  W =" ~ .(round(res$W, 2)) ~ "m"),
          x        = "Distance from Transect (m)",
          y        = "Detection Probability p(r)"
        ) +
        ggplot2::ylim(0, 1) +
        ggplot2::theme_minimal(base_size = 13)
    })
    
    # Remove NULLs (non-converged sides)
    plot_list <- Filter(Negate(is.null), plot_list)
    
    if (length(plot_list) == 0) return(NULL)
    
    # Arrange panels side by side using patchwork if multiple sides selected
    if (length(plot_list) == 1) {
      print(plot_list[[1]])
    } else {
      if (requireNamespace("patchwork", quietly = TRUE)) {
        print(patchwork::wrap_plots(plot_list, ncol = length(plot_list)))
      } else {
        print(plot_list[[1]])
        showNotification(
          "Install the 'patchwork' package to display multiple panels side by side.",
          type = "message"
        )
      }
    }
  })
  
  # ---------------------------------------------------------------------------
  # Output: combined left/right comparison plot
  # Only rendered when both Left and Right results are available, showing
  # both detection function curves and their respective W/2 lines on a
  # single mirrored axis (negative = left, positive = right).
  # ---------------------------------------------------------------------------
  
  output$combined_plot <- renderPlot({
    req(esw_results())
    
    res_L <- esw_results()[["Left"]]
    res_R <- esw_results()[["Right"]]
    
    if (is.null(res_L) || is.null(res_R) ||
        !res_L$converged || !res_R$converged) return(NULL)
    
    # Mirror left-side distances to negative axis
    fit_L        <- res_L$fit_df
    fit_L$dist   <- -fit_L$dist
    fit_L$side   <- "Left"
    
    fit_R        <- res_R$fit_df
    fit_R$side   <- "Right"
    
    obs_L        <- res_L$prob_df
    obs_L$dist   <- -obs_L$dist
    obs_L$side   <- "Left"
    
    obs_R        <- res_R$prob_df
    obs_R$side   <- "Right"
    
    obs_all <- rbind(
      obs_L[, c("dist", "prob", "side")],
      obs_R[, c("dist", "prob", "side")]
    )
    
    # Build plot using separate geom_ribbon calls per side to avoid
    # the unit() error that occurs when geom_area uses a fill grouping
    # aesthetic with mirrored (negative) x values
    ggplot2::ggplot() +
      
      # Shaded areas — one ribbon per side
      ggplot2::geom_ribbon(
        data = fit_L,
        ggplot2::aes(x = dist, ymin = 0, ymax = fitted),
        fill  = "#e74c3c",
        alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = fit_R,
        ggplot2::aes(x = dist, ymin = 0, ymax = fitted),
        fill  = "#3498db",
        alpha = 0.2
      ) +
      
      # Fitted curves — one line per side
      ggplot2::geom_line(
        data = fit_L,
        ggplot2::aes(x = dist, y = fitted),
        color = "#e74c3c", linewidth = 1.1
      ) +
      ggplot2::geom_line(
        data = fit_R,
        ggplot2::aes(x = dist, y = fitted),
        color = "#3498db", linewidth = 1.1
      ) +
      
      # Observed probabilities
      ggplot2::geom_point(
        data = obs_all,
        ggplot2::aes(x = dist, y = prob, color = side),
        size = 2.5
      ) +
      
      # W/2 reference lines
      ggplot2::geom_vline(
        xintercept = -res_L$W / 2,
        linetype = "dashed", color = "#e74c3c", linewidth = 0.8
      ) +
      ggplot2::geom_vline(
        xintercept =  res_R$W / 2,
        linetype = "dashed", color = "#3498db", linewidth = 0.8
      ) +
      ggplot2::geom_vline(
        xintercept = 0,
        color = "grey50", linewidth = 0.5
      ) +
      
      # W/2 annotations
      ggplot2::annotate(
        "text",
        x     = -res_L$W / 2,
        y     = 0.92,
        label = paste0("W/2 = ", round(res_L$W / 2, 2), " m"),
        hjust = 1.1, color = "#e74c3c", size = 3.5
      ) +
      ggplot2::annotate(
        "text",
        x     =  res_R$W / 2,
        y     = 0.92,
        label = paste0("W/2 = ", round(res_R$W / 2, 2), " m"),
        hjust = -0.1, color = "#3498db", size = 3.5
      ) +
      
      ggplot2::scale_color_manual(
        values = c("Left" = "#e74c3c", "Right" = "#3498db")
      ) +
      
      ggplot2::labs(
        title    = "Combined Detection Functions \u2014 Left and Right",
        subtitle = paste0("Total ESW = ",
                          round(res_L$W / 2 + res_R$W / 2, 2), " m"),
        x        = "Distance from Transect (m), Left \u2190  \u2192 Right",
        y        = "Detection Probability p(r)",
        color    = "Side"
      ) +
      ggplot2::ylim(0, 1) +
      ggplot2::theme_minimal(base_size = 13)
  })
}