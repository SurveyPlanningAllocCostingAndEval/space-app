# =============================================================================
#  mod_sweep_width_server.R
#  SPACE — Sweep Width Estimation module server
# =============================================================================

sweepWidthServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # -------------------------------------------------------------------------
    # Back to Home navigation
    # -------------------------------------------------------------------------

    nav_home <- reactiveVal(0)

    observeEvent(input$back_home, {
      nav_home(nav_home() + 1)
    })

    observeEvent(input$logo_home, {
      nav_home(nav_home() + 1)
    })

    # -------------------------------------------------------------------------
    # Reset Module — confirmation dialog and reset logic
    # -------------------------------------------------------------------------

    observeEvent(input$reset_module, {
      showModal(modalDialog(
        title     = "Reset Module",
        easyClose = FALSE,
        footer    = tagList(
          actionButton(ns("cancel_reset"),  "Cancel",        class = "btn btn-default"),
          actionButton(ns("confirm_reset"), "Confirm Reset", class = "btn btn-danger")
        ),
        tags$p(
          style = "font-size: 15px; margin-bottom: 0;",
          tags$strong("Warning: "),
          "Are you sure you want to reset this module? ",
          "All currently loaded data and results will be lost."
        )
      ))
    })

    observeEvent(input$cancel_reset, {
      removeModal()
    })

    observeEvent(input$confirm_reset, {
      removeModal()

      # Reset reactive values
      rv$master                 <- NULL
      rv$records                <- NULL
      rv$master_error           <- NULL
      rv$records_error          <- NULL
      rv$n_runs                 <- 1
      rv$records_classified     <- NULL
      rv$classification_summary <- NULL
      rv$classification_error   <- NULL

      # Reset file inputs
      shinyjs::reset("master_file")
      shinyjs::reset("records_file")

      # Reset numeric inputs (defaults from UI declarations)
      updateNumericInput(session, "n_runs",  value = 1)
      updateNumericInput(session, "b_start", value = 0.5)
      updateNumericInput(session, "k_start", value = 0.05)

      # Reset slider inputs
      updateSliderInput(session, "perp_scale",  value = 0.20)
      updateSliderInput(session, "along_fixed", value = 1.00)

      # Return to default tab
      updateTabsetPanel(session, "main_tabs", selected = "Data Input")
    })

    # -------------------------------------------------------------------------
    # Shared reactive values
    # -------------------------------------------------------------------------

    rv <- reactiveValues(
      master                 = NULL,
      records                = NULL,
      master_error           = NULL,
      records_error          = NULL,
      n_runs                 = 1,
      records_classified     = NULL,
      classification_summary = NULL,
      classification_error   = NULL
    )

    # =========================================================================
    # Data Input
    # =========================================================================

    MASTER_REQUIRED  <- c("LDist", "Dist", "LorR", "Type")
    RECORDS_REQUIRED <- c("LDist", "Dist", "LorR", "Type")

    validate_csv <- function(df, required_cols, file_label) {
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(paste(file_label, "is empty or could not be parsed."))
      }
      missing <- setdiff(required_cols, names(df))
      if (length(missing) > 0) {
        return(paste0(
          file_label, " is missing required column(s): ",
          paste(missing, collapse = ", "), "."
        ))
      }
      if (!is.numeric(df$Dist)) {
        return(paste(file_label, "column 'Dist' must be numeric."))
      }
      if (!is.numeric(df$LDist)) {
        return(paste(file_label, "column 'LDist' must be numeric."))
      }
      if (any(df$Dist < 0, na.rm = TRUE)) {
        return(paste(
          file_label, "column 'Dist' contains negative values.",
          "Distances should be positive; side is indicated by 'LorR'."
        ))
      }
      if (!all(df$LorR %in% c("Left", "Right"))) {
        return(paste(
          file_label, "column 'LorR' contains values other than 'Left'",
          "and 'Right'. Please check your data."
        ))
      }
      return(NULL)
    }

    observeEvent(input$master_file, {
      req(input$master_file)
      df <- tryCatch(
        readr::read_csv(input$master_file$datapath, show_col_types = FALSE),
        error = function(e) NULL
      )
      err <- validate_csv(df, MASTER_REQUIRED, "Master file")
      if (!is.null(err)) {
        rv$master       <- NULL
        rv$master_error <- err
      } else {
        rv$master       <- as.data.frame(df)
        rv$master_error <- NULL
      }
    })

    observeEvent(input$records_file, {
      req(input$records_file)
      df <- tryCatch(
        readr::read_csv(input$records_file$datapath, show_col_types = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df) && "DistanceError" %in% names(df)) {
        df$DistanceError <- NULL
      }
      err <- validate_csv(df, RECORDS_REQUIRED, "Records file")
      if (!is.null(err)) {
        rv$records       <- NULL
        rv$records_error <- err
      } else {
        rv$records       <- as.data.frame(df)
        rv$records_error <- NULL
      }
    })

    observeEvent(input$load_sample, {
      master_path  <- file.path("www", "sweep_width", "master.csv")
      records_path <- file.path("www", "sweep_width", "records.csv")

      if (!file.exists(master_path) || !file.exists(records_path)) {
        rv$master_error  <- "Sample data files not found."
        rv$records_error <- "Sample data files not found."
        return()
      }

      master_df <- tryCatch(
        as.data.frame(readr::read_csv(master_path,  show_col_types = FALSE)),
        error = function(e) NULL
      )
      records_df <- tryCatch(
        as.data.frame(readr::read_csv(records_path, show_col_types = FALSE)),
        error = function(e) NULL
      )

      if (!is.null(records_df) && "DistanceError" %in% names(records_df)) {
        records_df$DistanceError <- NULL
      }

      rv$master        <- master_df
      rv$records       <- records_df
      rv$master_error  <- NULL
      rv$records_error <- NULL

      updateNumericInput(session, "n_runs", value = 1)
    })

    observeEvent(input$n_runs, {
      val <- input$n_runs
      if (is.null(val) || is.na(val) || !is.numeric(val)) {
        rv$n_runs <- 1
        updateNumericInput(session, "n_runs", value = 1)
        return()
      }
      if (val < 1 || val != floor(val)) {
        showNotification(
          "Number of runs must be a positive whole number. Rounding down to nearest valid value.",
          type = "warning"
        )
        val <- max(1, floor(val))
        updateNumericInput(session, "n_runs", value = val)
      }
      rv$n_runs <- as.integer(val)
    })

    output$master_feedback <- renderUI({
      if (!is.null(rv$master_error)) {
        tags$div(class = "alert alert-danger", rv$master_error)
      } else if (!is.null(rv$master)) {
        tags$div(
          class = "alert alert-success",
          paste0("Master file loaded: ", nrow(rv$master), " artifacts.")
        )
      }
    })

    output$records_feedback <- renderUI({
      if (!is.null(rv$records_error)) {
        tags$div(class = "alert alert-danger", rv$records_error)
      } else if (!is.null(rv$records)) {
        tags$div(
          class = "alert alert-success",
          paste0("Records file loaded: ", nrow(rv$records), " detections.")
        )
      }
    })

    output$master_preview <- renderTable({
      req(rv$master)
      head(rv$master, 10)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$records_preview <- renderTable({
      req(rv$records)
      head(rv$records, 10)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$master_plot <- renderPlot({
      req(rv$master)
      df <- rv$master
      df$DistSigned <- ifelse(df$LorR == "Left", -df$Dist, df$Dist)
      ggplot2::ggplot(df, ggplot2::aes(x = DistSigned, y = LDist,
                                       color = Type)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = "grey50") +
        ggplot2::labs(
          title = "Seeded Artifact Locations (Master)",
          x     = "Distance from Transect (m), Left \u2190  \u2192 Right",
          y     = "Along-Transect Distance (m)",
          color = "Artifact Type"
        ) +
        ggplot2::theme_minimal()
    })

    output$records_plot <- renderPlot({
      req(rv$records)
      df <- rv$records
      df$DistSigned <- ifelse(df$LorR == "Left", -df$Dist, df$Dist)
      ggplot2::ggplot(df, ggplot2::aes(x = DistSigned, y = LDist,
                                       color = Type)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = "grey50") +
        ggplot2::labs(
          title = "Surveyor Detections (Records)",
          x     = "Distance from Transect (m), Left \u2190  \u2192 Right",
          y     = "Along-Transect Distance (m)",
          color = "Artifact Type"
        ) +
        ggplot2::theme_minimal()
    })

    # =========================================================================
    # Classification
    # =========================================================================

    observeEvent(input$run_classification, {

      req(rv$master, rv$records)

      perp_scale  <- input$perp_scale
      along_fixed <- input$along_fixed

      records_tol <- tryCatch(
        compute_tolerance(rv$records,
                          perp_scale  = perp_scale,
                          along_fixed = along_fixed),
        error = function(e) {
          rv$classification_error <- paste("Tolerance computation failed:", e$message)
          NULL
        }
      )

      req(records_tol)

      records_classified <- tryCatch(
        classify_detections(records_tol, rv$master),
        error = function(e) {
          rv$classification_error <- paste("Classification failed:", e$message)
          NULL
        }
      )

      req(records_classified)

      rv$records_classified     <- records_classified
      rv$classification_summary <- summarise_detections(records_classified)
      rv$classification_error   <- NULL
    })

    output$classification_feedback <- renderUI({
      if (!is.null(rv$classification_error)) {
        tags$div(class = "alert alert-danger", rv$classification_error)
      } else if (!is.null(rv$records_classified)) {
        n_true  <- sum(rv$records_classified$Detected)
        n_total <- nrow(rv$records_classified)
        tags$div(
          class = "alert alert-success",
          paste0("Classification complete. ",
                 n_true, " true detections out of ",
                 n_total, " total records.")
        )
      }
    })

    output$classification_summary_table <- renderTable({
      req(rv$classification_summary)
      rv$classification_summary
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)

    output$ellipse_plot <- renderPlot({
      req(rv$records_classified)

      df <- rv$records_classified
      df$DistSigned      <- ifelse(df$LorR == "Left", -df$Dist,  df$Dist)
      df$semi_major_sign <- df$semi_major

      ggplot2::ggplot(df, ggplot2::aes(x = DistSigned, y = LDist,
                                       color = Detected)) +
        ggforce::geom_ellipse(
          ggplot2::aes(
            x0    = DistSigned,
            y0    = LDist,
            a     = semi_major_sign,
            b     = semi_minor,
            angle = 0
          ),
          fill      = NA,
          alpha     = 0.4,
          linewidth = 0.4
        ) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = "grey50") +
        ggplot2::scale_color_manual(
          values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
          labels = c("TRUE" = "True detection", "FALSE" = "False / unmatched")
        ) +
        ggplot2::labs(
          title = "Detections with Elliptical Tolerance Zones",
          x     = "Distance from Transect (m), Left \u2190  \u2192 Right",
          y     = "Along-Transect Distance (m)",
          color = "Classification"
        ) +
        ggplot2::theme_minimal()
    })

    output$overlay_plot <- renderPlot({
      req(rv$master, rv$records_classified)

      master_df <- rv$master
      master_df$DistSigned <- ifelse(master_df$LorR == "Left",
                                     -master_df$Dist, master_df$Dist)
      master_df$Source <- "Seeded (Master)"

      rec_df <- rv$records_classified
      rec_df$DistSigned <- ifelse(rec_df$LorR == "Left",
                                  -rec_df$Dist, rec_df$Dist)
      rec_df$Source <- ifelse(rec_df$Detected, "True Detection", "False Detection")

      plot_df <- rbind(
        master_df[, c("DistSigned", "LDist", "Source")],
        rec_df[,    c("DistSigned", "LDist", "Source")]
      )

      ggplot2::ggplot(plot_df, ggplot2::aes(x = DistSigned, y = LDist,
                                            color = Source, shape = Source)) +
        ggplot2::geom_point(size = 2.5, alpha = 0.8) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = "grey50") +
        ggplot2::scale_color_manual(values = c(
          "Seeded (Master)"  = "#2c3e50",
          "True Detection"   = "#2ecc71",
          "False Detection"  = "#e74c3c"
        )) +
        ggplot2::scale_shape_manual(values = c(
          "Seeded (Master)"  = 17,
          "True Detection"   = 16,
          "False Detection"  = 4
        )) +
        ggplot2::labs(
          title = "Master Artifacts and Classified Detections",
          x     = "Distance from Transect (m), Left \u2190  \u2192 Right",
          y     = "Along-Transect Distance (m)",
          color = "Source",
          shape = "Source"
        ) +
        ggplot2::theme_minimal()
    })

    # =========================================================================
    # ESW Results
    # =========================================================================

    esw_results <- eventReactive(input$run_esw, {

      req(rv$master, rv$records_classified)

      sides <- input$esw_sides
      if (is.null(sides) || length(sides) == 0) {
        showNotification("Please select at least one side to compute.",
                         type = "warning")
        return(NULL)
      }

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

    output$esw_messages <- renderUI({
      req(esw_results())
      msgs <- lapply(esw_results(), function(res) {
        cls <- if (res$converged) "alert alert-success" else "alert alert-warning"
        tags$div(class = cls, tags$pre(res$message))
      })
      do.call(tagList, msgs)
    })

    output$detection_function_plot <- renderPlot({
      req(esw_results())

      plot_list <- lapply(esw_results(), function(res) {

        if (!res$converged || is.null(res$prob_df)) return(NULL)

        half_W <- res$W / 2

        ggplot2::ggplot() +
          ggplot2::geom_ribbon(
            data = res$fit_df,
            ggplot2::aes(x = dist, ymin = 0, ymax = fitted),
            fill  = "steelblue",
            alpha = 0.2
          ) +
          ggplot2::geom_line(
            data = res$fit_df,
            ggplot2::aes(x = dist, y = fitted),
            color     = "steelblue",
            linewidth = 1.1
          ) +
          ggplot2::geom_point(
            data  = res$prob_df,
            ggplot2::aes(x = dist, y = prob),
            size  = 2.5,
            color = "black"
          ) +
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

      plot_list <- Filter(Negate(is.null), plot_list)

      if (length(plot_list) == 0) return(NULL)

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

    output$combined_plot <- renderPlot({
      req(esw_results())

      res_L <- esw_results()[["Left"]]
      res_R <- esw_results()[["Right"]]

      if (is.null(res_L) || is.null(res_R) ||
          !res_L$converged || !res_R$converged) return(NULL)

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

      ggplot2::ggplot() +

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

        ggplot2::geom_point(
          data = obs_all,
          ggplot2::aes(x = dist, y = prob, color = side),
          size = 2.5
        ) +

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

    return(list(nav_home = nav_home))

  }) # end moduleServer
}
