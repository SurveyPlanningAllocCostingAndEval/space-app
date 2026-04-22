# =============================================================================
# server_classification.R
# Sweep Width Calibration App
#
# Purpose:
#   Reactive logic for the classification tab. Takes the validated master and
#   records data from rv (populated by server_data_input.R), applies the
#   elliptical tolerance zone computation and detection classification, and
#   exposes results for the ESW results tab.
#
# Reactive objects read:
#   rv$master   : validated master data frame
#   rv$records  : validated records data frame
#
# Reactive objects written:
#   rv$records_classified : records data frame with Detected column appended
#   rv$classification_summary : tidy summary table of detection counts
#
# Dependencies:
#   functions/distance_error.R      (compute_tolerance, point_in_ellipse)
#   functions/classify_detections.R (classify_detections, summarise_detections)
# =============================================================================

server_classification <- function(input, output, session, rv) {
  
  # ---------------------------------------------------------------------------
  # Core reactive: run classification when button is pressed
  # ---------------------------------------------------------------------------
  
  observeEvent(input$run_classification, {
    
    req(rv$master, rv$records)
    
    # Retrieve tolerance parameters from UI inputs
    perp_scale  <- input$perp_scale
    along_fixed <- input$along_fixed
    
    # Step 1: compute elliptical tolerance zones
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
    
    # Step 2: classify detections
    records_classified <- tryCatch(
      classify_detections(records_tol, rv$master),
      error = function(e) {
        rv$classification_error <- paste("Classification failed:", e$message)
        NULL
      }
    )
    
    req(records_classified)
    
    # Step 3: store results and clear any previous error
    rv$records_classified    <- records_classified
    rv$classification_summary <- summarise_detections(records_classified)
    rv$classification_error  <- NULL
  })
  
  # ---------------------------------------------------------------------------
  # Output: error feedback
  # ---------------------------------------------------------------------------
  
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
  
  # ---------------------------------------------------------------------------
  # Output: classification summary table
  # ---------------------------------------------------------------------------
  
  output$classification_summary_table <- renderTable({
    req(rv$classification_summary)
    rv$classification_summary
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)
  
  # ---------------------------------------------------------------------------
  # Output: tolerance ellipse visualisation
  # Plots detection records with ellipses overlaid, colour-coded by
  # Detected status. Requires ggforce for geom_ellipse.
  # ---------------------------------------------------------------------------
  
  output$ellipse_plot <- renderPlot({
    req(rv$records_classified)
    
    df <- rv$records_classified
    df$DistSigned      <- ifelse(df$LorR == "Left", -df$Dist,  df$Dist)
    df$semi_major_sign <- df$semi_major  # ellipse axes are always positive
    
    ggplot2::ggplot(df, ggplot2::aes(x = DistSigned, y = LDist,
                                     color = Detected)) +
      ggforce::geom_ellipse(
        ggplot2::aes(
          x0 = DistSigned,
          y0 = LDist,
          a  = semi_major_sign,
          b  = semi_minor,
          angle = 0
        ),
        fill  = NA,
        alpha = 0.4,
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
        title  = "Detections with Elliptical Tolerance Zones",
        x      = "Distance from Transect (m), Left \u2190  \u2192 Right",
        y      = "Along-Transect Distance (m)",
        color  = "Classification"
      ) +
      ggplot2::theme_minimal()
  })
  
  # ---------------------------------------------------------------------------
  # Output: overlay plot of master artifacts and classified records
  # Shows both datasets together so the user can visually inspect matches.
  # ---------------------------------------------------------------------------
  
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
        title  = "Master Artifacts and Classified Detections",
        x      = "Distance from Transect (m), Left \u2190  \u2192 Right",
        y      = "Along-Transect Distance (m)",
        color  = "Source",
        shape  = "Source"
      ) +
      ggplot2::theme_minimal()
  })
}