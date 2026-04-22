# =============================================================================
# server_data_input.R
# Sweep Width Calibration App
#
# Purpose:
#   Handles all reactive logic for the data input tab. Reads and validates
#   uploaded CSV files for the master and records datasets, and exposes them
#   as reactive objects for use by downstream server modules.
#
# Reactive objects exported (via reactiveValues passed in from server_main.R):
#   rv$master  : validated master data frame
#   rv$records : validated records data frame
#
# Dependencies: none beyond base R and readr
# =============================================================================

server_data_input <- function(input, output, session, rv) {
  
  # ---------------------------------------------------------------------------
  # Constants: required columns for each file
  # ---------------------------------------------------------------------------
  
  MASTER_REQUIRED  <- c("LDist", "Dist", "LorR", "Type")
  RECORDS_REQUIRED <- c("LDist", "Dist", "LorR", "Type")
  
  # ---------------------------------------------------------------------------
  # Helper: validate a loaded data frame against required columns
  # Returns NULL if valid, or a character string describing the problem.
  # ---------------------------------------------------------------------------
  
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
  
  # ---------------------------------------------------------------------------
  # Master file upload
  # ---------------------------------------------------------------------------
  
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
  
  # ---------------------------------------------------------------------------
  # Records file upload
  # ---------------------------------------------------------------------------
  
  observeEvent(input$records_file, {
    
    req(input$records_file)
    
    df <- tryCatch(
      readr::read_csv(input$records_file$datapath, show_col_types = FALSE),
      error = function(e) NULL
    )
    
    # Records may already contain a DistanceError column from a previous
    # session; we drop it here so the app always recomputes it fresh.
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
  
  # ---------------------------------------------------------------------------
  # Load sample data
  # ---------------------------------------------------------------------------
  
  observeEvent(input$load_sample, {
    master_path  <- file.path("data", "master.csv")
    records_path <- file.path("data", "records.csv")
    
    if (!file.exists(master_path) || !file.exists(records_path)) {
      rv$master_error  <- "Sample data files not found in the 'data/' folder."
      rv$records_error <- "Sample data files not found in the 'data/' folder."
      return()
    }
    
    master_df  <- tryCatch(
      as.data.frame(readr::read_csv(master_path,  show_col_types = FALSE)),
      error = function(e) NULL
    )
    records_df <- tryCatch(
      as.data.frame(readr::read_csv(records_path, show_col_types = FALSE)),
      error = function(e) NULL
    )
    
    # Drop any pre-existing DistanceError column
    if (!is.null(records_df) && "DistanceError" %in% names(records_df)) {
      records_df$DistanceError <- NULL
    }
    
    rv$master        <- master_df
    rv$records       <- records_df
    rv$master_error  <- NULL
    rv$records_error <- NULL

    # Reset n_runs to 1 — sample data is a single pass
    updateNumericInput(session, "n_runs", value = 1)
  })
  
  # ---------------------------------------------------------------------------
  # Number of runs: sync input to rv with validation
  # ---------------------------------------------------------------------------

  observeEvent(input$n_runs, {
    val <- input$n_runs

    # Guard against NULL, NA, non-numeric
    if (is.null(val) || is.na(val) || !is.numeric(val)) {
      rv$n_runs <- 1
      updateNumericInput(session, "n_runs", value = 1)
      return()
    }

    # Must be a positive integer
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

  # ---------------------------------------------------------------------------
  # Output: validation feedback messages
  # ---------------------------------------------------------------------------
  
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
  
  # ---------------------------------------------------------------------------
  # Output: data preview tables
  # ---------------------------------------------------------------------------
  
  output$master_preview <- renderTable({
    req(rv$master)
    head(rv$master, 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$records_preview <- renderTable({
    req(rv$records)
    head(rv$records, 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---------------------------------------------------------------------------
  # Output: spatial preview plots
  # ---------------------------------------------------------------------------
  
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
}