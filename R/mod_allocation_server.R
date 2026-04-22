# =============================================================================
#  mod_allocation_server.R
#  SPACE — Bayesian Optimal Allocation module server
# =============================================================================

allocationServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # -------------------------------------------------------------------------
    # Back to Home navigation
    # -------------------------------------------------------------------------

    nav_home <- reactiveVal(0)

    observeEvent(input$back_home, {
      updateTabsetPanel(session, "mainTabs", selected = "Allocations")
      nav_home(nav_home() + 1)
    })

    observeEvent(input$logo_home, {
      updateTabsetPanel(session, "mainTabs", selected = "Allocations")
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
      rv$df_pre_in        <- NULL
      rv$day1_alloc       <- NULL
      rv$day1_dropped     <- NULL
      rv$day1_params      <- NULL
      rv$sf_pre_in        <- NULL
      rv$is_spatial       <- FALSE
      rv$posterior_inputs <- NULL
      rv$clean_results    <- NULL
      rv$update_table     <- NULL
      rv$posteriors       <- NULL
      rv$post_log         <- NULL
      rv$updated_priors   <- NULL
      rv$latest_inputs    <- NULL

      # Reset file inputs (shinyjs resolves namespace automatically)
      shinyjs::reset("pre_file")
      shinyjs::reset("posterior_inputs_file")
      shinyjs::reset("results_file")

      # Reset numeric inputs (default declared in UI: value = NA)
      updateNumericInput(session, "total_effort", value = NA)

      # Return to default tab
      updateTabsetPanel(session, "mainTabs", selected = "Allocations")
    })

    # -------------------------------------------------------------------------
    # Module-local reactive values
    # -------------------------------------------------------------------------

    rv <- reactiveValues(
      # Allocation workflow
      df_pre_in        = NULL,
      day1_alloc       = NULL,
      day1_dropped     = NULL,
      day1_params      = NULL,

      # Spatial data
      sf_pre_in        = NULL,
      is_spatial       = FALSE,

      # Posterior update workflow
      posterior_inputs = NULL,
      clean_results    = NULL,
      update_table     = NULL,
      posteriors       = NULL,
      post_log         = NULL,

      # Full updated priors
      updated_priors   = NULL,

      # Track latest uploaded dataset (for unified Inputs tab)
      latest_inputs    = NULL
    )

    # -------------------------------------------------------------------------
    # Output directory (per-session tempdir)
    # -------------------------------------------------------------------------

    get_output_dir <- function() {
      dir <- file.path(tempdir(), paste0("allocation_outputs_", id))
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      dir
    }

    output_dir_reactive <- reactive({ get_output_dir() })

    # =========================================================================
    # Upload Inputs (Allocation workflow)
    # =========================================================================

    observeEvent(input$pre_file, {
      req(input$pre_file)
      tryCatch({
        message("Loading inputs for allocation workflow...")

        result <- read_uploaded_spatial(input$pre_file)
        df_raw <- result$data

        # Normalize and standardize column names
        df <- df_raw
        names(df) <- trimws(tolower(names(df)))
        names(df) <- gsub("\\s+", "_", names(df))

        rename_map <- c(
          "polygons"      = "unit_id",
          "polygon"       = "unit_id",
          "area_(m2)"     = "area",
          "area_m2"       = "area",
          "probability"   = "probability",
          "prior"         = "probability",
          "sweep_width"   = "sweep_width",
          "sweepwidth"    = "sweep_width",
          "visibility"    = "visibility"
        )

        for (old in names(rename_map)) {
          new <- rename_map[[old]]
          if (old %in% names(df) && !(new %in% names(df))) {
            names(df)[names(df) == old] <- new
          }
        }

        required <- c("unit_id", "area", "probability", "sweep_width")
        missing  <- setdiff(required, names(df))
        if (length(missing) > 0) {
          stop("Missing required column(s): ", paste(missing, collapse = ", "))
        }

        df <- df |>
          dplyr::mutate(
            unit_id     = as.character(unit_id),
            area        = suppressWarnings(as.numeric(area)),
            probability = suppressWarnings(as.numeric(probability)),
            sweep_width = suppressWarnings(as.numeric(sweep_width)),
            visibility  = if ("visibility" %in% names(df)) as.character(visibility) else NA_character_
          )

        rv$df_pre_in     <- df
        rv$sf_pre_in     <- result$sf
        rv$is_spatial    <- result$is_spatial
        rv$latest_inputs <- df

        message("Allocation inputs loaded and standardized successfully.")
        showNotification("Inputs for allocation loaded successfully.", type = "message")

      }, error = function(e) {
        showNotification(paste("Error loading allocation inputs:", e$message), type = "error")
        show_error_modal(session, "Input Upload Error", e$message)
        message("Error loading allocation inputs: ", e$message)
      })
    }, ignoreInit = TRUE)

    # =========================================================================
    # Upload Inputs (Posterior workflow)
    # =========================================================================

    observeEvent(input$posterior_inputs_file, {
      req(input$posterior_inputs_file)
      tryCatch({
        df <- read_inputs_from_upload(input$posterior_inputs_file)
        rv$posterior_inputs <- df
        rv$latest_inputs    <- df
        showNotification("Inputs loaded and standardized successfully.", type = "message")
      }, error = function(e) {
        showNotification("Error loading inputs. Please check file format.", type = "error")
        show_error_modal(session, "Input Upload Error", e$message)
      })
    }, ignoreInit = TRUE)

    # =========================================================================
    # Assign / Update Sweep Widths modal
    # =========================================================================

    observeEvent(input$update_sweep_btn, {
      req(rv$df_pre_in)
      df <- rv$df_pre_in

      if (!"visibility" %in% names(df)) {
        showNotification("No 'visibility' column found in uploaded data.", type = "error")
        return()
      }

      vis_classes    <- unique(df$visibility)
      sweep_defaults <- sapply(vis_classes, function(v) {
        val <- df$sweep_width[df$visibility == v][1]
        ifelse(!is.na(val), val, NA)
      })

      showModal(modalDialog(
        title     = "Assign or Update Sweep Widths by Visibility Category",
        easyClose = TRUE,
        footer    = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_sweep"), "Save / Confirm", class = "btn-primary")
        ),
        fluidPage(
          lapply(seq_along(vis_classes), function(i) {
            fluidRow(
              column(6, strong(vis_classes[i])),
              column(6, numericInput(
                inputId = ns(paste0("sweep_", i)),
                label   = NULL,
                value   = sweep_defaults[i],
                min     = 0, step = 0.1
              ))
            )
          })
        )
      ))

      observeEvent(input$confirm_sweep, {
        new_vals <- sapply(seq_along(vis_classes),
                           function(i) input[[paste0("sweep_", i)]])
        names(new_vals) <- vis_classes

        df$sweep_width <- vapply(df$visibility, function(v) new_vals[v], numeric(1))
        rv$df_pre_in   <- df
        removeModal()
        showNotification("Sweep widths updated successfully.", type = "message")
      }, once = TRUE)
    })

    # =========================================================================
    # Generate Allocations
    # =========================================================================

    observeEvent(input$run_day1, {
      req(rv$df_pre_in, input$total_effort)
      L_val <- as.numeric(input$total_effort)

      validate(
        need(is.finite(L_val) && L_val >= 0,
             "Please enter a valid non-negative total effort (L).")
      )

      tryCatch({
        message("Generate Allocations button clicked")

        df_check <- rv$df_pre_in
        required <- c("unit_id", "area", "probability", "sweep_width")
        missing  <- setdiff(required, names(df_check))
        if (length(missing) > 0)
          stop("Input data is missing required column(s): ",
               paste(missing, collapse = ", "))

        res <- filter_and_rerun_allocation(
          input_df    = df_check,
          L           = L_val,
          output_dir  = output_dir_reactive(),
          prefix      = "iteration",
          max_iters   = 10,
          write_steps = TRUE
        )

        rv$day1_alloc   <- res$final_alloc
        rv$day1_dropped <- res$dropped_log
        rv$day1_params  <- NULL

        showNotification(
          "Allocations successfully generated and stored in memory.",
          type = "message"
        )
        updateTabsetPanel(session, "mainTabs", selected = "Allocations")
        message("Allocation completed successfully.")

      }, error = function(e) {
        showNotification(paste("Error generating allocations:", e$message), type = "error")
        show_error_modal(session, "Allocation Error", e$message)
        message("Error generating allocations: ", e$message)
      })
    })

    # =========================================================================
    # Compute Posteriors
    # =========================================================================

    # Helper: Read any tabular file type
    .read_any <- function(datapath, name) {
      ext <- tolower(tools::file_ext(name))
      switch(
        ext,
        "xlsx" = readxl::read_excel(datapath),
        "xls"  = readxl::read_excel(datapath),
        "csv"  = readr::read_csv(datapath, show_col_types = FALSE),
        "txt"  = readr::read_csv(datapath, show_col_types = FALSE),
        stop("Unsupported file type: ", ext)
      )
    }

    # Helper: Merge priors and results into update table
    .build_update_table <- function(priors_df, results_df) {
      pri <- std_names(priors_df)
      res <- std_names(results_df)

      names(pri) <- tolower(names(pri))
      names(res) <- tolower(names(res))

      rename_map <- list(
        "unit_id"        = c("unitid", "polygon", "polygons", "id", "unit"),
        "l_walked_today" = c("lwalkedtoday", "l_walked_today", "metres_walked",
                             "meters_walked", "distance_walked", "transect_length"),
        "success"        = c("success", "found", "detected", "result", "presence")
      )
      for (target in names(rename_map)) {
        for (alias in rename_map[[target]]) {
          if (alias %in% names(res)) names(res)[names(res) == alias] <- target
        }
      }

      req_priors  <- c("unit_id", "probability", "sweep_width", "area")
      req_results <- c("unit_id", "l_walked_today", "success")

      if (any(!req_priors %in% names(pri)))
        stop("Missing required priors columns: ", paste(setdiff(req_priors, names(pri)), collapse = ", "))
      if (any(!req_results %in% names(res)))
        stop("Missing required results columns: ", paste(setdiff(req_results, names(res)), collapse = ", "))

      update_tbl <- pri |>
        dplyr::left_join(res, by = "unit_id") |>
        dplyr::select(unit_id, l_walked_today, success, probability, sweep_width, area) |>
        dplyr::arrange(unit_id)

      update_tbl
    }

    # Helper: Apply posteriors back onto full priors table
    .apply_posteriors_to_priors <- function(priors_df, posterior_summary) {
      pri_std <- std_names(priors_df)
      names(pri_std) <- tolower(names(pri_std))

      post_tbl <- posterior_summary |>
        dplyr::transmute(unit_id  = as.character(unit_id),
                         post_prob = as.numeric(post_prob))

      updated <- pri_std |>
        dplyr::mutate(unit_id     = as.character(unit_id),
                      probability = suppressWarnings(as.numeric(probability))) |>
        dplyr::left_join(post_tbl, by = "unit_id") |>
        dplyr::mutate(probability = dplyr::coalesce(post_prob, probability)) |>
        dplyr::select(-post_prob)

      core    <- c("unit_id", "area", "probability", "sweep_width", "visibility")
      ordered <- c(intersect(core, names(updated)),
                   setdiff(names(updated), core))
      updated[, ordered, drop = FALSE]
    }

    observeEvent(input$compute_post_btn, {
      tryCatch({
        message("Computing posterior probabilities...")
        req(input$posterior_inputs_file, input$results_file)

        priors_df  <- .read_any(input$posterior_inputs_file$datapath, input$posterior_inputs_file$name)
        results_df <- .read_any(input$results_file$datapath,           input$results_file$name)

        rv$posterior_inputs <- priors_df

        update_tbl      <- .build_update_table(priors_df, results_df)
        rv$update_table <- update_tbl

        post_summary <- compute_posteriors(update_tbl)

        surveyed_units <- unique(update_tbl$unit_id[!is.na(update_tbl$l_walked_today)])
        post_summary   <- post_summary |>
          dplyr::mutate(updated = unit_id %in% surveyed_units)

        rv$posteriors     <- post_summary
        rv$updated_priors <- .apply_posteriors_to_priors(priors_df, post_summary)

        showNotification("Posterior probabilities computed successfully.", type = "message")
        updateTabsetPanel(session, "mainTabs", selected = "Posteriors")
        message("Posterior computation complete.")

      }, error = function(e) {
        showNotification(paste("Error computing posteriors:", e$message), type = "error")
        show_error_modal(session, "Posterior Update Error", e$message)
        message("Error computing posteriors: ", e$message)
      })
    })

    # =========================================================================
    # Download handlers
    # =========================================================================

    # Download single Excel workbook
    output$dl_alloc_xlsx <- downloadHandler(
      filename = function() paste0("allocations_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(rv$day1_alloc)
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "allocations")
        openxlsx::writeData(wb, "allocations", rv$day1_alloc)
        if (!is.null(rv$day1_dropped) && nrow(rv$day1_dropped) > 0) {
          openxlsx::addWorksheet(wb, "dropped_log")
          openxlsx::writeData(wb, "dropped_log", rv$day1_dropped)
        }
        if (!is.null(rv$day1_params)) {
          openxlsx::addWorksheet(wb, "params")
          openxlsx::writeData(wb, "params", rv$day1_params)
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    # Download full bundle (zip)
    output$dl_alloc_zip <- downloadHandler(
      filename = function() paste0("allocation_bundle_", Sys.Date(), ".zip"),
      content  = function(file) {
        out_dir <- output_dir_reactive()
        if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

        if (!is.null(rv$day1_alloc))
          readr::write_csv(rv$day1_alloc,    file.path(out_dir, "allocations.csv"))
        if (!is.null(rv$day1_dropped) && nrow(rv$day1_dropped) > 0)
          readr::write_csv(rv$day1_dropped,  file.path(out_dir, "dropped_log.csv"))
        if (!is.null(rv$day1_params))
          readr::write_csv(rv$day1_params,   file.path(out_dir, "params.csv"))

        files <- list.files(out_dir, full.names = TRUE)
        zip::zipr(file, files)
      }
    )

    # Download posteriors CSV
    output$dl_post_csv <- downloadHandler(
      filename = function() paste0("posteriors_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(rv$updated_priors)
        readr::write_csv(rv$updated_priors, file)
      }
    )

    # =========================================================================
    # Output tables
    # =========================================================================

    output$inputs_tbl <- DT::renderDT({
      req(rv$latest_inputs)
      DT::datatable(rv$latest_inputs, options = list(pageLength = 15))
    })

    output$results_tbl <- DT::renderDT({
      req(rv$posterior_inputs)
      DT::datatable(rv$posterior_inputs, options = list(pageLength = 15))
    })

    output$day1_alloc_tbl <- DT::renderDT({
      req(rv$day1_alloc)
      df <- rv$day1_alloc

      # Keep display columns; rename rec_L_of_transect -> allocation
      keep_cols <- intersect(
        c("unit_id", "area", "probability", "sweep_width", "visibility", "rec_L_of_transect"),
        names(df)
      )
      df <- df[, keep_cols, drop = FALSE]
      colnames(df)[colnames(df) == "rec_L_of_transect"] <- "allocation"

      if ("area"       %in% names(df) && is.numeric(df$area))       df$area       <- round(df$area, 2)
      if ("allocation" %in% names(df) && is.numeric(df$allocation)) df$allocation <- round(df$allocation, 2)

      DT::datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
    })

    output$day1_dropped_tbl <- DT::renderDT({
      req(rv$day1_dropped)
      df <- rv$day1_dropped

      keep_cols <- intersect(
        c("unit_id", "area", "probability", "sweep_width", "visibility", "rec_L_of_transect", "reason_dropped"),
        names(df)
      )
      df <- df[, keep_cols, drop = FALSE]
      if ("rec_L_of_transect" %in% names(df))
        colnames(df)[colnames(df) == "rec_L_of_transect"] <- "allocation"

      if ("area"       %in% names(df) && is.numeric(df$area))       df$area       <- round(df$area, 2)
      if ("allocation" %in% names(df) && is.numeric(df$allocation)) df$allocation <- round(df$allocation, 2)

      DT::datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
    })

    output$posteriors_tbl <- DT::renderDT({
      req(rv$posteriors)
      df <- rv$posteriors

      if (!is.null(input$posterior_filter) &&
          input$posterior_filter == "Show only updated units") {
        if ("updated" %in% names(df)) {
          df <- df[df$updated == TRUE, , drop = FALSE]
        }
      }

      if ("prior_prob" %in% names(df) && is.numeric(df$prior_prob))
        df$prior_prob <- round(df$prior_prob, 2)
      if ("post_prob"  %in% names(df) && is.numeric(df$post_prob))
        df$post_prob  <- round(df$post_prob, 2)

      if ("updated" %in% names(df)) {
        updated_col_index <- which(names(df) == "updated") - 1  # 0-based for DT
        DT::datatable(
          df,
          caption  = htmltools::tags$caption(
            style = "caption-side: top; text-align: left; font-weight: 600; color: #1E3765;",
            "Posterior probabilities summary (prior vs updated)"
          ),
          options  = list(
            pageLength  = 10,
            autoWidth   = TRUE,
            columnDefs  = list(list(targets = updated_col_index, visible = FALSE))
          ),
          rownames = FALSE
        )
      } else {
        DT::datatable(
          df,
          caption  = htmltools::tags$caption(
            style = "caption-side: top; text-align: left; font-weight: 600; color: #1E3765;",
            "Posterior probabilities summary (prior vs updated)"
          ),
          options  = list(pageLength = 10, autoWidth = TRUE),
          rownames = FALSE
        )
      }
    })

    # =========================================================================
    # Dynamic sample data / template download buttons (Instructions tab)
    # =========================================================================

    observe({
      output$sample_data_downloads <- renderUI({
        req(input$sample_data_select)
        if (input$sample_data_select == "Initial Inputs Sample") {
          tagList(
            downloadButton(ns("download_inputs_sample_csv"),  "Download CSV",  class = "btn-sm btn-primary"),
            downloadButton(ns("download_inputs_sample_xlsx"), "Download XLSX", class = "btn-sm btn-primary")
          )
        } else {
          tagList(
            downloadButton(ns("download_field_results_sample_csv"),  "Download CSV",  class = "btn-sm btn-primary"),
            downloadButton(ns("download_field_results_sample_xlsx"), "Download XLSX", class = "btn-sm btn-primary")
          )
        }
      })

      output$template_data_downloads <- renderUI({
        req(input$template_data_select)
        if (input$template_data_select == "Initial Inputs Template") {
          tagList(
            downloadButton(ns("download_inputs_template_csv"),  "Download CSV",  class = "btn-sm btn-success"),
            downloadButton(ns("download_inputs_template_xlsx"), "Download XLSX", class = "btn-sm btn-success")
          )
        } else {
          tagList(
            downloadButton(ns("download_field_results_template_csv"),  "Download CSV",  class = "btn-sm btn-success"),
            downloadButton(ns("download_field_results_template_xlsx"), "Download XLSX", class = "btn-sm btn-success")
          )
        }
      })
    })

    output$download_inputs_sample_csv <- downloadHandler(
      filename = function() "inputs_sample.csv",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "inputs_sample.csv"), file)
      }
    )
    output$download_inputs_sample_xlsx <- downloadHandler(
      filename = function() "inputs_sample.xlsx",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "inputs_sample.xlsx"), file)
      }
    )
    output$download_field_results_sample_csv <- downloadHandler(
      filename = function() "field_results_sample.csv",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "field_results_sample.csv"), file)
      }
    )
    output$download_field_results_sample_xlsx <- downloadHandler(
      filename = function() "field_results_sample.xlsx",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "field_results_sample.xlsx"), file)
      }
    )
    output$download_inputs_template_csv <- downloadHandler(
      filename = function() "inputs_template.csv",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "inputs_template.csv"), file)
      }
    )
    output$download_inputs_template_xlsx <- downloadHandler(
      filename = function() "inputs_template.xlsx",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "inputs_template.xlsx"), file)
      }
    )
    output$download_field_results_template_csv <- downloadHandler(
      filename = function() "field_results.csv",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "field_results.csv"), file)
      }
    )
    output$download_field_results_template_xlsx <- downloadHandler(
      filename = function() "field_results.xlsx",
      content  = function(file) {
        file.copy(file.path("www", "allocation", "field_results.xlsx"), file)
      }
    )

    # =========================================================================
    # Map server logic
    # =========================================================================

    # Bridge rv$is_spatial to JS for conditionalPanel
    output$is_spatial_flag <- reactive({
      isTRUE(rv$is_spatial)
    })
    outputOptions(output, "is_spatial_flag", suspendWhenHidden = FALSE)

    # Helper: prepare sf object for leaflet rendering
    prepare_sf <- function(sf_in) {
      sf::st_make_valid(sf_in)
    }

    # Render base map (rebuilds when rv$sf_pre_in changes)
    output$survey_map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() |>
        leaflet::addTiles(group = "OpenStreetMap") |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") |>
        leaflet::setView(lng = 0, lat = 20, zoom = 2)

      sf_data <- rv$sf_pre_in
      if (!is.null(sf_data)) {
        tryCatch({
          sf_obj <- prepare_sf(sf_data)
          bbox   <- sf::st_bbox(sf_obj)

          popup_text <- paste0(
            "<b>Unit ID:</b> ", sf_obj$unit_id, "<br>",
            "<b>Area:</b> ",    sf_obj$area
          )

          m <- m |>
            leaflet::addPolygons(
              data        = sf_obj,
              fillColor   = "#4a7fb5",
              fillOpacity = 0.4,
              color       = "#1E3765",
              weight      = 1,
              popup       = popup_text
            ) |>
            leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
        }, error = function(e) {
          showNotification(paste("Map render error:", e$message), type = "error")
        })
      }

      m
    })
    outputOptions(output, "survey_map", suspendWhenHidden = FALSE)

    # Basemap toggle
    observeEvent(input$basemap_choice, {
      proxy <- leaflet::leafletProxy(ns("survey_map"), session)
      if (input$basemap_choice == "CartoDB Positron") {
        proxy |> leaflet::clearTiles() |> leaflet::addProviderTiles("CartoDB.Positron")
      } else {
        proxy |> leaflet::clearTiles() |> leaflet::addTiles()
      }
    }, ignoreInit = TRUE)

    # Choropleth update when posteriors become available
    observeEvent(rv$posteriors, {
      req(rv$posteriors, rv$sf_pre_in)
      tryCatch({
        post_df <- rv$posteriors[, c("unit_id", "post_prob")]
        sf_obj  <- prepare_sf(rv$sf_pre_in)

        sf_joined <- merge(sf_obj, post_df, by = "unit_id", all.x = TRUE)

        pal <- leaflet::colorNumeric(
          palette  = "YlOrRd",
          domain   = sf_joined$post_prob,
          na.color = "#cccccc"
        )

        popup_text <- paste0(
          "<b>Unit ID:</b> ",        sf_joined$unit_id, "<br>",
          "<b>Area:</b> ",           sf_joined$area,    "<br>",
          "<b>Post. Probability:</b> ", round(sf_joined$post_prob, 4)
        )

        leaflet::leafletProxy(ns("survey_map"), session) |>
          leaflet::clearShapes()  |>
          leaflet::clearControls() |>
          leaflet::addPolygons(
            data        = sf_joined,
            fillColor   = ~pal(post_prob),
            fillOpacity = 0.7,
            color       = "#555555",
            weight      = 1,
            popup       = popup_text
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal      = pal,
            values   = sf_joined$post_prob,
            title    = "Posterior<br>Probability",
            opacity  = 0.8
          )
      }, error = function(e) {
        showNotification(paste("Choropleth update error:", e$message), type = "error")
      })
    }, ignoreNULL = TRUE)

    # -------------------------------------------------------------------------
    # Return nav_home signal
    # -------------------------------------------------------------------------

    return(list(nav_home = nav_home))

  }) # end moduleServer
}
