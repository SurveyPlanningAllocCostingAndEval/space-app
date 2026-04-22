# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Server-side logic for displaying output tables,
#               including allocations, dropped units, and results summaries
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
# =====================================================================

outputs_server <- function(rv, input, output) {
  
  # Helper for consistent DT formatting
  render_clean_dt <- function(df, caption = NULL, page_len = 15) {
    datatable(
      df,
      caption = if (!is.null(caption)) htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: 600; color: #1E3765;",
        caption
      ),
      options = list(pageLength = page_len, scrollX = TRUE),
      rownames = FALSE
    )
  }
  
  # Allocation workflow
  output$day1_alloc_tbl <- renderDT({
    req(rv$day1_alloc)
    df <- rv$day1_alloc
    
    # Keep only the selected columns and rename rec_L_of_transect -> allocation
    df <- df[, c("unit_id", "area", "probability", "sweep_width", "visibility", "rec_L_of_transect"), drop = FALSE]
    colnames(df)[colnames(df) == "rec_L_of_transect"] <- "allocation"
    
    # Round area and allocation to 2 decimal places if numeric
    if ("area" %in% names(df) && is.numeric(df$area)) {
      df$area <- round(df$area, 2)
    }
    if ("allocation" %in% names(df) && is.numeric(df$allocation)) {
      df$allocation <- round(df$allocation, 2)
    }
    
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  
  output$day1_dropped_tbl <- renderDT({
    req(rv$day1_dropped)
    df <- rv$day1_dropped
    
    keep_cols <- c("unit_id", "area", "probability", "sweep_width", "visibility", "rec_L_of_transect", "reason_dropped")
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    df <- df[, keep_cols, drop = FALSE]
    
    if ("rec_L_of_transect" %in% names(df)) {
      colnames(df)[colnames(df) == "rec_L_of_transect"] <- "allocation"
    }
    
    if ("area" %in% names(df) && is.numeric(df$area)) {
      df$area <- round(df$area, 2)
    }
    if ("allocation" %in% names(df) && is.numeric(df$allocation)) {
      df$allocation <- round(df$allocation, 2)
    }
    
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  
  # Posterior update workflow
  output$posteriors_tbl <- renderDT({
    req(rv$posteriors)
    df <- rv$posteriors
    
    # Apply dropdown filter (Show all units / Show only updated units)
    if (!is.null(input$posterior_filter) && input$posterior_filter == "Show only updated units") {
      if ("updated" %in% names(df)) {
        df <- df[df$updated == TRUE, , drop = FALSE]
      } else if ("surveyed" %in% names(df)) {
        df <- df[df$surveyed == TRUE, , drop = FALSE]
      } else if ("was_surveyed" %in% names(df)) {
        df <- df[df$was_surveyed == TRUE, , drop = FALSE]
      }
    }
    
    # Round numeric columns (prior_prob and post_prob) to 2 decimal places
    if ("prior_prob" %in% names(df) && is.numeric(df$prior_prob)) {
      df$prior_prob <- round(df$prior_prob, 2)
    }
    if ("post_prob" %in% names(df) && is.numeric(df$post_prob)) {
      df$post_prob <- round(df$post_prob, 2)
    }
    
    # Hide the "updated" column if it exists
    if ("updated" %in% names(df)) {
      updated_col_index <- which(names(df) == "updated") - 1  # 0-based index for DT
      datatable(
        df,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-weight: 600; color: #1E3765;",
          "Posterior probabilities summary (prior vs updated)"
        ),
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = list(list(targets = updated_col_index, visible = FALSE))
        ),
        rownames = FALSE
      )
    } else {
      datatable(
        df,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-weight: 600; color: #1E3765;",
          "Posterior probabilities summary (prior vs updated)"
        ),
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
    }
  })
  
}
