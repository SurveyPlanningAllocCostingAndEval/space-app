# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Server logic for assigning sweep widths to units based
#               on user inputs or visibility category selections
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Handles user-driven updates to sweep-width values.
#   - Ensures consistency across visibility classes and allocation inputs.
# =====================================================================

assign_sweep_widths_server <- function(rv, input, session) {
  
  observeEvent(input$update_sweep_btn, {
    req(rv$df_pre_in)
    df <- rv$df_pre_in
    
    if (!"visibility" %in% names(df)) {
      showNotification("No 'visibility' column found in uploaded data.", type = "error")
      return()
    }
    
    vis_classes <- unique(df$visibility)
    sweep_defaults <- sapply(vis_classes, function(v) {
      val <- df$sweep_width[df$visibility == v][1]
      ifelse(!is.na(val), val, NA)
    })
    
    showModal(modalDialog(
      title = "Assign or Update Sweep Widths by Visibility Category",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_sweep", "Save / Confirm", class = "btn-primary")
      ),
      fluidPage(
        lapply(seq_along(vis_classes), function(i) {
          fluidRow(
            column(6, strong(vis_classes[i])),
            column(6, numericInput(
              inputId = paste0("sweep_", i),
              label = NULL,
              value = sweep_defaults[i],
              min = 0, step = 0.1
            ))
          )
        })
      )
    ))
    
    # Confirm handler
    observeEvent(input$confirm_sweep, {
      new_vals <- sapply(seq_along(vis_classes),
                         function(i) input[[paste0("sweep_", i)]])
      names(new_vals) <- vis_classes
      
      df$sweep_width <- vapply(df$visibility, function(v) new_vals[v], numeric(1))
      rv$df_pre_in <- df
      removeModal()
      showNotification("Sweep widths updated successfully.", type = "message")
    }, once = TRUE)
  })
}
