# =====================================================================
#  SPACE — Landing Page UI
#  Generates the module-selection card grid from the SPACE_MODULES registry.
# =====================================================================

#' Landing page UI — card grid of registered modules.
#'
#' @param modules  The SPACE_MODULES list from global.R.
#' @return A tagList suitable for embedding in the top-level UI.
landingPageUI <- function(modules) {

  # Build one card per module
  cards <- lapply(modules, function(mod) {
    column(
      width = 4,
      tags$div(
        class = "space-card",
        tags$div(
          class = "space-card-body",
          tags$h4(class = "space-card-title", mod$title),
          tags$p(class  = "space-card-desc",  mod$description),
          actionButton(
            inputId = paste0("go_to_", mod$id),
            label   = "Open Module",
            class   = "custom-btn space-card-btn"
          )
        )
      )
    )
  })

  tagList(
    # ---- Header bar (mirrors module header style) -------------------------
    tags$div(
      class = "header-bar",
      tags$div(
        class = "header-inner",
        tags$img(
          src = "space_banner.png",
          alt = "SPACE"
        ),
        tags$div(
          tags$p(
            class = "header-subtitle",
            "Survey Planning, Allocation, Costing and Evaluation"
          )
        )
      )
    ),

    # ---- Card grid --------------------------------------------------------
    tags$div(
      style = "padding: 40px 30px;",
      fluidRow(cards)
    )
  )
}
