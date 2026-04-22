# =====================================================================
#  SPACE — Survey Planning, Allocation, Costing and Evaluation
#  Entry point: sources global.R, defines top-level UI + server.
#  Run with: shiny::runApp() from the space/ directory.
#
#  Navigation wiring (v1.0.0):
#    Landing → Module : input$go_to_<id> observer (lapply over SPACE_MODULES)
#    Module → Landing : nav_home reactive returned by each moduleServer(),
#                       observed individually below (alloc_nav, sw_nav).
#    Back buttons live inside each module UI; modules call nav_home()
#    themselves — no back_from_<id> inputs are emitted at the top level.
# =====================================================================

source(file.path("global.R"))
source(file.path("R", "landing_page.R"))

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  useShinyjs(),
  space_theme(),

  # Landing page (visible on startup)
  tags$div(
    id = "landing_page",
    landingPageUI(SPACE_MODULES)
  ),

  # One hidden div per module — generated from registry.
  lapply(SPACE_MODULES, function(mod) {
    tags$div(
      id    = mod$id,
      style = "display:none;",
      mod$ui_fn(mod$id)
    )
  })
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  # Landing → Module: card "Go" buttons hide the landing page and reveal
  # the target module div. IDs are generated from the SPACE_MODULES registry.
  lapply(SPACE_MODULES, function(mod) {
    local({
      m <- mod
      observeEvent(input[[paste0("go_to_", m$id)]], {
        shinyjs::hide("landing_page")
        shinyjs::show(m$id)
      })
    })
  })

  alloc_nav <- allocationServer("allocation")

  observeEvent(alloc_nav$nav_home(), {
    req(alloc_nav$nav_home() > 0)
    shinyjs::hide("allocation")
    shinyjs::show("landing_page")
  })

  sw_nav <- sweepWidthServer("sweep_width")

  observeEvent(sw_nav$nav_home(), {
    req(sw_nav$nav_home() > 0)
    shinyjs::hide("sweep_width")
    shinyjs::show("landing_page")
  })
}

# ============================================================
# Launch
# ============================================================

shinyApp(ui = ui, server = server)
