# =============================================================================
# tab_esw_results.R
# Sweep Width Calibration App
#
# Purpose:
#   UI definition for the ESW Results tab. Provides controls for selecting
#   which sides to compute (Left, Right, Total), optional regression starting
#   value controls, a run button, summary results table, fitted detection
#   function plots, and the combined left/right comparison plot.
# =============================================================================

tab_esw_results <- tabPanel(
  title = "ESW Results",
  icon  = icon("chart-line"),
  
  fluidPage(
    
    # -------------------------------------------------------------------------
    # Header
    # -------------------------------------------------------------------------
    
    tags$h3("Effective Sweep Width Estimation"),
    tags$p(
      "This step fits a Gaussian detection function to the classified
      calibration data and computes the Effective Sweep Width (ESW).
      The detection function takes the form:"
    ),
    withMathJax(
      tags$p(
        class = "text-center",
        "$$p(r) = b \\cdot e^{-kr^2}$$"
      )
    ),
    tags$p(
      "where ", tags$em("r"), " is the perpendicular distance from the
      transect (m), ", tags$em("b"), " is the detection probability at the
      transect line, and ", tags$em("k"), " is the decay constant. Sweep
      width is computed as the area under this curve:"
    ),
    withMathJax(
      tags$p(
        class = "text-center",
        "$$W = b\\sqrt{\\frac{\\pi}{k}}$$"
      )
    ),
    tags$p(
      "Parameters are estimated by constrained nonlinear least squares
      (Levenberg-Marquardt algorithm via ",
      tags$code("minpack.lm"), ") following Banning et al. (2017)."
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Controls
    # -------------------------------------------------------------------------
    
    tags$h4("Estimation Controls"),
    
    fluidRow(
      
      column(4,
             checkboxGroupInput(
               inputId  = "esw_sides",
               label    = "Compute ESW for:",
               choices  = c("Left", "Right", "Total"),
               selected = c("Left", "Right")
             )
      ),
      
      column(4,
             tags$h5("Regression starting values"),
             tags$p(class = "text-muted", tags$small(
               "In most cases the defaults will work. Adjust only if the
          model fails to converge."
             )),
             numericInput(
               inputId = "b_start",
               label   = "Starting value for b",
               value   = 0.5,
               min     = 0.01,
               max     = 1.00,
               step    = 0.05
             ),
             numericInput(
               inputId = "k_start",
               label   = "Starting value for k",
               value   = 0.05,
               min     = 0.001,
               max     = 1.00,
               step    = 0.005
             )
      ),
      
      column(4,
             tags$br(),
             tags$br(),
             actionButton(
               inputId = "run_esw",
               label   = "Compute ESW",
               icon    = icon("calculator"),
               class   = "btn btn-success btn-block"
             )
      )
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Results summary
    # -------------------------------------------------------------------------
    
    tags$h4("Results Summary"),
    uiOutput("esw_messages"),
    tags$br(),
    tableOutput("esw_summary_table"),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Detection function plots (one panel per side)
    # -------------------------------------------------------------------------
    
    tags$h4("Fitted Detection Functions"),
    tags$p(class = "text-muted",
           "Observed detection probabilities (points) and the fitted Gaussian
      curve (line) for each selected side. The shaded region represents
      the area under the curve. The dashed red line marks W/2, the
      one-sided half sweep width."),
    
    plotOutput("detection_function_plot", height = "420px"),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Combined left/right plot
    # Only shown when both Left and Right are selected and converged.
    # -------------------------------------------------------------------------
    
    tags$h4("Combined Left and Right Detection Functions"),
    tags$p(class = "text-muted",
           "Left-side distances are mirrored onto the negative x-axis to
      produce a bilateral view of the transect. Dashed lines mark the
      W/2 boundary for each side. Total ESW is the sum of the two
      half-widths. This panel is only rendered when both Left and Right
      results are available."),
    
    plotOutput("combined_plot", height = "420px")
  )
)