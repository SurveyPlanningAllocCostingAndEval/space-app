# =============================================================================
#  mod_sweep_width_ui.R
#  SPACE — Sweep Width Estimation module UI
# =============================================================================

sweepWidthUI <- function(id) {
  ns <- NS(id)

  tagList(

    # -------------------------------------------------------------------------
    # Header bar
    # -------------------------------------------------------------------------
    div(
      class = "header-bar",
      tags$div(
        class = "header-inner",
        actionLink(
          inputId = ns("logo_home"),
          label   = tags$img(src = "space_banner.png", alt = "SPACE"),
          class   = "header-logo-link"
        ),
        tags$div(
          tags$h2(
            class = "header-subtitle",
            "Sweep Width Estimator"
          )
        ),
        tags$div(
          style = "flex: 0 0 auto; display: flex; align-items: center; gap: 10px;",
          actionButton(
            inputId = ns("reset_module"),
            label   = "\u21ba Reset Module",
            class   = "btn btn-reset-module"
          ),
          actionButton(
            inputId = ns("back_home"),
            label   = "\u2190 Back",
            class   = "btn btn-default",
            style   = "color: white; background-color: transparent; border-color: rgba(255,255,255,0.5); font-weight: 600;"
          )
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Main content
    # -------------------------------------------------------------------------
    tags$div(
      style = "padding: 15px;",

      tabsetPanel(
        id   = ns("main_tabs"),
        type = "tabs",

        # --- Data Input tab --------------------------------------------------
        tabPanel(
          title = "Data Input",
          icon  = icon("upload"),

          div(
            class = "alloc-layout",

            # Sidebar
            div(
              class = "alloc-sidebar",
              div(
                class = "sidebar-panel",
                helpText(HTML(
                  "Upload calibration CSV files, then review previews before proceeding."
                )),

                # Step 1: Upload Master File
                div(
                  class = "step-label",
                  tags$span("Step 1: Upload Master File"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Contains known seeded artifact locations. Required columns: LDist, Dist, LorR, Type. Distances must be positive; side is indicated by LorR."
                  )
                ),
                fileInput(
                  inputId     = ns("master_file"),
                  label       = NULL,
                  accept      = ".csv",
                  placeholder = "No file selected"
                ),
                uiOutput(ns("master_feedback")),

                hr(),

                # Step 2: Upload Calibration Records
                div(
                  class = "step-label",
                  tags$span("Step 2: Upload Calibration Records"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Contains the surveyor's reported detections. Same column requirements as Master file: LDist, Dist, LorR, Type."
                  )
                ),
                fileInput(
                  inputId     = ns("records_file"),
                  label       = NULL,
                  accept      = ".csv",
                  placeholder = "No file selected"
                ),
                uiOutput(ns("records_feedback")),

                div(
                  class = "step-label",
                  tags$span("Number of survey runs"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "How many independent surveyor passes are pooled in the records file? Leave at 1 if records represent a single pass."
                  )
                ),
                numericInput(
                  inputId = ns("n_runs"),
                  label   = NULL,
                  value   = 1,
                  min     = 1,
                  step    = 1
                ),

                hr(),

                # Step 3: Load Sample Data
                div(
                  class = "step-label",
                  tags$span("Step 3: Load Sample Data")
                ),
                actionButton(
                  inputId = ns("load_sample"),
                  label   = "Load Sample Data",
                  icon    = icon("database"),
                  class   = "custom-btn"
                )
              )
            ),

            # Main panel
            div(
              class = "alloc-main",
              div(class = "main-heading", "Calibration Data Preview"),
              tabsetPanel(
                tabPanel(
                  "Data Tables",
                  br(),
                  fluidRow(
                    column(6,
                      tags$h5("Master (first 10 rows)"),
                      tableOutput(ns("master_preview"))
                    ),
                    column(6,
                      tags$h5("Calibration Records (first 10 rows)"),
                      tableOutput(ns("records_preview"))
                    )
                  )
                ),
                tabPanel(
                  "Spatial Plots",
                  br(),
                  fluidRow(
                    column(6,
                      plotOutput(ns("master_plot"), height = "400px")
                    ),
                    column(6,
                      plotOutput(ns("records_plot"), height = "400px")
                    )
                  )
                )
              )
            )
          )
        ),

        # --- Classification tab ----------------------------------------------
        tabPanel(
          title = "Classification",
          icon  = icon("circle-check"),

          div(
            class = "alloc-layout",

            # Sidebar
            div(
              class = "alloc-sidebar",
              div(
                class = "sidebar-panel",
                helpText(HTML(
                  "Classify detections using elliptical tolerance zones. Adjust tolerance parameters, then click Run Classification."
                )),

                # Step 1: Perpendicular Scaling Factor
                div(
                  class = "step-label",
                  tags$span("Step 1: Perpendicular Scaling Factor"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Fraction of reported perpendicular distance used as the ellipse semi-major axis. Default: 0.20 (20%). Increase if too many genuine detections are rejected."
                  )
                ),
                sliderInput(
                  inputId = ns("perp_scale"),
                  label   = NULL,
                  min     = 0.05,
                  max     = 0.50,
                  value   = 0.20,
                  step    = 0.01
                ),

                hr(),

                # Step 2: Along-Transect Fixed Tolerance
                div(
                  class = "step-label",
                  tags$span("Step 2: Along-Transect Fixed Tolerance (m)"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Fixed semi-minor axis of the tolerance ellipse along the transect. Default: 1.0 m. Surveyor along-transect position is measured against a tape and is treated as approximately fixed."
                  )
                ),
                sliderInput(
                  inputId = ns("along_fixed"),
                  label   = NULL,
                  min     = 0.25,
                  max     = 3.00,
                  value   = 1.00,
                  step    = 0.25
                ),

                hr(),

                # Step 3: Run Classification
                div(
                  class = "step-label",
                  tags$span("Step 3: Run Classification")
                ),
                actionButton(
                  inputId = ns("run_classification"),
                  label   = "Run Classification",
                  icon    = icon("play"),
                  class   = "custom-btn"
                ),
                br(),
                uiOutput(ns("classification_feedback"))
              )
            ),

            # Main panel
            div(
              class = "alloc-main",
              div(class = "main-heading", "Classification Results"),
              tabsetPanel(
                tabPanel(
                  "Summary Table",
                  br(),
                  helpText("True and false detection counts broken down by transect side."),
                  tableOutput(ns("classification_summary_table"))
                ),
                tabPanel(
                  "Ellipse Plot",
                  br(),
                  plotOutput(ns("ellipse_plot"), height = "450px")
                ),
                tabPanel(
                  "Overlay Plot",
                  br(),
                  plotOutput(ns("overlay_plot"), height = "450px")
                )
              )
            )
          )
        ),

        # --- ESW Results tab -------------------------------------------------
        tabPanel(
          title = "ESW Results",
          icon  = icon("chart-line"),

          div(
            class = "alloc-layout",

            # Sidebar
            div(
              class = "alloc-sidebar",
              div(
                class = "sidebar-panel",
                helpText(HTML(
                  "Fits a Gaussian detection function p(r) = b\u00b7exp(\u2212kr\u00b2) to classified data. See the <strong>Model</strong> tab for the full formulation."
                )),

                # Step 1: Compute ESW for
                div(
                  class = "step-label",
                  tags$span("Step 1: Compute ESW for"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Select Left, Right, and/or Pooled (Symmetric Total) for ESW computation. Pooled Total fits one shared Gaussian curve to left+right data combined, and differs from the Bilateral Total shown on the Combined Plot (which sums independently-fit Left and Right half-widths) whenever detection performance is asymmetric between sides. See the Documentation tab for details."
                  )
                ),
                checkboxGroupInput(
                  inputId  = ns("esw_sides"),
                  label    = NULL,
                  choices  = c("Left", "Right", "Pooled (Symmetric Total)" = "Total"),
                  selected = c("Left", "Right")
                ),

                hr(),

                # Step 2: Starting value for b
                div(
                  class = "step-label",
                  tags$span("Step 2: Starting value for b"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Detection probability at the transect line. Constrained 0\u20131. Adjust only if the model fails to converge."
                  )
                ),
                numericInput(
                  inputId = ns("b_start"),
                  label   = NULL,
                  value   = 0.5,
                  min     = 0.01,
                  max     = 1.00,
                  step    = 0.05
                ),

                # Step 3: Starting value for k
                div(
                  class = "step-label",
                  tags$span("Step 3: Starting value for k"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Decay constant. Must be > 0. Adjust only if the model fails to converge."
                  )
                ),
                numericInput(
                  inputId = ns("k_start"),
                  label   = NULL,
                  value   = 0.05,
                  min     = 0.001,
                  max     = 1.00,
                  step    = 0.005
                ),

                hr(),

                # Step 4: SE / CI Method
                div(
                  class = "step-label",
                  tags$span("Step 4: SE / CI Method for W"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Method used to estimate the standard error and 95% confidence interval of W. Delta method: fast, uses a Taylor-series approximation around the fitted b and k. Bootstrap: slower (refits the model 1000 times via case resampling), does not rely on asymptotic normality."
                  )
                ),
                radioButtons(
                  inputId  = ns("se_method"),
                  label    = NULL,
                  choices  = c("Delta method" = "Delta", "Bootstrap" = "Bootstrap"),
                  selected = "Delta"
                ),

                hr(),

                # Step 5: Compute ESW
                div(
                  class = "step-label",
                  tags$span("Step 5: Compute ESW")
                ),
                actionButton(
                  inputId = ns("run_esw"),
                  label   = "Compute ESW",
                  icon    = icon("calculator"),
                  class   = "custom-btn"
                )
              )
            ),

            # Main panel
            div(
              class = "alloc-main",
              div(class = "main-heading", "Effective Sweep Width Estimation"),
              tabsetPanel(
                tabPanel(
                  "Results",
                  br(),
                  uiOutput(ns("esw_messages")),
                  br(),
                  tableOutput(ns("esw_summary_table")),
                  tags$div(
                    class = "esw-table-legend",
                    tags$strong("Column reference: "),
                    tags$dl(
                      tags$dt("Side"),       tags$dd("Left, Right, Pooled Total (one curve fit to combined data), or Bilateral Total (sum of independently-fit Left/Right halves) — see Documentation tab."),
                      tags$dt("n_runs"),     tags$dd("Number of pooled surveyor passes used for this fit."),
                      tags$dt("ESW_m"),      tags$dd("Point estimate of the Effective Sweep Width W, in metres."),
                      tags$dt("W_SE"),       tags$dd("Standard error of W, from the selected SE/CI method."),
                      tags$dt("W_95CI_m"),   tags$dd("95% confidence interval for W."),
                      tags$dt("SE_Method"),  tags$dd("How W_SE / W_95CI_m were computed: Delta method, Bootstrap, or Derived (for the Bilateral Total row, propagated from the Left and Right SEs)."),
                      tags$dt("Skewed"),     tags$dd("\"Yes\" if the point estimate falls outside the central 50% of the bootstrap distribution — a sign the sampling distribution of W may be skewed (Bootstrap method only)."),
                      tags$dt("b, k"),       tags$dd("Fitted detection-function parameters: b = detection probability at the transect line; k = decay constant."),
                      tags$dt("b_SE, k_SE"), tags$dd("Standard errors of b and k, from the nonlinear fit's covariance matrix."),
                      tags$dt("Converged"),  tags$dd("Whether the nonlinear regression fit succeeded for this row.")
                    )
                  )
                ),
                tabPanel(
                  "Detection Functions",
                  br(),
                  helpText(
                    "Observed detection probabilities (points) and the fitted Gaussian
                    curve (line) for each selected side. The blue shaded region
                    represents the area under the curve (this is a visual aid, not
                    an uncertainty measure). The red shaded band is the ±1 SE
                    envelope around the fitted curve, computed using the SE/CI
                    method selected in Step 4. The dashed red line marks W/2, the
                    one-sided half sweep width."
                  ),
                  plotOutput(ns("detection_function_plot"), height = "420px")
                ),
                tabPanel(
                  "Combined Plot",
                  br(),
                  helpText(
                    "Left-side distances are mirrored onto the negative x-axis to
                    produce a bilateral view of the transect. Dashed lines mark the
                    W/2 boundary for each side. The Bilateral Total ESW shown here
                    is the sum of the two independently-fit half-widths (L/2 + R/2),
                    which differs from the Pooled Total (one shared curve fit to
                    left+right data combined, available as an option in Step 1)
                    whenever detection performance is asymmetric between sides —
                    see the Documentation tab. This panel is only rendered when
                    both Left and Right results are available."
                  ),
                  plotOutput(ns("combined_plot"), height = "420px")
                ),
                tabPanel(
                  "Model",
                  br(),
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
                  )
                )
              )
            )
          )
        ),

        # --- Documentation tab -----------------------------------------------
        tabPanel(
          title = "Documentation",
          icon  = icon("circle-info"),

          fluidPage(

            fluidRow(
              column(8, offset = 2,

                     tags$h3("About This App"),
                     tags$p(
                       "The Sweep Width Calibration App automates the estimation of
                Effective Sweep Width (ESW) for archaeological pedestrian surveys.
                It is an R Shiny implementation of the workflow described in
                Banning et al. (2017) and builds on the earlier Python module
                developed by Milwid (2016, 2017)."
                     ),

                     tags$hr(),

                     tags$h4("What is Effective Sweep Width?"),
                     tags$p(
                       "Effective Sweep Width (ESW) is the corridor width within which a
                surveyor's detection performance can be summarised. Formally, it is
                the distance at which the number of undetected artifacts inside that
                range equals the number of detected artifacts beyond it. ESW
                integrates the effects of surveyor ability, artifact type,
                visibility, and field conditions into a single, interpretable number."
                     ),
                     tags$p(
                       "ESW can be used to calculate survey coverage (area swept = ESW \u00d7
                transect length), to set evidence-based transect spacing, and to
                compare surveyor performance across conditions and individuals."
                     ),

                     tags$hr(),

                     tags$h4("Workflow"),
                     tags$ol(
                       tags$li(tags$strong("Data Input: "),
                               "Upload a Master CSV (known seeded artifact locations) and a
                  Calibration Records CSV (surveyor detections). Both files must
                  contain columns: LDist, Dist, LorR, and Type."
                       ),
                       tags$li(tags$strong("Classification: "),
                               "Each surveyor detection is tested against the known artifact
                  locations using an elliptical tolerance zone. The zone is wider
                  in the perpendicular direction (scaling with reported distance)
                  and narrower along the transect (fixed), reflecting the
                  asymmetry of distance estimation error."
                       ),
                       tags$li(tags$strong("ESW Estimation: "),
                               "Detections are aggregated into observed probabilities by
                  distance bin. A Gaussian detection function p(r) = b\u00b7exp(\u2212kr\u00b2)
                  is fitted by constrained nonlinear least squares, and ESW is
                  computed as W = b\u221a(\u03c0/k). A standard error and 95% confidence
                  interval for W are obtained via either the delta method or a
                  bootstrap (user-selectable) \u2014 see the Technical Notes section
                  below."
                       )
                     ),

                     tags$hr(),

                     tags$h4("Pooled Total vs. Bilateral Total"),
                     tags$p(
                       "The word \u201cTotal\u201d refers to two different, deliberately
                separate calculations in this module \u2014 both are legitimate, but
                they answer different questions and will only be numerically
                equal when Left and Right detection performance happens to be
                symmetric:"
                     ),
                     tags$ul(
                       tags$li(
                         tags$strong("Pooled Total "),
                         "(Step 1 checkbox, ESW Results tab): pools all Left and
                  Right detection records and seeded artifacts into a single
                  dataset, then fits ", tags$strong("one"), " shared Gaussian
                  curve p(r) = b\u00b7exp(\u2212kr\u00b2) to the combined data. This assumes
                  detection performance is symmetric between sides."
                       ),
                       tags$li(
                         tags$strong("Bilateral Total "),
                         "(Combined Plot tab, and the derived row in the ESW
                  Results summary table when both Left and Right are
                  selected): fits Left and Right ", tags$strong("independently"),
                  " as two separate models, then sums their half-widths
                  (W", tags$sub("Left"), "/2 + W", tags$sub("Right"), "/2). This
                  allows detection performance to differ by side."
                       )
                     ),
                     tags$p(
                       "If Left and Right detection performance differs
                substantially in your data, expect Pooled Total and Bilateral
                Total to diverge \u2014 this reflects a real modelling choice
                (symmetric vs. asymmetric detection assumption), not an error.
                Report whichever matches your study design, and consider
                reporting both with an explanation if the two diverge
                meaningfully."
                     ),

                     tags$hr(),

                     tags$h4("Tolerance Zone Parameters"),
                     tags$p(
                       "The elliptical tolerance zones are based on the distance
                perception research of Loomis & Philbeck (2008), who found that
                perceived egocentric distance has a linear relationship with actual
                distance with a slope of approximately 0.8, implying systematic
                underestimation that scales proportionally with distance. The
                perpendicular scaling factor (default 0.20) and along-transect
                fixed tolerance (default 1.0 m) can be adjusted in the
                Classification tab. Banning (pers. comm.) notes that the earlier
                circular tolerance implementation was too conservative; the
                elliptical approach adopted here is less likely to reject genuine
                detections at greater distances."
                     ),

                     tags$hr(),

                     tags$h4("Input Data Format"),
                     tags$p("Both CSV files must contain the following columns:"),
                     tags$table(
                       class = "table table-bordered table-sm",
                       tags$thead(tags$tr(
                         tags$th("Column"), tags$th("Type"),
                         tags$th("Description")
                       )),
                       tags$tbody(
                         tags$tr(
                           tags$td(tags$code("LDist")),
                           tags$td("Numeric"),
                           tags$td("Distance along the transect (metres)")
                         ),
                         tags$tr(
                           tags$td(tags$code("Dist")),
                           tags$td("Numeric (\u2265 0)"),
                           tags$td("Perpendicular distance from the transect (metres).
                                 Must be positive; side is indicated by LorR.")
                         ),
                         tags$tr(
                           tags$td(tags$code("LorR")),
                           tags$td("Character"),
                           tags$td("Side of the transect: must be exactly 'Left' or 'Right'")
                         ),
                         tags$tr(
                           tags$td(tags$code("Type")),
                           tags$td("Character"),
                           tags$td("Artifact category (e.g. Small Lithic, Large Potsherd)")
                         )
                       )
                     ),

                     tags$hr(),

                     tags$h4("References"),
                     tags$ul(
                       tags$li(
                         "Banning, E.B., Hawkins, A.L., Stewart, S.T., Hitchings, P., &
                  Edwards, S. (2017). Quality assurance in archaeological survey.",
                         tags$em("Journal of Archaeological Method and Theory,"),
                         "24, 466\u2013488."
                       ),
                       tags$li(
                         "Banning, E.B., Hawkins, A.L., & Stewart, S.T. (2011). Sweep
                  widths and the detection of artifacts in archaeological survey.",
                         tags$em("Journal of Archaeological Science,"), "38, 3447\u20133458."
                       ),
                       tags$li(
                         "Banning, E.B., Hawkins, A.L., & Stewart, S.T. (2006).
                  Detection functions for archaeological survey.",
                         tags$em("American Antiquity,"), "71(4), 723\u2013742."
                       ),
                       tags$li(
                         "Loomis, J.M., & Philbeck, J.W. (2008). Measuring spatial
                  perception with spatial updating and action. In R.L. Klatzky,
                  B. MacWhinney, & M. Behrman (Eds.),",
                         tags$em("Embodiment, ego-space, and action"), "(pp. 1\u201343).
                  Psychology Press."
                       ),
                       tags$li(
                         "Milwid, Y. (2017). Automated calculation of sweep width for
                  archaeological survey planning and analysis. Masters
                  thesis, University of Toronto."
                       )
                     ),

                     tags$hr(),

                     tags$h4("Technical Notes"),
                     tags$p(
                       "Nonlinear regression uses the Levenberg-Marquardt algorithm
                implemented in the ", tags$code("minpack.lm"), " package.
                Parameters ", tags$em("b"), " and ", tags$em("k"), " are
                constrained to 0 \u2264 b \u2264 1 and k > 0. If the model fails to
                converge, try adjusting the starting values for b and k in
                the ESW Results tab, or check whether sufficient distance bins
                are present in your data (a minimum of 3 are required)."
                     ),
                     tags$p(
                       "Two methods are available for estimating the standard error
                and 95% confidence interval of W (selected in Step 4 of the ESW
                Results tab):"
                     ),
                     tags$ul(
                       tags$li(
                         tags$strong("Delta method: "),
                         "a first-order Taylor expansion of W = b\u221a(\u03c0/k) around
                  the fitted values of b and k, using the full covariance matrix
                  returned by the nonlinear fit (including the b\u2013k covariance
                  term, not just their individual variances). The 95% confidence
                  interval is computed as W \u00b1 t", tags$sub("0.975, df"),
                  " \u00b7 SE(W), using the t-distribution with the residual degrees
                  of freedom from the fit (falling back to a normal-based
                  critical value if residual degrees of freedom are unavailable).
                  This method is fast but relies on asymptotic normality and a
                  linear approximation around the fitted parameters."
                       ),
                       tags$li(
                         tags$strong("Bootstrap: "),
                         "a nonparametric case-resampling bootstrap (1000
                  resamples). Both the classified calibration records and the
                  master artifact table are independently resampled with
                  replacement, and the full detection-function fitting pipeline
                  is re-run on each resample. The standard error is the standard
                  deviation of the resampled W values, and the 95% confidence
                  interval is the 2.5th\u201397.5th percentile range of that
                  distribution. This method does not assume normality and can
                  reveal skew in the sampling distribution of W, at the cost of
                  additional computation time. If the original point estimate of
                  W falls outside the interquartile range of the bootstrap
                  distribution, a warning is shown, since this indicates the
                  sampling distribution of W may be meaningfully skewed \u2014 in
                  that case, report the bootstrap result and note the
                  discrepancy rather than treating the delta-method interval as
                  authoritative."
                       )
                     ),
                     tags$p(
                       "Multi-panel plot layout uses the ", tags$code("patchwork"),
                       " package when multiple sides are selected. If this package is
                not installed, only the first panel will be shown."
                     ),
                     tags$p(
                       tags$strong("Required packages: "),
                       "shiny, readr, ggplot2, ggforce, minpack.lm, patchwork."
                     )
              )
            )
          )
        )

      ), # end tabsetPanel

      # Footer
      tags$div(
        class = "app-footer",
        tags$p(
          "Sweep Width Calibration Module \u2014 part of the ",
          tags$span(class = "highlight", "SPACE"),
          " toolkit."
        )
      )
    ) # end main content div
  ) # end tagList
}
