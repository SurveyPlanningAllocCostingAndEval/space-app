# =============================================================================
# tab_about.R
# Sweep Width Calibration App
#
# Purpose:
#   Static informational tab documenting the methodology, data requirements,
#   references, and guidance on interpreting results.
# =============================================================================

tab_about <- tabPanel(
  title = "About",
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
               "ESW can be used to calculate survey coverage (area swept = ESW ×
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
            distance bin. A Gaussian detection function p(r) = b·exp(−kr²)
            is fitted by constrained nonlinear least squares, and ESW is
            computed as W = b√(π/k)."
               )
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
                   tags$td("Numeric (≥ 0)"),
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
                 "24, 466–488."
               ),
               tags$li(
                 "Banning, E.B., Hawkins, A.L., & Stewart, S.T. (2011). Sweep
            widths and the detection of artifacts in archaeological survey.",
                 tags$em("Journal of Archaeological Science,"), "38, 3447–3458."
               ),
               tags$li(
                 "Banning, E.B., Hawkins, A.L., & Stewart, S.T. (2006).
            Detection functions for archaeological survey.",
                 tags$em("American Antiquity,"), "71(4), 723–742."
               ),
               tags$li(
                 "Loomis, J.M., & Philbeck, J.W. (2008). Measuring spatial
            perception with spatial updating and action. In R.L. Klatzky,
            B. MacWhinney, & M. Behrman (Eds.),",
                 tags$em("Embodiment, ego-space, and action"), "(pp. 1–43).
            Psychology Press."
               ),
               tags$li(
                 "Milwid, Y. (2017). Automated calculation of sweep width for
            archaeological survey planning and analysis. Masters
            thesis, University of Toronto."
               ),
               
             ),
             
             tags$hr(),
             
             tags$h4("Technical Notes"),
             tags$p(
               "Nonlinear regression uses the Levenberg-Marquardt algorithm
          implemented in the ", tags$code("minpack.lm"), " package.
          Parameters ", tags$em("b"), " and ", tags$em("k"), " are
          constrained to 0 ≤ b ≤ 1 and k > 0. If the model fails to
          converge, try adjusting the starting values for b and k in
          the ESW Results tab, or check whether sufficient distance bins
          are present in your data (a minimum of 3 are required)."
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