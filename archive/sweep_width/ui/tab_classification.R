# =============================================================================
# tab_classification.R
# Sweep Width Calibration App
#
# Purpose:
#   UI definition for the Classification tab. Provides controls for the
#   elliptical tolerance zone parameters, a button to run classification,
#   feedback on results, and plots showing tolerance ellipses and the
#   overlay of master artifacts with classified detections.
# =============================================================================

tab_classification <- tabPanel(
  title = "Classification",
  icon  = icon("circle-check"),
  
  fluidPage(
    
    # -------------------------------------------------------------------------
    # Header
    # -------------------------------------------------------------------------
    
    tags$h3("Detection Classification"),
    tags$p(
      "This step classifies each surveyor detection as a true positive or
      false detection by testing whether any seeded artifact from the Master
      dataset falls within an elliptical tolerance zone centred on the
      reported detection location."
    ),
    tags$p(
      "Tolerance zones are elliptical rather than circular, reflecting the
      asymmetry of distance estimation error: perpendicular distance
      (away from the transect) is estimated by eye and scales with reported
      distance, while along-transect position is measured against a tape
      and is treated as approximately fixed."
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Tolerance parameter controls
    # -------------------------------------------------------------------------
    
    tags$h4("Tolerance Parameters"),
    
    fluidRow(
      
      column(4,
             sliderInput(
               inputId = "perp_scale",
               label   = tags$span(
                 "Perpendicular scaling factor",
                 tags$small(class = "text-muted",
                            " — fraction of reported distance used as the ellipse semi-major axis")
               ),
               min   = 0.05,
               max   = 0.50,
               value = 0.20,
               step  = 0.01
             ),
             tags$p(class = "text-muted", tags$small(
               "Default: 0.20 (20% of reported perpendicular distance). Increase this value if classifications appear too
          conservative (too many false negatives)."
             ))
      ),
      
      column(4,
             sliderInput(
               inputId = "along_fixed",
               label   = tags$span(
                 "Along-transect fixed tolerance (m)",
                 tags$small(class = "text-muted",
                            " — fixed semi-minor axis of the tolerance ellipse")
               ),
               min   = 0.25,
               max   = 3.00,
               value = 1.00,
               step  = 0.25
             ),
             tags$p(class = "text-muted", tags$small(
               "Default: 1.0 m. This is the fixed tolerance applied along the
          transect direction, where surveyor position estimates are most
          reliable."
             ))
      ),
      
      column(4,
             tags$br(),
             tags$br(),
             actionButton(
               inputId = "run_classification",
               label   = "Run Classification",
               icon    = icon("play"),
               class   = "btn btn-primary btn-block"
             ),
             tags$br(),
             uiOutput("classification_feedback")
      )
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Classification summary table
    # -------------------------------------------------------------------------
    
    tags$h4("Classification Summary"),
    tags$p(class = "text-muted",
           "True and false detection counts broken down by transect side."),
    tableOutput("classification_summary_table"),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Visualisation plots
    # -------------------------------------------------------------------------
    
    tags$h4("Tolerance Ellipse Visualisation"),
    tags$p(class = "text-muted",
           "Each detection is shown with its elliptical tolerance zone.
      Green points are true detections (matched to a seeded artifact);
      red points are false or unmatched detections. Note that ellipses
      appear elongated along the transect axis due to axis scaling."),
    
    plotOutput("ellipse_plot", height = "450px"),
    
    tags$hr(),
    
    tags$h4("Master Artifacts and Classified Detections"),
    tags$p(class = "text-muted",
           "Seeded artifact locations (triangles) overlaid with classified
      surveyor detections. Use this plot to visually verify that true
      detections are spatially associated with known artifact locations."),
    
    plotOutput("overlay_plot", height = "450px")
  )
)