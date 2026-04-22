# =============================================================================
# tab_data_input.R
# Sweep Width Calibration App
#
# Purpose:
#   UI definition for the Data Input tab. Provides file upload controls for
#   the master and records CSV files, a button to load sample data, validation
#   feedback, data preview tables, and spatial preview plots.
# =============================================================================

tab_data_input <- tabPanel(
  title = "Data Input",
  icon  = icon("upload"),
  
  fluidPage(
    
    # -------------------------------------------------------------------------
    # Header
    # -------------------------------------------------------------------------
    
    tags$h3("Load Calibration Data"),
    tags$p(
      "Upload your Master and Calibration Records CSV files below. Both files
      must contain the columns: ", tags$code("LDist"), ", ",
      tags$code("Dist"), ", ", tags$code("LorR"), ", and ",
      tags$code("Type"), ". Distances must be positive; left/right side is
      indicated by the ", tags$code("LorR"), " column.",
      "If you do not have your own data, click ",
      tags$strong("Load Sample Data"), " to use the provided synthetic dataset."
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # File upload controls and sample data button
    # -------------------------------------------------------------------------
    
    fluidRow(
      
      column(4,
             tags$h4("Master File"),
             tags$p(class = "text-muted",
                    "Contains the known locations of seeded artifacts."),
             fileInput(
               inputId  = "master_file",
               label    = NULL,
               accept   = ".csv",
               placeholder = "No file selected"
             ),
             uiOutput("master_feedback")
      ),
      
      column(4,
             tags$h4("Calibration Records File"),
             tags$p(class = "text-muted",
                    "Contains the surveyor's reported detections."),
             fileInput(
               inputId  = "records_file",
               label    = NULL,
               accept   = ".csv",
               placeholder = "No file selected"
             ),
             uiOutput("records_feedback"),
             tags$div(
               style = "margin-top: 12px;",
               numericInput(
                 inputId = "n_runs",
                 label   = "Number of survey runs",
                 value   = 1,
                 min     = 1,
                 step    = 1
               ),
               tags$p(class = "text-muted", tags$small(
                 "How many independent surveyor passes are pooled in the
                  records file? Leave at 1 if records represent a single pass."
               ))
             )
      ),
      
      column(4,
             tags$h4("Sample Data"),
             tags$p(class = "text-muted",
                    "Load the synthetic dataset bundled with the app to explore
          the workflow before using your own data."),
             tags$br(),
             actionButton(
               inputId = "load_sample",
               label   = "Load Sample Data",
               icon    = icon("database"),
               class   = "btn btn-info btn-block"
             )
      )
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Data preview tables
    # -------------------------------------------------------------------------
    
    tags$h4("Data Previews"),
    tags$p(class = "text-muted",
           "The first 10 rows of each loaded file are shown below.
      Review these to confirm the data has been parsed correctly."),
    
    fluidRow(
      
      column(6,
             tags$h5("Master (first 10 rows)"),
             tableOutput("master_preview")
      ),
      
      column(6,
             tags$h5("Calibration Records (first 10 rows)"),
             tableOutput("records_preview")
      )
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------------
    # Spatial preview plots
    # -------------------------------------------------------------------------
    
    tags$h4("Spatial Previews"),
    tags$p(class = "text-muted",
           "Review the spatial distribution of loaded data before proceeding.
      Left-side artifacts are plotted on the negative x-axis and right-side
      artifacts on the positive x-axis. Verify that locations appear
      reasonable before continuing to the Classification step."),
    
    fluidRow(
      
      column(6,
             plotOutput("master_plot", height = "400px")
      ),
      
      column(6,
             plotOutput("records_plot", height = "400px")
      )
    )
  )
)