# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Interface for the step-by-step instructions tab,
#               documenting workflow requirements and user guidance
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Provides the user-facing documentation for formatting inputs,
#     running allocations, and interpreting outputs.
#   - Designed as an in-app reference for survey teams and analysts.
# =====================================================================

instructions_tab_ui <- function() {
  tabPanel(
    "Instructions",
    div(
      class = "instructions",
      
      # Global button styling
      tags$style(HTML("
        .btn-primary, .btn-success {
          background-color: #1E3765 !important;
          border-color: #1E3765 !important;
          color: white !important;
        }
        .btn-primary:hover, .btn-success:hover {
          background-color: #16294d !important;
          border-color: #16294d !important;
          color: white !important;
        }
        .btn-primary:focus, .btn-success:focus {
          box-shadow: 0 0 0 0.15rem rgba(30, 55, 101, 0.4) !important;
        }
      ")),
      
      # ======================================================
      # Overview
      # ======================================================
      h2(
        style = "color:#1E3765; font-weight:700; margin-bottom:20px;",
        "Instructions"
      ),
      p("This module implements a two-part workflow for survey planning and daily learning updates using Bayesian optimal allocation principles:"),
      
      div(
        style = "margin-left:20px;",
        tags$ul(
          tags$li(HTML("<strong>1. Allocation Workflow:</strong> Generates optimized survey allocations (transect lengths) based on current priors.")),
          tags$li(HTML("<strong>2. Posterior Update Workflow:</strong> Incorporates field results to compute updated priors (posteriors) for subsequent allocation cycles."))
        )
      ),
      
      p("Follow the detailed guidance below to prepare your data and complete each workflow successfully."),
      
      # ======================================================
      # Requirements: Data Formatting
      # ======================================================
      h3(style = "color:#1E3765; font-weight:700; margin-top:30px;", "Requirements: Data Formatting"),
      HTML("<p>Two types of input data are required to complete the full Bayesian Optimal Allocation (SPACE) cycle:</p>"),
      tags$ul(
        tags$li(HTML("<strong>Initial Inputs:</strong> A table of spatial survey units containing prior probabilities and associated attributes.")),
        tags$li(HTML("<strong>Field Results:</strong> A table recording the observed survey effort and detection outcomes from the field."))
      ),
      HTML("<p>Both files can be uploaded in <strong>.csv</strong> or <strong>.xlsx</strong> format. 
           Column names are automatically standardized, but the following structure is recommended.</p>"),
      
      # Data Dictionary
      h4(style = "color:#1E3765; font-weight:600; margin-top:15px;", "Data Dictionary"),
      tags$table(
        class = "table table-bordered",
        style = "width:95%; font-size:14px; margin-left:10px;",
        tags$thead(
          tags$tr(
            tags$th("Column Name"),
            tags$th("Description"),
            tags$th("Expected Data Type / Range"),
            tags$th("Required For")
          )
        ),
        tags$tbody(
          tags$tr(tags$td("unit_id"), tags$td("Unique identifier for each survey unit (polygon or grid cell)."),
                  tags$td("Character (non-empty, unique)"), tags$td("All workflows")),
          tags$tr(tags$td("area"), tags$td("Area of the unit (e.g., m² or ha; units must be consistent)."),
                  tags$td("Numeric (> 0)"), tags$td("Initial Inputs")),
          tags$tr(tags$td("probability"), tags$td("Prior detection probability for the unit."),
                  tags$td("Numeric (0–1)"), tags$td("Initial Inputs")),
          tags$tr(tags$td("sweep_width"), tags$td("Expected effective sweep width (in metres). Used in search-theory optimization."),
                  tags$td("Numeric (≥ 0)"), tags$td("Initial Inputs")),
          tags$tr(tags$td("visibility"), tags$td("Optional descriptive visibility class or condition (categorical text)."),
                  tags$td("Character (optional)"), tags$td("Initial Inputs (optional)")),
          tags$tr(tags$td("l_walked_today"), tags$td("Transect length (metres) surveyed within the unit during the current field day."),
                  tags$td("Numeric (≥ 0)"), tags$td("Field Results")),
          tags$tr(tags$td("success"), tags$td("Detection outcome: 1 = site found, 0 = not found."),
                  tags$td("Integer (0 or 1)"), tags$td("Field Results"))
        )
      ),
      
      HTML("<p><em>Note:</em> The module automatically recognizes common synonyms (e.g., 
           <strong>polygon</strong>, <strong>id</strong> → unit_id; 
           <strong>metres_walked</strong>, <strong>transect_length</strong> → l_walked_today; 
           <strong>found</strong>, <strong>detected</strong> → success).</p>"),
      HTML("<p><strong>Sweep widths:</strong> Initial values are required but can be refined interactively
           in Step 3 of the Allocation Workflow. These values are crucial for determining search effectiveness.</p>"),

      # ======================================================
      # Spatial Input Formats
      # ======================================================
      h4(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Spatial Input Formats"),
      HTML("<p>In addition to CSV and Excel, the <strong>Initial Inputs</strong> upload accepts
           spatially-enabled file formats. When a spatial file is uploaded, polygon geometry is used
           for map display; the allocation pipeline continues to operate on the non-spatial attributes
           exactly as it does with tabular inputs.</p>"),
      tags$table(
        class = "table table-bordered",
        style = "width:95%; font-size:14px; margin-left:10px;",
        tags$thead(
          tags$tr(
            tags$th("Format"),
            tags$th("Extension(s)"),
            tags$th("Notes")
          )
        ),
        tags$tbody(
          tags$tr(tags$td("GeoPackage"), tags$td(".gpkg"),
                  tags$td("Preferred spatial format. Single file; polygon layer auto-detected.")),
          tags$tr(tags$td("GeoJSON"), tags$td(".geojson, .json"),
                  tags$td("Single file.")),
          tags$tr(tags$td("Shapefile"), tags$td(".shp + .dbf, .shx, .prj"),
                  tags$td(HTML("All four component files must be selected together in the upload dialog.
                               Use <strong>Ctrl+Click</strong> or <strong>Shift+Click</strong> to select multiple files.")))
        )
      ),

      h4(style = "color:#1E3765; font-weight:600; margin-top:20px;", "Map Tab"),
      HTML("<p>The <strong>Map</strong> tab activates automatically when a spatial file is uploaded.
           It displays survey-unit polygons on an interactive leaflet basemap
           (OpenStreetMap or CartoDB Positron). After posteriors are computed, the map updates to show a
           choropleth coloured by posterior probability, providing a spatial overview of how detection
           probabilities have changed across the study area.</p>"),

      # Sample Data and Templates
      h4(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Downloadable Sample Data and Templates"),
      p("Use the sample data to explore the workflows and the templates to structure your own project data."),
      
      fluidRow(
        column(
          width = 6,
          tags$label(strong("Sample Data:")),
          selectInput("sample_data_select", label = NULL,
                      choices = c("Initial Inputs Sample", "Field Results Sample"),
                      selected = "Initial Inputs Sample", width = "100%"),
          uiOutput("sample_data_downloads")
        ),
        column(
          width = 6,
          tags$label(strong("Data Templates:")),
          selectInput("template_data_select", label = NULL,
                      choices = c("Initial Inputs Template", "Field Results Template"),
                      selected = "Initial Inputs Template", width = "100%"),
          uiOutput("template_data_downloads")
        )
      ),
      
      br(),
      
      # ======================================================
      # Allocation Workflow
      # ======================================================
      h3(style = "color:#1E3765; font-weight:700; margin-top:25px;", "1) Allocation Workflow"),
      p("This workflow generates optimized survey allocations (transect lengths) from your current priors."),
      
      tags$ol(
        tags$li(HTML("<span class='highlight'>Step 1 — Upload Inputs (Current Priors):</span> 
                      Upload your priors file containing <strong>unit_id</strong>, <strong>area</strong>, 
                      <strong>probability</strong>, <strong>sweep_width</strong>, and (optional) <strong>visibility</strong>. 
                      The app standardizes and validates these inputs automatically.")),
        
        tags$li(HTML("<span class='highlight'>Step 2 — Assign Total Daily Effort:</span> 
                      Specify the total survey effort (metres) available for the day (e.g., total transect length).")),
        
        tags$li(HTML("<span class='highlight'>Step 3 — Assign / Update Sweep Widths:</span> 
                      Review and, if needed, update sweep width values interactively by visibility class before allocation.")),
        
        tags$li(HTML("<span class='highlight'>Step 4 — Generate Allocations:</span> 
                      Run the optimization to compute recommended transect lengths for each unit. 
                      The results appear under the <strong>Initial Allocations</strong> tab, including dropped units if applicable.")),
        
        tags$li(HTML("<span class='highlight'>Step 5 — Download Allocations:</span> 
                      Once complete, use the download buttons to save your results. 
                      The <strong>.xlsx</strong> file includes the allocation and dropped-unit logs; 
                      the <strong>.zip</strong> file bundles all temporary/intermediate files."))
      ),
      
      HTML("<p><em>Tip:</em> The <strong>Data → Inputs (Current Probabilities)</strong> tab always displays whichever dataset (Allocation or Posterior) was most recently uploaded.</p>"),
      
      br(),
      
      # ======================================================
      # Posterior Update Workflow
      # ======================================================
      h3(style = "color:#1E3765; font-weight:700; margin-top:25px;", "2) Posterior Update Workflow"),
      p("After fieldwork, use this workflow to update priors based on survey effort and detection outcomes, 
         generating posteriors for the next allocation cycle."),
      
      tags$ol(
        tags$li(HTML("<span class='highlight'>Step 1 — Upload Inputs (Current Priors):</span> 
                      Upload the priors file again (same format as above). These are the probabilities you are updating.")),
        
        tags$li(HTML("<span class='highlight'>Step 2 — Upload Field Results:</span> 
                      Provide a results file containing <strong>unit_id</strong>, <strong>l_walked_today</strong> (metres), 
                      and <strong>success</strong> (1/0). Units with non-missing <strong>l_walked_today</strong> are treated as surveyed.")),
        
        tags$li(HTML("<span class='highlight'>Step 3 — Compute Posteriors:</span> 
                      The app merges priors and results to compute posterior probabilities. 
                      The <strong>Posteriors</strong> tab displays a summary table 
                      (<strong>unit_id</strong>, <strong>prior_prob</strong>, <strong>post_prob</strong>) 
                      with an option to filter for updated units only.")),
        
        tags$li(HTML("<span class='highlight'>Step 4 — Download Posteriors:</span> 
                      Click <strong>Download Posteriors (.csv)</strong> to export the updated priors (posteriors) 
                      as a single CSV file. This becomes your input for the next allocation cycle."))
      ),
      
      br(),
      
      # ======================================================
      # Iteration & Good Practice
      # ======================================================
      h3(style = "color:#1E3765; font-weight:700; margin-top:25px;", "Iteration & Good Practice"),
      tags$ul(
        tags$li(HTML("<strong>Iterate daily:</strong> Run Allocation → Field Survey → Posterior Update to continually refine probabilities.")),
        tags$li(HTML("<strong>Maintain consistent IDs:</strong> Keep <strong>unit_id</strong> values identical across priors and results.")),
        tags$li(HTML("<strong>Use consistent units:</strong> Ensure measurements (e.g., metres) match between <strong>sweep_width</strong> and <strong>l_walked_today</strong>.")),
        tags$li(HTML("<strong>Inspect diagnostics:</strong> Use the <strong>Full Bundle (.zip)</strong> download if you want to review intermediate allocation files."))
      ),
      
      br(),
      p("For methodological background and implementation details, refer to the accompanying Bayesian Optimal Allocation (SPACE) documentation and publications.")
    )
  )
}
