# =============================================================================
#  mod_allocation_ui.R
#  SPACE — Bayesian Optimal Allocation module UI
# =============================================================================

allocationUI <- function(id) {
  ns <- NS(id)

  tagList(

    # -------------------------------------------------------------------------
    # Header bar with banner and back button
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
            "Optimal Allocation Calculator"
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
    # Main tab bar (full-width, no sidebar)
    # -------------------------------------------------------------------------
    tags$div(
      style = "padding: 15px;",

      tabsetPanel(
        id       = ns("mainTabs"),
        type     = "tabs",
        selected = "Allocations",

        # =====================================================================
        # Tab 1: Allocations
        # =====================================================================
        tabPanel(
          title = tagList(icon("chart-bar"), " Allocations"),
          value = "Allocations",

          div(
            class = "alloc-layout",

            # -----------------------------------------------------------------
            # Left sidebar — workflow controls
            # -----------------------------------------------------------------
            div(
              class = "alloc-sidebar",
              div(
                class = "sidebar-panel",
                helpText(HTML(
                  "See the <i>Documentation</i> tab for data formatting requirements and workflow guidelines."
                )),

                # Step 1: Upload Inputs
                div(
                  class = "step-label",
                  tags$span("Step 1: Upload Inputs (Current Probabilities)"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Upload your priors file (.csv, .xlsx, .gpkg, .geojson, or Shapefile). See the Documentation tab for data formatting requirements."
                  )
                ),
                fileInput(
                  ns("pre_file"),
                  label    = NULL,
                  accept   = c(".csv", ".txt", ".xlsx", ".xls", ".gpkg", ".geojson",
                               ".json", ".shp", ".dbf", ".shx", ".prj"),
                  multiple = TRUE
                ),

                hr(),

                # Step 2: Total Daily Effort
                div(
                  class = "step-label",
                  tags$span("Step 2: Enter Total Daily Effort (m/day)"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = paste(
                      "Total Daily Effort is the estimated total distance (in meters) that the survey team can cover in a single day.",
                      "",
                      "This is an estimate that depends on team size, time in the field, terrain, and average walking pace.",
                      "",
                      "Example: If 4 surveyors can each cover ~2,000 m in a day, Total Daily Effort \u2248 8,000 m.",
                      sep = "\n"
                    )
                  )
                ),
                numericInput(ns("total_effort"), NULL, min = 0, value = NA, step = 100),

                hr(),

                # Step 3: Assign / Update Sweep Widths
                div(class = "step-label", "Step 3: Assign / Update Sweep Widths"),
                actionButton(ns("update_sweep_btn"), "Assign / Update Sweep Widths", class = "custom-btn"),

                hr(),

                # Step 4: Generate Allocations
                div(
                  class = "step-label",
                  tags$span("Step 4: Generate Allocations"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "See the Documentation tab for data formatting requirements and workflow guidelines."
                  )
                ),
                actionButton(ns("run_day1"), "Generate Allocations", class = "custom-btn"),

                hr(),

                # Step 5: Download Results
                div(
                  class = "step-label",
                  tags$span("Step 5: Download Allocations"),
                  tags$span(
                    "?",
                    class            = "help-icon",
                    `data-toggle`    = "tooltip",
                    `data-placement` = "right",
                    title            = "Once allocations are generated, use the buttons below to download your results."
                  )
                ),
                downloadButton(ns("dl_alloc_xlsx"), "Download Allocations (.xlsx)", class = "custom-btn"),
                br(), br(),
                downloadButton(ns("dl_alloc_zip"), "Download Full Bundle (.zip)", class = "custom-btn")
              )
            ),

            # -----------------------------------------------------------------
            # Right main panel — data display sub-tabs
            # -----------------------------------------------------------------
            div(
              class = "alloc-main",
              div(class = "main-heading", "Generate Survey Allocations"),
              tabsetPanel(
                tabPanel(
                  "Inputs (Current Probabilities)",
                  br(),
                  helpText("Displays the uploaded or selected input file containing current prior probabilities and related parameters."),
                  DT::DTOutput(ns("inputs_tbl"))
                ),
                tabPanel(
                  "Recommended Allocations",
                  br(),
                  helpText(HTML("<strong>Interpretation:</strong> Allocation values (<em>allocation</em>) are expressed in meters of walking effort (m/day) and represent the portion of Total Daily Effort assigned to each polygon for the current day.")),
                  helpText("Each row represents a survey unit (unit_id) with computed allocation parameters such as area, probability, sweep_width, visibility, and recommended transect length (allocation)."),
                  DT::DTOutput(ns("day1_alloc_tbl"))
                ),
                tabPanel(
                  "Dropped Units",
                  br(),
                  helpText("Lists any units that were dropped during the iterative allocation process (e.g., negative recommended transect lengths)."),
                  DT::DTOutput(ns("day1_dropped_tbl"))
                ),
                tabPanel(
                  title = tagList(icon("map"), " Map"),

                  div(
                    style = "padding: 10px 15px;",

                    # Shown when no spatial data is loaded
                    conditionalPanel(
                      condition = paste0("output['", ns("is_spatial_flag"), "'] == false"),
                      div(
                        style = "margin-top: 30px; color: #666; font-size: 15px;",
                        "Upload a GeoPackage, GeoJSON, or Shapefile to enable the map."
                      )
                    ),

                    # Shown when spatial data is loaded
                    conditionalPanel(
                      condition = paste0("output['", ns("is_spatial_flag"), "'] == true"),
                      div(
                        style = "margin-bottom: 8px;",
                        selectInput(
                          inputId  = ns("basemap_choice"),
                          label    = "Basemap",
                          choices  = c("OpenStreetMap", "CartoDB Positron"),
                          selected = "OpenStreetMap",
                          width    = "250px"
                        )
                      ),
                      leaflet::leafletOutput(ns("survey_map"), height = "600px")
                    )
                  )
                )
              )
            )
          )
        ),

        # =====================================================================
        # Tab 3: Posteriors
        # =====================================================================
        tabPanel(
          title = tagList(icon("chart-line"), " Posteriors"),
          value = "Posteriors",

          div(
            class = "alloc-layout",

            # --- Sidebar ---
            div(
              class = "alloc-sidebar",
              div(
                class = "sidebar-panel",
                helpText(HTML(
                  "See the <i>Documentation</i> tab for data formatting requirements and workflow guidelines."
                )),

                # Step 1: Upload Inputs (Priors)
                div(class = "step-label", "Step 1: Upload Inputs (Current Probabilities)"),
                fileInput(ns("posterior_inputs_file"), NULL, accept = c(".csv", ".txt", ".xlsx", ".xls")),

                # Step 2: Upload Field Results
                div(class = "step-label", "Step 2: Upload Field Results"),
                fileInput(ns("results_file"), NULL, accept = c(".csv", ".txt", ".xlsx", ".xls")),

                # Step 3: Compute Posteriors
                div(class = "step-label", "Step 3: Compute Posteriors"),
                actionButton(ns("compute_post_btn"), "Compute Posteriors", class = "custom-btn btn-block"),

                br(),

                # Step 4: Download Posteriors
                div(class = "step-label", "Step 4: Download Posteriors"),
                helpText("Click below to download the updated priors (posteriors) as a CSV file."),
                downloadButton(ns("dl_post_csv"), "Download Posteriors (.csv)", class = "custom-btn btn-block")
              )
            ),

            # --- Main Panel ---
            div(
              class = "alloc-main",
              div(class = "main-heading", "Update Probabilities Based on Field Results"),
              tabsetPanel(
                tabPanel(
                  "Field Results",
                  br(),
                  helpText("Displays the uploaded field results file used for posterior computation."),
                  DT::DTOutput(ns("results_tbl"))
                ),
                tabPanel(
                  "Posterior Probabilities",
                  br(),
                  helpText("Displays the posterior probability for each unit after Bayesian updating. Use the dropdown to filter for updated units only."),
                  selectInput(
                    ns("posterior_filter"),
                    label    = NULL,
                    choices  = c("Show all units", "Show only updated units"),
                    selected = "Show all units",
                    width    = "280px"
                  ),
                  DT::DTOutput(ns("posteriors_tbl"))
                )
              )
            )
          )
        ),

        # =====================================================================
        # Tab 3: Documentation
        # =====================================================================
        tabPanel(
          title = tagList(icon("circle-info"), " Documentation"),
          value = "Documentation",

          fluidRow(
            column(
              width  = 8,
              offset = 2,

              # --- Introduction content ---------------------------------------
              h2(
                style = "color:#1E3765; font-weight:700; margin-bottom:20px;",
                "Welcome to the Bayesian Optimal Allocation App"
              ),

              p("This application is part of the broader ",
                strong("Survey Planning, Allocation, Costing, and Evaluation (SPACE) Project"),
                ", a collaborative initiative dedicated to improving how archaeologists design,
                execute, and evaluate surveys. The SPACE project seeks to make advanced survey
                planning methods accessible to practitioners by automating mathematical models
                and embedding them in an intuitive, web-based platform."
              ),

              p("The Bayesian Optimal Allocation app focuses on one of SPACE's core goals —
                helping survey teams allocate effort more effectively using evidence-based methods.
                By combining probabilistic reasoning with real-world field constraints, it supports
                survey design that is both efficient and transparent, while continuously learning
                from results gathered in the field."
              ),

              h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "What are 'Allocations'?"),
              p("In this app, an ", strong("allocation"), " is the total amount of transect distance (in meters) that the survey team is recommended to walk within a given survey unit during a single field day. ",
                "Because Total Daily Effort is entered in meters per day, allocation values are also expressed in meters per day. ",
                "Put simply: the app distributes your team's available transect meters across survey units, assigning more distance to units with higher probability of containing archaeological material."
              ),

              h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Core Workflows"),
              p("The app is built around two interconnected workflows that work together to plan,
                 evaluate, and adapt survey strategies across successive field days:"),

              tags$ul(
                tags$li(HTML("<strong>1. Allocation Workflow</strong> –
                             Uses Bayesian search allocation principles to distribute available
                             survey effort across spatial units based on prior probabilities and
                             survey parameters. This produces a daily field plan that optimizes
                             where to spend effort to maximize the chance of discovery.")),
                tags$li(HTML("<strong>2. Posterior Update Workflow</strong> –
                             Incorporates observed field results (e.g., surveyed distance and
                             detections) to update probabilities and generate new priors for
                             the next day's allocation. This creates a self-correcting feedback
                             loop where each day's work informs the next."))
              ),

              h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "How It Fits into SPACE"),
              p("The Bayesian Optimal Allocation app represents one of several modular tools under
                 development within the SPACE framework. Other planned modules address visibility
                 estimation, sweep width calibration, survey costing, coverage evaluation, and
                 sample-size determination. Each module is designed to interconnect with the others,
                 allowing archaeologists to move seamlessly from survey design to implementation
                 and quality assessment within a unified decision-support environment."
              ),

              h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Why Use This App?"),
              tags$ul(
                tags$li("Provides a reproducible, data-informed method for allocating survey effort."),
                tags$li("Reduces subjectivity by grounding decisions in quantitative models."),
                tags$li("Continuously improves survey design through Bayesian updating."),
                tags$li("Integrates seamlessly with other SPACE modules for end-to-end survey planning."),
                tags$li("Offers an intuitive, browser-based interface that requires no coding experience.")
              ),

              p("Together, these capabilities support a more systematic and adaptive approach to
                archaeological survey. The SPACE platform's goal is to empower survey teams to plan,
                evaluate, and refine their work efficiently — ensuring that each day's field effort
                builds upon the knowledge gained from the last."),

              hr(),

              # --- Instructions content ---------------------------------------
              div(
                class = "instructions",

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

                h3(style = "color:#1E3765; font-weight:700; margin-top:30px;", "Requirements: Data Formatting"),
                HTML("<p>Two types of input data are required to complete the full Bayesian Optimal Allocation (SPACE) cycle:</p>"),
                tags$ul(
                  tags$li(HTML("<strong>Initial Inputs:</strong> A table of spatial survey units containing prior probabilities and associated attributes.")),
                  tags$li(HTML("<strong>Field Results:</strong> A table recording the observed survey effort and detection outcomes from the field."))
                ),
                HTML("<p>Both files can be uploaded in <strong>.csv</strong> or <strong>.xlsx</strong> format.
                     Column names are automatically standardized, but the following structure is recommended.</p>"),

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
                    tags$tr(tags$td("unit_id"),       tags$td("Unique identifier for each survey unit (polygon or grid cell)."),
                            tags$td("Character (non-empty, unique)"), tags$td("All workflows")),
                    tags$tr(tags$td("area"),           tags$td("Area of the unit (e.g., m\u00b2 or ha; units must be consistent)."),
                            tags$td("Numeric (> 0)"), tags$td("Initial Inputs")),
                    tags$tr(tags$td("probability"),    tags$td("Prior detection probability for the unit."),
                            tags$td("Numeric (0\u20131)"), tags$td("Initial Inputs")),
                    tags$tr(tags$td("sweep_width"),    tags$td("Expected effective sweep width (in metres). Used in search-theory optimization."),
                            tags$td("Numeric (\u2265 0)"), tags$td("Initial Inputs")),
                    tags$tr(tags$td("visibility"),     tags$td("Optional descriptive visibility class or condition (categorical text)."),
                            tags$td("Character (optional)"), tags$td("Initial Inputs (optional)")),
                    tags$tr(tags$td("l_walked_today"), tags$td("Transect length (metres) surveyed within the unit during the current field day."),
                            tags$td("Numeric (\u2265 0)"), tags$td("Field Results")),
                    tags$tr(tags$td("success"),        tags$td("Detection outcome: 1 = site found, 0 = not found."),
                            tags$td("Integer (0 or 1)"), tags$td("Field Results"))
                  )
                ),

                HTML("<p><em>Note:</em> The module automatically recognizes common synonyms (e.g.,
                     <strong>polygon</strong>, <strong>id</strong> \u2192 unit_id;
                     <strong>metres_walked</strong>, <strong>transect_length</strong> \u2192 l_walked_today;
                     <strong>found</strong>, <strong>detected</strong> \u2192 success).</p>"),
                HTML("<p><strong>Sweep widths:</strong> Initial values are required but can be refined interactively
                     in Step 3 of the Allocation Workflow. These values are crucial for determining search effectiveness.</p>"),

                h4(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Spatial Input Formats"),
                HTML("<p>In addition to CSV and Excel, the <strong>Initial Inputs</strong> upload accepts
                     spatially-enabled file formats. When a spatial file is uploaded, polygon geometry is used
                     for map display; the allocation pipeline continues to operate on the non-spatial attributes
                     exactly as it does with tabular inputs.</p>"),
                tags$table(
                  class = "table table-bordered",
                  style = "width:95%; font-size:14px; margin-left:10px;",
                  tags$thead(
                    tags$tr(tags$th("Format"), tags$th("Extension(s)"), tags$th("Notes"))
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

                h4(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Downloadable Sample Data and Templates"),
                p("Use the sample data to explore the workflows and the templates to structure your own project data."),

                fluidRow(
                  column(
                    width = 6,
                    tags$label(strong("Sample Data:")),
                    selectInput(ns("sample_data_select"), label = NULL,
                                choices  = c("Initial Inputs Sample", "Field Results Sample"),
                                selected = "Initial Inputs Sample", width = "100%"),
                    uiOutput(ns("sample_data_downloads"))
                  ),
                  column(
                    width = 6,
                    tags$label(strong("Data Templates:")),
                    selectInput(ns("template_data_select"), label = NULL,
                                choices  = c("Initial Inputs Template", "Field Results Template"),
                                selected = "Initial Inputs Template", width = "100%"),
                    uiOutput(ns("template_data_downloads"))
                  )
                ),

                br(),

                h3(style = "color:#1E3765; font-weight:700; margin-top:25px;", "1) Allocation Workflow"),
                p("This workflow generates optimized survey allocations (transect lengths) from your current priors."),

                tags$ol(
                  tags$li(HTML("<span class='highlight'>Step 1 \u2014 Upload Inputs (Current Priors):</span>
                                Upload your priors file containing <strong>unit_id</strong>, <strong>area</strong>,
                                <strong>probability</strong>, <strong>sweep_width</strong>, and (optional) <strong>visibility</strong>.
                                The app standardizes and validates these inputs automatically.")),
                  tags$li(HTML("<span class='highlight'>Step 2 \u2014 Assign Total Daily Effort:</span>
                                Specify the total survey effort (metres) available for the day (e.g., total transect length).")),
                  tags$li(HTML("<span class='highlight'>Step 3 \u2014 Assign / Update Sweep Widths:</span>
                                Review and, if needed, update sweep width values interactively by visibility class before allocation.")),
                  tags$li(HTML("<span class='highlight'>Step 4 \u2014 Generate Allocations:</span>
                                Run the optimization to compute recommended transect lengths for each unit.
                                The results appear under the <strong>Allocations</strong> tab, including dropped units if applicable.")),
                  tags$li(HTML("<span class='highlight'>Step 5 \u2014 Download Allocations:</span>
                                Once complete, use the download buttons to save your results.
                                The <strong>.xlsx</strong> file includes the allocation and dropped-unit logs;
                                the <strong>.zip</strong> file bundles all temporary/intermediate files."))
                ),

                HTML("<p><em>Tip:</em> The <strong>Data \u2192 Inputs (Current Probabilities)</strong> tab always displays whichever dataset (Allocation or Posterior) was most recently uploaded.</p>"),

                br(),

                h3(style = "color:#1E3765; font-weight:700; margin-top:25px;", "2) Posterior Update Workflow"),
                p("After fieldwork, use this workflow to update priors based on survey effort and detection outcomes,
                   generating posteriors for the next allocation cycle."),

                tags$ol(
                  tags$li(HTML("<span class='highlight'>Step 1 \u2014 Upload Inputs (Current Priors):</span>
                                Upload the priors file again (same format as above). These are the probabilities you are updating.")),
                  tags$li(HTML("<span class='highlight'>Step 2 \u2014 Upload Field Results:</span>
                                Provide a results file containing <strong>unit_id</strong>, <strong>l_walked_today</strong> (metres),
                                and <strong>success</strong> (1/0). Units with non-missing <strong>l_walked_today</strong> are treated as surveyed.")),
                  tags$li(HTML("<span class='highlight'>Step 3 \u2014 Compute Posteriors:</span>
                                The app merges priors and results to compute posterior probabilities.
                                The <strong>Posteriors</strong> tab displays a summary table
                                (<strong>unit_id</strong>, <strong>prior_prob</strong>, <strong>post_prob</strong>)
                                with an option to filter for updated units only.")),
                  tags$li(HTML("<span class='highlight'>Step 4 \u2014 Download Posteriors:</span>
                                Click <strong>Download Posteriors (.csv)</strong> to export the updated priors (posteriors)
                                as a single CSV file. This becomes your input for the next allocation cycle."))
                ),

                br(),

                h3(style = "color:#1E3765; font-weight:700; margin-top:25px;", "Iteration & Good Practice"),
                tags$ul(
                  tags$li(HTML("<strong>Iterate daily:</strong> Run Allocation \u2192 Field Survey \u2192 Posterior Update to continually refine probabilities.")),
                  tags$li(HTML("<strong>Maintain consistent IDs:</strong> Keep <strong>unit_id</strong> values identical across priors and results.")),
                  tags$li(HTML("<strong>Use consistent units:</strong> Ensure measurements (e.g., metres) match between <strong>sweep_width</strong> and <strong>l_walked_today</strong>.")),
                  tags$li(HTML("<strong>Inspect diagnostics:</strong> Use the <strong>Full Bundle (.zip)</strong> download if you want to review intermediate allocation files."))
                ),

                br(),
                p("For methodological background and implementation details, refer to the accompanying Bayesian Optimal Allocation (SPACE) documentation and publications.")
              )
            )
          )
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Footer
    # -------------------------------------------------------------------------
    tags$div(
      class = "app-footer",
      tags$p(
        "Optimal Allocation Calculator \u2014 part of the ",
        tags$span(class = "highlight", "SPACE"),
        " toolkit."
      )
    )
  )
}
