# SPACE Project Tree

Annotated directory structure for the Bayesian Optimal Allocation application.

```
optimal_allocation/
├── app.R                                  # Thin Shiny controller — sources all scripts/UI/server, defines rv
├── CLAUDE.md                              # AI assistant instructions
├── README.md                              # User-facing documentation
├── SPACE_pipeline_map.md                  # Runtime data-flow documentation
├── SPACE_project_tree.md                  # This file — annotated directory tree
├── LICENSE                                # MIT License
├── BayesianOptimalAllocationApp.Rproj     # RStudio project file
├── renv.lock                              # renv lockfile for reproducible package versions
├── .gitignore
├── .Rprofile                              # renv bootstrap
│
├── scripts/                               # Pipeline scripts — define functions, no side effects on source
│   ├── 00_setup.R                         # Shared helpers: std_names(), read_uploaded_spatial(), show_error_modal()
│   ├── 01_read_inputs.R                   # read_inputs_file() — reads/validates tabular input files
│   ├── 01_functions.R                     # Core Bayesian math/stat functions
│   ├── 02_run_allocation.R                # run_allocation_from_inputs() — Day 1 allocation engine
│   ├── 03_filter_and_rerun.R              # Iterative constraint filtering (negative allocation removal)
│   ├── 04_ingest_results.R                # Field-results cleaning and validation
│   ├── 04a_read_results.R                 # Results file reader (CSV/XLSX)
│   ├── 05_build_update_table.R            # Merges priors + results into Bayesian update structure
│   └── 06_compute_posteriors.R            # Computes posterior probabilities via Bayesian updating
│
├── ui/                                    # Shiny UI modules — one file per tab or panel
│   ├── header_ui.R                        # App header / banner
│   ├── sidebar_ui.R                       # Sidebar panel: file uploads, effort input, action buttons
│   ├── main_tabs_ui.R                     # tabsetPanel assembling all content tabs
│   ├── intro_tab_ui.R                     # Introduction / welcome tab
│   ├── instructions_tab_ui.R              # In-app user instructions and data dictionary
│   ├── initial_allocations_tab_ui.R       # Day 1 allocation results display
│   ├── ingested_results_tab_ui.R          # Field-results preview after upload
│   ├── update_posteriors_tab_ui.R         # Posterior computation results display
│   └── map_tab_ui.R                       # Map tab — leaflet map, conditionally visible when spatial data loaded
│
├── server/                                # Shiny server modules — one file per workflow section
│   ├── upload_inputs_server.R             # Handles initial-input upload, calls read_uploaded_spatial()
│   ├── upload_previous_allocations_server.R  # Handles previous-allocation file upload
│   ├── assign_sweep_widths_server.R       # Interactive sweep-width editing by visibility class
│   ├── run_allocation_server.R            # Runs allocation engine, stores results in rv
│   ├── compute_posteriors_server.R        # (Legacy) posterior computation server logic
│   ├── posterior_workflow_server.R         # Full posterior update workflow orchestration
│   ├── outputs_server.R                   # Download handlers for allocation/posterior outputs
│   └── map_server.R                       # Leaflet map rendering and posterior choropleth updates
│
├── data/                                  # Reproducibility scripts and derived spatial samples
│   └── make_sample_spatial.R              # Regenerates wq_survey_units.gpkg/.geojson from original shapefile
│
├── www/                                   # Static assets served by Shiny
│   ├── inputs_sample.csv                  # 28-polygon initial input sample (CSV)
│   ├── inputs_sample.xlsx                 # Same, Excel format
│   ├── inputs_sample.gpkg                 # Same, GeoPackage format (with polygon geometry)
│   ├── field_results_sample.csv           # 8-polygon field results sample (CSV)
│   ├── field_results_sample.xlsx          # Same, Excel format
│   ├── inputs_template.csv                # Blank input template (CSV)
│   ├── inputs_template.xlsx               # Blank input template (Excel)
│   ├── field_results.csv                  # Blank field-results template (CSV)
│   ├── field_results.xlsx                 # Blank field-results template (Excel)
│   ├── space_banner.png                   # App banner image
│   ├── space_banner_cropped.png           # Cropped banner variant
│   └── space_banner_tall.png              # Tall banner variant
│
└── rsconnect/                             # shinyapps.io deployment configuration
    └── documents/app.R/shinyapps.io/steven-edwards/
```
