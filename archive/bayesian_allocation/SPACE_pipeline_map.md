# SPACE Pipeline Map

Runtime data flow through the Bayesian Optimal Allocation application.
Each stage corresponds to one or more pipeline scripts sourced by `app.R` at startup.

---

## Stage A — Setup (`scripts/00_setup.R`)

Defines shared helpers used across all stages:

- `std_names()` — normalize column names (`trimws(tolower(...))`, spaces to underscores)
- `coerce_core_cols()` — enforce numeric types on core columns
- `check_required_cols()` — validate required schema columns are present
- `clamp01()` — clamp probability values to [0, 1]
- `show_error_modal()` — display an error modal in the Shiny session
- `read_uploaded_spatial(file_df)` — reads uploaded files (CSV, XLSX, GeoPackage, GeoJSON, Shapefile); returns a named list:
  - `$data` — plain `data.frame` (geometry stripped via `sf::st_drop_geometry()` for spatial formats)
  - `$sf` — `sf` object reprojected to EPSG:4326, or `NULL` for tabular formats
  - `$is_spatial` — logical flag

No side effects on source. Functions only.

---

## Stage B — Input Ingestion (`scripts/01_read_inputs.R`, `server/upload_inputs_server.R`)

**Trigger:** User uploads an initial input file via `input$pre_file`.

### Tabular path (CSV / XLSX)
1. `read_uploaded_spatial()` detects a tabular extension and reads via `readr::read_csv()` or `readxl::read_excel()`.
2. Returns `$data` (data frame), `$sf = NULL`, `$is_spatial = FALSE`.

### Spatial path (GeoPackage / GeoJSON / Shapefile)
1. `read_uploaded_spatial()` reads the file with `sf::read_sf()`.
   - For GeoPackages: detects polygon layers via `sf::st_layers()`.
   - For Shapefiles: identifies the `.shp` row among the multi-file upload.
2. Reprojects to EPSG:4326 with `sf::st_transform()`.
3. Strips geometry with `sf::st_drop_geometry()` to produce a plain data frame.
4. Returns `$data` (plain df), `$sf` (sf object in WGS 84), `$is_spatial = TRUE`.

### Reactive values set
| Reactive value | Content |
|---|---|
| `rv$df_pre_in` | Plain data frame — pipeline input for all downstream stages |
| `rv$sf_pre_in` | `sf` object (EPSG:4326) for map display, or `NULL` |
| `rv$is_spatial` | Logical — controls Map tab visibility |
| `rv$latest_inputs` | Same as `rv$df_pre_in` (drives the Data tab preview) |

Column standardization (`std_names()`, `coerce_core_cols()`, `check_required_cols()`) is applied inside `upload_inputs_server.R` after reading.

---

## Stage C — Bayesian Math Functions (`scripts/01_functions.R`)

Defines the core allocation and detection-probability functions used by Stages D and F. Pure math — no Shiny dependencies.

---

## Stage D — Initial Allocation (`scripts/02_run_allocation.R`, `scripts/03_filter_and_rerun.R`)

**Trigger:** User clicks "Run Allocation" after setting total daily effort (L).

1. `run_allocation_from_inputs()` computes optimal transect lengths per unit.
2. Iterative filter-and-rerun loop removes units with negative/invalid allocations and redistributes effort until stable.

### Reactive values set
| Reactive value | Content |
|---|---|
| `rv$day1_alloc` | Final allocation table (unit_id, allocated effort, detection probabilities) |
| `rv$day1_dropped` | Log of units dropped during filtering |
| `rv$day1_params` | Model parameters used for this run |

---

## Stage E — Ingest Field Results (`scripts/04_ingest_results.R`, `scripts/04a_read_results.R`)

**Trigger:** User uploads a field-results file via `input$posterior_inputs_file` or the posterior workflow upload.

1. Reads and validates results (unit_id, l_walked_today, success).
2. Merges with known units from priors.

### Reactive values set
| Reactive value | Content |
|---|---|
| `rv$posterior_inputs` | Priors loaded for the posterior workflow |
| `rv$clean_results` | Validated field results |

---

## Stage F — Build Update Table & Compute Posteriors (`scripts/05_build_update_table.R`, `scripts/06_compute_posteriors.R`)

**Trigger:** User clicks "Compute Posteriors" in the posterior workflow.

1. `build_update_table()` merges priors, results, and model parameters into a single update table.
2. `compute_posteriors()` applies Bayesian updating to produce posterior probabilities.

### Reactive values set
| Reactive value | Content |
|---|---|
| `rv$update_table` | Combined evidence structure (priors + results + likelihoods) |
| `rv$posteriors` | Posterior probabilities (unit_id, probability) |
| `rv$updated_priors` | Full updated priors table for next-day input |

---

## Stage G — Map Display (`server/map_server.R`, `ui/map_tab_ui.R`)

**Trigger:** `rv$sf_pre_in` becomes non-NULL (spatial file uploaded).

1. `prepare_sf()` applies `sf::st_make_valid()` to repair invalid geometries.
2. Polygons rendered on a leaflet map with default fill styling.
3. Basemap toggle: OpenStreetMap (default) or CartoDB Positron.

**Choropleth update trigger:** `rv$posteriors` becomes non-NULL.

1. Posteriors merged onto `rv$sf_pre_in` by `unit_id`.
2. Polygons re-rendered with `YlOrRd` colour palette scaled to posterior probability.
3. Legend added at bottom-right.

### Conditional visibility
- `output$is_spatial_flag` (logical reactive) bridges `rv$is_spatial` to JavaScript.
- `conditionalPanel` in `map_tab_ui.R` shows the map only when `output.is_spatial_flag == true`.

---

## Data Flow Diagram

```
Upload file
  │
  ├─ tabular ──► rv$df_pre_in (plain df)
  │
  └─ spatial ──► rv$df_pre_in (plain df, geometry stripped)
                 rv$sf_pre_in (sf, EPSG:4326) ──► Map tab
                 rv$is_spatial = TRUE
  │
  ▼
Run Allocation (L) ──► rv$day1_alloc
  │
  ▼
Upload Field Results ──► rv$clean_results
  │
  ▼
Build Update Table ──► rv$update_table
  │
  ▼
Compute Posteriors ──► rv$posteriors ──► Map choropleth update
                       rv$updated_priors
```
