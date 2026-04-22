# SPACE: Survey Planning, Allocation, Costing and Evaluation

## Overview

SPACE is an integrated R Shiny application providing tools for planning, executing, and evaluating archaeological and forensic searches. It brings together quantitative methods for survey design into a single, professional-grade toolkit suitable for researchers, cultural resource management practitioners, and field crews. SPACE currently includes two modules — a Sweep Width Estimator and an Optimal Allocation tool — and its architecture is designed to support additional modules over time.

## Modules

### Sweep Width Estimator

Fits detection functions to data collected during calibration searches and estimates effective sweep widths for use in search planning.

### Optimal Allocation

A Bayesian framework for allocating search effort across multiple search zones, with support for updating prior beliefs using field results.

## How to Run

Clone or download this repository, open R in the `space/` directory, and run:

```r
shiny::runApp()
```

The app is also hosted online at <https://steven-edwards.shinyapps.io/SPACE/>

## Dependencies

Package dependencies are managed with [renv](https://rstudio.github.io/renv/). To restore the project library, run:

```r
renv::restore()
```

## Citation

Citation details will be added upon Zenodo deposit. DOI: [TBD]

## License

This project is licensed under the MIT License. See LICENSE file for details.
