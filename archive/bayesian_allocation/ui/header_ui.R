# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Defines the header layout and top-level visual identity
#               for the SPACE Shiny application
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Controls branding, title bar styling, and app-wide header elements.
#   - Provides a consistent look-and-feel across workflow tabs.
#   - Updated to display banner image instead of text header.
# =====================================================================

header_ui <- function() {
  tagList(
    
    # ============================================================
    # Global Header Styles
    # ============================================================
    tags$head(
      # Enable Bootstrap tooltips app-wide (used for inline help icons)
      tags$script(HTML("$(function () { $('[data-toggle=\\\"tooltip\\\"]').tooltip({container: 'body'}); });")),
      tags$style(HTML("
        /* Full-width blue header bar with subtle drop shadow */
        .header-bar {
          background-color: #092646;
          color: white;
          width: 100%;
          padding: 0;                 /* remove padding so banner fits top edge */
          margin: 0;
          display: block;
          box-sizing: border-box;
          box-shadow: 0 3px 12px rgba(0,0,0,0.18);
        }

        .header-title {
          font-family: 'Impact','Arial Black',sans-serif;
          font-size: 36px;
          font-weight: 500;
          letter-spacing: 1px;
          margin: 0;
        }

        .header-subtitle {
          font-size: 22px;
          font-weight: 400;
          margin: 0;
        }

        /* Modern action buttons with gradient + hover glow */
        .custom-btn {
          background: linear-gradient(180deg, #1E3765 0%, #162a4e 100%) !important;
          color: white !important;
          border: none !important;
          width: 100%;
          margin-top: 8px;
          margin-bottom: 8px;
          height: 42px;
          font-weight: 600;
          font-size: 15px;
          border-radius: 6px;
          box-shadow: 0 1px 2px rgba(0,0,0,0.1);
          transition: all 0.25s ease-in-out;
        }

        .custom-btn:hover {
          transform: translateY(-1px);
          background: linear-gradient(180deg, #2A4D8F 0%, #203d73 100%) !important;
          box-shadow: 0 3px 10px rgba(30,55,101,0.35);
        }

        .step-label {
          color: #1E3765;
          font-weight: 700;
          margin-top: 18px;
          margin-bottom: 6px;
          font-size: 14px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          transition: all 0.2s ease-in-out;
        }

        .step-label:hover {
          color: #2A4D8F;
          transform: translateX(2px);
          cursor: default;
        }

        /* Inline help icon (used for tooltips beside input labels) */
        .help-icon {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 18px;
          height: 18px;
          margin-left: 6px;
          font-size: 12px;
          font-weight: 700;
          color: #1E3765;
          border: 1px solid #b8c2d1;
          border-radius: 50%;
          background: #ffffff;
          cursor: help;
          user-select: none;
        }

        .help-icon:hover {
          background: #eef3fb;
          border-color: #8aa0c6;
        }

        .instructions {
          line-height:1.6;
          font-size:16px;
          color:#222;
          max-width:900px;
        }

        .highlight {
          font-weight:600;
          color:#1E3765;
        }

        /* Sidebar panel styling */
        .sidebar-panel {
          background-color: #f7f8fa;
          border-radius: 10px;
          padding: 20px;
          box-shadow: 0 1px 4px rgba(0,0,0,0.1);
          margin-top: 10px;
        }

        .sidebar-title {
          text-align: center;
          font-weight: 500;
          color: #1E3765;
          margin-top: 10px;
          margin-bottom: 15px;
          font-size: 20px;
        }

        hr {
          border: 0;
          border-top: 1px solid rgba(30,55,101,0.15);
          margin: 16px 0;
        }

        body, .shiny-bound-output {
          background-color: #fbfbfc;
        }

        input, select, .form-control {
          border-radius: 6px !important;
          border: 1px solid #ccc !important;
          box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);
        }
      "))
    ),
    
    # ============================================================
    # Banner Image Header
    # ============================================================
    div(
      class = "header-bar",
      tags$img(
        src = "space_banner_cropped.png",
        style = "
          width: 40%;
          height: auto;
          display: block;
          object-fit: contain;
        "
      )
    )
  )
}