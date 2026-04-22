# =============================================================================
# header_ui.R
# Sweep Width Calibration App
# =============================================================================

header_ui <- function() {
  tagList(
    
    tags$head(
      tags$style(HTML("

        /* ---- Header bar ---- */
        .header-bar {
          background-color: #092646;
          color: white;
          width: 100%;
          padding: 0;
          margin: 0;
          display: block;
          box-sizing: border-box;
          box-shadow: 0 3px 12px rgba(0,0,0,0.18);
        }

        /* ---- Body ---- */
        body, .shiny-bound-output {
          background-color: #fbfbfc;
        }

        /* ---- General text ---- */
        p {
          font-size: 15px;
          color: #222;
          line-height: 1.6;
        }

        ol, ul {
          margin-left: 25px;
          font-size: 15px;
          line-height: 1.6;
        }

        li {
          margin-bottom: 10px;
        }

        /* ---- Headings ---- */
        h3, h4 {
          color: #1E3765;
          font-weight: 700;
        }

        h5 {
          color: #1E3765;
          font-weight: 600;
        }

        /* ---- Highlight class ---- */
        .highlight {
          font-weight: 600;
          color: #1E3765;
        }

        /* ---- Instructions block ---- */
        .instructions {
          line-height: 1.6;
          font-size: 16px;
          color: #222;
          max-width: 900px;
        }

        .instructions h3 {
          margin-top: 25px;
          margin-bottom: 10px;
          color: #1E3765;
          font-weight: 700;
        }

        .instructions p  { font-size: 15px; color: #222; }
        .instructions ol { margin-left: 25px; margin-top: 10px; }
        .instructions ul { margin-left: 25px; }

        /* ---- Horizontal rules ---- */
        hr {
          border: 0;
          border-top: 1px solid rgba(30,55,101,0.15);
          margin: 16px 0;
        }

        /* ---- Form controls ---- */
        input, select, .form-control {
          border-radius: 6px !important;
          border: 1px solid #ccc !important;
          box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);
        }

        /* ---- Tab strip — matches Optimal Allocation app style ---- */
        .nav-tabs {
          border-bottom: 2px solid #1E3765 !important;
          background-color: #f7f8fa;
          padding: 6px 15px 0 15px;
        }

        .nav-tabs > li > a {
          color: #1E3765 !important;
          font-size: 15px;
          font-weight: 600;
          border-radius: 4px 4px 0 0 !important;
          border: 1px solid transparent !important;
          padding: 8px 16px;
        }

        .nav-tabs > li > a:hover {
          background-color: #e8edf5 !important;
          border-color: #c5d0e0 #c5d0e0 transparent !important;
          color: #1E3765 !important;
        }

        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
          color: #1E3765 !important;
          font-weight: 700 !important;
          background-color: #ffffff !important;
          border: 1px solid #c5d0e0 !important;
          border-bottom-color: #ffffff !important;
        }

        /* ---- Tab content area ---- */
        .tab-content {
          background-color: #ffffff;
          border: 1px solid #c5d0e0;
          border-top: none;
          padding: 25px 20px;
          border-radius: 0 0 6px 6px;
        }

        /* ---- Action buttons ---- */
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

        /* ---- Standard button colour overrides ---- */
        .btn-primary {
          background-color: #1E3765 !important;
          border-color: #1E3765 !important;
          font-weight: 600;
        }

        .btn-primary:hover {
          background-color: #2A4D8F !important;
          border-color: #2A4D8F !important;
        }

        .btn-success {
          background-color: #1E6645 !important;
          border-color: #1E6645 !important;
          font-weight: 600;
        }

        .btn-success:hover {
          background-color: #27855a !important;
          border-color: #27855a !important;
        }

        .btn-info {
          background-color: #2A6496 !important;
          border-color: #2A6496 !important;
          font-weight: 600;
        }

        .btn-info:hover {
          background-color: #3a7abf !important;
          border-color: #3a7abf !important;
        }

        /* ---- Step labels ---- */
        .step-label {
          color: #1E3765;
          font-weight: 700;
          margin-top: 18px;
          margin-bottom: 6px;
          font-size: 14px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        /* ---- Alert boxes ---- */
        .alert-success {
          background-color: #eaf4ee;
          border-color: #b2dbbf;
          color: #1E3765;
        }

        .alert-danger {
          background-color: #fdf0f0;
          border-color: #f5c6cb;
          color: #7b1a1a;
        }

        /* ---- Tables ---- */
        .dataTables_wrapper { margin-top: 10px; }

        /* ---- Footer ---- */
        .app-footer {
          padding: 10px 15px;
          border-top: 1px solid rgba(30,55,101,0.15);
          margin-top: 20px;
          font-size: 0.85em;
          color: #666;
        }

      "))
    ),
    
    # -------------------------------------------------------------------------
    # Banner image
    # -------------------------------------------------------------------------
    div(
      class = "header-bar",
      tags$img(
        src   = paste0("space_banner_sw.png?v=",
                       format(Sys.time(), "%Y%m%d%H%M%S")),
        style = "
          width: 65%;
          height: auto;
          display: block;
          object-fit: contain;
        "
      )
    )
  )
}