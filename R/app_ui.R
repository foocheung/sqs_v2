#' The Application User-Interface - IMPROVED VERSION WITH DATA EXPORT
#'
#' Enhanced UI with data export functionality and neutral terminology
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function() {
  shinyUI(
    fluidPage(
      # Use a modern theme
      theme = shinythemes::shinytheme("flatly"),

      # Custom CSS for better styling
      tags$head(
        tags$style(HTML("
          .main-header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            margin-bottom: 20px;
            border-radius: 8px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          }
          .main-header h2 {
            margin: 0;
            font-weight: 600;
            font-size: 28px;
          }
          .main-header p {
            margin: 5px 0 0 0;
            font-size: 14px;
            opacity: 0.9;
          }
          .card-section {
            background: white;
            padding: 20px;
            margin-bottom: 15px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          }
          .card-section h4 {
            margin-top: 0;
            color: #667eea;
            font-weight: 600;
            border-bottom: 2px solid #f0f0f0;
            padding-bottom: 10px;
          }
          .btn-action {
            margin-bottom: 10px;
            width: 100%;
            font-weight: 500;
            transition: all 0.3s ease;
          }
          .btn-action:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 8px rgba(0,0,0,0.15);
          }
          .status-box {
            padding: 15px;
            border-radius: 6px;
            margin-top: 15px;
            font-size: 14px;
          }
          .status-success {
            background-color: #d4edda;
            border: 1px solid #c3e6cb;
            color: #155724;
          }
          .status-error {
            background-color: #f8d7da;
            border: 1px solid #f5c6cb;
            color: #721c24;
          }
          .info-text {
            color: #6c757d;
            font-size: 13px;
            margin-top: 5px;
          }
          .tab-content {
            padding: 20px;
            background: white;
            border-radius: 0 0 8px 8px;
          }
          .nav-tabs {
            border-bottom: 2px solid #667eea;
          }
          .nav-tabs > li.active > a {
            background-color: #667eea !important;
            color: white !important;
            border-color: #667eea !important;
          }
        "))
      ),

      # Header
      div(
        class = "main-header",
        h2(
          icon("chart-line"),
          " Plasma Proteomics Quality Control Report"
        ),
        p("Comprehensive standardization assessment and quality control for proteomics assay data")
      ),

      # Main Layout
      sidebarLayout(

        # Sidebar Panel
        sidebarPanel(
          width = 3,

          # Data Input Section
          div(
            class = "card-section",
            h4(icon("upload"), " Data Input"),
            mod_dataInput_ui("dataInput_ui_meta"),
            div(
              class = "info-text",
              icon("info-circle"),
              " Upload your assay data file to begin analysis"
            )
          ),

          # Report Generation Section
          div(
            class = "card-section",
            h4(icon("file-alt"), " Report Generation"),

            actionButton(
              "generateReport",
              label = tagList(icon("cogs"), " Generate HTML Report"),
              class = "btn btn-primary btn-action"
            ),

            div(
              class = "info-text",
              "Creates a comprehensive QC report with plots and tables"
            ),

            # Status indicator
            uiOutput("reportStatus")
          ),

          # Download/Save Section
          div(
            class = "card-section",
            h4(icon("download"), " Report Export"),

            downloadButton(
              "downloadReport",
              label = "Download Report (HTML)",
              class = "btn btn-success btn-action"
            ),

            actionButton(
              "saveReportLocal",
              label = tagList(icon("save"), " Save to Local Folder"),
              class = "btn btn-info btn-action"
            ),

            div(
              class = "info-text",
              icon("lightbulb"),
              " Tip: Save to a specific folder for easy access later"
            )
          ),

          # App Info Section
          div(
            class = "card-section",
            h4(icon("info"), " Information"),
            p(
              style = "font-size: 12px; color: #6c757d; margin: 0;",
              strong("Version:"), " 2.1 (With Data Export)", br(),
              strong("Max Upload:"), " 500 MB", br(),
              strong("Features:"), " QC Assessment, Reports, Data Export"
            )
          )
        ),

        # Main Panel
        mainPanel(
          width = 9,

          tabsetPanel(
            id = "mainTabs",
            type = "tabs",

            # ---- TAB 1: Data & Tables ----
            tabPanel(
              title = tagList(icon("table"), " Data & Analysis"),
              value = "data_tab",
              div(
                class = "tab-content",
                h3("Data Overview"),
                p("View and analyze your proteomics assay data"),
                hr(),
                mod_table_ui("table_ui_1", "Meta Data")
              )
            ),

            # # ---- TAB 2: Quality Control Plots ----
            # tabPanel(
            #   title = tagList(icon("chart-area"), " QC Plots"),
            #   value = "qc_tab",
            #   div(
            #     class = "tab-content",
            #     h3("Quality Control Visualizations"),
            #     p("Interactive quality control plots including reference material trend analysis"),
            #     hr(),
            #
            #     fluidRow(
            #       column(
            #         width = 12,
            #         div(
            #           class = "card-section",
            #           h4("Plot Options"),
            #           p(
            #             class = "info-text",
            #             "QC plots will appear here after generating the report. ",
            #             "Trend plots include color-coded quality zones ",
            #             "(±1 SD, ±2 SD, ±3 SD) for quality assessment."
            #           )
            #         )
            #       )
            #     )
            #   )
            # ),

            # ---- TAB 3: HTML Report Preview ----
            tabPanel(
              title = tagList(icon("eye"), " Report Preview"),
              value = "preview_tab",
              div(
                class = "tab-content",
                h3("Report Preview"),
                p("View the generated HTML report in the browser"),
                hr(),
                shinycssloaders::withSpinner(
                  htmlOutput("htmlReportPreview"),
                  type = 8,
                  color = "#667eea"
                )
              )
            ),

            # ---- TAB 4: DATA EXPORT ----
            tabPanel(
              title = tagList(icon("file-export"), " Data Export"),
              value = "export_tab",
              div(
                class = "tab-content",
                h3("Export Data"),
                p("Export protein abundance matrices and annotations in various formats"),
                hr(),
                mod_dataExport_ui("dataExport_1")
              )
            ),

            # ---- TAB 5: Help & Documentation ----
            tabPanel(
              title = tagList(icon("question-circle"), " Help"),
              value = "help_tab",
              div(
                class = "tab-content",
                h3("Quick Start Guide"),

                div(
                  class = "card-section",
                  h4("1. Upload Data"),
                  p("Click 'Browse' in the Data Input section to upload your assay data file."),
                  p(
                    class = "info-text",
                    "Supported formats: Platform-specific data files up to 500 MB"
                  )
                ),

                div(
                  class = "card-section",
                  h4("2. Generate Report"),
                  p("Click 'Generate HTML Report' to create a comprehensive quality control report."),
                  p(
                    class = "info-text",
                    "This process analyzes your data and creates visualizations including:",
                    tags$ul(
                      tags$li("Sample type separation analysis (PCA)"),
                      tags$li("Reference material quality trend charts with QC zones"),
                      tags$li("Coefficient of variation analysis"),
                      tags$li("Standardization and reproducibility metrics")
                    )
                  )
                ),

                div(
                  class = "card-section",
                  h4("3. Export Data"),
                  p("Use the 'Data Export' tab to export:"),
                  tags$ul(
                    tags$li(
                      strong("Protein Abundance Matrix:"),
                      " Export quantitative protein data in CSV, TSV, Excel, or RData format"
                    ),
                    tags$li(
                      strong("Options:"),
                      " Choose matrix orientation (samples as rows/columns), include metadata, apply log2 transformation"
                    ),
                    tags$li(
                      strong("Protein Annotation Table:"),
                      " Export protein identifiers and annotations (Target, UniProt, Gene Symbols, etc.)"
                    ),
                    tags$li(
                      strong("Formats:"),
                      " CSV, TSV, Excel, or JSON"
                    )
                  ),
                  p(
                    class = "info-text",
                    "The annotation file links protein identifiers to protein names and reference database IDs."
                  )
                ),

                div(
                  class = "card-section",
                  h4("4. Review & Export Report"),
                  p("Review the report in the 'Report Preview' tab, then download or save it."),
                  p(
                    class = "info-text",
                    "The report is a self-contained HTML file that can be shared with colleagues and included in supplementary materials."
                  )
                ),

                div(
                  class = "card-section",
                  h4("Key Features"),
                  tags$ul(
                    tags$li(
                      strong("Reference Material Trend Plots:"),
                      " Quality zone visualization (±1, ±2, ±3 SD) for QC assessment"
                    ),
                    tags$li(
                      strong("Enhanced Visualization:"),
                      " Standardized plot styling with clear legends and annotations"
                    ),
                    tags$li(
                      strong("Quality Control Zones:"),
                      " Color-coded and shape-coded QC reference zones for easy interpretation"
                    ),
                    tags$li(
                      strong("Data Export:"),
                      " Export matrices and annotations in multiple standard formats"
                    ),
                    tags$li(
                      strong("Comprehensive Metrics:"),
                      " Standardization factors, reproducibility measures, and sample-level QC assessment"
                    )
                  )
                ),

                div(
                  class = "card-section",
                  h4("Support"),
                  p(
                    "For questions about data quality assessment or interpretation, please refer to your assay platform documentation or contact your analytical team."
                  )
                )
              )
            )
          )
        )
      ),

      # Footer
      div(
        style = "margin-top: 30px; padding: 20px; text-align: center; border-top: 1px solid #ddd; color: #6c757d; font-size: 12px;",
        p(
          "Plasma Proteomics Quality Control Application | ",
          "Version 2.1 with Data Export | ",
          format(Sys.Date(), "%Y")
        )
      )
    )
  )
}

# Function for adding external resources (if using golem framework)
golem_add_external_resources <- function() {
  tryCatch({
    add_resource_path('www', app_sys('app/www'))
    tags$head(
      favicon(),
      bundle_resources(
        path = app_sys('app/www'),
        app_title = 'Proteomics QC'
      )
    )
  }, error = function(e) {
    # Silently fail if not using golem
    tags$head()
  })
}
