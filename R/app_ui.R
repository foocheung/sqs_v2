#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
## usethis::use_pipe()
app_ui <- function() {
  shinyUI(
    pageWithSidebar(
      # Header
      headerPanel(HTML("<H2>Extended - SomaScan Assay Quality Statement (SQS)</H2>")),

      # Sidebar Panel
      sidebarPanel(
        mod_dataInput_ui("dataInput_ui_meta"),

        hr(),

        # Report generation section
        h4("Report Generation"),
        actionButton("generateReport", "Generate HTML Report",
                     class = "btn-primary", width = "100%"),
        br(), br(),

        # Download section
        h4("Download/Save"),
        downloadButton("downloadReport", "Download Report",
                       class = "btn-success", width = "100%"),
        br(), br(),

        actionButton("saveReportLocal",
                     label = "💾 Save Report to Folder",
                     class = "btn-info",
                     width = "100%",
                     icon = icon("save")),
        br(), br(),

        # Status message
        uiOutput("reportStatus"),

        width = 3
      ),

      # Main Panel
      mainPanel(
        tabsetPanel(
          # ---- TAB 1: Data & Tables ----
          tabPanel(
            "Data & Analysis",
            mod_table_ui("table_ui_1", "Meta Data")
          ),

          # ---- TAB 2: HTML Report Preview ----
          tabPanel(
            "Report Preview",
            br(),
            shinycssloaders::withSpinner(
              htmlOutput("htmlReportPreview"),
              type = 8,
              color = "#0275d8"
            ),
            style = "padding: 20px;"
          )
        )
      )
    )
  )
}

golem_add_external_resources <- function(){
  add_resource_path(
    'www', app_sys('app/www')
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'SQS'
    )
  )
}
