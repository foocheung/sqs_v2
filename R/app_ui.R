#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
## usethis::use_pipe()
app_ui <- function() {
  shinyUI(pageWithSidebar(
    HTML("<CENTER><H2>Extended - SomaScan Assay Quality Statement (SQS)"),
    tabPanel(
      "Load Data",
      sidebarPanel(
        mod_dataInput_ui("dataInput_ui_meta"),
        downloadButton("downloadReport", "Download PDF Report")
      )
    ),
    mainPanel(
      mod_table_ui("table_ui_1", "Meta Data")
    )
  ))
}

golem_add_external_resources <- function(){
  add_resource_path(
    'www', app_sys('app/www')
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = ''
    )
  )
}