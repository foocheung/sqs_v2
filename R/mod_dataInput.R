#' dataInput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

addResourcePath("d", "extdata/")
mod_dataInput_ui <- function(id){
  ns <- NS(id)

  tagList(
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(id = "Options",
                                  radioButtons(ns("dataInput_soma"), "", list("Upload Your Data"=2,"Load Example Data"=1),selected = 2),
                                  conditionalPanel(condition="input.dataInput_soma=='2'", ns = ns,
                                                   fileInput(ns("adat_file"), label="Upload")
                                  ),
                                  actionButton(ns("go"), "Go!")
      )
    )
  )
}

#' dataInput Server Functions
#'
#' @noRd
mod_dataInput_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    userFile <- reactive({
      validate(need(input$adat_file !="", "Hello Please import a data file"))
      input$adat_file
    })

    datafile <- reactive({
      if (input$dataInput_soma == 1) {
        withProgress(message = 'Loading Example Data...', {
          # Load example ADAT data from local inst/data/
          load_example_adat()
        })
      } else {
        withProgress(message = 'Uploading and Parsing Data...', {
          SomaDataIO::read_adat(userFile()$datapath)
        })
      }
    })

    datafile2 <- reactive({
      if (input$dataInput_soma == 1) {
        withProgress(message = 'Loading Example Data...', {
          # Load and parse example ADAT header from local inst/data/
          load_example_adat_header()
        })
      } else {
        withProgress(message = 'Uploading and Parsing Data...', {
          SomaDataIO::parseHeader(userFile()$datapath)
        })
      }
    })

    return(list(go   = reactive({input$go}),
                df = datafile,
                df2 = datafile2
    ))
  })
}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
