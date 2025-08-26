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
    # tabsetPanel(
    #  tabPanel(
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
   #   withProgress(message = 'Uploading and Parsing Data...', {
    #  inFile <- input$adat_file
    #  file <- inFile$datapath
     # adat_tbl <- SomaDataIO::read_adat(file)

      if (input$dataInput_soma == 1) {
        withProgress(message = 'Uploading and Parsing Data...', {
          #  SomaDataIO::read_adat("d/CHI-23-009_v4.1_Serum.hybNorm.medNormInt.plateScale.calibrate.anmlQC.qcCheck.anmlSMP.adat")
          foodata2::load_data3()
          ff<<-foodata2::load_data3()
          })
           }

      else{
        withProgress(message = 'Uploading and Parsing Data...', {
         SomaDataIO::read_adat(userFile()$datapath)
        })
}
      })

    datafile2 <- reactive({
      #   withProgress(message = 'Uploading and Parsing Data...', {
      #  inFile <- input$adat_file
      #  file <- inFile$datapath
      # adat_tbl <- SomaDataIO::read_adat(file)

      if (input$dataInput_soma == 1) {
        withProgress(message = 'Uploading and Parsing Data...', {
        #  SomaDataIO::parseHeader("d/CHI-23-009_v4.1_Serum.hybNorm.medNormInt.plateScale.calibrate.anmlQC.qcCheck.anmlSMP.adat")
       foodata2::load_data3B()
            })
      }

      else{
        withProgress(message = 'Uploading and Parsing Data...', {
          SomaDataIO::parseHeader(userFile()$datapath)
        })
      }
    })



    return(list(go   = reactive({input$go}),
                df =datafile,
                df2=datafile2
    )
    )

#})
  })

}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
