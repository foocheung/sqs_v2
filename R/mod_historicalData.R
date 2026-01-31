#' Historical Data Upload UI Function
#'
#' @description Shiny Module for uploading custom historical CV reference data
#'
#' @param id Module namespace ID
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_historicalData_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4(icon("database"), " Historical Reference Data"),

    p(
      style = "font-size: 13px; color: #6c757d;",
      "Upload your own historical coefficient of variation (CV) data to use as reference ",
      "for Levey-Jennings plots. If no data is uploaded, the default built-in data will be used."
    ),

    # File input
    fileInput(
      ns("historicalData_file"),
      label = "Upload Excel file (.xlsx)",
      accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    ),

    # Info box with required columns
    div(
      style = "padding: 10px; background-color: #f8f9fa; border-left: 4px solid #667eea; margin-bottom: 15px; font-size: 12px;",
      strong("Required columns:"),
      tags$ul(
        style = "margin: 5px 0 0 0; padding-left: 20px;",
        tags$li(code("ExpDate"), " - Date (Date format)"),
        tags$li(code("SampleType"), " - Type (text)"),
        tags$li(code("PlateId"), " - Plate ID (text)"),
        tags$li(code("10%"), ", ", code("50%"), ", ", code("90%"), " - CV percentiles (numbers)")
      )
    ),

    # Reset button - MORE PROMINENT
    actionButton(
      ns("useDefault"),
      label = tagList(icon("undo"), " Reset to Default Data"),
      class = "btn btn-warning",
      style = "width: 100%; margin-bottom: 10px; font-weight: 500;"
    ),

    # Status message
    uiOutput(ns("dataStatus"))
  )
}

#' Historical Data Upload Server Function
#'
#' @param id Module namespace ID
#'
#' @noRd
mod_historicalData_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store uploaded data
    custom_cv_data <- reactiveVal(NULL)
    data_source <- reactiveVal("default")
    status_message <- reactiveVal(NULL)

    # Handle file upload
    observeEvent(input$historicalData_file, {
      req(input$historicalData_file)

      tryCatch({
        file_path <- input$historicalData_file$datapath

        # Read Excel file
        data <- readxl::read_xlsx(file_path)
        data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)

        # Clean column names
        colnames(data) <- gsub("`", "", colnames(data))

        # Validate required columns
        required_cols <- c("ExpDate", "SampleType", "PlateId", "10%", "50%", "90%")
        missing_cols <- setdiff(required_cols, colnames(data))

        if (length(missing_cols) > 0) {
          stop(paste(
            "Missing required columns:",
            paste(missing_cols, collapse = ", ")
          ))
        }

        # Convert ExpDate to Date if it's character/numeric
        if ("ExpDate" %in% colnames(data)) {
          data$ExpDate <- as.character(data$ExpDate)
          numeric_dates <- suppressWarnings(as.numeric(data$ExpDate))
          valid_numeric <- !is.na(numeric_dates) & numeric_dates > 0

          if (any(valid_numeric)) {
            data$ExpDate[valid_numeric] <- as.Date(numeric_dates[valid_numeric], origin = "1899-12-30")
            data$ExpDate[!valid_numeric] <- as.Date(data$ExpDate[!valid_numeric])
          } else {
            data$ExpDate <- as.Date(data$ExpDate)
          }
        }

        # Ensure numeric columns are numeric
        data$`10%` <- as.numeric(data$`10%`)
        data$`50%` <- as.numeric(data$`50%`)
        data$`90%` <- as.numeric(data$`90%`)

        # Convert to tibble
        data <- tibble::as_tibble(data)

        # Store in reactive (local to module)
        custom_cv_data(data)
        data_source("custom")

        # ALSO: Store in global environment as backup
        assign("CUSTOM_HISTORICAL_CV_DATA", data, envir = .GlobalEnv)

        cat("\n========================================\n")
        cat("✓ CUSTOM DATA LOADED\n")
        cat("  Dimensions:", nrow(data), "rows ×", ncol(data), "columns\n")
        cat("  Storage: .GlobalEnv variable created\n")
        cat("========================================\n\n")

        status_message(
          list(
            type = "success",
            message = paste0(
              "✓ Custom data loaded successfully! ",
              nrow(data), " rows. ",
              "Your Levey-Jennings plots will now use this data."
            )
          )
        )

      }, error = function(e) {
        status_message(
          list(
            type = "error",
            message = paste("✗ Error loading file:", e$message)
          )
        )
      })
    })

    # Handle "Use Default" button - REMOVES CUSTOM DATA
    observeEvent(input$useDefault, {
      # Clear reactive value
      custom_cv_data(NULL)
      data_source("default")

      # CRITICAL: Remove from global environment
      if (exists("CUSTOM_HISTORICAL_CV_DATA", envir = .GlobalEnv)) {
        rm("CUSTOM_HISTORICAL_CV_DATA", envir = .GlobalEnv)
        cat("\n========================================\n")
        cat("✓ CUSTOM DATA REMOVED\n")
        cat("  Deleted: CUSTOM_HISTORICAL_CV_DATA from .GlobalEnv\n")
        cat("  Status: Now using default built-in data\n")
        cat("========================================\n\n")

        status_message(
          list(
            type = "success",
            message = "✓ Reset complete! Now using default built-in historical data."
          )
        )
      } else {
        cat("\n========================================\n")
        cat("✓ ALREADY USING DEFAULT DATA\n")
        cat("  No custom data was loaded\n")
        cat("========================================\n\n")

        status_message(
          list(
            type = "info",
            message = "Already using default built-in historical data."
          )
        )
      }
    })

    # Status message display
    output$dataStatus <- renderUI({
      msg <- status_message()
      if (is.null(msg)) return(NULL)

      color <- switch(msg$type,
                      "success" = "#d4edda",
                      "error" = "#f8d7da",
                      "info" = "#d1ecf1",
                      "white"
      )
      text_color <- switch(msg$type,
                           "success" = "#155724",
                           "error" = "#721c24",
                           "info" = "#0c5460",
                           "black"
      )
      border_color <- switch(msg$type,
                             "success" = "#c3e6cb",
                             "error" = "#f5c6cb",
                             "info" = "#bee5eb",
                             "#ddd"
      )

      div(
        style = paste0(
          "margin-top: 10px; padding: 12px; background-color: ", color,
          "; border: 1px solid ", border_color,
          "; border-radius: 5px; color: ", text_color, "; font-size: 13px;"
        ),
        msg$message
      )
    })

    # Return reactive data for use in other modules
    return(
      reactive({
        custom_cv_data()
      })
    )
  })
}

## To be copied in the UI
# mod_historicalData_ui("historicalData_1")

## To be copied in the server
# historical_data_reactive <- mod_historicalData_server("historicalData_1")
