# ==============================================================================
# MODULE: Data Export
# ==============================================================================
# This module provides functionality to export:
# 1. Protein abundance matrix (quantitative protein expression data)
# 2. Protein annotation table (protein identifier mapping)
# ==============================================================================

#' Data Export UI Function
#'
#' @description UI for exporting plasma proteomics data matrices and annotations
#'
#' @param id Module namespace ID
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_dataExport_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "card-section",
      h4(icon("file-export"), " Data Export"),

      # Abundance Matrix Export Section
      div(
        style = "margin-bottom: 25px;",
        h5(
          icon("table"),
          " Protein Abundance Matrix",
          style = "color: #667eea; margin-bottom: 15px;"
        ),
        p(
          class = "info-text",
          "Export quantitative protein abundance data as a matrix for downstream analysis"
        ),

        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("matrix_format"),
              "File Format:",
              choices = c(
                "CSV (Comma-separated)" = "csv",
                "TSV (Tab-separated)" = "tsv",
                "Excel (.xlsx)" = "xlsx",
                "RData (.rds)" = "rds"
              ),
              selected = "csv"
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("matrix_orientation"),
              "Matrix Orientation:",
              choices = c(
                "Samples as rows" = "rows",
                "Samples as columns" = "cols"
              ),
              selected = "rows"
            )
          )
        ),

        checkboxGroupInput(
          ns("matrix_metadata"),
          "Include Sample Metadata:",
          choices = c(
            "PlateId" = "PlateId",
            "SampleId" = "SampleId",
            "SampleType" = "SampleType",
            "Barcode" = "Barcode",
            "All metadata columns" = "all_meta"
          ),
          selected = c("PlateId", "SampleId", "SampleType"),
          inline = FALSE
        ),

        checkboxInput(
          ns("matrix_log_transform"),
          "Apply log2 transformation",
          value = FALSE
        ),

        downloadButton(
          ns("download_matrix"),
          label = tagList(icon("download"), " Download Matrix"),
          class = "btn btn-primary btn-action",
          style = "width: 100%;"
        )
      ),

      hr(),

      # Protein Annotation Export Section
      div(
        style = "margin-bottom: 15px;",
        h5(
          icon("dna"),
          " Protein Annotation Table",
          style = "color: #667eea; margin-bottom: 15px;"
        ),
        p(
          class = "info-text",
          "Export mapping between protein identifiers and annotations"
        ),

        fluidRow(
          column(
            width = 12,
            selectInput(
              ns("mapping_format"),
              "File Format:",
              choices = c(
                "CSV (Comma-separated)" = "csv",
                "TSV (Tab-separated)" = "tsv",
                "Excel (.xlsx)" = "xlsx",
                "JSON" = "json"
              ),
              selected = "csv"
            )
          )
        ),

        checkboxGroupInput(
          ns("mapping_columns"),
          "Include Annotation Columns:",
          choices = c(
            "SeqId" = "SeqId",
            "Target" = "Target",
            "TargetFullName" = "TargetFullName",
            "UniProt" = "UniProt",
            "EntrezGeneID" = "EntrezGeneID",
            "EntrezGeneSymbol" = "EntrezGeneSymbol",
            "Organism" = "Organism",
            "Type" = "Type",
            "Dilution" = "Dilution",
            "ColCheck" = "ColCheck"
          ),
          selected = c("SeqId", "Target", "TargetFullName", "UniProt",
                       "EntrezGeneSymbol", "Organism"),
          inline = FALSE
        ),

        downloadButton(
          ns("download_mapping"),
          label = tagList(icon("download"), " Download Annotations"),
          class = "btn btn-success btn-action",
          style = "width: 100%;"
        )
      ),

      # Status messages
      uiOutput(ns("export_status"))
    )
  )
}

#' Data Export Server Function
#'
#' @description Server logic for exporting protein abundance matrices and annotations
#'
#' @param id Module namespace ID
#' @param metafile Reactive containing the uploaded data
#'
#' @noRd
mod_dataExport_server <- function(id, metafile) {
  moduleServer(id, function(input, output, session) {

    # Reactive for export status messages
    export_message <- reactiveVal(NULL)

    # ---- Download Handler: Protein Abundance Matrix ----
    output$download_matrix <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- switch(
          input$matrix_format,
          "csv" = "csv",
          "tsv" = "tsv",
          "xlsx" = "xlsx",
          "rds" = "rds"
        )
        paste0("Proteomics_Abundance_Matrix_", timestamp, ".", ext)
      },

      content = function(file) {
        req(metafile$df())

        tryCatch({
          withProgress(message = "Preparing abundance matrix export...", value = 0, {

            # Get the data
            incProgress(0.2, detail = "Extracting data...")
            df <- metafile$df()

            # Identify protein target columns
            seq_cols <- grep("^seq\\.", names(df), value = TRUE)

            # Determine metadata columns to include
            incProgress(0.3, detail = "Processing metadata...")
            if ("all_meta" %in% input$matrix_metadata) {
              # All non-protein columns
              meta_cols <- setdiff(names(df), seq_cols)
            } else if (length(input$matrix_metadata) > 0) {
              # Selected metadata columns
              meta_cols <- intersect(input$matrix_metadata, names(df))
            } else {
              # No metadata
              meta_cols <- character(0)
            }

            # Prepare matrix data
            incProgress(0.4, detail = "Building matrix...")
            if (length(meta_cols) > 0) {
              matrix_data <- df %>%
                select(all_of(c(meta_cols, seq_cols)))
            } else {
              matrix_data <- df %>%
                select(all_of(seq_cols))
            }

            # Apply log2 transformation if requested
            if (input$matrix_log_transform) {
              incProgress(0.5, detail = "Applying log2 transformation...")
              matrix_data <- matrix_data %>%
                mutate(across(starts_with("seq."),
                              ~log2(.x),
                              .names = "{.col}"))
            }

            # Transpose if requested
            incProgress(0.6, detail = "Orienting matrix...")
            if (input$matrix_orientation == "cols") {
              # Samples as columns
              if (length(meta_cols) > 0) {
                # Create row names from metadata
                row_id <- apply(matrix_data[, meta_cols, drop = FALSE], 1,
                                function(x) paste(x, collapse = "_"))
                expr_matrix <- as.data.frame(t(matrix_data[, seq_cols]))
                colnames(expr_matrix) <- row_id
                expr_matrix <- tibble::rownames_to_column(expr_matrix, var = "ProteinId")
                expr_matrix$ProteinId <- gsub("^seq\\.", "", expr_matrix$ProteinId)
                matrix_data <- expr_matrix
              } else {
                expr_matrix <- as.data.frame(t(matrix_data))
                matrix_data <- tibble::rownames_to_column(expr_matrix, var = "ProteinId")
                matrix_data$ProteinId <- gsub("^seq\\.", "", matrix_data$ProteinId)
              }
            } else {
              # Samples as rows - clean up protein column names
              matrix_data <- matrix_data %>%
                rename_with(~gsub("^seq\\.", "", .x), starts_with("seq."))
            }

            # Write to file based on format
            incProgress(0.8, detail = "Writing file...")
            switch(
              input$matrix_format,
              "csv" = write.csv(matrix_data, file, row.names = FALSE),
              "tsv" = write.table(matrix_data, file, sep = "\t",
                                  row.names = FALSE, quote = FALSE),
              "xlsx" = {
                if (!requireNamespace("writexl", quietly = TRUE)) {
                  stop("Package 'writexl' is required for Excel export")
                }
                writexl::write_xlsx(matrix_data, file)
              },
              "rds" = saveRDS(matrix_data, file)
            )

            incProgress(1, detail = "Complete!")
            export_message(paste(
              "Abundance matrix exported successfully:",
              nrow(matrix_data), "rows ×", ncol(matrix_data), "columns"
            ))
          })

        }, error = function(e) {
          export_message(paste("Error exporting matrix:", conditionMessage(e)))
          showNotification(
            paste("Export failed:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        })
      }
    )

    # ---- Download Handler: Protein Annotation Table ----
    output$download_mapping <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- switch(
          input$mapping_format,
          "csv" = "csv",
          "tsv" = "tsv",
          "xlsx" = "xlsx",
          "json" = "json"
        )
        paste0("Proteomics_Annotations_", timestamp, ".", ext)
      },

      content = function(file) {
        req(metafile$df2())

        tryCatch({
          withProgress(message = "Preparing annotation table export...", value = 0, {

            # Extract annotation data
            incProgress(0.3, detail = "Extracting annotations...")
            col_meta <- metafile$df2()$Col.Meta

            # Check which columns are available
            available_cols <- intersect(input$mapping_columns, names(col_meta))

            if (length(available_cols) == 0) {
              stop("No selected annotation columns are available in the data")
            }

            # Build annotation table
            incProgress(0.5, detail = "Building annotation table...")
            mapping_data <- col_meta %>%
              as.data.frame() %>%
              select(all_of(available_cols))

            # Add protein identifier if not selected
            if ("SeqId" %in% names(col_meta) && !"SeqId" %in% available_cols) {
              mapping_data <- cbind(
                SeqId = col_meta$SeqId,
                mapping_data
              )
            }

            # Write to file based on format
            incProgress(0.8, detail = "Writing file...")
            switch(
              input$mapping_format,
              "csv" = write.csv(mapping_data, file, row.names = FALSE),
              "tsv" = write.table(mapping_data, file, sep = "\t",
                                  row.names = FALSE, quote = FALSE),
              "xlsx" = {
                if (!requireNamespace("writexl", quietly = TRUE)) {
                  stop("Package 'writexl' is required for Excel export")
                }
                writexl::write_xlsx(mapping_data, file)
              },
              "json" = {
                if (!requireNamespace("jsonlite", quietly = TRUE)) {
                  stop("Package 'jsonlite' is required for JSON export")
                }
                jsonlite::write_json(mapping_data, file, pretty = TRUE)
              }
            )

            incProgress(1, detail = "Complete!")
            export_message(paste(
              "Annotation table exported successfully:",
              nrow(mapping_data), "proteins ×",
              ncol(mapping_data), "annotation fields"
            ))
          })

        }, error = function(e) {
          export_message(paste("Error exporting annotations:", conditionMessage(e)))
          showNotification(
            paste("Export failed:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        })
      }
    )

    # ---- Export Status Display ----
    output$export_status <- renderUI({
      msg <- export_message()
      if (is.null(msg)) return(NULL)

      is_error <- grepl("^Error", msg)
      div(
        class = if (is_error) "status-error" else "status-success",
        style = "margin-top: 15px;",
        icon(if (is_error) "exclamation-circle" else "check-circle"),
        " ", msg
      )
    })

  })
}

## To be copied in the UI
# mod_dataExport_ui("dataExport_1")

## To be copied in the server
# mod_dataExport_server("dataExport_1", metafile)
