# Required packages
library(shiny)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(forcats)
library(tidyselect)

# Source global functions (includes plot_levey and ks_test)


# ---- Helpers (bytes, duration, CV, Levey-Jennings, HTML inject) ----

pretty_bytes <- function(bytes) {
  units <- c("B","KB","MB","GB","TB")
  if (is.na(bytes) || bytes < 1) return("0 B")
  pow <- min(floor(log(bytes, 1024)), length(units) - 1)
  sprintf("%.1f %s", bytes / (1024^pow), units[pow + 1])
}

pretty_duration <- function(sec) {
  sec <- as.numeric(sec)
  if (sec < 60) return(sprintf("%.2fs", sec))
  mins <- floor(sec / 60); secs <- round(sec - mins * 60, 1)
  sprintf("%dm %.1fs", mins, secs)
}

safe_cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(m) || m <= 0) return(NA_real_)
  s / m
}

# Inject a small HTML footer with run metadata into the report
.inject_report_footer <- function(html_path, run_time, duration_str, size_str) {
  footer <- paste0(
    "\n<!-- QC Report metadata -->\n",
    "<div style='margin-top:2rem;padding-top:1rem;border-top:1px solid #ddd;",
    "font-size:0.9em;color:#555;'>\n",
    "<strong>Report generated:</strong> ", run_time, "<br/>\n",
    "<strong>Duration:</strong> ", duration_str, "<br/>\n",
    "<strong>File size:</strong> ", size_str, "\n",
    "</div>\n"
  )

  txt <- tryCatch(readLines(html_path, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
  if (is.null(txt)) {
    cat(footer, file = html_path, append = TRUE)
    return(invisible(TRUE))
  }

  body_idx <- tail(grep("</body>", txt, ignore.case = TRUE), 1)
  if (length(body_idx) == 1L && is.finite(body_idx)) {
    txt <- append(txt, values = footer, after = body_idx - 1)
    writeLines(txt, html_path, useBytes = TRUE)
  } else {
    writeLines(c(txt, footer), html_path, useBytes = TRUE)
  }
  invisible(TRUE)
}

# ============================================================================
# MAIN SERVER FUNCTION
# ============================================================================

app_server <- function(input, output, session) {
  # Set upload size limit once at app start
  options(shiny.maxRequestSize = 500 * 1024^2)

  # Modules provided by your app
  metafile <- mod_dataInput_server("dataInput_ui_meta")
  callModule(mod_table_server, "table_ui_1", metafile)

  # NEW: Data Export Module
  mod_dataExport_server("dataExport_1", metafile)

  # ---- Reactive values to store generated HTML ----
  html_report <- reactiveVal(NULL)
  report_error <- reactiveVal(NULL)

  # ---- GENERATE REPORT BUTTON ----
  observeEvent(input$generateReport, {
    req(metafile$df(), metafile$df2())

    report_error(NULL)

    tryCatch({
      shiny::withProgress(message = "Generating HTML report...", value = 0, {

        # Workspace setup
        shiny::incProgress(0.1, detail = "Setting up workspace...")
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
        temp_dir <- file.path(tempdir(), paste0("proteomics_qc_", timestamp))
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
        plot_dir <- file.path(temp_dir, "plots")
        dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

        # Validate inputs
        shiny::incProgress(0.15, detail = "Validating input...")
        if (is.null(metafile$df()) || is.null(metafile$df2())) {
          stop("Input data is missing. Please ensure data is loaded correctly.")
        }

        # Generate plots
        shiny::incProgress(0.35, detail = "Generating plots...")
        plot_files <- generate_plots(metafile, plot_dir)

        # Generate R Markdown content
        shiny::incProgress(0.6, detail = "Preparing R Markdown...")
        rmd_content <- generate_rmd_report_html(metafile, plot_files, temp_dir)
        rmd_file <- file.path(temp_dir, "report.Rmd")
        writeLines(rmd_content, rmd_file)

        # Render to self-contained HTML (no subprocess)
        shiny::incProgress(0.85, detail = "Rendering HTML...")
        output_file <- file.path(temp_dir, "report.html")

        rmarkdown::render(
          input = rmd_file,
          output_file = output_file,
          output_format = rmarkdown::html_document(
            toc = TRUE,
            number_sections = TRUE,
            df_print = "paged",
            theme = "flatly",
            highlight = "tango",
            self_contained = TRUE
          ),
          quiet = TRUE,
          envir = new.env(parent = globalenv())
        )

        # Add metadata footer
        run_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
        dur <- "~"
        size <- if (file.exists(output_file)) pretty_bytes(file.info(output_file)$size) else "0 B"

        try(.inject_report_footer(output_file, run_stamp, dur, size), silent = TRUE)

        # Read HTML and store in reactive
        html_content <- readLines(output_file, warn = FALSE, encoding = "UTF-8")
        html_content <- paste(html_content, collapse = "\n")
        html_report(html_content)

        # Store file path and timestamp for download/save
        session$userData$report_file <- output_file
        session$userData$report_timestamp <- timestamp

        shiny::incProgress(1, message = paste("Done!", size))
      })

    }, error = function(e) {
      report_error(paste("Error generating report:", conditionMessage(e)))
      shiny::showNotification(
        paste("Report generation failed:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
    })
  })

  # ---- Display Report Status ----
  output$reportStatus <- renderUI({
    if (!is.null(report_error())) {
      return(div(
        style = "color: red; margin-top: 20px; padding: 10px; border: 1px solid red; border-radius: 5px;",
        icon("exclamation-circle"),
        report_error()
      ))
    }
    if (!is.null(html_report())) {
      return(div(
        style = "color: green; margin-top: 20px; padding: 10px; border: 1px solid green; border-radius: 5px;",
        icon("check-circle"),
        "✓ Report ready! Switch to 'Report Preview' tab to view."
      ))
    }
    return(NULL)
  })

  # ---- Display HTML Report in Tab ----
  output$htmlReportPreview <- renderUI({
    req(html_report())

    tags$iframe(
      srcdoc = html_report(),
      style = "width: 100%; height: 900px; border: none;",
      title = "Proteomics QC Report"
    )
  })

  # ---- DOWNLOAD BUTTON ----
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Proteomics_QC_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(html_report())

      if (is.null(session$userData$report_file)) {
        showNotification(
          "Please generate report first using 'Generate HTML Report' button",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      file.copy(
        session$userData$report_file,
        file,
        overwrite = TRUE
      )
    }
  )

  # ---- SAVE LOCAL BUTTON ----
  observeEvent(input$saveReportLocal, {
    req(html_report())

    if (is.null(session$userData$report_file)) {
      showNotification(
        "Please generate report first using 'Generate HTML Report' button",
        type = "error",
        duration = 5
      )
      return()
    }

    tryCatch({
      default_name <- paste0("Proteomics_QC_Report_", Sys.Date(), ".html")
      save_path <- file.choose(new = TRUE)

      if (!is.null(save_path) && nzchar(save_path)) {
        file.copy(
          session$userData$report_file,
          save_path,
          overwrite = TRUE
        )
        showNotification(
          paste("Report saved to:", save_path),
          type = "message",
          duration = 5
        )
      }
    }, error = function(e) {
      showNotification(
        "Save operation cancelled or failed",
        type = "warning",
        duration = 3
      )
    })
  })

  # ---- PLOT GENERATION FUNCTION ----
  generate_plots <- function(metafile, plot_dir) {
    # PCA plot
    pca_file <- file.path(plot_dir, "pca_sample_type.png")
    p_pca <- tryCatch({
      # Perform PCA
      pca_res <- stats::prcomp(
        metafile$df() %>%
          dplyr::select(tidyselect::starts_with("seq.")) %>%
          as.matrix(),
        scale. = TRUE
      )

      # Calculate variance explained
      var_exp <- pca_res$sdev^2 / sum(pca_res$sdev^2) * 100

      # Create data frame for plotting
      pca_scores <- as.data.frame(pca_res$x)
      plot_data <- cbind(
        metafile$df()[, c("SampleType", "PlateId", "SampleId")],
        pca_scores
      )

      # Create ggplot directly (avoids autoplot issues)
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = PC1, y = PC2, color = SampleType)
      ) +
        ggplot2::geom_point(size = 3, alpha = 0.7) +
        ggplot2::labs(
          title = "Principal Component Analysis by Sample Type",
          x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
          y = paste0("PC2 (", round(var_exp[2], 1), "%)"),
          color = "Sample Type"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 12),
          legend.position = "bottom"
        )

    }, error = function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = paste("PCA failed:", e$message),
          size = 4,
          color = "red"
        ) +
        ggplot2::theme_void()
    })

    ggplot2::ggsave(pca_file, p_pca, width = 8, height = 6, dpi = 150)

    # Levey-Jennings plots
    levey_cal_file <- file.path(plot_dir, "levey_calibrator.png")
    levey_qc_file <- file.path(plot_dir, "levey_somalogic_qc.png")

    df_cvs_all <- tryCatch({
      foodata2::load_data4()
    }, error = function(e) {
      NULL
    })

    if (!is.null(df_cvs_all)) {

      adat_header <- metafile$df2()

      p_levey_cal <- tryCatch({
        plot_levey(
          metafile$df(),
          adat_header,
          df_cvs_all,
          sample_type = "Calibrator",
          center = "median",
          show_zones = TRUE
        )
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = paste("Calibrator trend plot failed:", e$message),
            size = 4,
            color = "red"
          ) +
          ggplot2::theme_void()
      })
      ggplot2::ggsave(levey_cal_file, p_levey_cal, width = 10, height = 6, dpi = 150)

      p_levey_qc <- tryCatch({
        plot_levey(
          metafile$df(),
          adat_header,
          df_cvs_all,
          sample_type = "QC",
          center = "median",
          show_zones = TRUE
        )
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = paste("Reference material trend plot failed:", e$message),
            size = 4,
            color = "red"
          ) +
          ggplot2::theme_void()
      })
      ggplot2::ggsave(levey_qc_file, p_levey_qc, width = 10, height = 6, dpi = 150)
    } else {
      # Create placeholder plots
      placeholder_plot <- ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Historical data not available",
          size = 4,
          color = "grey50"
        ) +
        ggplot2::theme_void()
      ggplot2::ggsave(levey_cal_file, placeholder_plot, width = 10, height = 6, dpi = 150)
      ggplot2::ggsave(levey_qc_file, placeholder_plot, width = 10, height = 6, dpi = 150)
    }

    list(
      pca_sample_type = pca_file,
      levey_calibrator = levey_cal_file,
      levey_somalogic_qc = levey_qc_file
    )
  }

  # ---- R MARKDOWN REPORT GENERATION ----
  generate_rmd_report_html <- function(metafile, plot_files, temp_dir) {
    # Sample summary
    samp_summary <- metafile$df() %>%
      dplyr::group_by(SampleType) %>%
      dplyr::summarise(Count = dplyr::n(), .groups = "drop")

    # Flagged samples
    flagged_samples <- metafile$df() %>%
      dplyr::filter(RowCheck == "FLAG") %>%
      dplyr::select(PlateId, SampleId, SampleType, RowCheck) %>%
      dplyr::arrange(PlateId, SampleId)

    # Median normalization summary
    med_norm_summary <- metafile$df() %>%
      dplyr::select(SampleType, NormScale_20) %>%
      dplyr::group_by(SampleType) %>%
      dplyr::summarise(
        Mean = round(mean(NormScale_20, na.rm = TRUE), 2),
        SD   = round(sd(NormScale_20, na.rm = TRUE), 2),
        .groups = "drop"
      )

    # ANML summary
    anml_summary <- metafile$df() %>%
      dplyr::filter(SampleType == "Sample") %>%
      dplyr::select(ANMLFractionUsed_20) %>%
      dplyr::summarise(
        `10%` = round(quantile(ANMLFractionUsed_20, 0.1, na.rm = TRUE), 2),
        `50%` = round(median(ANMLFractionUsed_20, na.rm = TRUE), 2),
        `90%` = round(quantile(ANMLFractionUsed_20, 0.9, na.rm = TRUE), 2)
      )

    # Pass/flag summary by dilution
    pass_flag <- metafile$df() %>%
      dplyr::filter(SampleType == "Sample") %>%
      dplyr::group_by(PlateId) %>%
      dplyr::summarise(Sample = dplyr::n(), .groups = "drop")

    med_norm_flag <- metafile$df() %>%
      dplyr::select(PlateId, NormScale_20, NormScale_0_005, NormScale_0_5) %>%
      dplyr::mutate(dplyr::across(-PlateId, ~ dplyr::if_else(. > 0.4 & . < 2.5, "Pass", "Fail"))) %>%
      tidyr::gather(key = "Dilution Group", value = "Decision") %>%
      dplyr::filter(Decision == "Pass") %>%
      dplyr::group_by(`Dilution Group`) %>%
      dplyr::summarise(Pass = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(
        Flag  = sum(pass_flag$Sample) - Pass,
        Total = sum(pass_flag$Sample)
      )

    # Header/meta
    adat_header <- metafile$df2()
    keys <- names(adat_header$Header.Meta$HEADER)

    # Plate scale
    df_plate_scale <- {
      keys_scalar <- grep("^PlateScale_Scalar", keys, value = TRUE)
      keys_pass   <- grep("^PlateScale_PassFlag", keys, value = TRUE)

      pass <- data.frame(`Plate Check` = unlist(adat_header$Header.Meta$HEADER[keys_pass]),
                         check.names = FALSE) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^PlateScale_PassFlag_", "", Plate))

      scalar <- data.frame(Value = unlist(adat_header$Header.Meta$HEADER[keys_scalar]),
                           check.names = FALSE) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^PlateScale_Scalar_", "", Plate))

      dplyr::inner_join(pass, scalar, by = "Plate") %>%
        dplyr::transmute(
          Plate,
          `Acceptance Criteria` = "0.4 - 2.5",
          `Plate Check` = .data[["Plate Check"]],
          Value = round(as.numeric(.data[["Value"]]), 2)
        )
    }

    # Calibrator percent in tails
    df_cal_perc_tails <- {
      keys_pct <- grep("^CalPlateTailPercent", keys, value = TRUE)
      keys_tst <- grep("^CalPlateTailTest",    keys, value = TRUE)

      test <- data.frame(`Plate Check` = unlist(adat_header$Header.Meta$HEADER[keys_tst]),
                         check.names = FALSE) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^CalPlateTailTest_", "", Plate))

      pct <- data.frame(Value = unlist(adat_header$Header.Meta$HEADER[keys_pct]),
                        check.names = FALSE) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^CalPlateTailPercent_", "", Plate))

      dplyr::inner_join(test, pct, by = "Plate") %>%
        dplyr::transmute(
          Plate,
          `Acceptance Criteria` = "Less than 10%",
          `Plate Check` = .data[["Plate Check"]],
          Value = round(as.numeric(.data[["Value"]]), 2)
        )
    }

    # SOMAmers in tails
    df_SOMAmers_tails <- data.frame(
      "SeqId"            = adat_header$Col.Meta$SeqId,
      "EntrezGeneSymbol" = adat_header$Col.Meta$EntrezGeneSymbol,
      "Organism"         = adat_header$Col.Meta$Organism,
      "ColCheck"         = adat_header$Col.Meta$ColCheck
    )
    counts <- table(df_SOMAmers_tails$ColCheck, useNA = "no")
    n_flag  <- if (!is.na(counts["FLAG"])) as.integer(counts["FLAG"]) else 0L
    n_pass  <- if (!is.na(counts["PASS"])) as.integer(counts["PASS"]) else 0L
    n_total <- n_flag + n_pass
    somamers_summary <- tibble::tibble(
      `Protein Target` = "QC Accuracy",
      `Acceptance Criteria` = "0.8 - 1.2",
      `FLAG` = n_flag,
      `PASS` = n_pass,
      `Total` = n_total
    )

    # Calibrator CVs (per plate)
    df_cvs <- metafile$df() %>%
      dplyr::filter(SampleType == "Calibrator") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("seq."), ~ suppressWarnings(as.numeric(.)))) %>%
      dplyr::select(PlateId, dplyr::starts_with("seq.")) %>%
      dplyr::group_by(PlateId) %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("seq."), safe_cv), .groups = "drop") %>%
      tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
      dplyr::filter(is.finite(CV)) %>%
      dplyr::group_by(PlateId) %>%
      dplyr::summarise(
        `10%` = if (dplyr::n() == 0) NA_real_ else round(quantile(CV, 0.1, na.rm = TRUE) * 100, 1),
        `50%` = if (dplyr::n() == 0) NA_real_ else round(quantile(CV, 0.5, na.rm = TRUE) * 100, 1),
        `90%` = if (dplyr::n() == 0) NA_real_ else round(quantile(CV, 0.9, na.rm = TRUE) * 100, 1),
        .groups = "drop"
      )

    # QC CVs (overall)
    df_cvs_qc <- metafile$df() %>%
      dplyr::filter(SampleType == "QC") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("seq."), ~ suppressWarnings(as.numeric(.)))) %>%
      dplyr::select(PlateId, dplyr::starts_with("seq.")) %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("seq."), safe_cv)) %>%
      tidyr::gather(key = "SeqId", value = "CV") %>%
      dplyr::filter(is.finite(CV)) %>%
      dplyr::summarise(
        `10%` = if (dplyr::n() == 0) NA_real_ else round(quantile(CV, 0.1, na.rm = TRUE) * 100, 1),
        `50%` = if (dplyr::n() == 0) NA_real_ else round(median(CV, na.rm = TRUE) * 100, 1),
        `90%` = if (dplyr::n() == 0) NA_real_ else round(quantile(CV, 0.9, na.rm = TRUE) * 100, 1)
      )

    qc_cv_summary <- metafile$df() %>%
      dplyr::filter(SampleType == "QC") %>%
      dplyr::select(Barcode) %>%
      dplyr::mutate(nSamples = dplyr::n()) %>%
      unique() %>%
      dplyr::bind_cols(., df_cvs_qc) %>%
      dplyr::rename(`QC Lot` = Barcode)

    # Save for Rmd
    saveRDS(samp_summary,      file.path(temp_dir, "samp_summary.rds"))
    saveRDS(flagged_samples,   file.path(temp_dir, "flagged_samples.rds"))
    saveRDS(med_norm_summary,  file.path(temp_dir, "med_norm_summary.rds"))
    saveRDS(anml_summary,      file.path(temp_dir, "anml_summary.rds"))
    saveRDS(df_plate_scale,    file.path(temp_dir, "df_plate_scale.rds"))
    saveRDS(df_cal_perc_tails, file.path(temp_dir, "df_cal_perc_tails.rds"))
    saveRDS(somamers_summary,  file.path(temp_dir, "somamers_summary.rds"))
    saveRDS(df_cvs,            file.path(temp_dir, "df_cvs.rds"))
    saveRDS(qc_cv_summary,     file.path(temp_dir, "qc_cv_summary.rds"))

    # R Markdown body
    c(
      '---',
      'title: "Plasma Proteomics Data Quality Control Report"',
      'subtitle: "Standardization and Reproducibility Assessment"',
      paste0('date: "', Sys.Date(), '"'),
      'output:',
      '  html_document:',
      '    toc: true',
      '    number_sections: true',
      '    df_print: paged',
      '    theme: flatly',
      '    highlight: tango',
      '    self_contained: true',
      '---',
      '',
      '```{r setup, include=FALSE}',
      'knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)',
      'library(dplyr); library(knitr); library(kableExtra)',
      '```',
      '',
      '# Introduction',
      'This report documents standardized quality control assessment of plasma proteomics assay data.',
      '',
      '# Experimental Design',
      '```{r sample_summary}',
      paste0('samp_summary <- readRDS("', file.path(temp_dir, "samp_summary.rds"), '")'),
      'kable(samp_summary, caption = "Sample Composition Summary") %>% kable_styling()',
      '```',
      '',
      '# Sample Type Separation',
      paste0('![](', plot_files$pca_sample_type, ')'),
      '',
      '# Sample Quality Assessment',
      '## Flagged Samples',
      '```{r flagged_samples}',
      paste0('flagged_samples <- readRDS("', file.path(temp_dir, "flagged_samples.rds"), '")'),
      'if (nrow(flagged_samples) == 0) { cat("No samples flagged for quality concerns.") } else { kable(flagged_samples) %>% kable_styling() }',
      '```',
      '',
      '## Normalization Scale Factors',
      '```{r med_norm_summary}',
      paste0('med_norm_summary <- readRDS("', file.path(temp_dir, "med_norm_summary.rds"), '")'),
      'kable(med_norm_summary, caption = "Normalization Scale Factor Summary") %>% kable_styling()',
      '```',
      '',
      '## ANML Fraction Used',
      '```{r anml_summary}',
      paste0('anml_summary <- readRDS("', file.path(temp_dir, "anml_summary.rds"), '")'),
      'kable(anml_summary, caption = "ANML Fraction Summary") %>% kable_styling()',
      '```',
      '',
      '# Calibration & Reproducibility',
      '## Plate Scale',
      '```{r plate_scale}',
      paste0('df_plate_scale <- readRDS("', file.path(temp_dir, "df_plate_scale.rds"), '")'),
      'kable(df_plate_scale) %>% kable_styling()',
      '```',
      '',
      '## Calibrator Signal in Tails',
      '```{r cal_perc_tails}',
      paste0('df_cal_perc_tails <- readRDS("', file.path(temp_dir, "df_cal_perc_tails.rds"), '")'),
      'kable(df_cal_perc_tails) %>% kable_styling()',
      '```',
      '',
      '## Protein Targets in Tails',
      '```{r somamers_summary}',
      paste0('somamers_summary <- readRDS("', file.path(temp_dir, "somamers_summary.rds"), '")'),
      'kable(somamers_summary, caption = "Calibration Accuracy Assessment") %>% kable_styling()',
      '```',
      '',
      '## Calibrator Precision per Plate',
      '```{r df_cvs}',
      paste0('df_cvs <- readRDS("', file.path(temp_dir, "df_cvs.rds"), '")'),
      'kable(df_cvs, caption = "Calibrator Coefficient of Variation") %>% kable_styling()',
      '```',
      '',
      '### Calibrator Quality Trend',
      paste0('![](', plot_files$levey_calibrator, ')'),
      '',
      '## Reference Material Precision',
      '```{r qc_cv_summary}',
      paste0('qc_cv_summary <- readRDS("', file.path(temp_dir, "qc_cv_summary.rds"), '")'),
      'kable(qc_cv_summary, caption = "Reference Material Coefficient of Variation") %>% kable_styling()',
      '```',
      '',
      '### Reference Material Quality Trend',
      paste0('![](', plot_files$levey_somalogic_qc, ')'),
      '',
      '# Conclusion',
      'Quality control assessment is complete. Review flagged samples and metrics above.',
      'Contact your analytical team for interpretation and next steps.'
    )
  }
}
