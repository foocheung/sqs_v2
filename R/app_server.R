# Required packages
library(shiny)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
library(callr)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(forcats)  # for factor ordering

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
    "\n<!-- SQS run metadata -->\n",
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

# Namespaced, robust Levey–Jennings plot (median-centered by default)
plot_levey <- function(adat_tbl, adat_header, df_cvs_all,
                       sample_type = "QC",
                       sd_levels = c(1, 2),
                       center = c("median", "mean"),
                       y_lab = "Per-plate median CV (%)") {
  center <- base::match.arg(center)
  
  df_cvs_per_plate <- adat_tbl |>
    dplyr::filter(.data$SampleType == sample_type) |>
    dplyr::select(.data$PlateId, tidyselect::starts_with("seq.")) |>
    dplyr::group_by(.data$PlateId) |>
    dplyr::summarise(dplyr::across(where(is.numeric), safe_cv), .groups = "drop")
  
  df_cvs_per_plate_quant <- df_cvs_per_plate |>
    tidyr::pivot_longer(-.data$PlateId, names_to = "SeqId", values_to = "CV") |>
    dplyr::group_by(.data$PlateId) |>
    dplyr::summarise(
      `10%` = round(stats::quantile(CV, 0.10, na.rm = TRUE) * 100, 1),
      `50%` = round(stats::median(CV, na.rm = TRUE) * 100, 1),
      `90%` = round(stats::quantile(CV, 0.90, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
  
  exp_date <- as.character(adat_header$Header.Meta$HEADER$ExpDate)
  
  ref_plot_dat <- df_cvs_all |>
    dplyr::filter(.data$SampleType == sample_type) |>
    dplyr::anti_join(df_cvs_per_plate_quant |> dplyr::select(.data$PlateId), by = "PlateId") |>
    dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
    dplyr::mutate(PlateKey = paste0(.data$ExpDate, "-", .data$PlateId), Data = "Reference")
  
  if (nrow(ref_plot_dat) == 0) {
    ref_plot_dat <- df_cvs_all |>
      dplyr::filter(.data$SampleType == sample_type) |>
      dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
      dplyr::mutate(PlateKey = paste0(.data$ExpDate, "-", .data$PlateId), Data = "Reference")
    warning("No historical plates left after exclusion; using all as reference.")
  }
  
  ref_center <- if (center == "median") {
    stats::median(ref_plot_dat$`50%`, na.rm = TRUE)
  } else {
    base::mean(ref_plot_dat$`50%`, na.rm = TRUE)
  }
  ref_sd <- stats::sd(ref_plot_dat$`50%`, na.rm = TRUE)
  
  samp_plot_dat <- df_cvs_per_plate_quant |>
    dplyr::mutate(ExpDate = exp_date) |>
    dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
    dplyr::mutate(PlateKey = paste0(.data$ExpDate, "-", .data$PlateId), Data = "Sample")
  
  plot_dat <- dplyr::bind_rows(ref_plot_dat, samp_plot_dat) |>
    dplyr::arrange(.data$ExpDate, .data$PlateId) |>
    dplyr::mutate(
      Data = factor(.data$Data, levels = c("Reference", "Sample")),
      PlateKey = as.character(.data$PlateKey),  # ensure character
      PlateKeyShort = dplyr::if_else(
        nchar(.data$PlateKey) > 80,
        paste0(substr(.data$PlateKey, 1, 40), "...", substr(.data$PlateKey, nchar(.data$PlateKey) - 39, nchar(.data$PlateKey))),
        .data$PlateKey
      ),
      PlateKeyShort = forcats::fct_inorder(PlateKeyShort)
    )
  
  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(x = .data$PlateKeyShort,
                                    y = `50%`,
                                    group = 1,
                                    color = .data$Data,
                                    text = .data$PlateKey)) +  # tooltip keeps full PlateKey
    ggplot2::geom_hline(yintercept = ref_center, linewidth = 0.5)
  
  for (k in sd_levels) {
    p <- p +
      ggplot2::geom_hline(yintercept = ref_center + k * ref_sd,
                          linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_hline(yintercept = ref_center - k * ref_sd,
                          linetype = "dashed", linewidth = 0.3)
  }
  
  p +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(breaks = c("Reference", "Sample"),
                                values = c("#56B4E9", "#E69F00")) +
    ggplot2::labs(y = y_lab, x = "Date - Plate ID") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
}



# Your app option
options(shiny.maxRequestSize = 500 * 1024^2)

app_server <- function(input, output, session) {
  # Set once at app start
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  # Modules provided by your app
  metafile <- mod_dataInput_server("dataInput_ui_meta")
  callModule(mod_table_server, "table_ui_1", metafile)
  
  # ---- Report download (HTML; no TinyTeX needed) ----
  output$downloadReport <- downloadHandler(
    filename = function() paste0("SomaScan_QC_Report_", Sys.Date(), ".html"),
    content  = function(file) {
      
      start_time <- Sys.time()
      success <- FALSE
      
      on.exit({
        end_time <- Sys.time()
        dur  <- pretty_duration(difftime(end_time, start_time, units = "secs"))
        size <- if (file.exists(file)) pretty_bytes(file.info(file)$size) else "0 B"
        shiny::showNotification(
          paste0("Report ", if (success) "completed" else "failed", ": ", dur, " · ", size),
          type = if (success) "message" else "error",
          duration = 10
        )
      }, add = TRUE)
      
      shiny::withProgress(message = "Generating HTML report…", value = 0, {
        # Workspace
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
        temp_dir  <- file.path(tempdir(), paste0("somascan_", timestamp))
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
        plot_dir  <- file.path(temp_dir, "plots")
        dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
        rmd_file  <- file.path(temp_dir, "report.Rmd")
        
        # Validate inputs
        shiny::incProgress(0.15, detail = "Validating input…")
        if (is.null(metafile$df()) || is.null(metafile$df2())) {
          stop("Input data is missing. Please ensure data is loaded correctly.")
        }
        
        # Plots
        shiny::incProgress(0.35, detail = "Generating plots…")
        plot_files <- generate_plots(metafile, plot_dir)
        
        # Rmd content
        shiny::incProgress(0.6, detail = "Preparing R Markdown…")
        rmd_content <- generate_rmd_report_html(metafile, plot_files, temp_dir)
        writeLines(rmd_content, rmd_file)
        
        # Render to self-contained HTML
        shiny::incProgress(0.85, detail = "Rendering HTML…")
        callr::r(
          function(rmd_file, out_file) {
            rmarkdown::render(
              input = rmd_file,
              output_file = out_file,
              output_format = rmarkdown::html_document(
                toc = TRUE,
                number_sections = TRUE,
                df_print = "paged",
                theme = "flatly",
                highlight = "tango",
                self_contained = TRUE
              ),
              quiet = TRUE,
              envir  = new.env(parent = globalenv())
            )
          },
          args = list(rmd_file = rmd_file, out_file = file)
        )
        
        # Mark success and compute final metrics
        success <- TRUE
        end_time <- Sys.time()
        dur  <- pretty_duration(difftime(end_time, start_time, units = "secs"))
        size <- if (file.exists(file)) pretty_bytes(file.info(file)$size) else "0 B"
        
        # Inject metadata footer into the generated HTML
        run_stamp <- format(end_time, "%Y-%m-%d %H:%M:%S %Z")
        try(.inject_report_footer(html_path = file,
                                  run_time = run_stamp,
                                  duration_str = dur,
                                  size_str = size),
            silent = TRUE)
        
        # Progress UI
        shiny::setProgress(1, message = paste0("Done in ", dur, " · ", size),
                           detail = "Click to download.")
      })
    }
  )
  
  # ---- helpers you already have elsewhere ----
  # generate_plots <- function(metafile, plot_dir) { ... }
  # generate_rmd_report_html <- function(metafile, plot_files, temp_dir) { ... }

  
  # ---- helpers ----
  
  # Generate and save plots
  generate_plots <- function(metafile, plot_dir) {
    # PCA: Sample Type
    pca_dat <- metafile$df() %>% dplyr::select(starts_with("seq."))
    pca_res <- prcomp(pca_dat, scale = TRUE)
    pca_scores <- as.data.frame(pca_res$x)
    plot_dat <- cbind(
      metafile$df()[, c("SampleType", "PlateId", "SampleId", "AssayNotes", "SampleNotes", "TimePoint", "SampleGroup")],
      pca_scores
    ) %>% dplyr::mutate(HoverText = paste0("PlateId: ", PlateId, "<br>SampleId: ", SampleId))
    variance_explained_pc1 <- round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 2)
    variance_explained_pc2 <- round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 2)
    plot_pca <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = PC1, y = PC2, color = SampleType)) +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = paste0("PC1 (", variance_explained_pc1, "%)"),
        y = paste0("PC2 (", variance_explained_pc2, "%)"),
        color = "Sample Type"
      ) +
      ggplot2::theme_minimal()
    pca_sample_type_file <- file.path(plot_dir, "pca_sample_type.png")
    ggplot2::ggsave(pca_sample_type_file, plot_pca, width = 8, height = 6, dpi = 300)
    
    # PCA: RowCheck
    avoid_SOMAmers <- foodata2::load_data2()
    avoid_prot <- avoid_SOMAmers %>%
      dplyr::pull(SeqId) %>% paste0("seq.", .) %>% stringr::str_replace_all("-", ".")
    adat_samp_tbl <- metafile$df() %>%
      dplyr::filter(SampleType == "Sample") %>%
      dplyr::select(PlateId, SampleId, RowCheck, starts_with("seq.")) %>%
      dplyr::select(!all_of(avoid_prot))
    pca_dat <- metafile$df() %>% dplyr::filter(SampleType == "Sample") %>%
      dplyr::select(starts_with("seq."))
    pca_res <- prcomp(pca_dat, scale = TRUE)
    pca_scores <- as.data.frame(pca_res$x)
    plot_samp_dat <- cbind(adat_samp_tbl[, c("PlateId", "SampleId", "RowCheck")], pca_scores) %>%
      dplyr::mutate(HoverText = paste0("PlateId: ", PlateId, "<br>SampleId: ", SampleId))
    variance_explained_pc1 <- round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 2)
    variance_explained_pc2 <- round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 2)
    plot_samp_pca_flag <- ggplot2::ggplot(plot_samp_dat, ggplot2::aes(x = PC1, y = PC2, color = RowCheck)) +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = paste0("PC1 (", variance_explained_pc1, "%)"),
        y = paste0("PC2 (", variance_explained_pc2, "%)"),
        color = "Check"
      ) +
      ggplot2::theme_minimal()
    pca_rowcheck_file <- file.path(plot_dir, "pca_sample_rowcheck.png")
    ggplot2::ggsave(pca_rowcheck_file, plot_samp_pca_flag, width = 8, height = 6, dpi = 300)
    
    # Levey–Jennings plots (your functions/data)
    df_cvs_all <- foodata2::load_data4()
    adat_header <- metafile$df2()
    levey_cal <- plot_levey(metafile$df(), adat_header, df_cvs_all, sample_type = "Calibrator")
    levey_calibrator_file <- file.path(plot_dir, "levey_calibrator.png")
    ggplot2::ggsave(levey_calibrator_file, levey_cal, width = 8, height = 6, dpi = 300)
    
    levey_qc <- plot_levey(metafile$df(), adat_header, df_cvs_all, sample_type = "QC")
    levey_somalogic_qc_file <- file.path(plot_dir, "levey_somalogic_qc.png")
    ggplot2::ggsave(levey_somalogic_qc_file, levey_qc, width = 8, height = 6, dpi = 300)
    
    list(
      pca_sample_type     = pca_sample_type_file,
      pca_sample_rowcheck = pca_rowcheck_file,
      levey_calibrator    = levey_calibrator_file,
      levey_somalogic_qc  = levey_somalogic_qc_file
    )
  }
  
  # Build Rmd content for HTML (no LaTeX; includes fixes + NA-safe CVs + robust SOMAmers)
  generate_rmd_report_html <- function(metafile, plot_files, temp_dir) {
    
    # ---- helpers for CVs ----
    safe_cv <- function(x) {
      m <- mean(x, na.rm = TRUE)
      if (!is.finite(m) || m == 0) return(NA_real_)
      sd(x, na.rm = TRUE) / m
    }
    
    # Sample summary
    samp_summary <- as.data.frame.matrix(table(metafile$df()$PlateId, metafile$df()$SampleType)) %>%
      tibble::rownames_to_column("PlateId")
    
    # Flags
    flagged_samples <- metafile$df() %>%
      dplyr::filter(RowCheck == "FLAG") %>%
      dplyr::select(PlateId, SampleId, SampleType)
    
    rowcheck_dat <- metafile$df() %>%
      dplyr::select(PlateId, SampleType, RowCheck)
    pass_flag <- as.data.frame.matrix(table(rowcheck_dat$RowCheck, rowcheck_dat$SampleType))
    
    # Median norm
    df_norm_scale <- metafile$df() %>%
      dplyr::select(PlateId, SampleId, SampleType, NormScale_0_005, NormScale_0_5, NormScale_20) %>%
      dplyr::filter(SampleType == "Sample") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("NormScale"),
                                  ~ ifelse(. < 0.4 | . > 2.5, "Flag", "Pass")))
    med_norm_summary <- df_norm_scale %>%
      dplyr::select(NormScale_0_005, NormScale_0_5, NormScale_20) %>%
      tidyr::gather(key = "Dilution Group", value = "Decision") %>%
      dplyr::filter(Decision == "Pass") %>%
      dplyr::group_by(`Dilution Group`) %>%
      dplyr::summarise(Pass = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(
        Flag  = sum(pass_flag$Sample) - Pass,
        Total = sum(pass_flag$Sample)
      )
    
    # ANML fraction
    df_anml_fraction <- metafile$df() %>%
      dplyr::select(PlateId, SampleId, SampleType, ANMLFractionUsed_0_005, ANMLFractionUsed_0_5, ANMLFractionUsed_20) %>%
      dplyr::filter(SampleType == "Sample") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("ANMLFractionUsed"),
                                  ~ ifelse(. < 0.3, "Flag", "Pass")))
    anml_summary <- df_anml_fraction %>%
      dplyr::select(ANMLFractionUsed_0_005, ANMLFractionUsed_0_5, ANMLFractionUsed_20) %>%
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
    
    # Plate scale (FIX: non-syntactic name "Plate Check")
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
    
    # Calibrator percent in tails (FIX: "Plate Check")
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
    
    # SOMAmers in tails — robust to missing FLAG/PASS
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
      `SOMAmer` = "QC Ratio",
      `Acceptance Criteria` = "0.8 - 1.2",
      `FLAG` = n_flag,
      `PASS` = n_pass,
      `Total` = n_total
    )
    
    # --- Calibrator CVs (per plate) — NA safe ---
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
    
    # --- QC CVs (overall; joined with lot) — NA safe ---
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
    
    # Rmd body (HTML)
    c(
      '---',
      'title: "SomaScan Assay Quality Statement (SQS)"',
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
      'This report provides a quality control overview of SomaScan assay data, including design summaries, PCA, normalization checks, and calibration metrics.',
      '',
      '# Experimental Design',
      '```{r sample_summary}',
      paste0('samp_summary <- readRDS("', file.path(temp_dir, "samp_summary.rds"), '")'),
      'kable(samp_summary, caption = "Sample Summary by Plate and Sample Type") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      '# Sample Type PCA',
      paste0('![](', plot_files$pca_sample_type, ')'),
      '',
      '# Sample Normalization',
      '## Flagged Samples',
      '```{r flagged_samples}',
      paste0('flagged_samples <- readRDS("', file.path(temp_dir, "flagged_samples.rds"), '")'),
      'if (nrow(flagged_samples) == 0) {"No flagged samples."} else {',
      'kable(flagged_samples, caption = "Flagged Samples") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '}',
      '```',
      '',
      '## PCA Highlighting QC Flags',
      paste0('![](', plot_files$pca_sample_rowcheck, ')'),
      '',
      '## Median Normalization Scale Factors (expected 0.4–2.5)',
      '```{r med_norm_summary}',
      paste0('med_norm_summary <- readRDS("', file.path(temp_dir, "med_norm_summary.rds"), '")'),
      'kable(med_norm_summary, caption = "Median Normalization Summary") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      '## ANML Fraction Used (> 0.3)',
      '```{r anml_summary}',
      paste0('anml_summary <- readRDS("', file.path(temp_dir, "anml_summary.rds"), '")'),
      'kable(anml_summary, caption = "ANML Fraction Summary") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      '# Calibration',
      '## Plate Scale (expected 0.4–2.5)',
      '```{r plate_scale}',
      paste0('df_plate_scale <- readRDS("', file.path(temp_dir, "df_plate_scale.rds"), '")'),
      'kable(df_plate_scale, caption = "Plate Scale Summary") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      '## Calibrator Percent in Tails (< 10%)',
      '```{r cal_perc_tails}',
      paste0('df_cal_perc_tails <- readRDS("', file.path(temp_dir, "df_cal_perc_tails.rds"), '")'),
      'kable(df_cal_perc_tails, caption = "Calibrator Percent In Tails") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      '## SOMAmers in Tails (QC ratio 0.8–1.2)',
      '```{r somamers_summary}',
      paste0('somamers_summary <- readRDS("', file.path(temp_dir, "somamers_summary.rds"), '")'),
      'kable(somamers_summary, caption = "SOMAmers In Tails Summary") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      '## Calibrator CVs (per plate)',
      '```{r df_cvs}',
      paste0('df_cvs <- readRDS("', file.path(temp_dir, "df_cvs.rds"), '")'),
      'kable(df_cvs, caption = "Calibrator CVs per Plate") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      paste0('![](', plot_files$levey_calibrator, ')'),
      '',
      '## SomaLogic QC Sample CVs',
      '```{r qc_cv_summary}',
      paste0('qc_cv_summary <- readRDS("', file.path(temp_dir, "qc_cv_summary.rds"), '")'),
      'kable(qc_cv_summary, caption = "QC Sample CVs") %>%',
      '  kable_styling(full_width = TRUE, bootstrap_options = c("striped","condensed"))',
      '```',
      '',
      paste0('![](', plot_files$levey_somalogic_qc, ')')
    )
  }
}
