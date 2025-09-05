# =========================================
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

# =========================================
# ---- Docker / Proxy friendly defaults ----
# NOTE: 504s usually come from the reverse proxy timing out while the R
# process works. Keep operations chatty so you can see where it stalls.
options(shiny.maxRequestSize = 500 * 1024^2)   # 500 MB uploads
options(timeout = 600)                         # 10 minutes global R timeout

# Toggle super-verbose debugging with SHINY_DEBUG=true in Docker env
DEBUG <- tolower(Sys.getenv("SHINY_DEBUG", "true")) %in% c("1","true","t","yes","y")

# Central log file (persists across the session; inspect with `docker logs`)
LOG_FILE <- file.path("/tmp", sprintf("shiny-debug-%s.log", Sys.getpid()))

# Helper: safe string builder
.sprintf <- function(fmt, ...) paste0(sprintf(fmt, ...))

# Helper: write to console + log + (optionally) UI
log_msg <- function(session = NULL, ..., type = c("info","warn","error"), notify = DEBUG) {
  type <- match.arg(type)
  msg  <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "-", .sprintf(...))
  # Console + file
  cat(msg, "\n")
  try(cat(msg, "\n", file = LOG_FILE, append = TRUE), silent = TRUE)
  # UI notification if desired
  if (!is.null(session) && isTRUE(notify)) {
    ntype <- switch(type, info = "message", warn = "warning", error = "error")
    try(shiny::showNotification(msg, type = ntype, duration = 8), silent = TRUE)
  }
  invisible(msg)
}

# Dump a small environment summary (great for Docker debugging)
dump_env_summary <- function(session = NULL) {
  log_msg(session, "R.version: %s", R.version.string)
  log_msg(session, "Platform: %s", paste(R.version$platform, R.version$arch, R.version$os))
  log_msg(session, "getwd(): %s | tempdir(): %s", getwd(), tempdir())
  log_msg(session, "pandoc: %s", Sys.which("pandoc"))
  log_msg(session, "RSTUDIO_PANDOC: %s", Sys.getenv("RSTUDIO_PANDOC", unset = "<unset>"))
  log_msg(session, "LibPaths: %s", paste(.libPaths(), collapse = " | "))
}

# ---- Environment validation ----
validate_environment <- function(session = NULL) {
  required_packages <- c(
    "shiny","dplyr","tidyr","tibble","stringr",
    "ggplot2","rmarkdown","knitr","kableExtra","forcats"
  )
  missing_packages <- required_packages[!sapply(required_packages, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })]
  if (length(missing_packages) > 0) {
    log_msg(session, "Missing required packages: %s", paste(missing_packages, collapse = ", "),
            type = "error")
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }
  
  if (Sys.info()[["sysname"]] == "Linux") {
    if (!dir.exists("/tmp")) stop("Temp directory /tmp not accessible")
    if (!nzchar(Sys.which("pandoc")))
      log_msg(session, "Pandoc not found — HTML rendering may fail; install pandoc.", type = "warn")
  }
  TRUE
}

# ---- Memory helpers ----
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

check_memory <- function(session = NULL, label = "") {
  # This is coarse; CRAN-safe and Docker-safe.
  mem <- sum(gc()[,2])  # MB used (approx)
  log_msg(session, "GC/Memory%s: %.1f MB used", if (nzchar(label)) paste0(" [", label, "]") else "", mem)
  invisible(mem)
}

safe_cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(m) || m <= 0) return(NA_real_)
  s / m
}

# ---- Timeout wrapper for long operations ----
timeout_wrapper <- function(expr, timeout_seconds = 300, session = NULL, step = "step") {
  t0 <- Sys.time()
  setTimeLimit(cpu = timeout_seconds, elapsed = timeout_seconds)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf), add = TRUE)
  withCallingHandlers(
    expr,
    warning = function(w) {
      log_msg(session, "[%s] WARNING: %s", step, conditionMessage(w), type = "warn")
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      log_msg(session, "[%s] %s", step, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
}

# ---- HTML footer inject (unchanged) ----
inject_report_footer <- function(html_path, run_time, duration_str, size_str) {
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

# ---- Levey–Jennings plot (unchanged logic; added logs in caller) ----
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
      PlateKey = as.character(.data$PlateKey),
      PlateKeyShort = dplyr::if_else(
        nchar(.data$PlateKey) > 80,
        paste0(substr(.data$PlateKey, 1, 40), "...",
               substr(.data$PlateKey, nchar(.data$PlateKey) - 39, nchar(.data$PlateKey))),
        .data$PlateKey
      ),
      PlateKeyShort = forcats::fct_inorder(PlateKeyShort)
    )
  
  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(x = .data$PlateKeyShort,
                                    y = `50%`,
                                    group = 1,
                                    color = .data$Data,
                                    text = .data$PlateKey)) +
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

# =========================================
# ---- Main Shiny server ----
app_server <- function(input, output, session) {
  
  log_msg(session, "DEBUG=%s | Log file: %s", as.character(DEBUG), LOG_FILE)
  dump_env_summary(session)
  
  # Validate environment at startup
  tryCatch({
    validate_environment(session)
    log_msg(session, "Environment validation: OK")
  }, error = function(e) {
    log_msg(session, "Environment validation failed: %s", e$message, type = "error")
    showNotification(paste("Environment validation failed:", e$message),
                     type = "error", duration = NULL)
    stop(e$message)
  })
  
  # If pandoc is present, set RSTUDIO_PANDOC to its folder (helps in Docker)
  if (nzchar(Sys.which("pandoc"))) {
    Sys.setenv(RSTUDIO_PANDOC = dirname(Sys.which("pandoc")))
    log_msg(session, "RSTUDIO_PANDOC set to %s", Sys.getenv("RSTUDIO_PANDOC"))
  }
  
  # Modules provided by your app
  metafile <- mod_dataInput_server("dataInput_ui_meta")
  callModule(mod_table_server, "table_ui_1", metafile)
  
  # =========================================
  # ---- Report download (HTML; Docker-optimized) ----
  output$downloadReport <- downloadHandler(
    filename = function() paste0("SomaScan_QC_Report_", Sys.Date(), ".html"),
    content  = function(file) {
      start_time <- Sys.time()
      success    <- FALSE
      error_msg  <- NULL
      
      on.exit({
        end_time <- Sys.time()
        dur      <- pretty_duration(difftime(end_time, start_time, units = "secs"))
        size     <- if (file.exists(file)) pretty_bytes(file.info(file)$size) else "0 B"
        if (!is.null(error_msg)) {
          log_msg(session, "FINAL: Report failed after %s: %s", dur, error_msg, type = "error")
          shiny::showNotification(paste0("Report failed: ", error_msg, " (", dur, ")"),
                                  type = "error", duration = 20)
        } else {
          log_msg(session, "FINAL: Report completed in %s · %s", dur, size)
          shiny::showNotification(paste0("Report completed: ", dur, " · ", size),
                                  type = "message", duration = 10)
        }
      }, add = TRUE)
      
      # Wrap the whole pipeline to capture warnings/messages/timing per step
      tryCatch({
        check_memory(session, "start")
        
        shiny::withProgress(message = "Generating HTML report…", value = 0, {
          # 1) Workspace setup
          shiny::incProgress(0.05, detail = "Preparing workspace…")
          t0 <- Sys.time()
          timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          temp_dir  <- file.path(tempdir(), paste0("somascan_", timestamp))
          
          if (!dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE, mode = "0755")) {
            stop("Failed to create temporary directory: ", temp_dir)
          }
          plot_dir <- file.path(temp_dir, "plots")
          if (!dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE, mode = "0755")) {
            stop("Failed to create plot directory: ", plot_dir)
          }
          rmd_file <- file.path(temp_dir, "report.Rmd")
          log_msg(session, "Workspace ready: %s | plots: %s", temp_dir, plot_dir)
          
          # 2) Validate inputs
          shiny::incProgress(0.15, detail = "Validating input…")
          if (is.null(metafile$df()) || is.null(metafile$df2())) {
            stop("Input data is missing. Ensure data are loaded (metafile$df/df2).")
          }
          if (nrow(metafile$df()) == 0) stop("Input data is empty.")
          log_msg(session, "Input rows: %s | cols: %s",
                  nrow(metafile$df()), ncol(metafile$df()))
          
          # 3) Generate plots (can be slow)
          shiny::incProgress(0.35, detail = "Generating plots…")
          plot_files <- timeout_wrapper({
            generate_plots(metafile, plot_dir, session = session)
          }, timeout_seconds = 300, session = session, step = "generate_plots")
          
          # Verify plots exist
          for (nm in names(plot_files)) {
            ok <- file.exists(plot_files[[nm]])
            log_msg(session, "Plot check: %s => %s", nm, if (ok) "OK" else "MISSING", 
                    type = if (ok) "info" else "error")
            if (!ok) stop("Failed to create plot: ", nm)
          }
          check_memory(session, "after plots")
          log_msg(session, "Plot step took %s",
                  pretty_duration(difftime(Sys.time(), t0, units = "secs")))
          
          # 4) Build Rmd content
          shiny::incProgress(0.6, detail = "Preparing R Markdown…")
          t1 <- Sys.time()
          rmd_content <- timeout_wrapper({
            generate_rmd_report_html(metafile, plot_files, temp_dir)
          }, timeout_seconds = 120, session = session, step = "generate_rmd_report_html")
          
          # Write Rmd
          tryCatch(writeLines(rmd_content, rmd_file),
                   error = function(e) stop("Failed to write Rmd: ", e$message))
          log_msg(session, "Rmd written: %s (%s bytes)", rmd_file,
                  pretty_bytes(file.info(rmd_file)$size))
          
          # 5) Render HTML
          shiny::incProgress(0.85, detail = "Rendering HTML…")
          t2 <- Sys.time()
          
          if (nzchar(Sys.which("pandoc"))) {
            Sys.setenv(RSTUDIO_PANDOC = dirname(Sys.which("pandoc")))
          } else {
            log_msg(session, "Pandoc not found — attempting rmarkdown render anyway", type = "warn")
          }
          
          out_path <- timeout_wrapper({
            rmarkdown::render(
              input = rmd_file,
              output_file = file,
              output_format = rmarkdown::html_document(
                toc = TRUE,
                number_sections = TRUE,
                df_print = "paged",
                theme = "flatly",
                highlight = "tango",
                self_contained = TRUE
              ),
              quiet = FALSE,  # in DEBUG we want to see messages
              envir  = new.env(parent = globalenv())
            )
          }, timeout_seconds = 300, session = session, step = "rmarkdown::render")
          
          log_msg(session, "Render returned path: %s", out_path)
          
          # Verify output exists and is non-empty
          if (!file.exists(file) || file.info(file)$size == 0) {
            stop("HTML report was not generated or is empty")
          }
          
          # 6) Post-process & finish
          success  <- TRUE
          end_time <- Sys.time()
          dur      <- pretty_duration(difftime(end_time, start_time, units = "secs"))
          size     <- pretty_bytes(file.info(file)$size)
          log_msg(session, "HTML ready: %s · %s (took %s)", file, size, dur)
          
          run_stamp <- format(end_time, "%Y-%m-%d %H:%M:%S %Z")
          try(inject_report_footer(file, run_stamp, dur, size), silent = TRUE)
          
          shiny::setProgress(1, message = paste0("Done in ", dur, " · ", size),
                             detail = "Click to download.")
        })
        
      }, error = function(e) {
        error_msg <<- e$message
        log_msg(session, "ERROR in downloadReport: %s", e$message, type = "error")
        stop(e$message)
      })
    }
  )
  
  # =========================================
  # ---- helpers you already have elsewhere ----
  # NOTE: I added `session` as an optional argument so we can log messages
  # from inside the helpers as well.
  
  generate_plots <- function(metafile, plot_dir, session = NULL) {
    t0 <- Sys.time()
    log_msg(session, "[generate_plots] START -> plot_dir=%s", plot_dir)
    
    # PCA: Sample Type
    pca_dat <- metafile$df() %>% dplyr::select(starts_with("seq."))
    log_msg(session, "[generate_plots] PCA matrix dims: %s x %s",
            nrow(pca_dat), ncol(pca_dat))
    pca_res <- prcomp(pca_dat, scale = TRUE)
    pca_scores <- as.data.frame(pca_res$x)
    plot_dat <- cbind(
      metafile$df()[, c("SampleType", "PlateId", "SampleId", "AssayNotes",
                        "SampleNotes", "TimePoint", "SampleGroup")],
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
    log_msg(session, "[generate_plots] Wrote %s (%s)", pca_sample_type_file,
            pretty_bytes(file.info(pca_sample_type_file)$size))
    
    # PCA: RowCheck
    avoid_SOMAmers <- foodata2::load_data2()
    avoid_prot <- avoid_SOMAmers %>%
      dplyr::pull(SeqId) %>% paste0("seq.", .) %>% stringr::str_replace_all("-", ".")
    adat_samp_tbl <- metafile$df() %>%
      dplyr::filter(SampleType == "Sample") %>%
      dplyr::select(PlateId, SampleId, RowCheck, starts_with("seq.")) %>%
      dplyr::select(!all_of(avoid_prot))
    pca_dat2 <- metafile$df() %>% dplyr::filter(SampleType == "Sample") %>%
      dplyr::select(starts_with("seq."))
    log_msg(session, "[generate_plots] PCA(RowCheck) dims: %s x %s",
            nrow(pca_dat2), ncol(pca_dat2))
    pca_res2 <- prcomp(pca_dat2, scale = TRUE)
    pca_scores2 <- as.data.frame(pca_res2$x)
    plot_samp_dat <- cbind(adat_samp_tbl[, c("PlateId", "SampleId", "RowCheck")], pca_scores2) %>%
      dplyr::mutate(HoverText = paste0("PlateId: ", PlateId, "<br>SampleId: ", SampleId))
    variance_explained_pc1 <- round(pca_res2$sdev[1]^2 / sum(pca_res2$sdev^2) * 100, 2)
    variance_explained_pc2 <- round(pca_res2$sdev[2]^2 / sum(pca_res2$sdev^2) * 100, 2)
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
    log_msg(session, "[generate_plots] Wrote %s (%s)", pca_rowcheck_file,
            pretty_bytes(file.info(pca_rowcheck_file)$size))
    
    # Levey–Jennings plots
    df_cvs_all  <- foodata2::load_data4()
    adat_header <- metafile$df2()
    levey_cal <- plot_levey(metafile$df(), adat_header, df_cvs_all, sample_type = "Calibrator")
    levey_calibrator_file <- file.path(plot_dir, "levey_calibrator.png")
    ggplot2::ggsave(levey_calibrator_file, levey_cal, width = 8, height = 6, dpi = 300)
    log_msg(session, "[generate_plots] Wrote %s (%s)", levey_calibrator_file,
            pretty_bytes(file.info(levey_calibrator_file)$size))
    
    levey_qc <- plot_levey(metafile$df(), adat_header, df_cvs_all, sample_type = "QC")
    levey_somalogic_qc_file <- file.path(plot_dir, "levey_somalogic_qc.png")
    ggplot2::ggsave(levey_somalogic_qc_file, levey_qc, width = 8, height = 6, dpi = 300)
    log_msg(session, "[generate_plots] Wrote %s (%s)", levey_somalogic_qc_file,
            pretty_bytes(file.info(levey_somalogic_qc_file)$size))
    
    log_msg(session, "[generate_plots] DONE in %s",
            pretty_duration(difftime(Sys.time(), t0, units = "secs")))
    
    list(
      pca_sample_type     = pca_sample_type_file,
      pca_sample_rowcheck = pca_rowcheck_file,
      levey_calibrator    = levey_calibrator_file,
      levey_somalogic_qc  = levey_somalogic_qc_file
    )
  }
  
  generate_rmd_report_html <- function(metafile, plot_files, temp_dir) {
    log_msg(session, "[generate_rmd_report_html] START -> temp_dir=%s", temp_dir)
    
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
    
    # SOMAmers in tails (robust)
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
    
    # Calibrator CVs
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
    
    # QC CVs
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
    
    # Save artifacts (so we can inspect them in Docker if render fails)
    saveRDS(samp_summary,      file.path(temp_dir, "samp_summary.rds"))
    saveRDS(flagged_samples,   file.path(temp_dir, "flagged_samples.rds"))
    saveRDS(med_norm_summary,  file.path(temp_dir, "med_norm_summary.rds"))
    saveRDS(anml_summary,      file.path(temp_dir, "anml_summary.rds"))
    saveRDS(df_plate_scale,    file.path(temp_dir, "df_plate_scale.rds"))
    saveRDS(df_cal_perc_tails, file.path(temp_dir, "df_cal_perc_tails.rds"))
    saveRDS(somamers_summary,  file.path(temp_dir, "somamers_summary.rds"))
    saveRDS(df_cvs,            file.path(temp_dir, "df_cvs.rds"))
    saveRDS(qc_cv_summary,     file.path(temp_dir, "qc_cv_summary.rds"))
    log_msg(session, "[generate_rmd_report_html] Saved intermediate RDS files to %s", temp_dir)
    
    # Construct the Rmd body (unchanged from your logic)
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
  
  # When the session ends, tell us where to look for logs
  session$onSessionEnded(function() {
    log_msg(NULL, "Session ended. Log file remains at: %s", LOG_FILE)
  })
}
