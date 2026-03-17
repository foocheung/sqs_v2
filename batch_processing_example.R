# ==============================================================================
# Batch Processing SomaScan Files with Report Generation
# ==============================================================================

library(SomaDataIO)
library(dplyr)
library(rmarkdown)
library(readxl)

# ==============================================================================
# Setup: Define paths and load reference data
# ==============================================================================

# Directory containing .adat files
adat_dir <- "path/to/your/adat/files"

# Load reference data from synthetic_data.xlsx
message("Loading reference data from synthetic_data.xlsx...")
df_cvs_all <- readxl::read_excel("synthetic_data.xlsx")

# Fix column names - remove escaped backticks if present
names(df_cvs_all) <- gsub("^`|`$", "", names(df_cvs_all))

# Convert ExpDate to Date class to match what global.R expects
df_cvs_all <- df_cvs_all %>%
  dplyr::mutate(ExpDate = as.Date(ExpDate))

# Verify required columns exist
required_cols <- c("ExpDate", "PlateId", "SampleType", "10%", "50%", "90%")
missing_cols <- required_cols[!required_cols %in% names(df_cvs_all)]
if (length(missing_cols) > 0) {
  cat("Available columns:", paste(names(df_cvs_all), collapse = ", "), "\n")
  stop("synthetic_data.xlsx is missing required columns: ", 
       paste(missing_cols, collapse = ", "))
}

message("Reference data loaded: ", nrow(df_cvs_all), " historical plates")

# Output directory for reports
output_dir <- "batch_qc_reports"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# Batch Processing Function
# ==============================================================================

process_adat_file <- function(file, df_cvs_all, output_dir) {
  
  message(paste("Processing:", basename(file)))
  
  tryCatch({
    # Read ADAT file
    adat <- SomaDataIO::read_adat(file)
    
    # Extract data and header
    adat_tbl <- adat
    adat_header <- attributes(adat)
    
    # Get experiment date for naming
    exp_date <- as.character(adat_header$Header.Meta$HEADER$ExpDate)
    file_basename <- tools::file_path_sans_ext(basename(file))
    
    # Create timestamped output filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    report_name <- paste0(file_basename, "_QC_Report_", timestamp, ".html")
    report_path <- file.path(output_dir, report_name)
    
    # Generate QC plots
    message("  - Generating QC plots...")
    
    # Levey-Jennings plot for QC samples
    levey_qc <- plot_levey(
      adat_tbl = adat_tbl,
      adat_header = adat_header,
      df_cvs_all = df_cvs_all,
      sample_type = "QC",
      sd_levels = c(1, 2, 3),
      show_zones = TRUE
    )
    
    # Levey-Jennings plot for Calibrators
    levey_cal <- plot_levey(
      adat_tbl = adat_tbl,
      adat_header = adat_header,
      df_cvs_all = df_cvs_all,
      sample_type = "Calibrator",
      sd_levels = c(1, 2, 3),
      show_zones = TRUE
    )
    
    # PCA plot
    pca_dat <- adat_tbl %>% 
      select(starts_with("seq."))
    pca_res <- prcomp(pca_dat, scale = TRUE)
    pca_scores <- as.data.frame(pca_res$x)
    
    plot_dat <- cbind(
      adat_tbl[, c("SampleType", "PlateId", "SampleId")],
      pca_scores
    )
    
    variance_pc1 <- round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 2)
    variance_pc2 <- round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 2)
    
    pca_plot <- ggplot2::ggplot(plot_dat, 
                                 ggplot2::aes(x = PC1, y = PC2, color = SampleType)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        x = paste0("PC1 (", variance_pc1, "%)"),
        y = paste0("PC2 (", variance_pc2, "%)"),
        title = "PCA by Sample Type"
      ) +
      ggplot2::theme_minimal()
    
    # Calculate summary statistics
    message("  - Calculating QC metrics...")
    
    # Sample counts
    sample_summary <- table(adat_tbl$SampleType, adat_tbl$PlateId)
    
    # Flagged samples
    flagged_samples <- adat_tbl %>%
      filter(RowCheck == "FLAG") %>%
      select(PlateId, SampleId, SampleType)
    
    # CV statistics for calibrators
    df_cvs_cal <- adat_tbl %>%
      filter(SampleType == "Calibrator") %>%
      select(PlateId, starts_with("seq.")) %>%
      group_by(PlateId) %>%
      summarise(across(starts_with("seq."), safe_cv), .groups = "drop") %>%
      tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
      filter(is.finite(CV)) %>%
      group_by(PlateId) %>%
      summarise(
        `10%` = round(quantile(CV, 0.1, na.rm = TRUE) * 100, 1),
        `50%` = round(median(CV, na.rm = TRUE) * 100, 1),
        `90%` = round(quantile(CV, 0.9, na.rm = TRUE) * 100, 1),
        .groups = "drop"
      )
    
    # Generate HTML report
    message("  - Rendering HTML report...")
    
    # Create temporary R Markdown file
    rmd_content <- c(
      '---',
      paste0('title: "QC Report - ', file_basename, '"'),
      paste0('date: "', Sys.Date(), '"'),
      'output:',
      '  html_document:',
      '    toc: true',
      '    toc_float: true',
      '    theme: flatly',
      '---',
      '',
      '```{r setup, include=FALSE}',
      'knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)',
      'library(knitr)',
      'library(kableExtra)',
      '```',
      '',
      '# Summary',
      '',
      paste0('**File:** ', basename(file), '  '),
      paste0('**Experiment Date:** ', exp_date, '  '),
      paste0('**Report Generated:** ', Sys.time(), '  '),
      paste0('**Reference Data:** synthetic_data.xlsx (', nrow(df_cvs_all), ' historical plates)  '),
      '',
      '# Sample Counts',
      '',
      '```{r sample_counts}',
      'kable(sample_summary, caption = "Sample Counts by Type and Plate") %>%',
      '  kable_styling(bootstrap_options = c("striped", "hover"))',
      '```',
      '',
      '# Flagged Samples',
      '',
      '```{r flagged_samples}',
      'if (nrow(flagged_samples) == 0) {',
      '  cat("No samples flagged during QC.")',
      '} else {',
      '  kable(flagged_samples, caption = "Flagged Samples") %>%',
      '    kable_styling(bootstrap_options = c("striped", "hover"))',
      '}',
      '```',
      '',
      '# PCA Plot',
      '',
      '```{r pca_plot, fig.width=8, fig.height=6}',
      'print(pca_plot)',
      '```',
      '',
      '# Levey-Jennings: QC Samples',
      '',
      '```{r levey_qc, fig.width=10, fig.height=6}',
      'print(levey_qc)',
      '```',
      '',
      '# Levey-Jennings: Calibrators',
      '',
      '```{r levey_cal, fig.width=10, fig.height=6}',
      'print(levey_cal)',
      '```',
      '',
      '# Calibrator CV Statistics',
      '',
      '```{r cal_cvs}',
      'kable(df_cvs_cal, caption = "Calibrator CV Quantiles by Plate") %>%',
      '  kable_styling(bootstrap_options = c("striped", "hover"))',
      '```'
    )
    
    # Write temporary Rmd file
    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(rmd_content, temp_rmd)
    
    # Ensure output directory exists before rendering
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Create environment with all necessary objects for the report
    report_env <- new.env(parent = globalenv())
    report_env$sample_summary <- sample_summary
    report_env$flagged_samples <- flagged_samples
    report_env$pca_plot <- pca_plot
    report_env$levey_qc <- levey_qc
    report_env$levey_cal <- levey_cal
    report_env$df_cvs_cal <- df_cvs_cal
    
    # Render to HTML (use output_dir and basename separately)
    rmarkdown::render(
      input = temp_rmd,
      output_file = report_name,
      output_dir = output_dir,
      envir = report_env,
      quiet = TRUE
    )
    
    message("  ✓ Report saved: ", report_path)
    
    # Return summary information
    return(list(
      file = basename(file),
      exp_date = exp_date,
      report_path = report_path,
      n_samples = nrow(adat_tbl),
      n_flagged = nrow(flagged_samples),
      n_plates = length(unique(adat_tbl$PlateId)),
      status = "Success"
    ))
    
  }, error = function(e) {
    message("  ✗ Error processing file: ", conditionMessage(e))
    return(list(
      file = basename(file),
      exp_date = NA,
      report_path = NA,
      n_samples = NA,
      n_flagged = NA,
      n_plates = NA,
      status = paste("Error:", conditionMessage(e))
    ))
  })
}

# ==============================================================================
# Execute Batch Processing
# ==============================================================================

# Find all .adat files
adat_files <- list.files(adat_dir, pattern = "\\.adat$", full.names = TRUE)

message(paste("\nFound", length(adat_files), ".adat files to process\n"))

# Process all files
results <- lapply(adat_files, function(file) {
  process_adat_file(file, df_cvs_all, output_dir)
})

# ==============================================================================
# Summarize Results
# ==============================================================================

# Convert results to data frame
results_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(
    File = x$file,
    ExpDate = x$exp_date,
    Samples = x$n_samples,
    Flagged = x$n_flagged,
    Plates = x$n_plates,
    Status = x$status,
    Report = basename(x$report_path),
    stringsAsFactors = FALSE
  )
}))

# Print summary
message("\n=== Batch Processing Summary ===")
print(results_df)

# Save summary
summary_file <- file.path(output_dir, paste0("batch_summary_", 
                                             format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                             ".csv"))
write.csv(results_df, summary_file, row.names = FALSE)
message("\nSummary saved to: ", summary_file)

# Count successes and failures
n_success <- sum(results_df$Status == "Success")
n_failed <- sum(results_df$Status != "Success")

message("\n", n_success, " files processed successfully")
if (n_failed > 0) {
  message(n_failed, " files failed - see summary for details")
}

message("\nAll reports saved to: ", output_dir)

# ==============================================================================
# Optional: Export summary statistics to Excel
# ==============================================================================

if (requireNamespace("writexl", quietly = TRUE)) {
  summary_excel <- file.path(output_dir, paste0("batch_summary_", 
                                                 format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                                 ".xlsx"))
  writexl::write_xlsx(results_df, summary_excel)
  message("Excel summary saved to: ", summary_excel)
}
