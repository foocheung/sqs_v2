# sqs: A Shiny App for SomaLogic SomaScan Quality Control

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`sqs` is an R package built with the [Golem](https://thinkr-open.github.io/golem/) framework, providing a Shiny-based web application for quality control (QC) analysis of SomaLogic SomaScan assay data. It enables proteomics researchers to upload `.adat` files, compute QC metrics (e.g., per-plate CVs, PCA, enhanced Levey-Jennings plots with QC zones), and generate automated HTML reports with professional visualizations. The package integrates with `SomaDataIO` for data handling and supports user-provided historical reference data for comprehensive quality monitoring.

---

## Video Tutorial — Step by Step Guide

Watch this comprehensive video tutorial to learn how to use the SQS application:

https://github.com/user-attachments/assets/27dc365c-8617-4366-af0c-152b20d281ab

**What you'll learn:**

- How to upload your SomaScan `.adat` files
- Navigating the interface and exploring data
- Generating comprehensive QC reports
- Interpreting Levey-Jennings plots with QC zones
- Downloading and saving reports
- Using custom reference data
- Automated processing of multiple datasets with individual reports

**Duration:** ~5 minutes | **Difficulty:** Beginner-friendly

---

## Example Report

A full example HTML report:

[![Example QC Report](https://github.com/foocheung/sqs_v2/raw/main/screencapture-file-Users-cheungf-Downloads-Proteomics-QC-Report-2026-03-20-1-html-2026-03-20-10_46_45.png)](https://github.com/foocheung/sqs_v2/blob/main/Proteomics_QC_Report_2026-03-06.html)

---

## Quality Control Interpretation Guide

### QC Pass/Fail Thresholds

The application uses the following acceptance criteria aligned with existing specifications.

> **Important:** Every column — `RowCheck`, `NormScale_*`, `ANMLFractionUsed_*`, `HybControlNormScale`, plate scale factors, and calibrator tail percentages — is already written into the `.adat` file by SomaLogic's processing pipeline before the data reaches you. This app reads, evaluates, and visualises these pre-computed values, applying acceptance criteria to flag samples or plates that fall outside expected ranges. The only metrics the app calculates itself are the CV statistics and Levey-Jennings plots, which are derived fresh from the raw `seq.*` protein signal columns on each run. This design means the app is fully consistent with SomaLogic's own QC outputs — it does not reinterpret or recompute the normalization, it surfaces and contextualises what SomaLogic has already determined.

---

### Section 4.1 — Sample-Level QC (RowCheck Flag)

Samples with `RowCheck = "FLAG"` are automatically identified during SomaScan processing.

**How it's calculated:**

The `RowCheck` flag is assigned by SomaLogic's internal QC algorithms during `.adat` file generation. The app extracts flagged samples directly from the `RowCheck` column and displays them.

```r
# Extract flagged samples from ADAT file
flagged_samples <- adat_tbl %>%
  filter(RowCheck == "FLAG") %>%
  select(PlateId, SampleId, SampleType, RowCheck)

# Count flagged samples
n_flagged <- nrow(flagged_samples)
```

**Action:** Review flagged samples and consider excluding them from downstream analysis.

---

### Section 4.2 — Normalization Scale Factors

| Result | Criteria |
|--------|----------|
| PASS | Scale factors between **0.4 and 2.5** (inclusive) |
| FLAG | Scale factors outside this range |

Applies to Sample-type rows across all three SomaScan dilution groups.

**How it's calculated:**

`NormScale_0_005`, `NormScale_0_5`, and `NormScale_20` are pre-computed by SomaLogic's normalization pipeline and stored as per-sample columns in the `.adat` file, one per dilution group (0.005%, 0.5%, 20%). These represent the final normalization scale factors after all corrections — including hybridization control normalization, median signal normalization, and plate scaling — have already been applied internally. The app reads these columns directly and applies the 0.4-2.5 acceptance criteria, summarising results as Pass/Flag counts per dilution group across all Sample-type rows.

`HybControlNormScale` is also present as a pre-computed per-sample column reflecting the hybridization efficiency correction applied early in SomaLogic's pipeline. It is available in the data but is not currently evaluated in the app's QC calculations.

```r
df_norm_scale <- adat_tbl %>%
  select(PlateId, SampleId, SampleType,
         NormScale_0_005, NormScale_0_5, NormScale_20) %>%
  filter(SampleType == "Sample") %>%
  mutate(across(starts_with("NormScale"),
                ~ifelse(. < 0.4 | . > 2.5, "Flag", "Pass")))

df_norm_scale %>%
  select(NormScale_0_005, NormScale_0_5, NormScale_20) %>%
  tidyr::gather(key = "Dilution Group", value = "Decision") %>%
  filter(Decision == "Pass") %>%
  group_by(`Dilution Group`) %>%
  summarise(Pass = n()) %>%
  mutate(Flag  = total_samples - Pass,
         Total = total_samples)
```

**Action:** Samples with scale factors outside 0.4-2.5 indicate potential technical issues (e.g., pipetting errors, sample degradation).

---

### Section 4.3 — ANML Fraction Used

| Result | Criteria |
|--------|----------|
| PASS | ANML fraction >= 0.3 |
| FLAG | ANML fraction < 0.3 |

Applies to all three dilution groups across Sample-type rows.

**How it's calculated:**

`ANMLFractionUsed_0_005`, `ANMLFractionUsed_0_5`, and `ANMLFractionUsed_20` are pre-computed by SomaLogic and stored as per-sample columns in the `.adat` file. Each records what fraction of protein targets were used in the Adaptive Normalization by Maximum Likelihood (ANML) step for that dilution group. Low values indicate fewer proteins were informative during normalization, which may reflect sample quality issues. The app reads these columns directly and flags values below 0.3.

```r
df_anml_fraction <- adat_tbl %>%
  select(PlateId, SampleId, SampleType,
         ANMLFractionUsed_0_005, ANMLFractionUsed_0_5, ANMLFractionUsed_20) %>%
  filter(SampleType == "Sample") %>%
  mutate(across(starts_with("ANMLFractionUsed"),
                ~ifelse(. < 0.3, "Flag", "Pass")))
```

**Action:** Low ANML fraction values may indicate poor sample quality or assay performance for that dilution group.

---

### Section 5.1 — Plate Scale Factors

| Result | Criteria |
|--------|----------|
| PASS | Plate scale factor between **0.4 and 2.5** |
| FAIL | Plate scale factor outside this range |

**How it's calculated:**

Plate scale factors are pre-computed by SomaLogic's pipeline and stored in the `.adat` header metadata — not as row-level columns. The app extracts them by scanning header keys matching `PlateScale_Scalar_*` and `PlateScale_PassFlag_*`.

```r
adat_header <- attributes(adat)
keys        <- names(adat_header$Header.Meta$HEADER)

scalar_keys <- keys[grep("^PlateScale_Scalar", keys)]
df_plate_scale_value <- data.frame(
  Value = unlist(adat_header$Header.Meta$HEADER[scalar_keys])
) %>%
  tibble::rownames_to_column(var = "Plate") %>%
  mutate(Plate = sub("^PlateScale_Scalar_", "", Plate))

pass_keys <- keys[grep("^PlateScale_PassFlag", keys)]
df_plate_scale_pass <- data.frame(
  `Plate Check` = unlist(adat_header$Header.Meta$HEADER[pass_keys])
) %>%
  tibble::rownames_to_column(var = "Plate") %>%
  mutate(Plate = sub("^PlateScale_PassFlag_", "", Plate))

df_plate_scale <- inner_join(df_plate_scale_pass, df_plate_scale_value, by = "Plate") %>%
  mutate(Value = round(as.numeric(Value), 2))
```

**Action:** Plate scale factors outside 0.4-2.5 indicate plate-level technical issues requiring investigation.

---

### Section 5.2 — Calibrator Signal in Tails

| Result | Criteria |
|--------|----------|
| PASS | < 10% of analytes in tails |
| FAIL | >= 10% of analytes in tails |

**How it's calculated:**

The tail percentage and pass/fail verdict are pre-computed by SomaLogic's pipeline and stored in the `.adat` header metadata under keys `CalPlateTailPercent_*` and `CalPlateTailTest_*`. The app reads these directly from the header rather than recalculating from raw signal data.

```r
adat_header <- attributes(adat)
keys        <- names(adat_header$Header.Meta$HEADER)

perc_keys <- keys[grep("^CalPlateTailPercent", keys)]
df_cal_perc_value <- data.frame(
  Value = unlist(adat_header$Header.Meta$HEADER[perc_keys])
) %>%
  tibble::rownames_to_column(var = "Plate") %>%
  mutate(Plate = sub("^CalPlateTailPercent_", "", Plate))

test_keys <- keys[grep("^CalPlateTailTest", keys)]
df_cal_perc_test <- data.frame(
  `Plate Check` = unlist(adat_header$Header.Meta$HEADER[test_keys])
) %>%
  tibble::rownames_to_column(var = "Plate") %>%
  mutate(Plate = sub("^CalPlateTailTest_", "", Plate))

df_cal_perc_tails <- inner_join(df_cal_perc_test, df_cal_perc_value, by = "Plate") %>%
  mutate(`Acceptance Criteria` = "Less than 10%",
         Value = round(as.numeric(Value), 2))
```

**Action:** High tail percentages suggest systematic bias or assay drift.

---

### Section 5.3 — Protein Targets in Tails (ColCheck)

| Result | Criteria |
|--------|----------|
| PASS | Signal ratio between **0.8 and 1.2** |
| FLAG | Signal ratio outside this range |

**How it's calculated:**

The per-protein pass/fail status is pre-computed by SomaLogic and stored in the `ColCheck` field of the `.adat` column metadata. The app tallies `PASS` and `FLAG` counts across all proteins.

```r
df_SOMAmers_tails <- data.frame(
  SeqId            = adat_header$Col.Meta$SeqId,
  EntrezGeneSymbol = adat_header$Col.Meta$EntrezGeneSymbol,
  Organism         = adat_header$Col.Meta$Organism,
  ColCheck         = adat_header$Col.Meta$ColCheck
)

table(df_SOMAmers_tails$ColCheck)
```

**Action:** A high FLAG count indicates widespread calibration inaccuracy across the protein panel.

---

### Section 5.4 — Calibrator Precision per Plate

Reports the 10th, 50th (median), and 90th percentile CV values.

**How it's calculated:**

This is one of the metrics the app calculates itself from the raw `seq.*` protein signal columns. For each plate, CV is computed across calibrator replicates for every protein, then summarised as percentiles.

1. For each plate, identify all calibrator samples (typically 3-6 replicates per plate)
2. For each protein (`seq.*` column), calculate CV across calibrator replicates: `CV = (Standard Deviation / Mean) x 100%`
3. Across all proteins, calculate percentiles:
   - **10th percentile:** 10% of proteins have CV below this value (best precision)
   - **50th percentile (Median):** Middle value — typical precision for the plate
   - **90th percentile:** 90% of proteins have CV below this value (acceptable upper limit)

```r
safe_cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(m) || m <= 0) return(NA_real_)
  s / m
}

df_cvs <- adat_tbl %>%
  filter(SampleType == "Calibrator") %>%
  select(PlateId, starts_with("seq.")) %>%
  group_by(PlateId) %>%
  summarise_if(is.numeric, function(x) sd(x) / mean(x)) %>%
  ungroup() %>%
  tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
  group_by(PlateId) %>%
  summarise(
    `10%` = round(quantile(CV, 0.1) * 100, 1),
    `50%` = round(median(CV) * 100, 1),
    `90%` = round(quantile(CV, 0.9) * 100, 1)
  )
```

**Action:** High CV values indicate poor replicate reproducibility, suggesting technical problems with the plate.

---

### Sections 5.4.1 and 5.5.1 — Plate-Level Quality Trends (Levey-Jennings Plots)

| Zone | Range | Status |
|------|-------|--------|
| Zone 1 | +/-1 SD | Optimal — PASS |
| Zone 2 | +/-2 SD | Warning — requires attention |
| Zone 3 | +/-3 SD | Action — investigate conditions |
| Beyond +/-3 SD | > +/-3 SD | Out of control — plate should be rejected or repeated |

**How it's calculated:**

This is also calculated by the app itself. The per-plate median CV (50th percentile) computed in Section 5.4 is plotted against a historical reference distribution to classify each plate into QC zones.

1. Calculate median CV (50th percentile) for each historical plate in reference data
2. Compute reference center: `median(historical median CVs)` or `mean(historical median CVs)`
3. Compute reference standard deviation: `SD(historical median CVs)`
4. For current plates, plot median CV against historical distribution
5. Classify each plate into QC zones:
   - `|Current CV - Reference Center| <= 1xSD` → Zone 1 (optimal)
   - `1xSD < |deviation| <= 2xSD` → Zone 2 (warning)
   - `2xSD < |deviation| <= 3xSD` → Zone 3 (action)
   - `|deviation| > 3xSD` → Out of control (fail)

```r
ref_center <- median(df_cvs_all$`50%`, na.rm = TRUE)
ref_sd     <- sd(df_cvs_all$`50%`, na.rm = TRUE)

current_plates <- df_cvs_quantiles %>%
  mutate(
    deviation = abs(`50%` - ref_center),
    QC_Zone = case_when(
      deviation <= ref_sd     ~ "Zone 1 (+/-1 SD)",
      deviation <= 2 * ref_sd ~ "Zone 2 (+/-2 SD)",
      deviation <= 3 * ref_sd ~ "Zone 3 (+/-3 SD)",
      TRUE                    ~ "Out of Control (>+/-3 SD)"
    ),
    Pass = deviation <= 3 * ref_sd
  )

plot_levey(
  adat_tbl    = adat_tbl,
  adat_header = adat_header,
  df_cvs_all  = df_cvs_all,
  sample_type = "Calibrator",
  sd_levels   = c(1, 2, 3),
  show_zones  = TRUE
)
```

**Action:** Follow Westgard rules — consecutive violations or trends indicate systematic problems.

---

### Overall Quality Metrics

| Metric | Target |
|--------|--------|
| Median CV | ~5% after normalization (excellent plate quality) |
| ColCheck PASS rate | >= 85% of protein targets with signal ratios between 0.8 and 1.2 |

> These thresholds follow both clinical laboratory QC standards (Westgard rules) and technical specifications for the platform.

---

## Features

### Data Management

- **Flexible Data Input:** Upload SomaScan `.adat` files directly through the web interface
- **Custom Reference Data:** Upload your own historical plate controls and calibrator data for comparison
- **Large File Support:** Handles large multi-plate files with optimized performance
- **Batch File Support:** Process multiple `.adat` files in one run using custom reference data, generating individual reports and summary statistics

---

## What's New in Version 2.0

- **Batch File Support:** Automated processing of multiple `.adat` files with user-provided historical reference data and individual HTML reports

### Enhanced Levey-Jennings Plots

The `plot_levey` function has been completely redesigned with professional QC visualization features.

**Color-Coded QC Zones:**

- Zone 1 (+/-1 SD)
- Zone 2 (+/-2 SD)
- Zone 3 (+/-3 SD)
- Beyond +/-3 SD

**Enhanced Visual Elements:**

- Different point shapes indicate QC zone membership
- Multiple line types distinguish SD levels (dashed, dotted, dot-dash)
- Professional titles showing sample type and statistics
- Dual legends for data type and QC zones
- Improved axis labels and grid lines

**Customization Options:**

```r
plot_levey(
  adat_tbl, adat_header, df_cvs_all,
  sample_type = "QC",
  sd_levels   = c(1, 2, 3),   # Configurable SD levels
  center      = "median",      # or "mean"
  show_zones  = TRUE,          # Toggle QC zone shading
  point_size  = 2.5            # Adjustable point size
)
```

### Improved Code Architecture

- **Consolidated Functions:** All utility functions now in `global.R` for better organization
- **No Code Duplication:** Removed redundant function definitions
- **Enhanced Documentation:** Roxygen2 comments for all functions
- **Better Error Handling:** Improved validation and informative warnings

### Modern User Interface

- **Professional Design:** Gradient header with modern card-based layout
- **Intuitive Navigation:** Tabbed interface with clear sections
- **Help and Documentation:** Built-in quick start guide and feature explanations
- **Status Indicators:** Real-time feedback on report generation
- **Responsive Layout:** Optimized for different screen sizes

---

## Installation

### Prerequisites

- R (>= 4.0.0)
- Dependencies: `shiny`, `shinythemes`, `shinycssloaders`, `SomaDataIO`, `tidyverse`, `plotly`, `rmarkdown`, `kableExtra`, `DT`, `arrow`, `knitr`, `forcats`, `broom`

Install `sqs` and its dependencies from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install sqs
devtools::install_github("foocheung/sqs_v2")
```

---

## Quick Start

### Launch the Application

```r
library(sqs)
run_app()
```

### Workflow Overview

**Step 1: Upload Data**

- Click "Browse" to upload your SomaScan `.adat` file
- Optionally upload historical reference data (CSV/RDS format)

**Step 2: Generate QC Report**

- Click "Generate HTML Report" to analyze data
- View progress indicators and status updates
- Reports include:
  - Sample summary tables
  - PCA plots for sample clustering
  - Enhanced Levey-Jennings charts with QC zones
  - CV distribution analysis
  - Quality control metrics and flags

**Step 3: Review and Export**

- Preview the report in the "Report Preview" tab
- Download as self-contained HTML
- Save to a local folder for archival

> **Tip:** Watch the [video tutorial](#video-tutorial--step-by-step-guide) above for a complete walkthrough!

---

## Using Custom Reference Data

You can provide your own historical reference data for Levey-Jennings plots:

```r
# Prepare your reference data
# Format: Data frame with columns: ExpDate, PlateId, SampleType, CV quantiles
reference_data <- data.frame(
  ExpDate    = c("2024-01-01", "2024-01-15"),
  PlateId    = c("Plate001", "Plate002"),
  SampleType = c("QC", "QC"),
  "10%"      = c(5.2, 5.5),
  "50%"      = c(7.8, 8.1),
  "90%"      = c(11.2, 11.5),
  check.names = FALSE
)

# Upload through the app interface
# The app will automatically use your reference data for comparison
```

---

## Performance

| Metric | Value |
|--------|-------|
| Benchmark | 113.2 seconds for 15 plates on Mac M1 (64 GB, macOS 14.7.6) |
| Max file size | Up to 500 MB |
| Batch processing | Automated processing of multiple datasets with individual reports |

---

## Demo

Try the app online: [https://webtools.shinyapps.io/sqs_v3_ori/](https://webtools.shinyapps.io/sqs_v3_ori/)

---

## Advanced Features

### Customizing QC Thresholds

Edit `global.R` to customize QC criteria:

```r
# Adjust SD levels
sd_levels = c(1, 2, 3)  # Standard
sd_levels = c(1.5, 2.5) # Custom thresholds

# Change center statistic
center = "median"  # More robust (default)
center = "mean"    # Traditional approach
```

### Batch Processing

Automatically process hundreds of `.adat` files against your own historical reference data with comprehensive QC reports:

```r
# ==============================================================================
# Batch Processing SomaScan Files with Report Generation
# ==============================================================================

library(SomaDataIO)
library(dplyr)
library(rmarkdown)
library(readxl)
source("./R/global.R")

# ==============================================================================
# Setup: Define paths and load reference data
# ==============================================================================

# Directory containing .adat files
adat_dir <- "path/to/your/adat/files"

# Load reference data from synthetic_data.xlsx
message("Loading reference data from synthetic_data.xlsx...")
df_cvs_all <- readxl::read_excel("inst/data/synthetic_data.xlsx")

# Fix column names - remove escaped backticks if present
names(df_cvs_all) <- gsub("^`|`$", "", names(df_cvs_all))

# Convert ExpDate to Date class to match what global.R expects
df_cvs_all <- df_cvs_all %>%
  dplyr::mutate(ExpDate = as.Date(ExpDate))

# Verify required columns exist
required_cols <- c("ExpDate", "PlateId", "SampleType", "10%", "50%", "90%")
missing_cols  <- required_cols[!required_cols %in% names(df_cvs_all)]
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
    adat        <- SomaDataIO::read_adat(file)
    adat_tbl    <- adat
    adat_header <- attributes(adat)

    exp_date      <- as.character(adat_header$Header.Meta$HEADER$ExpDate)
    file_basename <- tools::file_path_sans_ext(basename(file))

    timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
    report_name <- paste0(file_basename, "_QC_Report_", timestamp, ".html")
    report_path <- file.path(output_dir, report_name)

    message("  - Generating QC plots...")

    levey_qc <- plot_levey(
      adat_tbl    = adat_tbl,
      adat_header = adat_header,
      df_cvs_all  = df_cvs_all,
      sample_type = "QC",
      sd_levels   = c(1, 2, 3),
      show_zones  = TRUE
    )

    levey_cal <- plot_levey(
      adat_tbl    = adat_tbl,
      adat_header = adat_header,
      df_cvs_all  = df_cvs_all,
      sample_type = "Calibrator",
      sd_levels   = c(1, 2, 3),
      show_zones  = TRUE
    )

    pca_dat    <- adat_tbl %>% select(starts_with("seq."))
    pca_res    <- prcomp(pca_dat, scale = TRUE)
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
        x     = paste0("PC1 (", variance_pc1, "%)"),
        y     = paste0("PC2 (", variance_pc2, "%)"),
        title = "PCA by Sample Type"
      ) +
      ggplot2::theme_minimal()

    message("  - Calculating QC metrics...")

    sample_summary  <- table(adat_tbl$SampleType, adat_tbl$PlateId)

    flagged_samples <- adat_tbl %>%
      filter(RowCheck == "FLAG") %>%
      select(PlateId, SampleId, SampleType)

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

    message("  - Rendering HTML report...")

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

    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(rmd_content, temp_rmd)

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    }

    report_env <- new.env(parent = globalenv())
    report_env$sample_summary  <- sample_summary
    report_env$flagged_samples <- flagged_samples
    report_env$pca_plot        <- pca_plot
    report_env$levey_qc        <- levey_qc
    report_env$levey_cal       <- levey_cal
    report_env$df_cvs_cal      <- df_cvs_cal

    rmarkdown::render(
      input       = temp_rmd,
      output_file = report_name,
      output_dir  = output_dir,
      envir       = report_env,
      quiet       = TRUE
    )

    message("  Report saved: ", report_path)

    return(list(
      file        = basename(file),
      exp_date    = exp_date,
      report_path = report_path,
      n_samples   = nrow(adat_tbl),
      n_flagged   = nrow(flagged_samples),
      n_plates    = length(unique(adat_tbl$PlateId)),
      status      = "Success"
    ))

  }, error = function(e) {
    message("  Error processing file: ", conditionMessage(e))
    return(list(
      file        = basename(file),
      exp_date    = NA,
      report_path = NA,
      n_samples   = NA,
      n_flagged   = NA,
      n_plates    = NA,
      status      = paste("Error:", conditionMessage(e))
    ))
  })
}

# ==============================================================================
# Execute Batch Processing
# ==============================================================================

adat_files <- list.files(adat_dir, pattern = "\\.adat$", full.names = TRUE)
message(paste("\nFound", length(adat_files), ".adat files to process\n"))

results <- lapply(adat_files, function(file) {
  process_adat_file(file, df_cvs_all, output_dir)
})

# ==============================================================================
# Summarize Results
# ==============================================================================

results_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(
    File     = x$file,
    ExpDate  = x$exp_date,
    Samples  = x$n_samples,
    Flagged  = x$n_flagged,
    Plates   = x$n_plates,
    Status   = x$status,
    Report   = basename(x$report_path),
    stringsAsFactors = FALSE
  )
}))

message("\n=== Batch Processing Summary ===")
print(results_df)

summary_file <- file.path(output_dir, paste0("batch_summary_",
                           format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
write.csv(results_df, summary_file, row.names = FALSE)
message("\nSummary saved to: ", summary_file)

n_success <- sum(results_df$Status == "Success")
n_failed  <- sum(results_df$Status != "Success")

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
                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  writexl::write_xlsx(results_df, summary_excel)
  message("Excel summary saved to: ", summary_excel)
}
```

---

## Troubleshooting

### Functions not found after update

```r
# Restart R and reload
detach("package:sqs", unload = TRUE)
library(sqs)
```

### Plots look different than expected

This is expected with v2.0 enhancements. New plots include QC zones and enhanced styling. Use `show_zones = FALSE` for a cleaner look.

### Upload fails for large files

```r
# Increase upload limit in global.R
options(shiny.maxRequestSize = 1000 * 1024^2)  # 1 GB
```

### Reference data format error

- Ensure reference data includes: `ExpDate`, `PlateId`, `SampleType`, and CV quantile columns
- Column names must match exactly: `"10%"`, `"50%"`, `"90%"`
- Save as RDS or CSV format

---

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Submit a pull request with a clear description
4. Include tests for new functionality

---

## Issues

Report issues or suggest features on the [GitHub Issues page](https://github.com/foocheung/sqs_v2/issues).

When reporting issues, please include:

- R version and operating system
- Package version (`packageVersion("sqs")`)
- Minimal reproducible example
- Error messages or unexpected behavior

---

## License

`sqs` is licensed under the [MIT License](LICENSE).

---

## Acknowledgements

- Developed by Foo Cheung ([ORCID: add-orcid])
- Built with the [Golem](https://thinkr-open.github.io/golem/) framework
- Uses [SomaDataIO](https://somalogic.github.io/SomaDataIO/) for data handling
- Enhanced visualizations powered by [ggplot2](https://ggplot2.tidyverse.org/)
- Statistical process control concepts based on [Westgard QC guidelines](https://www.westgard.com/)

---

## Version History

### Version 2.0 (Current)

- Enhanced Levey-Jennings plots with color-coded QC zones
- User-uploadable reference data support
- Modern UI with professional styling
- Consolidated function architecture
- Comprehensive documentation
- Fixed code duplication issues
- Improved performance and error handling

### Version 1.0

- Initial release with basic QC functionality
- PCA visualization
- Standard Levey-Jennings plots
- HTML report generation

---

**Questions?** Open an issue or contact the author Foo Cheung.

**Happy Quality Controlling!**
