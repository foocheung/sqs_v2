# sqs: A Shiny App for SomaLogic SomaScan Quality Control

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`sqs` is an R package built with the [Golem](https://thinkr-open.github.io/golem/) framework, providing a Shiny-based web application for quality control (QC) analysis of SomaLogic SomaScan assay data. It enables proteomics researchers to upload `.adat` files, compute QC metrics (e.g., per-plate CVs, PCA, enhanced Levey-Jennings plots with QC zones), and generate automated HTML reports with professional visualizations. The package integrates with `SomaDataIO` for data handling and supports user-provided historical reference data for comprehensive quality monitoring.

## 📺 Video Tutorial - Step by Step Guide

Watch this comprehensive video tutorial to learn how to use the SQS application:

https://github.com/user-attachments/assets/27dc365c-8617-4366-af0c-152b20d281ab

**What you'll learn:**
- How to upload your SomaScan `.adat` files
- Navigating the interface and exploring data
- Generating comprehensive QC reports
- Interpreting Levey-Jennings plots with QC zones
- Downloading and saving reports
- Using custom reference data

**Duration:** ~5 minutes | **Difficulty:** Beginner-friendly

---

## 📊 Example Report

Click the image below to view a full example HTML report:

[![Example QC Report](https://github.com/foocheung/sqs_v2/raw/main/screencapture-file-Users-cheungf-Downloads-Proteomics-QC-Report-2026-03-06-html-2026-03-06-10_22_37.png)](https://github.com/foocheung/sqs_v2/blob/main/Proteomics_QC_Report_2026-03-06.html)

*Click the screenshot above to view the [complete interactive HTML report](https://github.com/foocheung/sqs_v2/blob/main/Proteomics_QC_Report_2026-03-06.html)*

---

## Features

### Data Management
- **Flexible Data Input**: Upload SomaScan `.adat` files directly through the web interface
- **Custom Reference Data**: Upload your own historical plate controls and calibrator data for comparison
- **Large File Support**: Handles files up to 500 MB with optimized performance

### Enhanced QC Metrics
- **Coefficient of Variation (CV) Analysis**: Calculate per-plate CV statistics with quantile summaries (10%, 50%, 90%)
- **Principal Component Analysis (PCA)**: Interactive sample clustering visualization with variance explained
- **Advanced Levey-Jennings Plots** (NEW v2.0):
  - Color-coded quality control zones (±1, ±2, ±3 SD)
  - Visual zone indicators with shaded backgrounds
  - Different point shapes for each QC zone
  - Enhanced titles and subtitles with key statistics
  - Multiple SD levels with distinct line types
  - Configurable appearance options

### Professional Reporting
- **Interactive Interface**: Modern, card-based UI with intuitive navigation and help documentation
- **Automated HTML Reports**: Self-contained reports with embedded plots and tables
- **Publication-Ready Plots**: High-resolution visualizations suitable for presentations and publications
- **Comprehensive QC Tables**: Formatted with `kableExtra` for professional appearance
- **Interactive Visualizations**: Powered by `ggplot2` and `plotly` for enhanced data exploration

### Quality Control Monitoring
- **Statistical Process Control**: Industry-standard QC zones following SPC conventions
- **Trend Detection**: Visual identification of systematic shifts or drifts
- **Out-of-Control Alerts**: Automatic flagging of plates beyond ±3 SD
- **Historical Comparison**: Compare current runs against user-provided reference populations

---

## What's New in Version 2.0

### Enhanced Levey-Jennings Plots
The `plot_levey` function has been completely redesigned with professional QC visualization features:

- **Color-Coded QC Zones**:
  - Zone 1 (±1 SD): Light blue - Optimal range (68% of data expected)
  - Zone 2 (±2 SD): Yellow - Warning range (95% of data expected)
  - Zone 3 (±3 SD): Light green - Action range (99.7% of data expected)
  - Beyond ±3 SD: Out of control - Requires immediate action

- **Enhanced Visual Elements**:
  - Different point shapes indicate QC zone membership
  - Multiple line types distinguish SD levels (dashed, dotted, dot-dash)
  - Professional titles showing sample type and statistics
  - Dual legends for data type and QC zones
  - Improved axis labels and grid lines

- **Customization Options**:
  ```r
  plot_levey(
    adat_tbl, adat_header, df_cvs_all,
    sample_type = "QC",
    sd_levels = c(1, 2, 3),    # Configurable SD levels
    center = "median",          # or "mean"
    show_zones = TRUE,          # Toggle QC zone shading
    point_size = 2.5            # Adjustable point size
  )
  ```

### Improved Code Architecture
- **Consolidated Functions**: All utility functions now in `global.R` for better organization
- **No Code Duplication**: Removed redundant function definitions
- **Enhanced Documentation**: Roxygen2 comments for all functions
- **Better Error Handling**: Improved validation and informative warnings

### Modern User Interface
- **Professional Design**: Gradient header with modern card-based layout
- **Intuitive Navigation**: Tabbed interface with clear sections
- **Help & Documentation**: Built-in quick start guide and feature explanations
- **Status Indicators**: Real-time feedback on report generation
- **Responsive Layout**: Optimized for different screen sizes

---

## Installation

### Prerequisites
- R (>= 4.0.0)
- Dependencies: `shiny`, `shinythemes`, `shinycssloaders`, `SomaDataIO`, `tidyverse`, `plotly`, `rmarkdown`, `kableExtra`, `DT`, `arrow`, `knitr`, `forcats`, `broom`

Install `sqs` and its dependencies from GitHub:

```R
# Install devtools if not already installed
install.packages("devtools")

# Install sqs
devtools::install_github("foocheung/sqs_v2")
```

---

## Quick Start

### Launch the Application

Start the Shiny app to perform QC analysis:

```R
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

**Step 3: Review & Export**
   - Preview the report in the "Report Preview" tab
   - Download as self-contained HTML
   - Save to a local folder for archival

> 💡 **Tip**: Watch the [video tutorial](#-video-tutorial---step-by-step-guide) above for a complete walkthrough!

---

## Using Custom Reference Data

You can now provide your own historical reference data for Levey-Jennings plots:

```R
# Prepare your reference data
# Format: Data frame with columns: ExpDate, PlateId, SampleType, CV quantiles
reference_data <- data.frame(
  ExpDate = c("2024-01-01", "2024-01-15"),
  PlateId = c("Plate001", "Plate002"),
  SampleType = c("QC", "QC"),
  "10%" = c(5.2, 5.5),
  "50%" = c(7.8, 8.1),
  "90%" = c(11.2, 11.5),
  check.names = FALSE
)

# Save as RDS or CSV
saveRDS(reference_data, "my_reference_data.rds")
write.csv(reference_data, "my_reference_data.csv")

# Upload through the app interface
# The app will automatically use your reference data for comparison
```

### Converting from Feather Format

If you have historical data in Feather format (e.g., from `foodata`):

```R
library(arrow)
library(dplyr)

# Read feather file
serum_cvs <- arrow::read_feather("serum-cvs.feather")

# Extract required columns
reference_data <- serum_cvs %>%
  select(ExpDate, PlateId, SampleType, `10%`, `50%`, `90%`)

# Save for upload
saveRDS(reference_data, "levey_reference_data.rds")
```

---

## Performance

- **Benchmark**: 113.2 seconds for 15 plates on Mac M1 (64 GB, macOS 14.7.6)
- **Optimized**: Efficient data processing with `tidyverse` and `arrow`
- **Scalable**: Handles large datasets up to 500 MB

---

## Demo

Try the enhanced app online: [https://webtools.shinyapps.io/sqs_v3_ori/](https://webtools.shinyapps.io/sqs_v3_ori/)

---

## Documentation

### Quick References
- **Quick Start Guide**: See `QUICK_START.md` for rapid implementation
- **Function Reference**: Detailed documentation in `global.R` with roxygen2 comments
- **Improvements Guide**: See `IMPROVEMENTS_README.md` for complete feature list
- **Plot Comparison**: See `PLOT_LEVEY_COMPARISON.md` for visualization enhancements

### Key Functions

#### `plot_levey()` - Enhanced Levey-Jennings Plot
```R
#' Creates an improved Levey-Jennings plot with QC zones
#'
#' @param adat_tbl Data table with SomaScan data
#' @param adat_header Header information from ADAT file
#' @param df_cvs_all Historical CV data (user-provided)
#' @param sample_type Sample type: "QC", "Calibrator", etc.
#' @param sd_levels SD levels for control limits (default: c(1,2,3))
#' @param center "median" or "mean" centering
#' @param show_zones Show color-coded QC zones (default: TRUE)
#' @param point_size Point size for plot (default: 2.5)
#' @return ggplot2 object with enhanced visualization
```

#### `safe_cv()` - Robust CV Calculation
```R
#' Calculate coefficient of variation safely
#'
#' @param x Numeric vector
#' @return CV as proportion (NA for invalid inputs)
```

#### `ks_test()` - Distribution Comparison
```R
#' Kolmogorov-Smirnov test for CV distributions
#'
#' @param df_cvs_samp Sample CV data
#' @param df_cvs_all Historical CV data (user-provided)
#' @param sample_type Sample type for comparison
#' @return Data frame with KS test results
```

---


## Quality Control Interpretation Guide

### Understanding QC Zones in Levey-Jennings Plots

**Zone 1 (±1 SD) - OPTIMAL** ✅
- 68% of points should fall here under normal distribution
- Process is in statistical control
- No action needed - continue monitoring

**Zone 2 (±2 SD) - WARNING** ⚠️
- 95% of points should be within this range
- Points here warrant attention and review
- May indicate increased variation
- Document and monitor closely

**Zone 3 (±3 SD) - ACTION** 🔶
- 99.7% of points should be within this range
- Points here require investigation
- Consider corrective action
- Review assay conditions

**Beyond ±3 SD - OUT OF CONTROL** ❌
- Rare event (0.3% probability if random)
- Likely represents a true quality issue
- **Immediate action required**
- Investigate root cause before proceeding

### Westgard Rules (Optional Implementation)
The enhanced plots support manual application of Westgard rules:
- **1₃ₛ**: Single point beyond ±3 SD (reject)
- **2₂ₛ**: Two consecutive points beyond ±2 SD (warning)
- **R₄ₛ**: Range of 4 SD between consecutive points (warning)
- **4₁ₛ**: Four consecutive points beyond ±1 SD (trend)
- **10ₓ**: Ten consecutive points on same side of center (systematic shift)

---

## Advanced Features

### Customizing QC Thresholds
Edit `global.R` to customize QC criteria:

```R
# Adjust SD levels
sd_levels = c(1, 2, 3)  # Standard
sd_levels = c(1.5, 2.5) # Custom thresholds

# Change center statistic
center = "median"  # More robust (default)
center = "mean"    # Traditional approach
```

### Batch Processing
Process multiple files programmatically:

```R
# List of adat files
adat_files <- list.files("data/", pattern = "\\.adat$", full.names = TRUE)

# Process each file
results <- lapply(adat_files, function(file) {
  adat <- SomaDataIO::read_adat(file)
  # Generate report
  # ...
})
```

### Exporting High-Resolution Plots
```R
# Save enhanced Levey-Jennings plot
plot <- plot_levey(data, header, reference, show_zones = TRUE)
ggsave("qc_plot.png", plot, width = 10, height = 6, dpi = 300)
```

---

## Troubleshooting

### Common Issues

**Issue**: Functions not found after update
```R
# Solution: Restart R and reload
detach("package:sqs", unload = TRUE)
library(sqs)
```

**Issue**: Plots look different than expected
- This is expected with v2.0 enhancements
- New plots include QC zones and enhanced styling
- Customize with `show_zones = FALSE` for cleaner look

**Issue**: Upload fails for large files
```R
# Solution: Increase upload limit in global.R
options(shiny.maxRequestSize = 1000 * 1024^2)  # 1 GB
```

**Issue**: Reference data format error
- Ensure reference data includes: ExpDate, PlateId, SampleType, CV quantiles
- Column names must match exactly: "10%", "50%", "90%"
- Save as RDS or CSV format

---

## Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request with clear description
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
- Statistical process control concepts based on Westgard QC guidelines

---

## Citation

If you use `sqs` in your research, please cite:

```bibtex
@software{cheung2024sqs,
  author = {Cheung, Foo},
  title = {sqs: A Shiny App for SomaLogic SomaScan Quality Control},
  year = {2024},
  url = {https://github.com/foocheung/sqs_v2},
  version = {2.0},
  note = {R package with enhanced Levey-Jennings plots and quality control zones}
}
```

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

**Don't forget to watch the [video tutorial](#-video-tutorial---step-by-step-guide) to get started quickly!**

**For detailed implementation guides and migration instructions, see the documentation folder.**

**Questions?** Open an issue or contact the development team.

**Happy Quality Controlling!**
