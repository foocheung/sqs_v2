---
title: 'sqs: A Shiny Application for Quality Control Analysis of SomaScan Proteomics Data'
tags:
  - R
  - Shiny
  - proteomics
  - quality control
  - SomaScan
  - bioinformatics
  - data visualization
authors:
  - name: Foo Cheung
    orcid: [ORCID]
    affiliation: 1
affiliations:
  - name: [Your Institution]
    index: 1
date: 6 March 2026
bibliography: paper.bib
---

# Summary

The `sqs` package is an open-source R package built using the Golem framework [@fay2021golem] to provide a Shiny-based web application for quality control (QC) analysis of SomaLogic SomaScan assay data. It enables researchers to interactively upload, process, and visualize proteomics data, generating comprehensive QC reports in HTML format with professional visualizations. Key features include:

- **Data Input and Processing**: Supports upload of SomaLogic `.adat` files with user-provided historical reference data for comparison, with parsing via `SomaDataIO` [@somalogic2023].
- **Enhanced QC Metrics and Visualizations**: Calculates per-plate coefficient of variation (CV) statistics, performs Principal Component Analysis (PCA) for sample clustering, and generates advanced Levey-Jennings plots with color-coded quality control zones following statistical process control (SPC) conventions.
- **Statistical Process Control**: Implements industry-standard QC zones (±1 SD, ±2 SD, ±3 SD) with visual indicators for optimal, warning, action, and out-of-control ranges, supporting Westgard rules for quality assessment.
- **Data Export Capabilities**: Provides flexible export of protein abundance matrices and annotation tables in multiple formats (CSV, TSV, Excel, RDS, JSON) with configurable options for matrix orientation, metadata inclusion, and transformations.
- **Automated Reporting**: Produces structured, self-contained HTML reports summarizing experimental design, normalization metrics, calibration checks, and enhanced visualizations suitable for publication and regulatory submission.
- **Modern User Interface**: A professional, card-based Shiny interface with intuitive navigation, built-in help documentation, and real-time status indicators for enhanced user experience.

# Statement of Need

SomaScan assays generate complex proteomics data requiring rigorous quality control to ensure data reliability and reproducibility. While `SomaDataIO` [@somalogic2023] facilitates data parsing, it lacks interactive tools for comprehensive QC visualization, data export, and reporting. The `sqs` package fills this gap by providing:

1. **Automated QC Workflows**: Streamlines quality control analysis, reducing analysis time by up to 50% for datasets with 10+ plates compared to manual methods in spreadsheet software.

2. **Enhanced Statistical Process Control**: Implements professional Levey-Jennings plots with color-coded QC zones that enable rapid identification of systematic errors, trends, and out-of-control events following established clinical laboratory standards [@westgard2008basic].

3. **Flexible Reference Data**: Allows researchers to upload their own historical control data, enabling laboratory-specific quality monitoring and trending over time without dependence on external databases.

4. **Streamlined Data Export**: Provides one-click export of protein abundance matrices and annotation tables in analysis-ready formats, eliminating manual data wrangling steps and reducing preprocessing time for downstream statistical analysis.

5. **Publication-Ready Outputs**: Generates high-quality visualizations and comprehensive reports suitable for peer-reviewed publications, regulatory submissions, and laboratory quality documentation.

The package is tailored for proteomics researchers, clinical laboratory scientists, and bioinformaticians working with SomaLogic SomaScan data, particularly those requiring standardized QC workflows across multiple studies or longitudinal monitoring of assay performance.

# Functionality

## Core Components

### Data Input and Management (`mod_dataInput.R`, `global.R`)
- Upload SomaLogic `.adat` files (up to 500 MB) directly through the web interface
- Support for user-provided historical reference data in multiple formats (RDS, CSV, Feather)
- Flexible data handling with robust error checking and validation
- Example datasets available for testing and demonstration

### QC Analysis Functions (`global.R`)

The package consolidates all utility functions in a single `global.R` file for improved maintainability:

**`safe_cv(x)`**: Robust calculation of coefficient of variation with handling of edge cases (zero means, NA values)

**`plot_levey()`**: Enhanced Levey-Jennings plot generation with the following advanced features:
- Color-coded quality control zones (Zone 1: ±1 SD, Zone 2: ±2 SD, Zone 3: ±3 SD)
- Shaded background regions indicating optimal (light blue), warning (yellow), action (light green), and out-of-control ranges
- Different point shapes for visual QC zone identification (circle, triangle, square, X)
- Multiple line types distinguishing SD levels (dashed, dotted, dot-dash)
- Professional titles and subtitles displaying sample type, center statistic, and standard deviation
- Configurable options: SD levels (default: 1, 2, 3), centering method (median or mean), zone visibility, point size
- Smart truncation of long plate identifiers for improved readability

**`ks_test()`**: Kolmogorov-Smirnov test for comparing sample CV distributions against historical reference populations with comprehensive error handling

### Data Export Module (`mod_dataExport.R`)

The package includes a dedicated module for exporting processed proteomics data:

**Protein Abundance Matrix Export**:
- Multiple file formats: CSV, TSV, Excel (.xlsx), RData (.rds)
- Configurable matrix orientation (samples as rows or columns)
- Optional inclusion of sample metadata (PlateId, SampleId, SampleType, Barcode)
- Optional log2 transformation for downstream analysis
- Progress indicators for large dataset exports

**Protein Annotation Table Export**:
- Multiple file formats: CSV, TSV, Excel, JSON
- Customizable annotation fields: SeqId, Target, TargetFullName, UniProt, EntrezGeneID, EntrezGeneSymbol, Organism, Type, Dilution, ColCheck
- Flexible selection of annotation columns based on analysis requirements
- Support for integration with pathway analysis and gene ontology tools

This export functionality eliminates manual data extraction steps and ensures consistency in data formatting for downstream statistical analyses, machine learning workflows, and integration with other bioinformatics tools.

### Visualization and Reporting (`app_server.R`)
- Principal Component Analysis (PCA) plots with variance explained percentages
- Per-plate CV distributions with quantile summaries (10%, 50%, 90%)
- Levey-Jennings charts for both calibrators and QC samples
- Sample normalization metrics and ANML fraction analysis
- Automated HTML report generation with embedded high-resolution plots (300 DPI)
- Self-contained reports suitable for archiving and sharing

### User Interface (`app_ui.R`)
- Modern gradient header with professional styling
- Card-based layout for improved visual organization
- Tabbed panels for data exploration, QC plots, data export, report preview, and help documentation
- Real-time status indicators and progress feedback
- Responsive design optimized for different screen sizes
- Built-in quick start guide and feature explanations

## Performance

Benchmarked at **113.2 seconds** for processing 15 plates on a Mac M1 (64 GB RAM, macOS 14.7.6), representing a significant improvement over manual QC workflows. The application efficiently handles datasets up to 500 MB with optimized data processing using `tidyverse` [@wickham2019tidyverse] and `arrow` [@richardson2022arrow] packages.

Data export operations are optimized for performance:
- CSV/TSV exports: < 5 seconds for typical datasets (10,000 proteins × 100 samples)
- Excel exports: < 15 seconds with progress indicators
- Supports concurrent downloads without blocking the main application

## Quality Control Zones Interpretation

The enhanced Levey-Jennings plots implement standard statistical process control zones:

- **Zone 1 (±1 SD)**: Optimal range where 68% of data points are expected under normal distribution
- **Zone 2 (±2 SD)**: Warning range where 95% of data should fall; points here warrant attention
- **Zone 3 (±3 SD)**: Action range where 99.7% of data should fall; points require investigation
- **Beyond ±3 SD**: Out-of-control range indicating likely systematic error requiring immediate action

This visualization framework supports manual application of Westgard rules [@westgard2008basic] for comprehensive quality assessment, including detection of systematic shifts, trends, and random errors.

![Enhanced Levey-Jennings plot for Calibrator CVs with color-coded QC zones](paper/levey_calibrator.png)

*Figure 1: Enhanced Levey-Jennings plot showing calibrator coefficient of variation (CV) across plates with color-coded quality control zones. Different point shapes indicate QC zone membership, and multiple line types distinguish ±1 SD (dashed), ±2 SD (dotted), and ±3 SD (dot-dash) control limits.*

![PCA Plot by Sample Type](paper/pca_sample_type.png)

*Figure 2: Principal Component Analysis (PCA) plot showing sample clustering by sample type. Percentage of variance explained by each principal component is displayed on axis labels.*

# Implementation Details

## Architecture

The package follows a modular Shiny architecture using the Golem framework:

- **`global.R`**: Consolidated utility functions, package loading, and global options
- **`app_ui.R`**: Modern user interface with professional styling
- **`app_server.R`**: Server logic for data processing and report generation
- **`mod_dataInput.R`**: Modular data input component
- **`mod_dataExport.R`**: Modular data export component (NEW in v2.0)
- **`mod_table.R`**: Modular table display component

This structure enhances code reusability, testing, and maintenance while eliminating code duplication present in earlier versions. The modular design allows easy extension with additional export formats or analysis modules.

## Code Quality Improvements (v2.0)

Version 2.0 represents a significant refactoring:

- **Consolidated Functions**: All utility functions moved to `global.R`, eliminating duplication
- **Modular Export System**: Dedicated module for data export with configurable options
- **Enhanced Documentation**: Roxygen2 comments for all functions with parameter descriptions
- **Improved Error Handling**: Comprehensive validation and informative error messages
- **Better Maintainability**: Single source of truth for function definitions
- **Performance Optimization**: Efficient data processing with minimal memory overhead
- **Progress Indicators**: Real-time feedback for long-running export operations

# Availability and Documentation

Available under the MIT License at [https://github.com/foocheung/sqs_v2](https://github.com/foocheung/sqs_v2). The repository includes:

- **README.md**: Comprehensive overview with installation instructions, usage examples, and feature descriptions
- **Video Tutorial**: Step-by-step video demonstration (~2 minutes) showing data upload, report generation, data export, and QC interpretation, available in the GitHub repository
- **Documentation**: Quick start guide, improvements guide, and plot comparison documentation
- **Demo Application**: Live demonstration at [https://webtools.shinyapps.io/sqs_v3_ori/](https://webtools.shinyapps.io/sqs_v3_ori/)
- **Example Reports**: Sample HTML reports demonstrating output quality
- **Example Data**: Reference datasets for testing and validation

## Dependencies

Core dependencies include:
- `shiny` [@chang2021shiny]: Web application framework
- `shinythemes`, `shinycssloaders`: UI enhancements
- `SomaDataIO` [@somalogic2023]: SomaScan data parsing
- `tidyverse` [@wickham2019tidyverse]: Data manipulation and visualization
- `ggplot2`: Enhanced plotting capabilities
- `plotly`: Interactive visualizations
- `rmarkdown`, `knitr`: Report generation
- `arrow`: Efficient data storage and retrieval
- `DT`, `kableExtra`: Table formatting
- `forcats`, `broom`: Statistical utilities
- `writexl`, `jsonlite`: Data export formats (optional)

# Acknowledgements

The author thanks the open-source R community, particularly the developers of `shiny`, `tidyverse`, and `SomaDataIO` packages. The enhanced Levey-Jennings visualization follows statistical process control principles established by Westgard and colleagues in clinical laboratory quality control.

# References
