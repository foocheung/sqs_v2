---

title: 'sqs: A Shiny Application for Quality Control Analysis of SomaScan Proteomics Data'
tags:

* R
* Shiny
* proteomics
* quality control
* SomaScan
* bioinformatics
* data visualization
  authors:
* name: Foo Cheung
  orcid: [ORCID]
  affiliation: 1
  affiliations:
* name: National Institutes of Health, Center for Human Immunology
  index: 1
  date: 6 March 2026
  bibliography: paper.bib

---

# Summary

The `sqs` package is an open-source R package built using the Golem framework [@fay2021golem] to provide a Shiny-based web application for quality control (QC) analysis of SomaLogic SomaScan assay data. It enables researchers to interactively upload, process, and visualize proteomics data, generating comprehensive QC reports in HTML format with professional visualizations. Key features include:

* **Data Input and Processing**: Supports upload of SomaLogic `.adat` files with user-provided historical reference data for comparison, with parsing via `SomaDataIO` [@somalogic2023].
* **Enhanced QC Metrics and Visualizations**: Calculates per-plate coefficient of variation (CV) statistics, performs Principal Component Analysis (PCA) for sample clustering, and generates advanced Levey-Jennings plots with color-coded quality control zones following statistical process control (SPC) conventions.
* **Statistical Process Control**: Implements industry-standard QC zones (±1 SD, ±2 SD, ±3 SD) with visual indicators for optimal, warning, action, and out-of-control ranges, supporting Westgard rules for quality assessment.
* **Data Export Capabilities**: Provides flexible export of protein abundance matrices and annotation tables in multiple formats (CSV, TSV, Excel, RDS, JSON) with configurable options for matrix orientation, metadata inclusion, and transformations.
* **Automated Reporting**: Produces structured, self-contained HTML reports summarizing experimental design, normalization metrics, calibration checks, and enhanced visualizations suitable for publication and regulatory submission.
* **Modern User Interface**: A professional, card-based Shiny interface with intuitive navigation, built-in help documentation, and real-time status indicators for enhanced user experience.

# Statement of Need

SomaScan assays generate complex proteomics data requiring rigorous quality control to ensure data reliability and reproducibility. While `SomaDataIO` [@somalogic2023] facilitates data parsing, it lacks interactive tools for comprehensive QC visualization, data export, and reporting. The `sqs` package fills this gap by providing:

1. **Automated QC Workflows**: Streamlines quality control analysis, reducing analysis time by up to 50% for datasets with 10+ plates compared to manual methods in spreadsheet software.

2. **Enhanced Statistical Process Control**: Implements professional Levey-Jennings plots with color-coded QC zones that enable rapid identification of systematic errors, trends, and out-of-control events following established clinical laboratory standards [@westgard2008basic].

3. **Flexible Reference Data**: Allows researchers to upload their own historical control data, enabling laboratory-specific quality monitoring and trending over time without dependence on external databases.

4. **Streamlined Data Export**: Provides one-click export of protein abundance matrices and annotation tables in analysis-ready formats, eliminating manual data wrangling steps and reducing preprocessing time for downstream statistical analysis.

5. **Publication-Ready Outputs**: Generates high-quality visualizations and comprehensive reports suitable for peer-reviewed publications, regulatory submissions, and laboratory quality documentation.

The `sqs` application has been used in NIH proteomics workflows to support quality control and visualization of SomaScan datasets in translational and clinical studies. For example, the tool was applied during data processing and QC review for multi-omics analyses investigating dietary interventions and immune responses [@link2024vegan] and for deep phenotyping studies of post-infectious myalgic encephalomyelitis/chronic fatigue syndrome [@walitt2024cfs]. In these projects, `sqs` enabled rapid QC assessment across multiple assay plates, improved reproducibility of analysis pipelines, and facilitated generation of publication-ready figures and reports for collaborative review.

The package is tailored for proteomics researchers, clinical laboratory scientists, and bioinformaticians working with SomaLogic SomaScan data, particularly those requiring standardized QC workflows across multiple studies or longitudinal monitoring of assay performance.

# Functionality

## Core Components

### Data Input and Management (`mod_dataInput.R`, `global.R`)

* Upload SomaLogic `.adat` files (up to 500 MB) directly through the web interface
* Support for user-provided historical reference data in multiple formats (RDS, CSV, Feather)
* Flexible data handling with robust error checking and validation
* Example datasets available for testing and demonstration

### QC Analysis Functions (`global.R`)

The package consolidates all utility functions in a single `global.R` file for improved maintainability:

**`safe_cv(x)`**: Robust calculation of coefficient of variation with handling of edge cases (zero means, NA values)

**`plot_levey()`**: Enhanced Levey-Jennings plot generation with the following advanced features:

* Color-coded quality control zones (Zone 1: ±1 SD, Zone 2: ±2 SD, Zone 3: ±3 SD)
* Shaded background regions indicating optimal (light blue), warning (yellow), action (light green), and out-of-control ranges
* Different point shapes for visual QC zone identification (circle, triangle, square, X)
* Multiple line types distinguishing SD levels (dashed, dotted, dot-dash)
* Professional titles and subtitles displaying sample type, center statistic, and standard deviation
* Configurable options: SD levels (default: 1, 2, 3), centering method (median or mean), zone visibility, point size
* Smart truncation of long plate identifiers for improved readability

**`ks_test()`**: Kolmogorov-Smirnov test for comparing sample CV distributions against historical reference populations with comprehensive error handling

### Data Export Module (`mod_dataExport.R`)

The package includes a dedicated module for exporting processed proteomics data:

**Protein Abundance Matrix Export**:

* Multiple file formats: CSV, TSV, Excel (.xlsx), RData (.rds)
* Configurable matrix orientation (samples as rows or columns)
* Optional inclusion of sample metadata (PlateId, SampleId, SampleType, Barcode)
* Optional log2 transformation for downstream analysis
* Progress indicators for large dataset exports

**Protein Annotation Table Export**:

* Multiple file formats: CSV, TSV, Excel, JSON
* Customizable annotation fields: SeqId, Target, TargetFullName, UniProt, EntrezGeneID, EntrezGeneSymbol, Organism, Type, Dilution, ColCheck
* Flexible selection of annotation columns based on analysis requirements
* Support for integration with pathway analysis and gene ontology tools

### Visualization and Reporting (`app_server.R`)

* Principal Component Analysis (PCA) plots with variance explained percentages
* Per-plate CV distributions with quantile summaries (10%, 50%, 90%)
* Levey-Jennings charts for both calibrators and QC samples
* Sample normalization metrics and ANML fraction analysis
* Automated HTML report generation with embedded high-resolution plots (300 DPI)
* Self-contained reports suitable for archiving and sharing

### User Interface (`app_ui.R`)

* Modern gradient header with professional styling
* Card-based layout for improved visual organization
* Tabbed panels for data exploration, QC plots, data export, report preview, and help documentation
* Real-time status indicators and progress feedback
* Responsive design optimized for different screen sizes
* Built-in quick start guide and feature explanations

# Performance

Benchmarked at **113.2 seconds** for processing 15 plates on a Mac M1 (64 GB RAM, macOS 14.7.6), representing a significant improvement over manual QC workflows. The application efficiently handles datasets up to 500 MB with optimized data processing using `tidyverse` [@wickham2019tidyverse] and `arrow` [@richardson2022arrow] packages.

# Implementation Details

## Architecture

The package follows a modular Shiny architecture using the Golem framework:

* **`global.R`**: Consolidated utility functions, package loading, and global options
* **`app_ui.R`**: Modern user interface with professional styling
* **`app_server.R`**: Server logic for data processing and report generation
* **`mod_dataInput.R`**: Modular data input component
* **`mod_dataExport.R`**: Modular data export component
* **`mod_table.R`**: Modular table display component

# Availability

Available under the MIT License at [https://github.com/foocheung/sqs_v2](https://github.com/foocheung/sqs_v2).

# Acknowledgements

The author thanks the open-source R community, particularly the developers of `shiny`, `tidyverse`, and `SomaDataIO` packages. The enhanced Levey-Jennings visualization follows statistical process control principles established by Westgard and colleagues in clinical laboratory quality control.

# References
