---
title: "sqs: A Shiny Application for Quality Control Analysis of SomaScan Proteomics Data"
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
    affiliation: 1
affiliations:
  - name: National Institutes of Health, Center for Human Immunology
    index: 1
date: 2026-03-06
bibliography: paper.bib
---

# Summary

`sqs` is an open-source R package that provides an interactive web application for quality control (QC) of SomaLogic SomaScan proteomics data. SomaScan assays measure thousands of proteins simultaneously, producing large and complex datasets that require careful quality checks before scientific interpretation. `sqs` addresses the challenge of performing standardized, reproducible QC by offering a browser-based interface that integrates data upload, statistical visualization, and automated reporting into a single workflow.

The application implements statistical process control methodologies adapted for multi-plate proteomics assays, including enhanced Levey-Jennings plots with color-coded quality zones, principal component analysis for outlier detection, and coefficient of variation tracking across experimental batches. Users can upload SomaScan data files, visualize quality metrics interactively, export processed data matrices in multiple formats, and generate publication-ready HTML reports, all without requiring advanced programming skills.

By providing standardized QC workflows in an accessible format, `sqs` enables researchers, clinicians, and bioinformaticians to quickly assess data quality, identify systematic errors, document QC decisions, and share reproducible outputs with collaborators.

---

# Statement of Need

High-throughput technologies generate large, multi-plate datasets can be sensitive to batch effects, assay drift, and technical variability. Ensuring data quality is essential for reliable biological interpretation, especially in clinical and translational research where results may inform downstream experiments or patient studies. However, QC workflows are often performed manually using spreadsheets or custom scripts, which can be time-consuming, inconsistent across studies, and difficult to reproduce.

Research teams need tools that:

1. **Standardize QC procedures** across multi-plate proteomics datasets to ensure consistent quality assessment
2. **Provide intuitive visualizations** that allow rapid identification of systematic errors, trends, or failed assay plates
3. **Generate automated documentation** of QC decisions for regulatory or publication requirements
4. **Support flexible data export** for downstream statistical analysis and integration with other bioinformatics tools
5. **Enable batch processing** of multiple datasets with user-provided historical reference data, generating individual reports and summary statistics in a single automated workflow
6. **Remain accessible** to users without extensive programming expertise

`sqs` was developed to meet these needs by implementing a complete QC workflow specifically designed for SomaScan data. The software has been used for QC in multi-omics studies for several years and for example more recently in published research involving dietary interventions and immune profiling [@link2024vegan], as well as deep phenotyping of post-infectious chronic fatigue syndrome [@walitt2024cfs], demonstrating its effectiveness in translational proteomics workflows.

---

# State of the Field

While data import tools exist for SomaScan data [@somalogic2023], interactive QC applications specifically designed for multi-plate proteomics quality control are limited. Custom QC pipelines require programming expertise and substantial development time, while general-purpose visualization tools require manual adaptation to proteomics-specific quality metrics. `sqs` addresses this gap by providing a dedicated, open-source application that combines QC logic, standardized statistical process control visualizations, and automated reporting in a reproducible framework. The decision to develop a standalone application was driven by the need for proteomics-tailored QC procedures, an accessible interface for non-programmers, and regulatory-suitable documentation capabilities.

---

# Key Features and Functionality

## Quality Control Visualizations

`sqs` implements multiple complementary visualization approaches for comprehensive QC assessment:

**Enhanced Levey-Jennings Plots**: The application generates statistical process control charts with color-coded quality zones (±1 SD, ±2 SD, ±3 SD) that align with established clinical laboratory practices [@westgard2008basic]. These plots use distinct visual elements—shaded background regions, multiple line types, and different point shapes—to indicate whether each plate falls within optimal, warning, action, or out-of-control ranges. This design allows users to identify systematic shifts or trends without requiring deep statistical expertise.

**Principal Component Analysis (PCA)**: Interactive PCA plots enable detection of sample clustering patterns, batch effects, and outliers. The application displays variance explained by each component and supports color-coding by sample type, plate, or other metadata factors.

**Coefficient of Variation (CV) Analysis**: Per-plate CV distributions are calculated and visualized with quantile summaries (10%, 50%, 90%), allowing users to track precision across experimental batches and identify plates with unusually high variability.

## Data Processing and Export

The application includes a dedicated export module (`mod_dataExport.R`) that provides:

**Protein Abundance Matrix Export**: Users can export quantitative protein expression data in multiple formats (CSV, TSV, Excel, RDS) with configurable options for matrix orientation (samples as rows or columns), metadata inclusion, and optional log2 transformation. This eliminates manual data wrangling steps and ensures consistent formatting for downstream analysis.

**Protein Annotation Table Export**: Mapping tables between protein identifiers and annotations (UniProt, Entrez Gene, target names) can be exported in CSV, TSV, Excel, or JSON formats with customizable column selection. This supports integration with pathway analysis tools and gene ontology databases.

**Flexible Configuration**: Export operations include progress indicators for large datasets and support concurrent downloads without blocking the main application interface.

## Automated Reporting

The application generates self-contained HTML reports that include:

- Sample summary tables with experimental design information
- High-resolution plots (300 DPI) suitable for publication
- Normalization metrics and quality flags
- Calibration statistics and plate-level QC results
- Embedded metadata documenting analysis parameters and timestamps

Reports are fully reproducible and can be archived for regulatory documentation or shared with collaborators who do not have access to the application.

## User-Provided Reference Data

Users can upload their own historical reference data for Levey-Jennings plots, enabling laboratory-specific quality monitoring and trending over time. The application accepts reference data in multiple formats (CSV, RDS, Feather) and automatically integrates it with current sample data for comparative QC assessment.

## Batch Processing

The application supports automated batch processing of multiple SomaScan datasets through a command-line workflow. Users can process hundreds of `.adat` files in a single run, with each file generating an individual HTML report containing sample counts, flagged samples, PCA plots, Levey-Jennings plots for QC and Calibrator samples, and CV statistics tables. The batch processing workflow:

- Loads user-provided historical reference data from Excel format
- Processes each dataset sequentially with error handling for individual file failures
- Generates comprehensive HTML reports using R Markdown templates
- Produces a summary CSV file documenting processing results for all datasets
- Provides progress tracking and status messages throughout execution

This capability is particularly valuable for large-scale studies requiring consistent QC assessment across many experimental batches, reducing manual effort while maintaining standardized quality criteria.

---

# Software Design

`sqs` is implemented as a modular Shiny application using the Golem framework [@fay2021golem], which promotes reproducibility, testing, and maintainability for production-ready web applications. The architecture separates concerns into reusable modules:

- **`global.R`**: Consolidated utility functions for CV calculation, statistical testing, and visualization
- **`mod_dataInput.R`**: Data upload and validation module
- **`mod_dataExport.R`**: Configurable data export module with format conversion
- **`mod_table.R`**: Interactive table display components
- **`app_ui.R`**: Professional card-based interface with gradient styling and responsive layout
- **`app_server.R`**: Server logic coordinating data processing, visualization, and reporting

This modular design supports future extension of QC metrics and export formats without requiring major refactoring. All QC functions are centralized in `global.R` to eliminate code duplication and improve maintainability.

Several design decisions prioritize usability and reproducibility:

**Accessibility Over Maximum Performance**: The application enables researchers to run QC analyses locally through a graphical interface rather than requiring command-line expertise or high-performance computing resources.

**R Markdown Integration**: Automated reporting uses R Markdown to ensure that QC outputs are fully reproducible, versioned, and compatible with existing research documentation workflows.

**Progressive Disclosure**: The interface uses a tabbed layout that reveals complexity gradually—users can perform basic QC workflows immediately while accessing advanced features (custom reference data, export configurations, detailed help) as needed.

**Statistical Process Control Standards**: Visualization design follows established clinical laboratory QC conventions [@westgard2008basic], making the software familiar to users with laboratory medicine backgrounds while remaining interpretable for basic research scientists.

---

# Performance and Benchmarking

The application is optimized for typical SomaScan dataset sizes encountered in research settings:

- **QC Report Generation**: 113.2 seconds for 15-plate datasets on Mac M1 hardware (64 GB RAM)
- **Data Export Operations**: < 5 seconds for CSV/TSV exports of typical datasets (10,000 proteins × 100 samples)
- **Excel Export**: < 15 seconds with progress indicators
- **Maximum Dataset Size**: Supports large datasets with efficient memory management

These performance characteristics enable rapid QC assessment during data review meetings and support integration into routine proteomics workflows without requiring specialized computing infrastructure.

---

# Research Impact

`sqs` has been integrated into ongoing proteomics workflows, supporting QC analysis for multi-omics and clinical studies. The tool has been used to evaluate numerous SomaScan datasets and contributed to published research in dietary intervention studies examining immune responses [@link2024vegan] and deep phenotyping investigations of post-infectious chronic fatigue syndrome [@walitt2024cfs].

In these projects, `sqs` reduced manual QC effort, improved reproducibility of quality assessment procedures, and facilitated generation of publication-ready figures for collaborative review. The standardized QC workflow will be particularly valuable in complex studies where consistent quality criteria must be applied across different laboratories, conditions and samples.

The open-source github repository includes comprehensive documentation, a step-by-step video tutorial (~5 minutes), and a live demonstration application.

---

# Availability and Documentation

The software is available under the MIT License at [https://github.com/foocheung/sqs_v2](https://github.com/foocheung/sqs_v2).

The repository includes:

- **Installation Instructions**: Detailed setup guide with dependency management
- **Video Tutorial**: Step-by-step demonstration of data upload, QC analysis, and report generation
- **Batch Processing Script**: Complete example workflow for automated processing of multiple datasets
- **Documentation**: Quick start guide, feature descriptions, and plot interpretation guidelines
- **Live Demo**: Interactive demonstration
- **Example Reports**: Sample HTML outputs demonstrating report format and quality

---

# AI Usage Disclosure

Generative AI tools were used to assist in editing and structuring the manuscript text to meet journal formatting and readability requirements. All technical content, software design decisions, implementation details, and validation of claims were performed by the author. The author reviewed and verified all AI-assisted edits and retains full responsibility for the accuracy of the manuscript.

---

# Acknowledgements

This work was supported by the Intramural Research Program of the National Institutes of Health. The author thanks collaborators and members of the NIH Center for Human Immunology for feedback during development and testing.

---

# References
