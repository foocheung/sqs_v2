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
  - name: Brian Sellers
    affiliation: 1
  - name: Thomas Langowski
    affiliation: 1
affiliations:
  - name: National Institutes of Health, Center for Human Immunology
    index: 1
date: 2026-03-06
bibliography: paper.bib
---

# Summary

`sqs` is an open-source R package that provides an interactive Shiny web application for quality control (QC) of SomaLogic SomaScan proteomics data. SomaScan assays measure thousands of proteins simultaneously, producing large, high-dimensional datasets that require careful QC prior to downstream analysis. However, QC workflows are often manual, inconsistent, and difficult to reproduce across studies. `sqs` addresses this challenge by providing a standardized, browser-based interface that integrates data upload, statistical visualization, and automated reporting into a unified workflow.

The application is built using the Shiny framework [@chang2021shiny] and implements statistical process control methods adapted for multi-plate proteomics datasets. These include enhanced Levey–Jennings plots [@levey1950use] with color-coded quality zones aligned with clinical QC standards [@westgard1981multirule; @westgard2008basic], principal component analysis (PCA) for detection of batch effects and outliers, and coefficient of variation (CV) tracking across experimental batches. Users can upload SomaScan data, interactively explore QC metrics, export processed datasets in multiple formats, and generate publication-ready HTML reports without requiring advanced programming expertise.

SomaScan's processing pipeline pre-computes a comprehensive set of QC metrics — including normalization scale factors, ANML fractions, plate scale factors, calibrator tail percentages, and per-sample quality flags — and writes these directly into the `.adat` file. `sqs` surfaces these pre-computed values through a consistent, accessible interface, applies configurable acceptance criteria, and places each run in historical context using user-provided reference data. In addition, the application independently calculates per-plate CV distributions and Levey–Jennings trend plots from the raw protein signal data, providing precision monitoring that goes beyond the static flags embedded in the file. By combining these capabilities into a single reproducible workflow, `sqs` enables researchers, clinicians, and bioinformaticians to rapidly assess data quality, identify systematic issues, document QC decisions, and produce reproducible outputs for collaborative and regulatory use.

---

# Statement of Need

High-throughput proteomics technologies such as SomaScan generate large, multi-plate datasets that are sensitive to batch effects, assay drift, and technical variability. Ensuring data quality is essential for reliable biological interpretation, particularly in clinical and translational research where results may inform downstream experiments or patient studies. Despite this, QC workflows are frequently implemented using spreadsheets or custom scripts, making them time-consuming, inconsistent across studies, and difficult to reproduce.

Researchers require tools that standardize QC procedures, provide intuitive visualizations, and generate automated documentation. Additional needs include flexible data export for downstream analysis, support for batch processing of multiple datasets, and accessibility for users without extensive programming expertise.

While SomaLogic's pipeline produces a rich set of per-sample and per-plate QC flags within the `.adat` file format, interpreting these values, applying acceptance criteria consistently, comparing performance against historical runs, and assembling findings into a shareable report requires substantial manual effort. `sqs` was developed to address this gap by providing a complete, interactive QC workflow that reads and contextualises SomaLogic's pre-computed metrics, independently calculates precision statistics from raw signal data, and packages all outputs into reproducible HTML reports. The software has been applied in NIH multi-omics studies and used in published research involving dietary interventions [@link2024vegan] and post-infectious chronic fatigue syndrome [@walitt2024cfs], demonstrating its utility in translational proteomics workflows.

---

# State of the Field

Existing approaches to SomaScan data analysis primarily focus on downstream statistical analysis [@somalogic2023], while QC workflows are often implemented through custom pipelines or general-purpose visualization tools. These approaches require programming expertise and substantial development time, and they lack standardized implementations of proteomics-specific QC metrics.

General visualization platforms do not natively support statistical process control methods commonly used in laboratory QC, requiring manual adaptation. As a result, QC practices vary widely across research groups, limiting reproducibility and comparability of results.

`sqs` addresses these limitations by providing a dedicated, open-source application that reads and evaluates SomaLogic's embedded QC outputs, supplements them with independently calculated precision metrics and historical trend analysis, and integrates everything into a standardized, reproducible reporting framework. The software is designed to be accessible to non-programmers while remaining flexible for advanced users, bridging the gap between usability and methodological rigor.

---

# Key Features and Functionality

`sqs` provides a comprehensive set of tools for QC assessment, data processing, and reporting:

**Levey–Jennings Plots**
The application generates statistical process control charts [@levey1950use] with color-coded quality zones (±1, ±2, ±3 standard deviations) based on established clinical laboratory practices [@westgard1981multirule; @westgard2008basic]. Per-plate median CV values are calculated by the application from the raw `seq.*` protein signal columns and plotted against a user-provided historical reference distribution. Shaded regions and distinct point shapes indicate acceptable, warning, and out-of-control ranges, enabling rapid identification of systematic trends or assay drift across sequential runs.

**Principal Component Analysis (PCA)**
Interactive PCA plots allow users to detect clustering patterns, batch effects, and outliers. Variance explained by each component is displayed, and samples can be colored by metadata variables such as plate or sample type.

**Coefficient of Variation Analysis**
Per-plate CV distributions are calculated by the application from the raw protein signal data and summarized using the 10th, 50th, and 90th percentiles, allowing users to monitor assay precision across batches and identify plates with elevated variability.

**Normalization and Calibration Metrics**
The application reads and evaluates the QC metrics pre-computed by SomaLogic's pipeline and embedded in the `.adat` file. These include per-sample normalization scale factors across three dilution groups (`NormScale_0_005`, `NormScale_0_5`, `NormScale_20`), ANML fraction used per dilution group, plate-level scale factors extracted from the file header, calibrator tail percentages, per-protein calibration accuracy flags (`ColCheck`), and per-sample quality flags (`RowCheck`). Configurable acceptance criteria are applied to each metric and results are summarised as Pass/Flag counts, making systematic issues immediately visible without requiring users to inspect raw file contents.

**Data Export**
Processed protein abundance matrices can be exported in multiple formats (CSV, TSV, Excel, RDS) with configurable options for orientation, metadata inclusion, and transformation. Annotation tables mapping protein identifiers to metadata (e.g., UniProt, Entrez) can also be exported in multiple formats.

**Automated Reporting**
The application generates self-contained HTML reports that include sample summaries, QC plots, normalization metrics, calibration statistics, and analysis metadata. Reports are reproducible and suitable for publication or regulatory documentation.

**Batch Processing**
A command-line workflow supports automated QC analysis of multiple datasets, generating individual reports and summary outputs. This enables scalable QC analysis for large studies while maintaining standardized criteria.

**User-Provided Reference Data**
Users can incorporate historical reference datasets for Levey–Jennings plots, enabling laboratory-specific QC monitoring and detection of long-term trends.

---

# Software Design

`sqs` is implemented as a modular Shiny application [@chang2021shiny] using the Golem framework [@fay2021golem], which supports reproducibility, testing, and maintainability. The application leverages the tidyverse ecosystem [@wickham2019tidyverse] for data manipulation and Apache Arrow [@richardson2022arrow] for efficient handling of large datasets.

The architecture separates functionality into reusable modules for data input, processing, visualization, and export. This modular design enables extension of QC metrics and features without major refactoring. Core QC functions are centralized in `global.R` to ensure consistency and reduce duplication.

Design decisions emphasize usability and reproducibility. A tab-based interface enables progressive disclosure, allowing users to perform basic QC workflows immediately while accessing advanced features as needed. Integration with R Markdown ensures that all outputs are reproducible and compatible with standard research documentation workflows.

Visualization design follows established statistical process control conventions [@levey1950use; @westgard1981multirule; @westgard2008basic], making outputs interpretable for users with laboratory and clinical backgrounds while remaining accessible to general researchers.

---

# Performance and Benchmarking

`sqs` is optimized for typical SomaScan dataset sizes encountered in research:

* QC report generation: approximately 113 seconds for 15 plates on Apple M1 (64 GB, macOS 14.7.6)
* Data export operations: <5 seconds for standard formats
* Excel export: <15 seconds with progress indicators
* Maximum supported file size: 500 MB
* Batch processing: multiple `.adat` files processed sequentially with individual HTML reports per file

These performance characteristics enable rapid QC assessment during data review and integration into routine workflows without requiring specialized computing infrastructure.

---

# Research Impact

`sqs` has been integrated into proteomics workflows at the NIH Center for Human Immunology and has supported QC analysis in multi-omics and clinical studies. The software has contributed to published research in dietary intervention studies examining immune responses [@link2024vegan] and deep phenotyping of post-infectious chronic fatigue syndrome [@walitt2024cfs].

In these applications, `sqs` reduced manual QC effort, improved reproducibility, and facilitated generation of publication-ready outputs. The standardized workflow is particularly valuable in multi-site studies requiring consistent QC criteria across laboratories.

The open-source repository includes documentation, example datasets, tutorials, and a live demonstration application, supporting adoption by external research groups. Because SomaScan is widely used in biomarker discovery and clinical proteomics, `sqs` provides a broadly applicable QC solution across diverse study designs.

---

# Availability and Documentation

The software is available under the MIT License at:
[https://github.com/foocheung/sqs_v2](https://github.com/foocheung/sqs_v2)

The repository includes installation instructions, example datasets, batch processing workflows, documentation, and a live demonstration application to support user adoption.

---

# AI Usage Disclosure

Generative AI tools were used to assist in editing and structuring the manuscript text. All technical content, software design decisions, and validation of claims were performed by the author.

---

# Acknowledgements

This work was supported by the Intramural Research Program of the National Institutes of Health. The author thanks collaborators for feedback during development and testing.

---
