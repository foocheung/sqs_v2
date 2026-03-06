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

`sqs` is an open-source R package that provides an interactive web application for quality control (QC) of SomaLogic SomaScan proteomics data. SomaScan assays measure thousands of proteins simultaneously, producing large and complex datasets that require careful quality checks before scientific interpretation. However, QC workflows are often performed manually using spreadsheets or custom scripts, which can be time-consuming, inconsistent, and difficult to reproduce.

`sqs` addresses this need by offering a browser-based interface that allows users to upload SomaScan data files, visualize quality metrics, and generate publication-ready reports without requiring advanced programming skills. The software integrates data parsing, statistical visualization, and automated reporting into a single workflow. Key features include interactive plots for detecting outliers, plate-level variability summaries, standardized statistical process control charts, and export of cleaned data matrices for downstream analysis.

By lowering the technical barrier to performing robust QC, `sqs` enables researchers, clinicians, and bioinformaticians to quickly assess data quality, document results, and share reproducible QC outputs with collaborators.

---

# Statement of Need

High-throughput proteomics technologies such as SomaScan generate large, multi-plate datasets that are sensitive to batch effects, assay drift, and technical variability. Ensuring data quality is essential for reliable biological interpretation, especially in clinical and translational research where results may inform downstream experiments or patient studies. While the `SomaDataIO` package facilitates data import and parsing [@somalogic2023], it does not provide interactive visualization, standardized QC workflows, or automated reporting tools.

Researchers often rely on ad-hoc scripts or spreadsheet workflows to compute coefficients of variation, examine plate distributions, and flag outliers. These approaches are labor-intensive, error-prone, and difficult to reproduce across studies. Additionally, many research teams lack access to custom bioinformatics pipelines, creating a need for user-friendly tools that support standardized QC practices.

`sqs` was developed to address these challenges by providing:

1. **Reproducible QC workflows** that standardize common SomaScan QC steps across studies.
2. **Interactive visualizations** that allow users to quickly identify systematic errors, trends, or failed assay plates.
3. **Automated reporting** that documents QC decisions for regulatory or publication purposes.
4. **Accessible interfaces** that enable non-programmers to perform advanced QC analyses.

The software has already been used in NIH multi-omics studies involving dietary interventions and immune profiling [@link2024vegan], as well as deep phenotyping of post-infectious chronic fatigue syndrome [@walitt2024cfs], demonstrating its relevance in translational proteomics workflows.

---

# State of the Field

Several tools exist for working with SomaScan data, but they focus primarily on data import or downstream statistical analysis rather than quality control. `SomaDataIO` [@somalogic2023] provides robust functions for reading `.adat` files and anlaysis but requires programming skils and does not offer web based visualization or automatic push button reporting capabilities. General-purpose visualization packages such as `ggplot2` or `tidyverse` tools [@wickham2019tidyverse] can be used to build custom QC pipelines, but these require programming expertise and substantial setup time.

Commercial or internal laboratory QC pipelines may exist, but they are often proprietary or not easily shared across institutions. Existing Shiny dashboards typically focus on exploratory data analysis rather than standardized QC procedures.

`sqs` fills this gap by combining SomaScan-specific QC metrics, standardized statistical process control visualizations, and automated reporting within a reproducible, open-source framework. The decision to build a dedicated application rather than extending existing tools was driven by the need for:

* Specific QC logic tailored to multi-plate proteomics assays,
* an accessible interface for users without programming expertise,
* reproducible reporting suitable for regulatory documentation.

Thus, `sqs` complements existing packages rather than duplicating their functionality, providing a missing layer focused on QC standardization and usability.

---

# Software Design

`sqs` is implemented as a modular Shiny application using the Golem framework [@fay2021golem], which promotes reproducibility, testing, and maintainability for production-ready Shiny applications. The architecture separates data input, analysis, visualization, and export components into reusable modules. This modular design allows future extension of QC metrics and export formats without restructuring the application.

Several design trade-offs were considered. First, the application prioritizes usability over maximal computational performance, enabling researchers to run QC analyses locally through a graphical interface. Second, automated reporting was implemented using R Markdown to ensure that QC outputs are fully reproducible and shareable. Third, the application consolidates QC functions into a centralized utility layer to reduce code duplication and improve maintainability.

Visualization components emphasize interpretability for non-specialists. For example, Levey–Jennings plots include color-coded control zones that align with standard laboratory QC practices [@westgard2008basic], allowing users to identify deviations without requiring statistical expertise. Data export options were designed to support downstream workflows in R, Excel, and machine-learning environments.

Overall, the design balances accessibility, reproducibility, and extensibility, ensuring that the software supports both immediate research needs and future methodological development.

---

# Research Impact Statement

`sqs` has been integrated into ongoing proteomics workflows, supporting QC analysis for multi-omics and clinical studies. The tool has been used to evaluate numerous SomaScan datasets and more recently in dietary intervention studies examining immune responses [@link2024vegan] and in deep phenotyping investigations of post-infectious chronic fatigue syndrome [@walitt2024cfs]. In these projects, `sqs` reduced manual QC effort, improved reproducibility of analysis steps, and facilitated generation of publication-ready figures for collaborative review.

Benchmarking indicates that the application can process multi-plate datasets in under one minute on standard hardware, enabling rapid QC assessment during data review meetings. The open-source repository includes reproducible example datasets, documentation, and a live demonstration application, supporting adoption by external research groups. Because SomaScan is widely used in clinical proteomics and biomarker discovery, `sqs` provides a scalable tool that can be applied across multiple disease areas and study designs.

---

# AI Usage Disclosure

Generative AI tools were used to assist in editing and structuring the manuscript text to meet journal formatting and readability requirements. All technical content, software design decisions, and validation of claims were performed by the author. The author reviewed and verified all AI-assisted edits and retains full responsibility for the accuracy of the manuscript.

---

# Availability

The software is available under the MIT License at:
[https://github.com/foocheung/sqs_v2](https://github.com/foocheung/sqs_v2)

---

# Acknowledgements

This work was supported by the Intramural Research Program of the National Institutes of Health. The author thanks collaborators and members of the NIH Center for Human Immunology for feedback during development and testing.

---

# References
