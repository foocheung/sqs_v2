# sqs: A Shiny App for SomaLogic SomaScan Quality Control

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`sqs` is an R package built with the [Golem](https://thinkr-open.github.io/golem/) framework, providing a Shiny-based web application for quality control (QC) analysis of SomaLogic SomaScan assay data. It enables proteomics researchers to upload `.adat` files, compute QC metrics (e.g., per-plate CVs, PCA, Levey-Jennings plots), and generate automated HTML/PDF reports. The package integrates with `SomaDataIO` for data handling.

## Features

- **Data Input**: Upload SomaScan `.adat` files).
- **QC Metrics**: Calculate coefficient of variation (CV) statistics, PCA for sample clustering, and Levey-Jennings plots to assess assay precision using `foodata2` datasets, with support for comparison against historical plate controls and calibrators.
- **Interactive Interface**: A modular Shiny app with tabbed panels for exploring sample summaries, visualizations, and QC results.
- **Automated Reporting**: Generate comprehensive QC reports with tables (via `kableExtra`) and interactive plots (via `plotly`).
- **Performance**: Benchmarked at 113.2 seconds for 15 plates on a Mac M1 (64 GB, macOS 14.7.6).

## Installation

### Prerequisites

- R (>= 4.0.0)
- Dependencies: `shiny`, `SomaDataIO`, `tidyverse`, `plotly`, `rmarkdown`, `kableExtra`, `DT`, `arrow`, `knitr`, `foodata2`

Install `sqs` and its dependencies from GitHub:

```R
# Install devtools if not already installed
install.packages("devtools")

# Install foodata2 dependency
devtools::install_github("foocheung/foodata2")

# Install sqs
devtools::install_github("foocheung/sqs_v2")
```

## Usage

Launch the Shiny app to perform QC analysis:

```R
library(sqs)
run_app()
```

This opens a web interface where you can:
1. Upload an `.adat` file or load example data from `foodata2`.
2. Compute QC metrics (e.g., CVs, PCA, Levey-Jennings plots).
3. Generate and download a QC report.


REMOVE OR ADD See the [vignette](vignettes/sqs.Rmd) for a detailed walkthrough.

## Demo

Try the app online [here](https://webtools.shinyapps.io/sqs_v3_ori/).

## Documentation

REMOVE OR ADD - [Vignette](vignettes/sqs.Rmd): Step-by-step guide to performing QC on a sample SomaScan dataset.
- [Function Reference](https://github.com/foocheung/sqs_v2/): Detailed documentation for `safe_cv`, `plot_levey`, and other functions.
- [JOSS Paper IN PROGRESS](paper.md): Describes the package’s purpose, functionality, and implementation.

## Folder Structure

- `R/`: Core R scripts (e.g., `app_ui.R`, `mod_dataInput.R`, `app_server.R`).
- `inst/extdata/`: Example `.adat` files (if included).
REMOVE OR ADD - `vignettes/`: Vignette for QC workflow.
REMOVE OR ADD  - `tests/`: Unit tests for functions (e.g., `safe_cv`, `plot_levey`).
- `paper.md`: JOSS submission files and images (e.g., `levey_calibrator.png`).

## Issues

Report issues or suggest features on the [GitHub Issues page](https://github.com/your-username/sqs/issues).

## License

`sqs` is licensed under the [MIT License](LICENSE).

## Acknowledgements

- Developed by [Foo Cheung] ([ORCID: add-orcid]).

## Citation

If you use `sqs` in your research, please cite:

```

## Contact

For questions, contact Foo Cheung at foocheung@yahoo.com
