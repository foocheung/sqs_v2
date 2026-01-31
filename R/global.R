# ==============================================================================
# GLOBAL.R - Plasma Proteomics Quality Control Application
# ==============================================================================
# This file loads all required packages, sets global options, and defines
# utility functions for standardized QC assessment of proteomics assay data.
# ==============================================================================

# ---- Package Loading ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
 # library(shinycssloaders)
  library(tictoc)
  library(tidyverse)
  library(SomaDataIO)
  library(kableExtra)
  library(ggfortify)
  library(DT)
  library(readxl)
  library(plotly)
  library(arrow)
  library(rmarkdown)
  library(knitr)
  library(tinytex)
  library(forcats)
  library(broom)
})

# ---- Global Options ----
options(
  shiny.maxRequestSize = 500 * 1024^2,  # 500 MB upload limit
  shiny.sanitize.errors = FALSE,        # Show full error messages in development
  scipen = 999                          # Avoid scientific notation
)

# ---- Utility Functions ----

#' Calculate Coefficient of Variation (CV) safely
#' @param x Numeric vector of abundance values
#' @return CV as a proportion (not percentage)
safe_cv <- function(x) {
  m <- base::mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!base::is.finite(m) || m <= 0) return(NA_real_)
  s / m
}

#' Pretty format for bytes
#' @param bytes Numeric value of bytes
#' @return Character string with formatted size
pretty_bytes <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  if (is.na(bytes) || bytes < 1) return("0 B")
  pow <- min(floor(log(bytes, 1024)), length(units) - 1)
  sprintf("%.1f %s", bytes / (1024^pow), units[pow + 1])
}

#' Pretty format for duration
#' @param sec Numeric value in seconds
#' @return Character string with formatted duration
pretty_duration <- function(sec) {
  sec <- as.numeric(sec)
  if (is.na(sec)) return("0s")
  if (sec < 60) return(sprintf("%.1fs", sec))
  mins <- floor(sec / 60)
  secs <- round(sec - mins * 60, 1)
  if (mins < 60) return(sprintf("%dm %.1fs", mins, secs))
  hours <- floor(mins / 60)
  mins <- mins - hours * 60
  sprintf("%dh %dm", hours, mins)
}

# ---- Statistical Functions ----

#' Reference Material Quality Trend Plot
#'
#' Creates a Levey-Jennings style plot for monitoring coefficient of variation
#' trends across sequential assay runs, with customizable QC zone visualization.
#'
#' @param adat_tbl Data table with assay data
#' @param adat_header Header information from data file
#' @param df_cvs_all Historical CV data for establishing reference distribution
#' @param sample_type Sample type to plot (default: "QC")
#' @param sd_levels SD levels for control limits (default: c(1, 2, 3))
#' @param center Centering method: "median" or "mean" (default: "median")
#' @param y_lab Y-axis label
#' @param show_zones Show QC zones with color coding (default: TRUE)
#' @param point_size Size of points (default: 2.5)
#' @return ggplot2 object

#' Reference Material Quality Trend Plot
#'
#' Creates a Levey-Jennings style plot for monitoring coefficient of variation
#' trends across sequential assay runs, with customizable QC zone visualization.
#'
#' @param adat_tbl Data table with assay data
#' @param adat_header Header information from data file
#' @param df_cvs_all Historical CV data for establishing reference distribution
#' @param sample_type Sample type to plot (default: "QC")
#' @param sd_levels SD levels for control limits (default: c(1, 2, 3))
#' @param center Centering method: "median" or "mean" (default: "median")
#' @param y_lab Y-axis label
#' @param show_zones Show QC zones with color coding (default: TRUE)
#' @param point_size Size of points (default: 2.5)
#' @return ggplot2 object
plot_levey <- function(adat_tbl,
                       adat_header,
                       df_cvs_all,
                       sample_type = "QC",
                       sd_levels = c(1, 2, 3),
                       center = c("median", "mean"),
                       y_lab = "Per-plate median CV (%)",
                       show_zones = TRUE,
                       point_size = 2.5) {

  center <- base::match.arg(center)

  # Calculate CV per plate across all protein targets
  df_cvs_per_plate <- adat_tbl |>
    dplyr::filter(.data$SampleType == sample_type) |>
    dplyr::select(.data$PlateId, tidyselect::starts_with("seq.")) |>
    dplyr::group_by(.data$PlateId) |>
    dplyr::summarise(dplyr::across(where(is.numeric), safe_cv), .groups = "drop")

  # Calculate plate-level CV quantiles (%)
  df_cvs_per_plate_quant <- df_cvs_per_plate |>
    tidyr::pivot_longer(-.data$PlateId, names_to = "SeqId", values_to = "CV") |>
    dplyr::group_by(.data$PlateId) |>
    dplyr::summarise(
      `10%` = base::round(stats::quantile(CV, 0.10, na.rm = TRUE) * 100, 1),
      `50%` = base::round(stats::median(CV, na.rm = TRUE) * 100, 1),
      `90%` = base::round(stats::quantile(CV, 0.90, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )

  # Get ExpDate and ensure it's Date class
  exp_date_raw <- adat_header$Header.Meta$HEADER$ExpDate
  exp_date <- as.Date(exp_date_raw)

  # Build reference data (historical runs excluding current batch)
  ref_plot_dat <- df_cvs_all |>
    dplyr::filter(.data$SampleType == sample_type) |>
    dplyr::anti_join(
      df_cvs_per_plate_quant |> dplyr::select(.data$PlateId),
      by = "PlateId"
    ) |>
    dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
    dplyr::mutate(
      ExpDate = as.Date(.data$ExpDate),  # EXPLICIT: ensure Date class
      PlateKey = base::paste0(.data$ExpDate, "-", .data$PlateId),
      Data = "Reference"
    )

  # Fallback: if no reference data, use all historical data
  if (base::nrow(ref_plot_dat) == 0) {
    ref_plot_dat <- df_cvs_all |>
      dplyr::filter(.data$SampleType == sample_type) |>
      dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
      dplyr::mutate(
        ExpDate = as.Date(.data$ExpDate),  # EXPLICIT: ensure Date class
        PlateKey = base::paste0(.data$ExpDate, "-", .data$PlateId),
        Data = "Reference"
      )
    warning("Insufficient reference data; using all historical data for comparison.")
  }

  # Calculate reference statistics (center and spread)
  ref_center <- if (center == "median") {
    stats::median(ref_plot_dat$`50%`, na.rm = TRUE)
  } else {
    base::mean(ref_plot_dat$`50%`, na.rm = TRUE)
  }
  ref_sd <- stats::sd(ref_plot_dat$`50%`, na.rm = TRUE)

  # Build current study data
  samp_plot_dat <- df_cvs_per_plate_quant |>
    dplyr::mutate(ExpDate = exp_date) |>  # Use the Date-converted value
    dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
    dplyr::mutate(
      ExpDate = as.Date(.data$ExpDate),  # EXPLICIT: ensure Date class
      PlateKey = base::paste0(.data$ExpDate, "-", .data$PlateId),
      Data = "Sample"
    )

  # Combine and prepare plot data
  # NOTE: Both ExpDate columns are now explicitly Date class, so bind_rows will work
  plot_dat <- dplyr::bind_rows(ref_plot_dat, samp_plot_dat) |>
    dplyr::arrange(.data$ExpDate, .data$PlateId) |>
    dplyr::mutate(
      Data = base::factor(.data$Data, levels = c("Reference", "Sample")),
      PlateKey = forcats::fct_inorder(.data$PlateKey),
      # Classify into QC zones based on SD from reference center
      QC_Zone = dplyr::case_when(
        abs(`50%` - ref_center) <= ref_sd ~ "Zone 1 (±1 SD)",
        abs(`50%` - ref_center) <= 2 * ref_sd ~ "Zone 2 (±2 SD)",
        abs(`50%` - ref_center) <= 3 * ref_sd ~ "Zone 3 (±3 SD)",
        TRUE ~ "Out of Control (>±3 SD)"
      ),
      QC_Zone = factor(QC_Zone, levels = c(
        "Zone 1 (±1 SD)",
        "Zone 2 (±2 SD)",
        "Zone 3 (±3 SD)",
        "Out of Control (>±3 SD)"
      )),
      # Shorten long plate identifiers for display
      PlateKeyShort = dplyr::if_else(
        nchar(as.character(.data$PlateKey)) > 30,
        paste0(substr(as.character(.data$PlateKey), 1, 14), "...",
               substr(as.character(.data$PlateKey),
                      nchar(as.character(.data$PlateKey)) - 13,
                      nchar(as.character(.data$PlateKey)))),
        as.character(.data$PlateKey)
      )
    )

  # Initialize plot
  p <- ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(
      x = .data$PlateKeyShort,
      y = `50%`,
      group = 1
    )
  )

  # Add shaded QC zone backgrounds if requested
  if (show_zones) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = ref_center - 3 * ref_sd,
        ymax = ref_center + 3 * ref_sd,
        fill = "lightgreen", alpha = 0.1
      ) +
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = ref_center - 2 * ref_sd,
        ymax = ref_center + 2 * ref_sd,
        fill = "yellow", alpha = 0.1
      ) +
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = ref_center - ref_sd,
        ymax = ref_center + ref_sd,
        fill = "lightblue", alpha = 0.1
      )
  }

  # Add center line
  p <- p + ggplot2::geom_hline(
    yintercept = ref_center,
    linewidth = 0.7,
    color = "darkblue",
    linetype = "solid"
  )

  # Add control limit lines (±1, ±2, ±3 SD)
  line_types <- c("dashed", "dotted", "dotdash")
  for (i in seq_along(sd_levels)) {
    k <- sd_levels[i]
    ltype <- if (i <= length(line_types)) line_types[i] else "dashed"

    p <- p +
      ggplot2::geom_hline(
        yintercept = ref_center + k * ref_sd,
        linetype = ltype,
        linewidth = 0.4,
        color = "red"
      ) +
      ggplot2::geom_hline(
        yintercept = ref_center - k * ref_sd,
        linetype = ltype,
        linewidth = 0.4,
        color = "red"
      )
  }

  # Add data points and trend lines
  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(color = .data$Data),
      linewidth = 0.5,
      alpha = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$Data, shape = .data$QC_Zone),
      size = point_size
    ) +
    ggplot2::scale_color_manual(
      name = "Data Type",
      breaks = c("Reference", "Sample"),
      values = c("Reference" = "#56B4E9", "Sample" = "#E69F00")
    ) +
    ggplot2::scale_shape_manual(
      name = "QC Zone",
      values = c(16, 17, 15, 4),  # circle, triangle, square, X
      guide = if (show_zones) ggplot2::guide_legend() else "none"
    )

  # Add labels and theme
  p <- p +
    ggplot2::labs(
      y = y_lab,
      x = "Assay Run (Date - Plate ID)",
      title = paste0(sample_type, " Reference Material Quality Trend"),
      subtitle = paste0(
        "Reference ", center, ": ", round(ref_center, 2), "%; ",
        "SD: ", round(ref_sd, 2), "% | ",
        "Zones: ±1, ±2, ±3 SD"
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 9
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.2, color = "grey90"),
      legend.position = "bottom",
      legend.box = "vertical",
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 10, color = "grey40")
    )

  return(p)
}


#' Kolmogorov-Smirnov Test for Distribution Comparison
#'
#' Performs KS test to compare current sample CV distribution against
#' historical reference distribution.
#'
#' @param df_cvs_samp Current sample CV data
#' @param df_cvs_all Historical CV reference data
#' @param sample_type Sample type to test (default: "Calibrator")
#' @return Data frame with KS test results per plate
ks_test <- function(df_cvs_samp,
                    df_cvs_all,
                    sample_type = "Calibrator") {

  df_cvs_per_plate <- df_cvs_samp

  # Build reference population (exclude current plates)
  ref_pop_ks <- df_cvs_all %>%
    dplyr::filter(SampleType == sample_type) %>%
    dplyr::filter(!PlateId %in% df_cvs_per_plate$Plate) %>%
    dplyr::select(`10%`, `50%`, `90%`) %>%
    as.vector() %>%
    unlist()

  # Check if reference data are available
  if (length(ref_pop_ks) == 0) {
    warning("No reference population available for statistical comparison")
    return(tibble::tibble(
      PlateId = character(),
      Statistic = numeric(),
      `P-value` = numeric()
    ))
  }

  # Perform KS test for each plate
  df_ks_out <- tibble::tibble()
  for (i in seq_len(nrow(df_cvs_per_plate))) {
    samp_ks <- df_cvs_per_plate[i, ] %>%
      dplyr::select(-Plate) %>%
      as.vector() %>%
      unlist()

    plate_id <- df_cvs_per_plate[[i, 1]]

    tryCatch({
      ks_res <- suppressWarnings(stats::ks.test(samp_ks, ref_pop_ks)) %>%
        broom::tidy()

      df_ks_out <- dplyr::bind_rows(
        df_ks_out,
        tibble::tibble(PlateId = plate_id, ks_res)
      )
    }, error = function(e) {
      warning(paste("Statistical test failed for plate", plate_id, ":", e$message))
      df_ks_out <<- dplyr::bind_rows(
        df_ks_out,
        tibble::tibble(
          PlateId = plate_id,
          statistic = NA_real_,
          p.value = NA_real_,
          method = "Test failed",
          alternative = NA_character_
        )
      )
    })
  }

  # Clean up column names
  df_ks_out <- df_ks_out %>%
    dplyr::rename(
      Statistic = statistic,
      `P-value` = p.value
    ) %>%
    dplyr::select(PlateId, Statistic, `P-value`)

  return(df_ks_out)
}

# ---- Session Info ----
message("Global.R loaded successfully")
message("Shiny max upload size: ", round(getOption("shiny.maxRequestSize") / 1024^2, 0), " MB")
