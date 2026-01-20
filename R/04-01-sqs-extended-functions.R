safe_cv <- function(x) {
  m <- base::mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!base::is.finite(m) || m <= 0) return(NA_real_)
  s / m
}

plot_levey <- function(adat_tbl, adat_header, df_cvs_all,
                       sample_type = "QC",
                       sd_levels = c(1, 2),
                       center = c("median", "mean"),
                       y_lab = "Per-plate median CV (%)") {
  
  center <- base::match.arg(center)
  
  # CV per plate across seq.* columns
  df_cvs_per_plate <- adat_tbl |>
    dplyr::filter(.data$SampleType == sample_type) |>
    dplyr::select(.data$PlateId, tidyselect::starts_with("seq.")) |>
    dplyr::group_by(.data$PlateId) |>
    dplyr::summarise(dplyr::across(where(is.numeric), safe_cv), .groups = "drop")
  
  # Plate-level quantiles (%)
  df_cvs_per_plate_quant <- df_cvs_per_plate |>
    tidyr::pivot_longer(-.data$PlateId, names_to = "SeqId", values_to = "CV") |>
    dplyr::group_by(.data$PlateId) |>
    dplyr::summarise(
      `10%` = base::round(stats::quantile(CV, 0.10, na.rm = TRUE) * 100, 1),
      `50%` = base::round(stats::median(CV, na.rm = TRUE) * 100, 1),
      `90%` = base::round(stats::quantile(CV, 0.90, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
  
  exp_date <- base::as.character(adat_header$Header.Meta$HEADER$ExpDate)
  
  # Reference: historical medians, excluding plates present in this sample set
  ref_plot_dat <- df_cvs_all |>
    dplyr::filter(.data$SampleType == sample_type) |>
    dplyr::anti_join(df_cvs_per_plate_quant |>
                       dplyr::select(.data$PlateId),
                     by = "PlateId") |>
    dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
    dplyr::mutate(PlateKey = base::paste0(.data$ExpDate, "-", .data$PlateId),
                  Data = "Reference")
  
  # Fallback: if all plates are from the same day (no reference left), use all
  if (base::nrow(ref_plot_dat) == 0) {
    ref_plot_dat <- df_cvs_all |>
      dplyr::filter(.data$SampleType == sample_type) |>
      dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
      dplyr::mutate(PlateKey = base::paste0(.data$ExpDate, "-", .data$PlateId),
                    Data = "Reference")
    warning("No historical plates left after exclusion; using all as reference.")
  }
  
  # Reference center & SD
  ref_center <- if (center == "median") {
    stats::median(ref_plot_dat$`50%`, na.rm = TRUE)
  } else {
    base::mean(ref_plot_dat$`50%`, na.rm = TRUE)
  }
  ref_sd <- stats::sd(ref_plot_dat$`50%`, na.rm = TRUE)
  
  # Current sample plates
  samp_plot_dat <- df_cvs_per_plate_quant |>
    dplyr::mutate(ExpDate = exp_date) |>
    dplyr::select(.data$ExpDate, .data$PlateId, `50%`) |>
    dplyr::mutate(PlateKey = base::paste0(.data$ExpDate, "-", .data$PlateId),
                  Data = "Sample")
  
  plot_dat <- dplyr::bind_rows(ref_plot_dat, samp_plot_dat) |>
    dplyr::arrange(.data$ExpDate, .data$PlateId) |>
    dplyr::mutate(
      Data = base::factor(.data$Data, levels = c("Reference", "Sample")),
      PlateKey = forcats::fct_inorder(.data$PlateKey)
    )
  
  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(x = .data$PlateKey, y = `50%`, group = 1, color = .data$Data)) +
    ggplot2::geom_hline(yintercept = ref_center, linewidth = 0.5)
  
  # Add ±k SD bands
  for (k in sd_levels) {
    p <- p +
      ggplot2::geom_hline(yintercept = ref_center + k * ref_sd,
                          linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_hline(yintercept = ref_center - k * ref_sd,
                          linetype = "dashed", linewidth = 0.3)
  }
  
  p +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(breaks = c("Reference", "Sample"),
                                values = c("#56B4E9", "#E69F00")) +
    ggplot2::labs(y = y_lab, x = "Date - Plate ID") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
}

ks_test <- function(df_cvs_samp, df_cvs_all, sample_type = "Calibrator") {
  df_cvs_per_plate <- df_cvs_samp
  
  # <- adat_tbl %>% dplyr::filter(SampleType == sample_type) %>%
  #   dplyr::select(PlateId, starts_with("seq.")) %>%
  #   dplyr::group_by(PlateId) %>%
  #   dplyr::summarise_if(is.numeric, function(x) sd(x)/mean(x)) %>%
  #   ungroup() %>%
  #   tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
  #   group_by(PlateId) %>%
  #   summarise("10%" = round(quantile(CV, 0.1) * 100, 3),
  #             "50%" = round(median(CV) * 100, 3),
  #             "90%" = round(quantile(CV, 0.9) * 100, 3)) %>%
  #   ungroup()
  
  ref_pop_ks <- df_cvs_all %>%
    dplyr::filter(SampleType == sample_type) %>%
    dplyr::filter(!PlateId %in% df_cvs_per_plate$`Cal Precision(%)`) %>%
    dplyr::select(`10%`, `50%`, `90%`) %>%
    as.vector() %>%
    unlist()
  
  df_ks_out <- tibble::tibble()
  for(i in 1:nrow(df_cvs_per_plate)){
    samp_ks <- df_cvs_per_plate[i,] %>% dplyr::select(-`Cal Precision(%)`) %>%
      as.vector() %>%
      unlist()
    
    plate_id <- df_cvs_per_plate[[i,1]]
    
    ks_res <- ks.test(samp_ks, ref_pop_ks) %>%
      broom::tidy()
    
    df_ks_out <- dplyr::bind_rows(df_ks_out, tibble::tibble("PlateId" = plate_id,
                                                            ks_res))
  }
  
  df_ks_out <- df_ks_out %>%
    dplyr::rename("Statistics" = statistic) %>%
    dplyr::rename("P-value" = p.value) %>%
    dplyr::select(PlateId, Statistics, `P-value`)
  
  return(df_ks_out)
}

