plot_levey <- function(adat_tbl, adat_header, df_cvs_all, sample_type = "QC"){

  df_cvs_per_plate <- adat_tbl %>% dplyr::filter(SampleType == sample_type) %>%
    dplyr::select(PlateId, starts_with("seq.")) %>%
    dplyr::group_by(PlateId) %>%
    dplyr::summarise_if(is.numeric, function(x) sd(x)/mean(x)) %>%
    dplyr::ungroup()

  df_cvs_per_plate_quant <- df_cvs_per_plate%>%
    tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
    dplyr::group_by(PlateId) %>%
    dplyr::summarise("10%" = round(quantile(CV, 0.1) * 100, 1),
              "50%" = round(median(CV) * 100, 1),
              "90%" = round(quantile(CV, 0.9) * 100, 1)) %>%
    dplyr::ungroup()

  exp_date <- as.character(adat_header$Header.Meta$HEADER$ExpDate)

  ref_plot_dat <- df_cvs_all %>%
    dplyr::filter(SampleType == sample_type) %>%
    dplyr::filter(!PlateId %in% df_cvs_per_plate_quant$PlateId) %>%
    dplyr::select(ExpDate, PlateId, `50%`) %>%
    dplyr::mutate(PlateId = paste0(ExpDate, "-", PlateId)) %>%
    dplyr::mutate("Data" = "Reference")

  ref_median <- median(ref_plot_dat$`50%`)
  ref_sd <- sd(ref_plot_dat$`50%`)

  samp_plot_dat <- df_cvs_per_plate_quant %>%
    dplyr::mutate("ExpDate" = exp_date) %>%
    dplyr::select(ExpDate, PlateId, `50%`) %>%
    dplyr::mutate(PlateId = paste0(ExpDate, "-", PlateId)) %>%
    dplyr::mutate("Data" = "Sample")

  plot_dat <- dplyr::bind_rows(ref_plot_dat, samp_plot_dat)

  levey_plt <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = PlateId, y = `50%`, group = 1, color = Data)) +
    ggplot2::geom_hline(yintercept = ref_median) +
    ggplot2::geom_hline(yintercept = ref_median + ref_sd, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = ref_median - ref_sd, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = ref_median + ref_sd * 2, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = ref_median - ref_sd * 2, linetype = "dotted") +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(breaks = c("Reference", "Sample"),
                       values=c("#56B4E9", "#E69F00")) +
    ggplot2::labs(y = "Per Plate Median CV%", x = "Date - Plate ID") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  return(levey_plt)
}

ks_test <- function(df_cvs_samp, df_cvs_all, sample_type = "Calibrator"){
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
