# ==============================================================================
# mod_table.R - UPDATED TO USE LOCAL DATA FILES
# ==============================================================================
# This is a large file. KEY CHANGES marked with UPDATED comments.
# Replace these 4 lines in the original file:
# 1. Line ~95: foodata2::load_data2() -> load_filtered_somamers()
# 2. Line ~147: foodata2::load_data4() -> load_historical_cv_data()
# 3. Line ~222: foodata2::load_data4() -> load_historical_cv_data()
# 4. Line ~321: foodata2::load_data4() -> load_historical_cv_data()
# ==============================================================================

library(ggplot2)

`%>%` <- dplyr::`%>%`

addResourcePath("d", "extdata/")

mod_table_ui <- function(id, name){
  ns <- NS(id)

  tabsetPanel(
    tabPanel("Introduction",
             h4(strong("Quality Control Analysis Overview")),
             HTML("<h2>Plasma Proteomics Assay Quality Control</h2>
  <p>This application implements standardized quality control procedures for
  plasma proteomics assay data. The analysis encompasses multiple dimensions of
  data quality, from sample composition through calibration stability.</p>

  <h3>Assessment Components</h3>
  <ul>
    <li><strong>Sample Composition:</strong> Summary table of study samples,
    quality control replicates, calibration references, and other sample types
    included in the assay run.</li>

    <li><strong>Sample Type Separation:</strong> Principal component analysis to
    visualize sample clustering by type and assess separation between groups.
    Useful for identifying potential technical artifacts or batch effects.</li>

    <li><strong>Data Standardization:</strong> Overview of normalization and
    calibration procedures, including internal control correction, reference
    material normalization, and plate-level scaling factors.</li>

    <li><strong>Sample-Level Quality Metrics:</strong> Per-sample assessment
    including normalization scale factors, fraction of protein targets used in
    normalization calculations, and sample quality flags based on acceptance
    criteria.</li>

    <li><strong>Reproducibility Analysis:</strong> Coefficient of variation
    (CV) analysis for reference materials and quality control samples,
    including plate-level trends and comparison to historical distributions.</li>

    <li><strong>Calibration Assessment:</strong> Plate-scale factors, reference
    material performance metrics, and protein target-specific accuracy assessment
    relative to expected values.</li>
  </ul>

  <p><strong>Interpretation:</strong> Results from these quality control analyses
  inform downstream data processing decisions, including sample retention/exclusion,
  applicability of specific normalization methods, and confidence in protein
  abundance estimates.</p>"
             )),

    tabPanel("Experimental Design", DT::dataTableOutput(ns("tbl_sample_summary")),
             HTML("<h2>Experimental Design Summary</h2>
  <p>This table summarizes the sample composition across all assay runs.</p>"
             )),

    tabPanel("Sample Type Separation",
             selectInput(inputId=ns("pca_color"),
                         label="Color by:",
                         c("SampleType","PlateId","AssayNotes","SampleNotes",
                           "TimePoint", "SampleGroup"),
                         "SampleType"),
             plotly::plotlyOutput(ns("pca_sample_type")),
             HTML("<h2>Principal Component Analysis of Sample Separation</h2>
  <p>PCA visualization of sample relationships and clustering patterns.</p>"
             )),

    tabPanel("Data Standardization","",
             HTML("<h2>Data Standardization Procedures</h2>
  <p>Overview of standardization and normalization procedures.</p>"
             ),
             h4("Assessment Metrics"),
             p("The following sections provide detailed quality metrics for each standardization step.")
    ),

    tabPanel("Sample Quality",
             tabsetPanel(
               tabPanel("Pass/Fail Summary",
                        DT::dataTableOutput(ns("tbl_flag_pass_samples")),
                        HTML("<h2>Sample Quality Summary</h2>")),

               tabPanel("Sample Separation (PCA)",
                        plotly::plotlyOutput(ns("pca_sample_rowcheck")),
                        HTML("<h2>Sample Quality Assessment via PCA</h2>")),

               tabPanel("Flagged Samples per Plate",
                        DT::DTOutput(ns("tbl_flagged_samples")),
                        HTML("<h2>Quality Flags by Plate</h2>")),

               tabPanel("Normalization Scale Factors",
                        DT::dataTableOutput(ns("tbl_med_norm")),
                        HTML("<h2>Normalization Scale Factors Summary</h2>")),

               tabPanel("ANML Fraction Used",
                        DT::dataTableOutput(ns("tbl_anml_fraction")),
                        HTML("<h2>Adaptive Normalization - Fraction Used</h2>"))
             )
    ),

    tabPanel("Calibration & Reproducibility",
             tabsetPanel(
               tabPanel("Plate Scale",
                        DT::dataTableOutput(ns("tbl_plate_scale")),
                        HTML("<h2>Plate-Level Scaling Factors</h2>")),

               tabPanel("Calibrator Signal in Tails",
                        DT::dataTableOutput(ns("tbl_cal_perc")),
                        HTML("<h2>Calibrator Percent in Tails</h2>")),

               tabPanel("Protein Targets in Tails",
                        DT::dataTableOutput(ns("tbl_somamers_tails")),
                        HTML("<h2>Calibration Accuracy Assessment</h2>")),

               tabPanel("Reference Material Percent in Tails",
                        DT::dataTableOutput(ns("tbl_qc_perc_tails")),
                        HTML("<h2>Reference Material Accuracy Assessment</h2>")),

               tabPanel("Coefficient of Variation - Calibrator",
                        HTML("<h2>Calibrator Reference Material Precision</h2>"),
                        DT::dataTableOutput(ns("tbl_cal_cv")),
                        HTML("<h2>Calibrator Quality Trend Chart</h2>"),
                        plotly::plotlyOutput(ns("levey_calibrator")),
                        HTML("<h2>Statistical Comparison to Historical Data</h2>"),
                        DT::dataTableOutput(ns("tbl_ks_cal"))),

               tabPanel("Coefficient of Variation - Reference Material",
                        HTML("<h2>Reference Material Precision Metrics</h2>"),
                        DT::dataTableOutput(ns("tbl_qc_cv")),
                        HTML("<h2>Reference Material Quality Trend Chart</h2>"),
                        plotly::plotlyOutput(ns("levey_somalogic_qc")),
                        HTML("<h2>Statistical Comparison to Historical Reference</h2>"),
                        DT::dataTableOutput(ns("tbl_ks_qc")))
             )
    )
  )
}

# ==============================================================================
# Module Server
# ==============================================================================

mod_table_server <- function(input, output, session, file, cv_data_reactive = NULL){
  ns <- session$ns

  # If cv_data_reactive is not provided, create a default one
  if (is.null(cv_data_reactive)) {
    cv_data_reactive <- reactive({
      load_historical_cv_data()
    })
  }

  output$tbl_sample_summary <- DT::renderDataTable({
    req(file$go())
    withProgress(message = 'Generating sample summary table...', {
      samp_summary <- as.data.frame.matrix(table(file$df()$PlateId, file$df()$SampleType)) %>%
        tibble::rownames_to_column("PlateId")
      numeric_columns <- samp_summary %>%
        dplyr::select_if(is.numeric)
      column_sums <- colSums(numeric_columns, na.rm = TRUE)
      samp_summary <- dplyr::bind_rows(samp_summary, data.frame("PlateId" = "Total", t(column_sums)))
      samp_summary
    })
  })

  output$pca_sample_type <- plotly::renderPlotly({
    withProgress(message = 'Generating PCA plot...', {
      pca_dat <- file$df() %>% dplyr::select(starts_with("seq."))
      pca_res <- prcomp(pca_dat, scale=TRUE)
      pca_scores <- as.data.frame(pca_res$x)

      plot_dat <- cbind(file$df()[, c("SampleType", "PlateId", "SampleId","AssayNotes","SampleNotes", "TimePoint", "SampleGroup")], pca_scores) %>%
        dplyr::mutate(HoverText = paste0("PlateId: ", PlateId, "<br>SampleId: ", SampleId))
      variance_explained_pc1 <- round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 2)
      variance_explained_pc2 <- round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 2)

      plot_pca <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = PC1, y = PC2, color = get(input$pca_color), text = HoverText)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = paste0("PC1 (", variance_explained_pc1, "%)"),
                      y = paste0("PC2 (", variance_explained_pc2, "%)"),
                      color = "Group")
      plotly::ggplotly(plot_pca)
    })
  })

  output$tbl_flag_pass_samples <- DT::renderDataTable({
    withProgress(message = 'Generating sample quality summary...', {
      rowcheck_dat <- file$df() %>% dplyr::select(PlateId,SampleType, RowCheck)
      pass_flag <- as.data.frame.matrix(table(rowcheck_dat$RowCheck, rowcheck_dat$SampleType))
      pass_flag
    })
  })

  output$pca_sample_rowcheck <- plotly::renderPlotly({
    withProgress(message = 'Generating PCA plot with quality flags...', {
      # *** UPDATED: Use load_filtered_somamers() instead of foodata2::load_data2() ***
      avoid_SOMAmers <- load_filtered_somamers()

      avoid_prot <- avoid_SOMAmers %>%
        dplyr::pull(SeqId) %>%
        paste0("seq.",.) %>%
        stringr::str_replace_all(.,"-",".")

      adat_samp_tbl <- file$df() %>%
        dplyr::filter(SampleType == "Sample") %>%
        dplyr::select(PlateId, SampleId, RowCheck, starts_with("seq.")) %>%
        dplyr::select(!all_of(avoid_prot))

      pca_dat <- file$df() %>% dplyr::filter(SampleType == "Sample") %>% dplyr::select(starts_with("seq."))
      pca_res <- prcomp(pca_dat, scale=TRUE)
      pca_scores <- as.data.frame(pca_res$x)

      plot_samp_dat <- cbind(adat_samp_tbl[, c("PlateId", "SampleId", "RowCheck")], pca_scores) %>%
        dplyr::mutate(HoverText = paste0("PlateId: ", PlateId, "<br>SampleId: ", SampleId))

      variance_explained_pc1 <- round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 2)
      variance_explained_pc2 <- round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 2)

      plot_samp_pca_flag <- ggplot2::ggplot(plot_samp_dat, ggplot2::aes(x = PC1, y = PC2, color = RowCheck, text = HoverText)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = paste0("PC1 (", variance_explained_pc1, "%)"),
                      y = paste0("PC2 (", variance_explained_pc2, "%)"),
                      color = "QC Status")

      plotly::ggplotly(plot_samp_pca_flag)
    })
  })

  output$tbl_flagged_samples <- DT::renderDataTable({
    withProgress(message = 'Generating flagged samples table...', {
      total_flagged_samples <-file$df() %>% dplyr::filter(RowCheck == "FLAG") %>%
        dplyr::select(PlateId, SampleId, SampleType)
      total_flagged_samples
    })
  })

  output$tbl_med_norm <- DT::renderDataTable({
    withProgress(message = 'Generating normalization scale factors table...', {
      rowcheck_dat <- file$df() %>%
        dplyr::select(PlateId,SampleType, RowCheck)
      pass_flag <- as.data.frame.matrix(table(rowcheck_dat$RowCheck, rowcheck_dat$SampleType))

      df_norm_scale <- file$df() %>% dplyr::select(PlateId, SampleId, SampleType, NormScale_0_005, NormScale_0_5, NormScale_20) %>%
        dplyr::filter(SampleType == "Sample") %>%
        dplyr::mutate(across(starts_with("NormScale"), ~ifelse( . < 0.4 | . > 2.5, "Flag", "Pass")))
      df_norm_scale %>%
        dplyr::select(NormScale_0_005, NormScale_0_5, NormScale_20) %>%
        tidyr::gather(key = "Dilution Group", value = "Decision") %>%
        dplyr::filter(Decision == "Pass") %>%
        dplyr::group_by(`Dilution Group`) %>%
        dplyr::summarise(Pass = dplyr::n()) %>%
        dplyr::mutate(Flag = sum(pass_flag$Sample) - Pass,
                      Total = sum(pass_flag$Sample))
    })
  })

  output$tbl_anml_fraction <- DT::renderDataTable({
    withProgress(message = 'Generating ANML fraction table...', {
      rowcheck_dat <- file$df() %>%
        dplyr::select(PlateId,SampleType, RowCheck)

      pass_flag <- as.data.frame.matrix(table(rowcheck_dat$RowCheck, rowcheck_dat$SampleType))

      df_anml_fraction <- file$df() %>% dplyr::select(PlateId, SampleId, SampleType, ANMLFractionUsed_0_005, ANMLFractionUsed_0_5, ANMLFractionUsed_20) %>%
        dplyr::filter(SampleType == "Sample") %>%
        dplyr::mutate(across(starts_with("ANMLFractionUsed"), ~ifelse( . < 0.3, "Flag", "Pass")))

      df_anml_fraction %>% dplyr::select(ANMLFractionUsed_0_005, ANMLFractionUsed_0_5, ANMLFractionUsed_20) %>%
        tidyr::gather(key ="Dilution Group", value = "Decision") %>%
        dplyr::filter(Decision == "Pass") %>%
        dplyr::group_by(`Dilution Group`) %>%
        dplyr::summarise(Pass = dplyr::n()) %>%
        dplyr::mutate("Flag" = sum(pass_flag$Sample) - Pass) %>%
        dplyr::mutate("Total" = sum(pass_flag$Sample))
    })
  })

  output$tbl_plate_scale <- DT::renderDataTable({
    withProgress(message = 'Generating plate scale table...', {
      adat_header<-file$df2()
      keys <- names(adat_header$Header.Meta$HEADER)
      indices <- grep("^PlateScale_Scalar", keys)
      keys_with_plate_scale_scalar <- keys[indices]
      indices <- grep("^PlateScale_PassFlag", keys)
      keys_with_plate_scale_pass <- keys[indices]

      df_plate_scale_value <- data.frame("Value" = unlist(adat_header$Header.Meta$HEADER[keys_with_plate_scale_scalar])) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^PlateScale_Scalar_", "", Plate))

      df_plate_scale_pass <- data.frame("Plate Check" = unlist(adat_header$Header.Meta$HEADER[keys_with_plate_scale_pass])) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^PlateScale_PassFlag_", "", Plate))

      df_plate_scale <- tibble::tibble("Acceptance Criteria" = "0.4 - 2.5", dplyr::inner_join(df_plate_scale_pass, df_plate_scale_value, by = "Plate")) %>%
        dplyr::select(Plate, `Acceptance Criteria`, Plate.Check, Value) %>%
        dplyr::mutate(Value = round(as.numeric(Value),2))
      colnames(df_plate_scale) <- c("Plate", "Acceptance Criteria", "Plate Check", "Value")
      df_plate_scale
    })
  })

  output$tbl_cal_perc <- DT::renderDataTable({
    withProgress(message = 'Generating calibrator accuracy table...', {
      adat_header<-file$df2()
      keys <- names(adat_header$Header.Meta$HEADER)
      indices <- grep("^CalPlateTailPercent", keys)
      keys_cal_perc_tails <- keys[indices]
      indices <- grep("^CalPlateTailTest", keys)
      keys_cal_perc_tails_test <- keys[indices]

      df_cal_perc_tails_value <- data.frame("Value" = unlist(adat_header$Header.Meta$HEADER[keys_cal_perc_tails])) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^CalPlateTailPercent_", "", Plate))

      df_cal_perc_tails_test <- data.frame("Plate Check" = unlist(adat_header$Header.Meta$HEADER[keys_cal_perc_tails_test])) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^CalPlateTailTest_", "", Plate))

      df_cal_perc_tails <- tibble::tibble("Acceptance Criteria" = "Less than 10%", dplyr::inner_join(df_cal_perc_tails_test, df_cal_perc_tails_value, by = "Plate")) %>%
        dplyr::select(Plate, `Acceptance Criteria`, Plate.Check, Value) %>%
        dplyr::mutate(Value = round(as.numeric(Value),2))
      colnames(df_cal_perc_tails) <- c("Plate", "Acceptance Criteria", "Plate Check", "Value")
      df_cal_perc_tails
    })
  })

  output$tbl_somamers_tails <- DT::renderDataTable({
    withProgress(message = 'Generating protein target accuracy table...', {
      adat_header<-file$df2()
      keys <- names(adat_header$Header.Meta$HEADER)
      indices <- grep("^CalPlateTailPercent", keys)
      keys_cal_perc_tails <- keys[indices]
      indices <- grep("^CalPlateTailTest", keys)
      keys_cal_perc_tails_test <- keys[indices]

      df_cal_perc_tails_value <- data.frame("Value" = unlist(adat_header$Header.Meta$HEADER[keys_cal_perc_tails])) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^CalPlateTailPercent_", "", Plate))

      df_cal_perc_tails_test <- data.frame("Plate Check" = unlist(adat_header$Header.Meta$HEADER[keys_cal_perc_tails_test])) %>%
        tibble::rownames_to_column(var = "Plate") %>%
        dplyr::mutate(Plate = sub("^CalPlateTailTest_", "", Plate))

      df_cal_perc_tails <- tibble::tibble("Acceptance Criteria" = "Less than 10%", dplyr::inner_join(df_cal_perc_tails_test, df_cal_perc_tails_value, by = "Plate")) %>%
        dplyr::select(Plate, `Acceptance Criteria`, Plate.Check, Value) %>%
        dplyr::mutate(Value = round(as.numeric(Value),2))
      colnames(df_cal_perc_tails) <- c("Plate", "Acceptance Criteria", "Plate Check", "Value")
      df_cal_perc_tails
    })
  })

  output$tbl_qc_perc_tails <- DT::renderDataTable({
    withProgress(message = 'Generating reference material accuracy table...', {
      adat_header<-file$df2()
      df_SOMAmers_tails <- data.frame("SeqId" = adat_header$Col.Meta$SeqId,
                                      "EntrezGeneSymbol" = adat_header$Col.Meta$EntrezGeneSymbol,
                                      "Organism" = adat_header$Col.Meta$Organism,
                                      "ColCheck" = adat_header$Col.Meta$ColCheck)

      tibble::tibble("Reference Material" = "QC Accuracy", "Acceptance Criteria" = "0.8 - 1.2") %>%
        dplyr::bind_cols(data.frame(table(df_SOMAmers_tails$ColCheck)) %>%
                           tidyr::spread(key = Var1, value = Freq)) %>%
        dplyr::bind_cols(tibble::tibble("Total" =  .$FLAG + .$PASS))
    })
  })

  output$tbl_cal_cv <- DT::renderDataTable({
    withProgress(message = 'Calculating calibrator precision...', {
      # *** UPDATED: Use load_historical_cv_data() instead of foodata2::load_data4() ***
      df_cvs_all <- load_historical_cv_data()

      df_cvs <-  file$df() %>% dplyr::filter(SampleType == "Calibrator") %>%
        dplyr::select(PlateId, starts_with("seq.")) %>%
        dplyr::group_by(PlateId) %>%
        dplyr::summarise_if(is.numeric, function(x) sd(x)/mean(x)) %>%
        dplyr::ungroup() %>%
        tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
        dplyr::group_by(PlateId) %>%
        dplyr::summarise("10%" = round(quantile(CV, 0.1) * 100, 1),
                         "50%" = round(median(CV) * 100, 1),
                         "90%" = round(quantile(CV, 0.9) * 100, 1))

      colnames(df_cvs) <- c("Plate", "10%", "50%", "90%")
      df_cvs
    })
  })

  output$levey_calibrator <- plotly::renderPlotly({
    withProgress(message = 'Generating calibrator quality trend chart...', {
      # *** UPDATED: Use reactive cv_data passed from app_server ***
      df_cvs_all <- cv_data_reactive()
      adat_header <- file$df2()

      if (is.null(df_cvs_all)) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Historical data not available") +
            ggplot2::theme_void()
        ))
      }

      levey_cal <- plot_levey(file$df(), adat_header, df_cvs_all, sample_type = "Calibrator")
      if (is.null(levey_cal)) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Calibrator plot generation failed") +
            ggplot2::theme_void()
        ))
      }
      plotly::ggplotly(levey_cal)
    })
  })

  output$tbl_ks_cal<- DT::renderDataTable({
    withProgress(message = "Performing statistical comparison...", {
      # *** UPDATED: Use reactive cv_data passed from app_server ***
      df_cvs_all <- cv_data_reactive()

      if (is.null(df_cvs_all)) {
        return(tibble::tibble(
          PlateId = character(),
          Statistic = numeric(),
          `P-value` = numeric()
        ))
      }

      df_cvs <- file$df()  %>% dplyr::filter(SampleType == "Calibrator") %>%
        dplyr::select(PlateId, starts_with("seq.")) %>%
        dplyr::group_by(PlateId) %>%
        dplyr::summarise_if(is.numeric, function(x) sd(x)/mean(x)) %>%
        dplyr::ungroup() %>%
        tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
        dplyr::group_by(PlateId) %>%
        dplyr::summarise("10%" = round(quantile(CV, 0.1) * 100, 1),
                         "50%" = round(median(CV) * 100, 1),
                         "90%" = round(quantile(CV, 0.9) * 100, 1))

      colnames(df_cvs) <- c("Plate", "10%", "50%", "90%")
      ks_test(df_cvs, df_cvs_all, sample_type = "Calibrator")
    })
  })

  output$tbl_qc_cv <- DT::renderDataTable({
    withProgress(message = "Calculating reference material precision...", {
      df_cvs_qc <- file$df()   %>% dplyr::filter(SampleType == "QC") %>%
        dplyr::select(PlateId, starts_with("seq.")) %>%
        dplyr::summarise_if(is.numeric, function(x) sd(x)/mean(x)) %>%
        tidyr::gather(key = "SeqId", value = "CV") %>%
        dplyr::summarise("10%" = round(quantile(CV, 0.1) * 100, 1),
                         "50%" = round(median(CV) * 100, 1),
                         "90%" = round(quantile(CV, 0.9) * 100, 1))

      file$df()  %>% dplyr::filter(SampleType == "QC") %>%
        dplyr::select(Barcode) %>%
        dplyr::mutate(nSamples = dplyr::n()) %>%
        unique() %>%
        dplyr::bind_cols(., df_cvs_qc) %>%
        dplyr::rename("QC Lot" = "Barcode")
    })
  })

  output$levey_somalogic_qc <- plotly::renderPlotly({
    withProgress(message = "Generating reference material quality trend...", {
      # *** UPDATED: Use reactive cv_data passed from app_server ***
      df_cvs_all <- cv_data_reactive()
      adat_header <- file$df2()

      if (is.null(df_cvs_all)) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Historical data not available") +
            ggplot2::theme_void()
        ))
      }

      levey_qc <- plot_levey(file$df(), adat_header, df_cvs_all, sample_type = "QC")
      if (is.null(levey_qc)) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "QC plot generation failed") +
            ggplot2::theme_void()
        ))
      }
      plotly::ggplotly(levey_qc)
    })
  })

  output$tbl_ks_qc <- DT::renderDataTable({
    withProgress(message = "Performing statistical comparison...", {
      # *** UPDATED: Use reactive cv_data passed from app_server ***
      df_cvs_all <- cv_data_reactive()

      if (is.null(df_cvs_all)) {
        return(tibble::tibble(
          PlateId = character(),
          Statistic = numeric(),
          `P-value` = numeric()
        ))
      }

      df_cvs <- file$df()  %>% dplyr::filter(SampleType == "QC") %>%
        dplyr::select(PlateId, starts_with("seq.")) %>%
        dplyr::group_by(PlateId) %>%
        dplyr::summarise_if(is.numeric, function(x) sd(x)/mean(x)) %>%
        dplyr::ungroup() %>%
        tidyr::gather(key = "SeqId", value = "CV", -PlateId) %>%
        dplyr::group_by(PlateId) %>%
        dplyr::summarise("10%" = round(quantile(CV, 0.1) * 100, 1),
                         "50%" = round(median(CV) * 100, 1),
                         "90%" = round(quantile(CV, 0.9) * 100, 1))

      colnames(df_cvs) <- c("Plate", "10%", "50%", "90%")
      ks_test(df_cvs, df_cvs_all, sample_type = "QC")
    })
  })
}

## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
