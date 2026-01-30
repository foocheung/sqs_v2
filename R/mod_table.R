#' table UI Function
#'
#' @description A shiny Module for quality control data visualization and analysis.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

`%>%` <- dplyr::`%>%`

addResourcePath("d", "extdata/")

mod_table_ui <- function(id, name){
  ns <- NS(id)

  tabsetPanel(
    tabPanel("Introduction",
             h4(strong("Quality Control Analysis Overview")),
             HTML('<iframe width="560" height="315" src="https://github.com/foocheung/sqs/assets/25374694/7196212d-0d18-4624-ba92-1c37319969b0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),

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
  <p>This table summarizes the sample composition across all assay runs, including:</p>

  <ul>
    <li><strong>Study Samples:</strong> Primary samples from the research cohort
    or intervention group.</li>

    <li><strong>Quality Control Samples:</strong> Replicate samples used to assess
    assay reproducibility and technical performance.</li>

    <li><strong>Calibration References:</strong> Standard reference materials used
    to normalize protein abundance estimates across assay runs.</li>

    <li><strong>Negative Controls:</strong> Buffer-only or sham samples with no
    expected protein signal, used to assess background signal levels.</li>

    <li><strong>Positive Controls:</strong> Samples with known protein content,
    used to validate correct analyte detection and quantification.</li>
  </ul>

  <p>The frequency of each sample type per plate reflects the assay design and
  quality control strategy employed.</p>"
             )),

    tabPanel("Sample Type Separation",
             selectInput(inputId=ns("pca_color"),
                         label="Color by:",
                         c("SampleType","PlateId","AssayNotes","SampleNotes",
                           "TimePoint", "SampleGroup"),
                         "SampleType"),
             plotly::plotlyOutput(ns("pca_sample_type")),
             HTML("<h2>Principal Component Analysis of Sample Separation</h2>

  <p>Principal component analysis (PCA) was performed on the complete set of
  protein targets to visualize sample relationships and assess clustering patterns.
  Each point represents one sample, colored by the selected grouping variable.</p>

  <p><strong>Interpretation:</strong> Samples should cluster based on biological
  or technical similarity. Distinct sample type groupings suggest adequate separation.
  Outlier samples falling far from their expected cluster may warrant further
  investigation.</p>

  <hr>

  <h3>PCA for Quality Control</h3>
  <p>Principal component analysis is a widely-used exploratory technique for
  quality assessment in high-dimensional assay data. Benefits include:</p>

  <ul>
    <li>Detection of technical artifacts (e.g., plate effects, batch drift)</li>
    <li>Identification of biological outliers or unexpected samples</li>
    <li>Visual assessment of sample homogeneity within groups</li>
    <li>Validation of assay reproducibility across runs</li>
  </ul>

  <p>PC1 and PC2 are displayed, representing the first two sources of variance
  in the data. The percentage of total variance explained by each component is
  shown on the axes.</p>"
             )),

    tabPanel("Data Standardization","",
             HTML("<h2>Data Standardization Procedures</h2>

  <p>Plasma proteomics assay data undergo sequential standardization to mitigate
  technical variation within and between assay runs. This section describes the
  standardization pipeline applied to your data.</p>

  <h3>Standardization Steps</h3>
  <ol>
    <li><strong>Internal Control Normalization:</strong> Initial normalization
    using internal spike-in controls or platform-specific internal controls to
    reduce variation arising from technical steps including sample transfer,
    incubation, wash, and detection.</li>

    <li><strong>Reference Material Normalization:</strong> Normalization of each
    protein target to the median signal of calibration reference materials within
    the assay run. This step reduces technical drift in the reference material
    signal before calculating scaling factors.</li>

    <li><strong>Plate-Level and Per-Target Scaling:</strong> Decomposition of
    normalization factors into two components:
      <ul>
        <li><strong>Plate Scale:</strong> Median scaling factor applied across
        all protein targets on a plate, adjusting for overall signal intensity
        differences between runs (e.g., scanner settings, reagent lot variation).</li>
        <li><strong>Per-Target Calibration Scale:</strong> Target-specific scaling
        factors that adjust for protein-specific technical differences between runs,
        improving accuracy of abundance estimates.</li>
      </ul>
    </li>

    <li><strong>Normalization Method Selection:</strong> Application of an
    appropriate normalization approach based on data characteristics and
    reference material availability.</li>
  </ol>

  <h3>Normalization Approaches</h3>
  <table>
    <thead>
      <tr>
        <th>Approach</th>
        <th>Suitability Criteria</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Population-Based Reference Normalization</td>
        <td>Use when reference distribution data from the same platform, specimen
        type, and assay variant are available and representative of your samples.</td>
      </tr>
      <tr>
        <td>Study-Specific Normalization</td>
        <td>Use when population reference data are unavailable, non-comparable,
        or when assay conditions differ substantially from historical data.</td>
      </tr>
    </tbody>
  </table>

  <p><em>Note:</em> Different assay platforms and kit versions may employ different
  standardization procedures. Consult your platform-specific documentation for
  detailed methodology.</p>"
             ),
             h4("Assessment Metrics"),
             p("The following sections provide detailed quality metrics for each
             standardization step, including acceptance criteria and interpretation guidance.")
    ),

    tabPanel("Sample Quality",
             tabsetPanel(
               tabPanel("Pass/Fail Summary",
                        DT::dataTableOutput(ns("tbl_flag_pass_samples")),
                        HTML("<h2>Sample Quality Summary</h2>

  <p>This table summarizes sample quality assessment results:</p>

  <ul>
    <li><strong>Total Samples Analyzed:</strong> Number of samples processed through
    the quality control pipeline.</li>

    <li><strong>Pass/Fail Status:</strong> Count of samples meeting or failing quality
    acceptance criteria based on standardization metrics.</li>

    <li><strong>Sample Flags:</strong> Samples marked for further evaluation due to
    out-of-range quality metrics may require exclusion from analysis or investigation
    of technical causes.</li>
  </ul>

  <p><strong>Next Steps:</strong> Flagged samples should be reviewed individually.
  Determine whether quality issues are systematic (e.g., plate effect) or sample-specific,
  and decide whether to retain or exclude samples from downstream analysis.</p>
")),

               tabPanel("Sample Separation (PCA)",
                        plotly::plotlyOutput(ns("pca_sample_rowcheck")),
                        HTML("<h2>Sample Quality Assessment via PCA</h2>
                     <p>Principal component analysis restricted to study samples,
                     with points colored by quality flag status (PASS/FLAG). This
                     visualization helps identify whether flagged samples form
                     distinct clusters or are scattered throughout sample space.</p>

                     <p><strong>Interpretation:</strong></p>
                     <ul>
                     <li>Flagged samples clustering together may indicate a
                     systematic issue (e.g., plate batch effect).</li>

                     <li>Flagged samples scattered among passing samples suggest
                     sample-specific quality issues.</li>

                     <li>Flagged samples at extremes of PC space may be legitimate
                     biological outliers rather than technical failures.</li>
                     </ul>")),

               tabPanel("Flagged Samples per Plate",
                        DT::DTOutput(ns("tbl_flagged_samples")),
                        HTML("<h2>Quality Flags by Plate</h2>
                     <p>This table lists samples that failed quality acceptance
                     criteria, organized by assay plate and sample identifier.</p>

                     <p>High numbers of flagged samples on a single plate may indicate
                     a plate-specific technical issue (e.g., defective plate,
                     scanner error, reagent problem) that should be investigated before
                     including samples in downstream analysis.</p>
                       ")
               ),

               tabPanel("Normalization Scale Factors",
                        DT::dataTableOutput(ns("tbl_med_norm")),
                        HTML("<h2>Normalization Scale Factors Summary</h2>

                       <p>Normalization scale factors (NSF) represent the ratio of
                       reference material value to median protein target signal within
                       each dilution group. NSF values are calculated separately for
                       each of three typical dilution levels (high, medium, low).</p>

                       <p><strong>Quality Assessment:</strong> This table summarizes
                       the number of samples with NSF values within the expected range
                       (0.4 – 2.5) versus those flagged as out-of-range for each
                       dilution group.</p>

                       <p><strong>Interpretation:</strong></p>
                       <ul>
                       <li>NSF values outside the expected range suggest inadequate
                       normalization or unusual protein abundance patterns.</li>

                       <li>Systematic deviation (e.g., all samples in one dilution
                       too high) may indicate reference material instability or
                       calibration drift.</li>

                       <li>Out-of-range samples should be evaluated for technical
                       problems or biological legitimacy before exclusion.</li>
                       </ul>

                       <p><strong>Expected range:</strong> 0.4 – 2.5</p> ")
               ),

               tabPanel("ANML Fraction Used",
                        DT::dataTableOutput(ns("tbl_anml_fraction")),
                        HTML("<h2>Adaptive Normalization - Fraction Used</h2>
                       <p>When using population-based reference normalization, the
                       'ANML fraction used' represents the proportion of protein targets
                       in each dilution group that were successfully used in the
                       normalization calculation (i.e., fell within expected range of
                       the population reference).</p>

                       <p><strong>Interpretation:</strong></p>
                       <ul>
                       <li><strong>High fraction (>0.7):</strong> Sample characteristics
                       are consistent with reference population; normalization is
                       reliable.</li>

                       <li><strong>Low fraction (<0.3):</strong> Sample is unusual
                       relative to reference population; may warrant investigation or
                       use of alternative normalization method.</li>

                       <li><strong>Dilution-Specific Patterns:</strong> Different
                       fractions across dilutions suggest dilution-dependent sample
                       characteristics or potential technical issues specific to one
                       dilution level.</li>
                       </ul>

                       <p>Samples with unexpectedly low ANML fractions are flagged
                       for review prior to inclusion in statistical analysis.</p>")

               )
             )
    ),

    tabPanel("Calibration & Reproducibility",
             tabsetPanel(
               tabPanel("Plate Scale",
                        DT::dataTableOutput(ns("tbl_plate_scale")),
                        HTML("<h2>Plate-Level Scaling Factors</h2>
                       <p>Plate scale factors represent the median normalization
                       ratio across all protein targets on a plate, adjusted for
                       reference material signal.</p>

                       <p><strong>Interpretation:</strong> Plate scale primarily
                       reflects differences in overall signal intensity between runs,
                       typically driven by scanner settings, reagent lot effects, or
                       run-to-run variation in incubation temperature or timing.</p>

                       <p><strong>Expected Range:</strong> 0.4 – 2.5</p>

                       <p>Plates with scale factors outside this range suggest
                       systematic differences in assay performance and should be
                       investigated for technical root causes.</p>")
               ),

               tabPanel("Calibrator Signal in Tails",
                        DT::dataTableOutput(ns("tbl_cal_perc")),
                        HTML("<h2>Calibrator Percent in Tails</h2>
                       <p>'Percent in tails' refers to the percentage of protein
                       targets on a plate with calibration scale factors falling
                       outside the expected accuracy range (0.6 – 1.4).</p>

                       <p><strong>Interpretation:</strong> A small percentage of
                       targets in the tails is expected and acceptable. Higher
                       percentages suggest potential calibration issues, protein
                       target-specific problems, or unexpected run-to-run variation.</p>

                       <p><strong>Acceptance Criteria:</strong> Less than 10% of
                       targets in tails is generally considered acceptable for a
                       well-performing assay plate.</p>"
                        )),

               tabPanel("Protein Targets in Tails",
                        DT::dataTableOutput(ns("tbl_somamers_tails")),
                        HTML("<h2>Calibration Accuracy Assessment</h2>
                 <p>This table summarizes the protein targets within quality control
                 reference materials that exhibited accuracy metrics (calibration
                 scale factors) falling outside the expected range (0.6 – 1.4).</p>

                 <p><strong>Interpretation:</strong> Targets flagged as 'out of tails'
                 may have protein-specific calibration challenges or may represent
                 legitimate run-to-run variation. Many laboratories retain flagged
                 targets in analysis, as even targets with suboptimal calibration
                 accuracy often provide useful biological information.</p>

                 <p><strong>Decision:</strong> Determine whether to include flagged
                 targets in downstream analysis based on your analytical goals and
                 confidence in quantification accuracy.")
               ),

               tabPanel("Reference Material Percent in Tails",
                        DT::dataTableOutput(ns("tbl_qc_perc_tails")),
                        HTML("<h2>Reference Material Accuracy Assessment</h2>
                     <p>This table reports the percentage of protein targets in the
                     quality control reference material that exhibited accuracy metrics
                     (signal ratio to reference) falling outside the expected range
                     (0.8 – 1.2).</p>

                     <p><strong>Interpretation:</strong> The QC reference material
                     serves as a consistency check for the assay. If too many targets
                     are out-of-range in QC, this may indicate plate quality issues,
                     reagent problems, or calibration drift.</p>

                     <p><strong>Acceptance Criteria:</strong> Less than 20% of targets
                     out-of-range is typically acceptable; higher percentages warrant
                     investigation of systematic calibration or technical problems.</p>")
               ),

               tabPanel("Coefficient of Variation - Calibrator",
                        HTML("<h2>Calibrator Reference Material Precision</h2>
 <p>Coefficient of variation (CV) for calibration reference materials reflects
 the reproducibility of protein abundance measurements across replicate assays.
 This table presents the distribution (10th, 50th, 90th percentiles) of per-target
 CV values for calibrator samples on each plate.</p>

 <p><strong>Interpretation:</strong> The median (50th percentile) CV indicates
 typical measurement precision. Values below 10% indicate good reproducibility;
 values above 15% suggest potential technical problems.</p>

 <p>Comparing current CV distribution to historical data (Levey-Jennings plot below)
 helps assess whether current assay performance is typical or degraded.</p>"),

                        DT::dataTableOutput(ns("tbl_cal_cv")),

                        HTML("<h2>Calibrator Quality Trend Chart</h2>
  <p>The Levey-Jennings plot displays the median coefficient of variation for
  calibrator reference materials as a function of assay run (date-plate). Each
  point represents one plate; the solid line connects sequential plates to show
  temporal trends.</p>

  <p><strong>Interpretation:</strong></p>
  <ul>
  <li><strong>Center line (dark blue):</strong> Historical median CV from reference data</li>
  <li><strong>Dashed and dotted red lines:</strong> ±1 SD, ±2 SD, ±3 SD limits
  (control limits)</li>
  <li><strong>Shaded zones:</strong> Visual guides for ±1, ±2, and ±3 SD regions</li>
  <li><strong>Orange points:</strong> Current study plates</li>
  <li><strong>Blue points:</strong> Historical reference plates</li>
  </ul>

  <p><strong>Good Performance:</strong> Current study plates should fall within
  ±2 SD of the reference median. Plates outside ±3 SD warrant investigation.</p>"),

                        plotly::plotlyOutput(ns("levey_calibrator")),

                        HTML("<h2>Statistical Comparison to Historical Data</h2>
  <p>The Kolmogorov-Smirnov test quantitatively compares the distribution of
  coefficient of variation values in your current study to historical reference
  data. A high p-value (p > 0.05) indicates your data are consistent with historical
  performance; a low p-value suggests statistically significant differences.</p>

  <p><strong>Interpretation:</strong></p>
  <ul>
  <li><strong>p > 0.05:</strong> Current CV distribution is not significantly
  different from historical reference (good)</li>
  <li><strong>p < 0.05:</strong> Current CV distribution differs significantly
  from historical reference (investigate cause)</li>
  </ul>
"),
                        DT::dataTableOutput(ns("tbl_ks_cal"))
               ),

               tabPanel("Coefficient of Variation - Reference Material",
                        HTML("<h2>Reference Material Precision Metrics</h2>
  <p>Coefficient of variation (CV) for external quality control reference materials
  reflects assay reproducibility. This table presents the distribution of per-target
  CV values for quality control samples across all assay plates.</p>

  <p><strong>Expected Range:</strong> Median CV should be below 10% for acceptable
  reproducibility.</p>"),

                        DT::dataTableOutput(ns("tbl_qc_cv")),

                        HTML("<h2>Reference Material Quality Trend Chart</h2>
  <p>The Levey-Jennings plot displays the median coefficient of variation for
  external quality control reference materials across sequential assay runs.</p>

  <p><strong>Interpretation:</strong> Points outside ±2 SD suggest assay performance
  degradation; points outside ±3 SD are strong indicators of technical problems
  requiring investigation and possible corrective action.</p>
"),
                        plotly::plotlyOutput(ns("levey_somalogic_qc")),

                        HTML("<h2>Statistical Comparison to Historical Reference</h2>
  <p>Kolmogorov-Smirnov test comparing current quality control CV distribution
  to historical reference data.</p>"),

                        DT::dataTableOutput(ns("tbl_ks_qc"))
               )
             )
    )
  )

}




# Module Server

#' @rdname mod_table
#' @export
#' @keywords internal

mod_table_server <- function(input, output, session, file){
  ns <- session$ns

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

  #tbl_flag_pass_samples
  output$tbl_flag_pass_samples <- DT::renderDataTable({
    withProgress(message = 'Generating sample quality summary...', {

      rowcheck_dat <- file$df() %>% dplyr::select(PlateId,SampleType, RowCheck)
      rr<<-rowcheck_dat
      pass_flag <- as.data.frame.matrix(table(rowcheck_dat$RowCheck, rowcheck_dat$SampleType))
      pp<<-pass_flag


    })
  })


  output$pca_sample_rowcheck <- plotly::renderPlotly({

    withProgress(message = 'Generating PCA plot with quality flags...', {

      avoid_SOMAmers <- foodata2::load_data2()
      avoid_prot <- avoid_SOMAmers %>%
        dplyr::pull(SeqId) %>%
        paste0("seq.",.) %>%
        stringr::str_replace_all(.,"-",".")

      ap<<- avoid_prot

      adat_samp_tbl <- file$df() %>%
        dplyr::filter(SampleType == "Sample") %>%
        dplyr::select(PlateId, SampleId, RowCheck, starts_with("seq.")) %>%
        dplyr::select(!all_of(avoid_prot))


      pca_dat <- file$df() %>% dplyr::filter(SampleType == "Sample") %>% dplyr::select(starts_with("seq."))

      pca_res <- prcomp(pca_dat, scale=TRUE)
      pca_scores <- as.data.frame(pca_res$x)

      ast <<- adat_samp_tbl
      ps  <<- pca_scores

      plot_samp_dat <- cbind(adat_samp_tbl[, c("PlateId", "SampleId", "RowCheck")], pca_scores) %>%
        dplyr::mutate(HoverText = paste0("PlateId: ", PlateId, "<br>SampleId: ", SampleId))

      psd <<- plot_samp_dat

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
      rc<<-rowcheck_dat
      pass_flag <- as.data.frame.matrix(table(rowcheck_dat$RowCheck, rowcheck_dat$SampleType))

      pfss<<-pass_flag
      df_norm_scale <- file$df() %>% dplyr::select(PlateId, SampleId, SampleType, NormScale_0_005, NormScale_0_5, NormScale_20) %>%
        dplyr::filter(SampleType == "Sample") %>%
        dplyr::mutate(across(starts_with("NormScale"), ~ifelse( . < 0.4 | . > 2.5, "Flag", "Pass")))
      dfff<<-df_norm_scale
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

      keys <- names(adat_header$Header.Meta$HEADER)

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

      df_cvs_all <- foodata2::load_data4()

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
      df_cvs_all <- foodata2::load_data4()
      adat_header<-file$df2()

      kk<<-adat_header

      levey_cal <- plot_levey(file$df(), adat_header, df_cvs_all, sample_type = "Calibrator")
      plotly::ggplotly(levey_cal)

    })
  })

  output$tbl_ks_cal<- DT::renderDataTable({

    withProgress(message = "Performing statistical comparison...", {
        df_cvs_all <- foodata2::load_data4()
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

        df_cvs_all <- foodata2::load_data4()

        adat_header<- file$df2()

        kk<<-adat_header

        levey_qc <- plot_levey(file$df(), adat_header, df_cvs_all, sample_type = "QC")
        plotly::ggplotly(levey_qc)

      })
    })

    output$tbl_ks_qc <- DT::renderDataTable({
      withProgress(message = "Performing statistical comparison...", {
        df_cvs_all <- foodata2::load_data4()
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
