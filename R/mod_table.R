#' table UI Function
#'
#' @description A shiny Module.
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
##tbl_flag_pass_samples

tabsetPanel(
  tabPanel("Introduction",
         h4(strong("Introduction")),
         HTML('<iframe width="560" height="315" src="https://github.com/foocheung/sqs/assets/25374694/7196212d-0d18-4624-ba92-1c37319969b0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),

 HTML(" <h1>SomaLogic Data QC Analysis</h1> <h2>User Interface Function:</h2>
  <ul>
    <li><strong>Tabbed Interface:</strong> Defines a Shiny module for creating a tabbed interface.</li>
    <li><strong>Tab Content:</strong>
      <ul>
        <li><strong>Introduction:</strong> Provides introductory information about the purpose of the web tool developed by CHI bioinformatics and Proteomics.</li>
        <li><strong>Experimental Design:</strong> Displays a summary table of the assayed samples, including study samples, buffers, SomaLogic's QC, and CHI control samples.</li>
        <li><strong>Sample Type PCA Plots:</strong> Presents PCA plots contrasting between groups of interest, showing separation between different sample types.</li>
        <li><strong>Standardization:</strong> Provides information about the standardization process for SomaScan assay data, including normalization and calibration procedures.</li>
        <li><strong>Sample Normalization:</strong> Includes various sub-tabs for analyzing sample normalization, such as samples passed or flagged, PCA plots based on quality criteria, flagged samples per plate, median normalization scale factors, and ANML fraction used.</li>
        <li><strong>Calibration:</strong> Covers calibration-related analyses, including plate scale, calibrator percent in tails, SOMAmers in tails, QC percent in tails, calibrator CVs, and SomaLogic's QC sample CVs.</li>
      </ul>
    </li>
  </ul>

  <h2>Server Function:</h2>
  <ul>
    <li><strong>Server-Side Logic:</strong> Manages server-side logic for the Shiny module.</li>
    <li><strong>Output Functions (output$...):</strong>
      <ul>
        <li>Generates various visualizations and tables based on the SomaLogic data and user input.</li>
        <li>Conducts QC analyses and data manipulations.</li>
        <li>Handles file processing, data summarization, and visualization rendering.</li>
      </ul>
    </li>
    <li><strong>Specific Functionality:</strong>
      <ul>
        <li><strong>PCA Plots:</strong> Generates PCA plots to visualize sample clustering based on sample types.</li>
        <li><strong>Normalization and Standardization:</strong> Performs normalization and standardization procedures on the SomaScan assay data to mitigate technical variations.</li>
        <li><strong>QC Analysis:</strong> Analyzes flagged samples, calculates various QC metrics, and generates data tables summarizing QC results.</li>
        <li><strong>Data Processing:</strong> Processes SomaLogic data files, conducts calculations, and manipulates data for visualization and analysis purposes.</li>
      </ul>
    </li>
    <li><strong>Progress Indicator (withProgress):</strong> Displays a progress indicator while generating visualizations or processing data to provide feedback to the user about the ongoing operations.</li>
              </ul>")
  ),
tabPanel("Experimental Design", DT::dataTableOutput(ns("tbl_sample_summary")),
  HTML("<H1>Experimental Design</H1>
  <p>This summary encompasses various types of assayed samples, including:</p>

  <ul>
    <li><strong>Study Samples:</strong> Samples collected and analyzed as part of the study.</li>
    <li><strong>Buffers:</strong> Solutions used to maintain pH and osmolarity, typically without any proteins.</li>
    <li><strong>SomaLogic's QC Samples:</strong> Samples provided by SomaLogic to ensure assay quality and consistency.</li>
    <li><strong>CHI Control Samples:</strong> Samples used as controls to monitor assay performance and validate results.</li>
  </ul>

  <p>This summary provides an overview of the different types of samples analyzed in the study, offering insights into the composition and scope of the assay.</p>
</body>
")),
 tabPanel("Sample Type PCA Plots",  selectInput(inputId=ns("pca_color"), label="Color", c("SampleType","PlateId","AssayNotes","SampleNotes", "TimePoint", "SampleGroup"), "SampleType"),
          plotly::plotlyOutput(ns("pca_sample_type")),
HTML("<h1>Sample Type PCA Plots</h1>

  <p>These PCA plots display the results of Principal Component Analysis conducted on a filtered set of SOMAmers (7,289) recommended by SomaLogic for analysis. The plots contrast between different groups of interest and exclusively focus on the study samples, excluding all non-study samples.</p>
  <p><strong>What to look for:</strong> The samples should form clusters based on their type. Typically, PC1 illustrates the separation between buffers without any protein and other sample types containing proteins.</p>

  <hr>

  <p>Principal Component Analysis (PCA) serves as an effective method for preliminary Quality Control (QC) of SomaScan data. PCAs can reveal technical artifacts, such as defects in specific plates or sample types, as well as biological anomalies, such as outliers.</p>
  <p>The scatter plots shown in @fig-pca-sample-type and @fig-pca-plate-id represent the first two principal components (PC1 and PC2), with each dot representing a different sample type, including buffers, calibrators, SomaLogic Quality Control (QC) samples, CHI controls, and study samples. The samples are colored based on their sample type and plate ID. These PCA plots encompass all 7,596 SOMAmer® molecules detected in the assay.</p>"
     )),
 tabPanel("Standardization","",
          HTML("<h1>SomaScan Assay Data Normalization</h1>

  <p>SomaScan assay data undergoes several normalization steps to mitigate variation within the run and between runs:</p>

  <ol>
    <li><strong>Hybridization Control Normalization:</strong> Initial normalization using hybridization controls aims to reduce variation arising from steps like transfer to Agilent slides, hybridization, wash, and scan within the run.</li>
    <li><strong>Median Signal Normalization:</strong> Median signal normalization across pooled calibrator replicates within the run mitigates technical variation in the calibrator signal before scaling calculations. This involves calculating the ratios of the calibrator reference value to the median of calibrator replicates for each SOMAmer reagent.</li>
    <li><strong>Plate Scale and Calibration Scale:</strong> The ratios are decomposed into two terms: plate scale, adjusting for overall signal intensity differences between runs, and calibration scale, recalculating scale factors for each SOMAmer reagent to adjust for specific assay differences between runs.</li>
    <li><strong>Normalization Methods:</strong> Median signal normalization can be performed using Adaptive Normalization by Maximum Likelihood (ANML) for consistent specimen types and studies or median normalization to a study-specific reference. Acceptance criteria for these methods are detailed below.</li>
  </ol>

  <table>
    <thead>
      <tr>
        <th>Normalization Method</th>
        <th>Criteria</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Adaptive Normalization by Maximum Likelihood (ANML)</td>
        <td>Consistent with predefined population references</td>
      </tr>
      <tr>
        <td>Median Normalization to Study-Specific Reference</td>
        <td>Consistency with study-specific references</td>
      </tr>
    </tbody>
  </table>

  <p>Note: Non-Core Matrices may not undergo all data standardization procedures.</p>")
          ),
 tabPanel("Sample Normalization",
          tabsetPanel(
            tabPanel("Samples Passed or Flagged", DT::dataTableOutput(ns("tbl_flag_pass_samples")),
                     HTML("<h1>Summary of Sample Quality</h1>

  <p>This summary provides an overview of sample quality assessment:</p>

  <ul>
    <li><strong>Number of Samples:</strong> Indicates the total number of samples assessed.</li>
    <li><strong>Pass/Fail Status:</strong> Shows the count of samples that passed quality criteria and those flagged for potential issues.</li>
    <li><strong>Flagged Samples:</strong> Lists specific samples that were flagged based on quality criteria.</li>
  </ul>

  <p>Flagged samples typically undergo further evaluation to determine their suitability for analysis. This assessment includes verifying the effectiveness of normalization methods and references intended for the planned analysis.</p>
")),
            tabPanel("PCA Plot Based on Quality Criteria", plotly::plotlyOutput(ns("pca_sample_rowcheck")),
                     HTML("<h1>PCA Plot Based on Quality Criteria</h1>
                     <p>This PCA Plot provides insights into the flagged samples identified based on quality criteria. It visualizes the distribution of flagged samples within the principal component analysis (PCA) plot.</p>
                              <p>Analysis of the PCA plot:</p>
                              <ul>
                              <li>The flagged samples are highlighted or labeled within the PCA plot, indicating their positions relative to other samples.</li>
                              <li>This visualization helps in identifying clusters or patterns of flagged samples, which may indicate common characteristics or issues.</li>
                              <li>By examining the distribution of flagged samples, researchers can gain insights into potential data quality issues or outliers.</li>
                              </ul>")),
            tabPanel("Flagged Samples per Plate", DT::DTOutput(ns("tbl_flagged_samples")),
                    HTML("<h2>Flagged Samples per Plate</h2>
                     <p>This table provides an overview of the number of samples flagged by the quality criteria per plate.</p>
                      <p>This table allows for quick identification of plates with a higher number of flagged samples, which may indicate specific issues or anomalies in the data collection process.</p>
                       ")
            ),
            tabPanel("Median Normalization Scale Factors", DT::dataTableOutput(ns("tbl_med_norm")),
                     HTML("<h2>Median Normalization Scale Factors</h2>

                       <p>This section provides information on the median normalization scale factors.</p>

                       <p>It includes:</p>
                       <ul>
                       <li>The number of samples passed or flagged in the median normalization scale factors criteria for the three dilutions.</li>
                       <li>A list of flagged samples (if any) along with their scale factors.</li>
                       </ul>

                       <p>Median Normalization Scale Factors represent the median of the selected set of ratios for each sample of the SOMAmer reagent signal within the dilution group to the population reference.</p>

                       <p>The table below summarizes the number of samples passed or flagged in this category and lists the flagged samples along with their scale factors.</p>

                       <p><strong>Expected range:</strong> 0.4 - 2.5</p> ")
                     ),
            tabPanel("ANML Fraction Used", DT::dataTableOutput(ns("tbl_anml_fraction")),
                     HTML("<h2>ANML Fraction Used</h2>
                       <p>This section provides information on the ANML fraction used.</p>
                       <p>It includes:</p>
                       <ul>
                       <li>The number of samples passed or flagged in the ANML fraction used criteria for the three dilutions.</li>
                       <li>A list of flagged samples (if any) along with their scale factors.</li>
                       </ul>
                       <p>ANML (Adaptive Normalization by Maximum Likelihood) Fraction Used refers to the fraction of a dilution group for each sample that is between 2 population reference standard deviations of the normal population reference value.</p>
                       <p>Values between 2 population reference standard deviations are used to calculate a 'NormScale Factor' for a dilution group.</p>")

                     )
          )
 ),
 tabPanel("Calibration",
          tabsetPanel(
            tabPanel("Plate Scale", DT::dataTableOutput(ns("tbl_plate_scale")),
                    HTML("<h2>Plate Scale</h2>
                       <p>Plate Scaling is determined by the median of the set of calibrator reference ratios per plate.</p>
                       <p>This process results in a single scale factor applied across the plate.</p>
                       <p>Plate Scaling is typically associated with differences in scanner intensity from plate to plate.</p>")
                       ),
            tabPanel("Calibrator Percent In Tails", DT::dataTableOutput(ns("tbl_cal_perc")),
                     HTML("<h2>Calibrator Percent In Tails</h2>
                                   <p>Calibrator Percent In Tails indicates the percentage of plate calibration scale factors that fall outside the expected range.</p>
                                   <p>The expected range for this metric is between 0.6 and 1.4.</p>"
                     )),
            tabPanel("SOMAmers In Tails", DT::dataTableOutput(ns("tbl_somamers_tails")),
                 HTML("<h2>SOMAmers In Tails</h2>
  <p>SOMAmers In Tails represents the cumulative count of SOMAmer reagents (@tbl-SOMAmers-tails) within the QC control that exhibit a ratio outside the accepted accuracy range when compared to the reference.</p>
  <p>Flagged SOMAmer reagents are typically retained for analysis, as accuracy across all assay runs serves as a robust quality metric, although it's not mandatory for identifying meaningful biological signals.</p>")
                     ),
            tabPanel("QC Percent In Tails", DT::dataTableOutput(ns("tbl_qc_perc_tails")),
                     HTML("<h2>QC Percent In Tails</h2>
  <p>QC Percent In Tails represents the percentage of SOMAmer reagents within the QC Control that fall outside the accepted accuracy range when compared to the reference.</p>
  <p>The expected range for QC Percent In Tails is typically between 0.8 and 1.2.</p>")
                     ),
            tabPanel("Calibrator CVs",
HTML("<h2>Calibrator CVs per Plate</h2>
 <p>This table presents the percentiles (10%, 50%, and 90%) for the distribution of Calibrator CVs (coefficient of variation) on each plate.</p>
  <p>To assess the current assay's performance against historical data generated at the CHI on the same tissue type and assay version, refer to the Levey-Jennings chart of Calibrator CVs shown in @fig-levey-calibrator.</p>
  <p>The table @tbl-ks-cal provides Kolmogorov-Smirnov test statistics for comparing the 10%, 50%, and 90% percentiles of the Calibrator CVs from the current study with historical data.</p>
"),
                     DT::dataTableOutput(ns("tbl_cal_cv")),



HTML("<h2>Levey–Jennings Chart</h2>
  <p>This chart displays the Levey–Jennings plot for the calibrator samples. Each dot represents the median coefficient of variation (CV) of the 7,596 SOMAmers of the calibrator samples per plate.</p>
  <p>The solid, dashed, and dotted black horizontal lines represent the median, ± standard deviation, and ± 2 x standard deviation calculated based on the reference population.</p>
  <p>SomaLogic recommends a median CV% of less than 10% as acceptable.</p>"),
                     plotly::plotlyOutput(ns("levey_calibrator")),

HTML("<h2>Kolmogorov-Smirnov Test Statistics for Calibrator Samples</h2>
  <p>This table provides the Kolmogorov-Smirnov test statistics to compare the 10th, 50th, and 90th percentiles of the coefficient of variation (CV) for calibrator samples in the current study with historical data.</p>
"),
                   DT::dataTableOutput(ns("tbl_ks_cal"))
            ),
            tabPanel("SomaLogic's QC Sample CVs",
                     HTML("<h2>Calibrator CVs per Plate</h2>
  <p>This table displays the percentiles (10th, 50th, and 90th) for the distribution of coefficient of variation (CV) values for SomaLogic's QC samples across all plates.</p>
  <p>To assess the current assay's performance against historical data generated by CHI on the same tissue type and assay version, refer to the Levey-Jennings chart of QC CVs (@fig-levey-somalogic-qc) and the Kolmogorov-Smirnov test statistics (@tbl-ks-qc), which compare the 10th, 50th, and 90th percentiles of the QC sample CVs between the current study and historical data.</p>
  <p><strong>Expected Range:</strong> Median (50th percentile) CV should be below 10%.</p>"),
                     DT::dataTableOutput(ns("tbl_qc_cv")),
                     HTML("<h2>Levey–Jennings Chart</h2>
  <p>This chart depicts the Levey–Jennings plot for the QC samples. Each dot represents the median coefficient of variation (CV) of the 7,596 SOMAmers within the QC samples per plate.</p>
  <p>The solid, dashed, and dotted black horizontal lines represent the median, ± standard deviation, and ± 2 standard deviations, respectively, calculated based on the reference population.</p>
  <p>SomaLogic recommends that the median CV% be less than 10% for acceptability.</p>
"),
                     plotly::plotlyOutput(ns("levey_somalogic_qc")),
                     HTML("<h2>Kolmogorov-Smirnov Test Statistics</h2>
  <p>This section provides the Kolmogorov-Smirnov test statistics for the SomaLogic QC samples.</p>"),
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

mod_table_server <- function(input, output, session, file){  #,batches,sim){
  ns <- session$ns

  output$tbl_sample_summary <- DT::renderDataTable({
    req(file$go())
    withProgress(message = 'Generating tbl_sample_summary...', {
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
    withProgress(message = 'Generating pca_sample_type...', {
      pca_dat <- file$df() %>% dplyr::select(starts_with("seq."))
      pca_res <- prcomp(pca_dat, scale=TRUE)
      pca_scores <- as.data.frame(pca_res$x)
    #  plot_dat <- cbind(file$df()[, c("SampleType", "PlateId", "SampleId")], pca_scores) %>%
    #  ,"AssayNotes","SampleNotes", "TimePoint", "SampleGroup"

      ##New Code here
          plot_dat <- cbind(file$df()[, c("SampleType", "PlateId", "SampleId","AssayNotes","SampleNotes", "TimePoint", "SampleGroup")], pca_scores) %>%
         dplyr::mutate(HoverText = paste0("PlateId: ", PlateId, "<br>SampleId: ", SampleId))
      variance_explained_pc1 <- round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 2)
      variance_explained_pc2 <- round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 2)
    #  plot_pca <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = PC1, y = PC2, color = SampleType, text = HoverText)) +
        plot_pca <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = PC1, y = PC2, color = get(input$pca_color), text = HoverText)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = paste0("PC1 (", variance_explained_pc1, "%)"),
             y = paste0("PC2 (", variance_explained_pc2, "%)"),
             color = "Sample Type")
      plotly::ggplotly(plot_pca)
    })
  })
  output$standardization <-  renderUI({
    # Code for standardization tab
    # You can write a summary or visualization here based on your R Markdown document
    #"This is the standardization tab. Add your code here."
    p(style="text-align: justify; font-size = 25px",
      "SomaScan assay data are first normalized using hybridization controls to mitigate variation within the run that comes from the readout steps:
    transfer to Agilent slides, hybridization, wash, and scan. This is followed by median signal normalization across pooled calibrator replicates
    within the run to mitigate within-run technical variation in the calibrator signal prior to use in scaling calculations. The set of ratios of the
    calibrator reference value to the median of calibrator replicates for each SOMAmer reagent is calculated and decomposed into two terms:
    plate scale - the median ratio, and calibration scale - the recalculated set of scale factors, one for each SOMAmer reagent.
    Plate scale adjusts for overall signal intensity differences between runs. Calibration adjusts for SOMAmer reagent-specific assay differences between runs.
    Median signal normalization is performed using Adaptive Normalization by Maximum Likelihood (ANML) for specimen types and studies shown to be consistent with
    predefined population references or, alternatively, using median normalization to a study specific reference.
    Acceptance criteria are shown below. Non-Core Matrices are often not subject to all data standardization procedures."
    )
     })

#tbl_flag_pass_samples
  output$tbl_flag_pass_samples <- DT::renderDataTable({
    withProgress(message = 'Generating tbl_flag_pass_samples...', {
      # Code for samples passed or flagged tab
      # You can create a data table based on your R Markdown document
     # "This is the samples passed or flagged tab. Add your code here."

     rowcheck_dat <- file$df() %>% dplyr::select(PlateId,SampleType, RowCheck)
     rr<<-rowcheck_dat
     pass_flag <- as.data.frame.matrix(table(rowcheck_dat$RowCheck, rowcheck_dat$SampleType))
     pp<<-pass_flag


    })
  })


  output$pca_sample_rowcheck <- plotly::renderPlotly({
    # Code for PCA plot based on quality criteria tab
    # You can create a plotly plot based on your R Markdown document
    "This is the PCA plot based on quality criteria tab. Add your code here."
    withProgress(message = 'Generating pca_sample_rowcheck...', {
    #  avoid_SOMAmers <- readxl::read_xlsx("d/v4.1_filtered_SOMAmers.xlsx")
      avoid_SOMAmers <- foodata::load_data2()
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

      # Create the scatter plot using ggplot2
      plot_samp_pca_flag <- ggplot2::ggplot(plot_samp_dat, ggplot2::aes(x = PC1, y = PC2, color = RowCheck, text = HoverText)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = paste0("PC1 (", variance_explained_pc1, "%)"),
             y = paste0("PC2 (", variance_explained_pc2, "%)"),
             color = "Check")

      # Convert the ggplot to a plotly object
      plotly::ggplotly(plot_samp_pca_flag)

})
  })






    output$tbl_flagged_samples <- DT::renderDataTable({
      # Code for flagged samples per plate tab
      # You can create a datatable based on your R Markdown document
      "This is the flagged samples per plate tab. Add your code here."
      withProgress(message = 'Generating tbl_flagged_samples...', {

        total_flagged_samples <-file$df() %>% dplyr::filter(RowCheck == "FLAG") %>%
          dplyr::select(PlateId, SampleId, SampleType)

        total_flagged_samples

      })
    })



    output$tbl_med_norm <- DT::renderDataTable({
      # Code for median normalization scale factors tab
      # You can create a datatable based on your R Markdown document
      #"This is the median normalization scale factors tab. Add your code here."
      withProgress(message = 'Generating tbl_med_norm...', {
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
                        Total = sum(pass_flag$Sample)) #%>%


      })
    })



    output$tbl_anml_fraction <- DT::renderDataTable({
      withProgress(message = 'Generating tbl_anml_fraction...', {
        # Code for ANML fraction used tab
        # You can create a datatable based on your R Markdown document
        "This is the ANML fraction used tab. Add your code here."

        rowcheck_dat <- file$df() %>%
          dplyr::select(PlateId,SampleType, RowCheck)
        #   rc<<-rowcheck_dat
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



#########


    output$tbl_cal_perc <- DT::renderDataTable({
      withProgress(message = 'Generating tbl_cal_perc...', {
      #  if (input$dataInput_soma == 1) {
      #    adat_header <- SomaDataIO::parseHeader("CHI-23-009_v4.1_Serum.hybNorm.medNormInt.plateScale.calibrate.anmlQC.qcCheck.anmlSMP.adat")
      #  }
      #  else{
      #    inFile <- input$adat_file
      #    file <- inFile$datapath
      #    adat_header <- SomaDataIO::parseHeader(file)
      #  }
        adat_header<-file$df2()
        kk<<-adat_header

        keys <- names(adat_header$Header.Meta$HEADER)
        kkk<<-keys

        indices <- grep("^CalPlateTailPercent", keys)
        ii<<-indices
        # Extract the keys that match the pattern
        keys_cal_perc_tails <- keys[indices]

        # Use grep() to find indices of keys starting with "CalPlateTailTest"
        indices <- grep("^CalPlateTailTest", keys)
        # Extract the keys that match the pattern
        keys_cal_perc_tails_test <- keys[indices]

        # Generate df for values
        df_cal_perc_tails_value <- data.frame("Value" = unlist(adat_header$Header.Meta$HEADER[keys_cal_perc_tails])) %>%
          tibble::rownames_to_column(var = "Plate") %>%
          dplyr::mutate(Plate = sub("^CalPlateTailPercent_", "", Plate))



        # Generate df for pass falgs
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


    output$tbl_plate_scale <- DT::renderDataTable({
      withProgress(message = 'Generating tbl_cal_perc...', {

        adat_header<-file$df2()



        keys <- names(adat_header$Header.Meta$HEADER)
        # Use grep() to find indices of keys starting with "PlateScale_Scalar"
        indices <- grep("^PlateScale_Scalar", keys)
        # Extract the keys that match the pattern
        keys_with_plate_scale_scalar <- keys[indices]

        # Use grep() to find indices of keys starting with "PlateScale_PassFlag"
        indices <- grep("^PlateScale_PassFlag", keys)
        # Extract the keys that match the pattern
        keys_with_plate_scale_pass <- keys[indices]

        # Generate df for values
        df_plate_scale_value <- data.frame("Value" = unlist(adat_header$Header.Meta$HEADER[keys_with_plate_scale_scalar])) %>%
          tibble::rownames_to_column(var = "Plate") %>%
          dplyr::mutate(Plate = sub("^PlateScale_Scalar_", "", Plate))

        # Generate df for pass falgs
        df_plate_scale_pass <- data.frame("Plate Check" = unlist(adat_header$Header.Meta$HEADER[keys_with_plate_scale_pass])) %>%
          tibble::rownames_to_column(var = "Plate") %>%
          dplyr::mutate(Plate = sub("^PlateScale_PassFlag_", "", Plate))

        # Join and clean
        df_plate_scale <- tibble::tibble("Acceptance Criteria" = "0.4 - 2.5", dplyr::inner_join(df_plate_scale_pass, df_plate_scale_value, by = "Plate")) %>%
          dplyr::select(Plate, `Acceptance Criteria`, Plate.Check, Value) %>%
          dplyr::mutate(Value = round(as.numeric(Value),2))
        colnames(df_plate_scale) <- c("Plate", "Acceptance Criteria", "Plate Check", "Value")


        df_plate_scale



      })

    })


    output$tbl_somamers_tails <- DT::renderDataTable({
      withProgress(message = 'Generating tbl_cal_perc...', {
        adat_header<-file$df2()

        keys <- names(adat_header$Header.Meta$HEADER)

        # Get the header from the ADAT file
        # Use grep() to find indices of keys starting with "CalPlateTailPercent"
        indices <- grep("^CalPlateTailPercent", keys)
        # Extract the keys that match the pattern
        keys_cal_perc_tails <- keys[indices]

        # Use grep() to find indices of keys starting with "CalPlateTailTest"
        indices <- grep("^CalPlateTailTest", keys)
        # Extract the keys that match the pattern
        keys_cal_perc_tails_test <- keys[indices]

        # Generate df for values
        df_cal_perc_tails_value <- data.frame("Value" = unlist(adat_header$Header.Meta$HEADER[keys_cal_perc_tails])) %>%
          tibble::rownames_to_column(var = "Plate") %>%
          dplyr::mutate(Plate = sub("^CalPlateTailPercent_", "", Plate))

        # Generate df for pass falgs
        df_cal_perc_tails_test <- data.frame("Plate Check" = unlist(adat_header$Header.Meta$HEADER[keys_cal_perc_tails_test])) %>%
          tibble::rownames_to_column(var = "Plate") %>%
          dplyr::mutate(Plate = sub("^CalPlateTailTest_", "", Plate))

        # Join and clean
        df_cal_perc_tails <- tibble::tibble("Acceptance Criteria" = "Less than 10%", dplyr::inner_join(df_cal_perc_tails_test, df_cal_perc_tails_value, by = "Plate")) %>%
          dplyr::select(Plate, `Acceptance Criteria`, Plate.Check, Value) %>%
          dplyr::mutate(Value = round(as.numeric(Value),2))
        colnames(df_cal_perc_tails) <- c("Plate", "Acceptance Criteria", "Plate Check", "Value")

        df_cal_perc_tails



      })

    })

    output$tbl_qc_perc_tails <- DT::renderDataTable({
      withProgress(message = 'Generating tbl_cal_perc...', {


        adat_header<-file$df2()

        keys <- names(adat_header$Header.Meta$HEADER)

        df_SOMAmers_tails <- data.frame("SeqId" = adat_header$Col.Meta$SeqId,
                                        "EntrezGeneSymbol" = adat_header$Col.Meta$EntrezGeneSymbol,
                                        "Organism" = adat_header$Col.Meta$Organism,
                                        "ColCheck" = adat_header$Col.Meta$ColCheck)

        tibble::tibble("SOMAmer" = "QC Ratio", "Acceptance Criteria" = "0.8 - 1.2") %>%
          dplyr::bind_cols(data.frame(table(df_SOMAmers_tails$ColCheck)) %>%
                      tidyr::spread(key = Var1, value = Freq)) %>%
          dplyr::bind_cols(tibble::tibble("Total" =  .$FLAG + .$PASS)) #%>%
        #  kbl() %>%
        #  kable_paper()




      })

    })



    output$tbl_cal_cv <- DT::renderDataTable({
      withProgress(message = 'Calculating calibrator CVs...', {

         df_cvs_all <- foodata::load_data4()

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

        colnames(df_cvs) <- c("Cal Precision(%)", "10%", "50%", "90%")

        df_cvs
      })

    })

    output$levey_calibrator <- plotly::renderPlotly({
      withProgress(message = 'Generating Levey-Jennings plot for calibrator CVs...', {
      #  df_cvs_all <- arrow::read_feather("d/serum-cvs.feather")
        df_cvs_all <- foodata::load_data4()
        adat_header<-file$df2()

        kk<<-adat_header

        levey_cal <- plot_levey(file$df(), adat_header, df_cvs_all, sample_type = "Calibrator")
        plotly::ggplotly(levey_cal)


      })

    })

    output$tbl_ks_cal<- DT::renderDataTable({

      withProgress(message = 'Generating tbl_ks_cal...', {
      #  df_cvs_all <- arrow::read_feather("d/serum-cvs.feather")
        df_cvs_all <- foodata::load_data4()
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

        colnames(df_cvs) <- c("Cal Precision(%)", "10%", "50%", "90%")

        ks_test(df_cvs, df_cvs_all, sample_type = "Calibrator")

      })

    })

    output$tbl_qc_cv <- DT::renderDataTable({

      withProgress(message = 'Calculating tbl_qc_cv...', {
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

      withProgress(message = 'Calculating levey_somalogic_qc...', {

      #  df_cvs_all <- arrow::read_feather("d/serum-cvs.feather")
        df_cvs_all <- foodata::load_data4()


        adat_header<- file$df2()

        kk<<-adat_header

        levey_qc <- plot_levey(file$df(), adat_header, df_cvs_all, sample_type = "QC")
        plotly::ggplotly(levey_qc)



      })

    })


    output$tbl_ks_qc <- DT::renderDataTable({

      withProgress(message = 'Calculating tbl_ks_qc...', {

      #  df_cvs_all <- arrow::read_feather("d/serum-cvs.feather")
        df_cvs_all <- foodata::load_data4()
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

        colnames(df_cvs) <- c("Cal Precision(%)", "10%", "50%", "90%")


        ks_test(df_cvs, df_cvs_all, sample_type = "QC")

      })
    })









































}
## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
