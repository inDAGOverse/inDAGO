#' UI function for Summarization module in Shiny application
#' @param id Shiny module identifier

SummarizationUserInterface <- function(id) {
  bslib::layout_sidebar(

        htmltools::tags$head(
          htmltools::tags$style(htmltools::HTML("
        .tooltip-inner {
          max-width: 400px;  /* Adjust the width */
          white-space: normal;  /* Allow text to wrap */
          text-align: left;  /* Optional: aligns text to the left */
          border-radius: 4px !important;  /* Less curvature */
          background-color: #202020;      /* Optional: lighter background */
          color: #FFFFFF;                 /* Optional: darker text for readability */
          border: 1px solid #ccc;         /* Optional: subtle border */
        }
      "))
        ),
        htmltools::tags$style(".selectize-dropdown {position: static}"),
        shinyjs::useShinyjs(), # Enables JavaScript for more interactive features
        rintrojs::introjsUI(), # Allows the use of intro.js for creating interactive onboarding tours
        bslib::input_dark_mode(id = "dark_mode"), # Toggle for dark mode

      # Sidebar for Summarization operations
      sidebar = bslib::sidebar(width = "23%", bslib::accordion(
        rintrojs::introBox(
          bslib::accordion_panel(
            "BASIC OPERATIONS",
            icon = bsicons::bs_icon("exclamation-diamond"),
            # select directory where to upload
            shiny::fluidRow(shiny::column(12,align="left",
                            htmltools::tags$hr(htmltools::h5("Step 1 - Set input/output")),
                            htmltools::br())),

            # Input folder selection
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyFiles::shinyDirButton(shiny::NS(id,
                  "directorySumSamUpload"),
                  "select_input_folder",
                  "Please, select the folder containing the samples in 'SAM' or 'BAM' format."
                  ,
                  icon = shiny::icon("upload")
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "This is the input folder containing the mapped reads in 'SAM' or 'BAM' format.",
                  placement = "right"
                )
              ),
              # Displays the selected input directory path
              shiny::verbatimTextOutput(shiny::NS(id,"DirSumUploadSamPath")),

            )),
            # Output folder selection
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyFiles::shinyDirButton(shiny::NS(id, "directorySumDownload"),
                  "select_output_folder",
                  "Please, select an empty folder where the generated output files will be saved."
                  ,
                  icon = shiny::icon("download")
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "This is the empty output folder where the generated files will be saved.",
                  placement = "right"
                )
              ),
              # Displays the selected output directory path
              shiny::verbatimTextOutput(shiny::NS(id,"DirSumDownloadPath")),

            )),
            # Input reference annotation selection
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyFiles::shinyFilesButton(shiny::NS(id, "fileSumRefUpload"),
                  label = "Select_input_reference",
                  title = "Please select the reference genome file"
                  ,
                  icon = shiny::icon("upload"),
                  multiple = FALSE
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "This is the reference annotation file used for the summarization analysis. Annotations in 'GTF' format must be provided as a file, while annotations in 'SAF' format can be provided either as a file or a table. The file can be uncompressed or gzip-compressed. For more information about the 'GTF' format, please refer to the Ensembl website (https://www.ensembl.org/info/website/upload/gff.html). For details on the 'SAF' format, consult the Rsubread manual at (https://bioconductor.org/packages/release/bioc/vignettes/Rsubread/inst/doc/SubreadUsersGuide.pdf).",
                  placement = "right"
                )
              ),
              # Displays the selected reference annotation path
              shiny::verbatimTextOutput(shiny::NS(id,"FileSumUploadRefPath")),

            )),
            # Section to set annotation option
            shiny::fluidRow(shiny::column(12, align =
                              "left", htmltools::tags$hr(htmltools::h5(
                                "Step 2 - Annotation"
                              )), htmltools::br())),
            # Input for specifying the annotation format
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "isGTFAnnotationFile"),
                  label = "annotation_format",
                  choices = c("GTF" = "TRUE", "SAF" = "FALSE"),
                  selected = "TRUE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify whether the annotation is in 'GTF' or 'SAF' format. 'GTF' by default.",
                  placement = "right"
                )
              )
            )),

            # Conditional panel that appears if isGTFAnnotationFile is 'TRUE'
            conditionalPanel(
              condition = sprintf("input['%s'] == true", shiny::NS(id, "isGTFAnnotationFile")),
              # Input for specifying the feature type
              shiny::fluidRow(shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shinyWidgets::prettyRadioButtons(shiny::NS(id, "GTF.featureType"),
                    "feature_type",
                    choices = c("exon", "gene", "transcript", "CDS"),
                    selected = "exon",
                    status = "primary"
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Specify the type of feature to be extracted from the 3rd column of the annotation file for read summarization. You can add a new selectable argument, and by setting it to 'NULL', it can be removed. This argument is applicable only when the annotation format is 'GTF'. 'exon' by default.",
                    placement = "right"
                  )
                )
              )),


              # Input to add feature type
              shiny::fluidRow(shiny::column(
                width = 5, shiny::textInput(shiny::NS(id,"AddGTF.featureType"), label = NULL)
              ), shiny::column(
                width = 1, shiny::actionButton(shiny::NS(id,"ActionAddGTF.featureType"), "Add")
              )),
              # Input for specifying the attribute type
              shiny::fluidRow(shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shinyWidgets::prettyRadioButtons(shiny::NS(id,"GTF.attrType"),
                    "attribute_type",
                    choices = c("gene_id", "transcript_id", "protein_id", "gene"),
                    selected = "gene_id",
                    status = "primary"
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Specify the type of attribute to be extracted from the 9th column of the annotation file for read summarization. You can add a new selectable argument, and setting it to 'NULL' will remove it. This argument is applicable only when the annotation format is 'GTF'. 'gene_id' by default.",
                    placement = "right"
                  )
                )
              )),
              # Input for adding attribute type
              shiny::fluidRow(shiny::column(
                width = 5, shiny::textInput(shiny::NS(id,"AddGTF.attrType"), label = NULL, )
              ), shiny::column(
                width = 1, shiny::actionButton(shiny::NS(id,"ActionAddGTF.attrType"), "Add")
              ))
            ),

            # Input for specifying the level of summarization
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "useMetaFeatures"),
                  "use_meta_features",
                  choices = c("TRUE", "FALSE"),
                  selected = "TRUE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "This option controls the assignment of counts at the meta-feature level (e.g., gene_id) or at the feature level (e.g., exon). If set to 'TRUE', summarization will be performed at the meta-feature level. When using an annotation file in 'SAF' format, the annotation will be performed based on the 'GeneID' column (the only operation allowed for 'SAF' format). With a 'GTF' format annotation file, the summarization will occur at the 'attribute_type' parameter. If set to 'FALSE' (only permitted for 'GTF' format), summarization will be performed at the feature level using the 'feature_type' parameter. 'TRUE' by default.",
                  placement = "right"
                )
              )
            )),


          ),
          data.step = 1,
          data.intro = "Here, you can configure the 'BASIC OPERATIONS' related to required to 'Step 1 - Set input/output' and annotation details of summarizations analysis. It is possible choise between 'GTF' and 'SAF' format, specifing the summarization level: 1) feature: reads will be assigned to a specific feature level (e.g, exon). 2) meta-feature: reads will be assiged to the meta-feature level, e.g, gene_id, by groupping each related feature, e.g, exon."
        ),

        rintrojs::introBox(
          bslib::accordion_panel(
            "ADVANCED OPTIONS",icon = bsicons::bs_icon("sliders"),

            # Section to set overlap between reads and features
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(
                                htmltools::h5("Overlap between reads and features")
                              ), htmltools::br())),
            # Input for specifying multiple overlap
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "allowMultiOverlap"),
                  "allow_multi_overlap",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Features (or meta-features) can be present on both strands. If the 'allow_multi_overlap' parameter is set to 'TRUE', read pairs that overlap with multiple features (or meta-features) at the same mapping locus will be assigned to each of them. If the 'fraction' option is set to 'FALSE', each feature (or meta-feature) receives a count of 1. If 'fraction' is set to 'TRUE', each feature/meta-feature will receive a count of 1/x, where x is the total number of overlapping features/meta-features for that read. 'FALSE' by default.",
                  placement = "right"
                )
              )
            )),
            # Input for specifying minimum overlap
            shiny::fluidRow(shiny::column(
              width = 12, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id, "minOverlap"),
                  "min_overlap",
                  step = 1,
                  value = 1
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the minimum number of overlapping bases required for a read pair to be assigned to a feature (or meta-feature). If a negative value is provided, a gap up to the specified size will be allowed between the read pair and the feature it is assigned to. '1' by default",
                  placement = "right"
                )
              )
            )),
            # Input for specifying fraction of overlapping
            shiny::fluidRow(shiny::column(
              width = 12, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id, "fracOverlap"),
                  "frac_overlap",
                  step = 1,
                  min = 0,
                  max = 100,
                  value = 0
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the minimum percentage (0% - 100%) of overlapping bases required for a read pair to be assigned to a feature (or meta-feature). If a negative value is provided, a gap up to the specified size will be allowed between the read pairs and the feature they are assigned to. '0' by default",
                  placement = "right"
                )
              )
            )),
            # Input for specifying fraction of overlapping included in a feature
            shiny::fluidRow(shiny::column(
              width = 12, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id, "fracOverlapFeature"),
                  "frac_overlap_feature",
                  step = 1,
                  min = 0,
                  max = 100,
                  value = 0
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the minimum percentage (0% - 100%) of a feature that must be overlapped by read pairs for assignment. '0' by default.",
                  placement = "right"
                )
              )
            )),

            # Input for specifying largest overlap
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "largestOverlap"),
                  "largest_overlap",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', a read pair will be assigned to the feature (or meta-feature) with the largest number of overlapping bases when it overlaps with multiple features (or meta-features). 'FALSE' by default.",
                  placement = "right"
                )
              )
            )),

            # Input for specifying largest overlap
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(
                                htmltools::h5("Multi-mapping reads")
                              ), htmltools::br())),


            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "countMultiMappingReads"),
                  "count_multi_mapping_reads",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', multi-mapped read pairs identified in the 'SAM' or 'BAM' files from the mapping step will be counted. The 'NH' tag is used to identify multi-mapping reads in the input BAM/SAM files. If 'fraction' is set to 'TRUE', each alignment will receive a fractional count of 1/x, where x is the total number of alignments reported for the read. 'TRUE' by default.",
                  placement = "right"
                )
              )
            )),

            # Section to set fractional counting
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(
                                htmltools::h5("Fractional counting")
                              ), htmltools::br())),
            # Input for specifying fractional counting
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "fraction"),
                  "fraction",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', counts from multi-mapping and/or multi-overlapping reads will be fractionated. Each alignment will carry a fractional count of 1/x, where x is the total number of alignments reported for the read. If a read is both multi-mapping and multi-overlapping, each overlapping feature/meta-feature will receive a fractional count of 1/(x*y), where x is the number of alignments and y is the number of overlapping features/meta-features, provided 'countMultiMappingReads', 'allowMultiOverlap', and 'fraction' are all set to 'TRUE'. 'FALSE' by default.",
                  placement = "right"
                )
              )
            )),

            # Section to set read filtering
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(
                                htmltools::h5("Read filtering")
                              ), htmltools::br())),

            # Input for specifying minimum mapping quality score
            shiny::fluidRow(shiny::column(
              width = 12, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id, "minMQS"),
                  "min_MQS",
                  step = 1,
                  min = 0,
                  max = 255,
                  value = 0
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the minimum mapping quality score a read must achieve to be included (value between 0 and 250). '0' by default.",
                  placement = "right"
                )
              )
            )),
            # Input for specifying if only primary alignments should be counted
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "primaryOnly"),
                  "primary_only",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', only primary alignments in the dataset will be counted, regardless of whether they originate from multi-mapping reads (i.e., count_multi_mapping_reads is ignored). Primary and secondary alignments represent the mapping results for the two segments of the same read separated by a breakpoint (typically due to structural variant events). Primary and secondary alignments are identified using bit 0x100 in the Flag field of SAM/BAM files. 'FALSE' by default.",
                  placement = "right"
                )
              )
            )),
            # Input for specifying if reads marked as duplicates should be ignored
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "ignoreDup"),
                  "ignore_dup",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', duplicated reads will be ignored. For RNA-Seq data, this parameter should be used with caution, as many reads naturally align to the same reference location, making it challenging to distinguish between true duplicates and reads that are simply a result of PCR amplification. 'FALSE' by default.",
                  placement = "right"
                )
              )
            )),

            # Input for specifying if reads marked as duplicates should be ignored
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(htmltools::h5(
                                "Strandness"
                              )), htmltools::br())),

            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "strandSpecific"),
                  "strand_specific",
                  choices = c(
                    "unstranded" = "0",
                    "stranded" = "1",
                    "reversely_stranded" = "2"
                  ),
                  selected = "0",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify whether strand-specific read counting should be performed, depending on whether a strand-specific sequencing protocol was used. If the 'unstranded' argument is selected, reads will be counted regardless of strand-specific information. If the 'stranded' argument is selected, the first read (read 1) originates from the original RNA strand/template, and the second read (read 2) comes from the opposite strand (e.g., Ligation, Standard SOLiD methods/ kits). If the 'reversely_stranded' argument is selected, the second read (read 2) comes from the original RNA strand/template, and the first read (read 1) is from the opposite strand (e.g., dUTP, NSR, NNSR methods/kits). 'unstranded' by default.",
                  placement = "right"
                )
              )
            )),

            # Section to set paired end reads options
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(
                                htmltools::h5("Manage paired end reads")
                              ), htmltools::br())),

            # Input for specifying if reads marked as duplicates should be ignored
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "requireBothEndsMapped"),
                  "require_both_ends_mapped",
                  choices = c("TRUE", "FALSE"),
                  selected = "TRUE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', only paired-end reads with both ends successfully aligned will be included in the summarization. 'TRUE' by default",
                  placement = "right"
                )
              )
            )),
            # Input for specifying if a chimeric fragment should be counted or not
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "countChimericFragments"),
                  "count_chimeric_fragments",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', chimeric alignments of paired-end reads (reads with each end aligned to different chromosomes) will be included in the count. 'FALSE' by default.",
                  placement = "right"
                )
              )
            )),
            # Input for specifying if the automatic read sorting is enabled
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "autosort"),
                  "auto_sort",
                  choices = c("TRUE", "FALSE"),
                  selected = "TRUE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', paired-end reads will be automatically sorted by their names if the reads from the same pair are not adjacent in the input file. 'TRUE' by default.",
                  placement = "right"
                )
              )
            )),

            # Input for specifying if the two ends from the same fragment are required to satisify the fragment length criteria before the fragment can be assigned to a feature or meta-feature
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "checkFragLength"),
                  "check_frag_length",
                  choices = c("TRUE", "FALSE"),
                  selected = "TRUE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', both reads of a paired-end fragment must meet the specified fragment length criteria (defined by 'min_frag_length' and 'max_frag_length') before the fragment can be assigned to a feature or meta-feature. 'TRUE' by default.",
                  placement = "right"
                )
              )
            )),

            # Input for specifying the minimum fragment length for paired-end reads
            shiny::fluidRow(shiny::column(
              width = 12, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id, "minFragLength"),
                  "min_frag_length",
                  step = 1,
                  min = 1,
                  value = 50
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the minimum fragment length for paired-end reads.  '50' by default.",
                  placement = "right"
                )
              )
            )),

            # Input for specifying the maximum fragment length for paired-end reads
            shiny::fluidRow(shiny::column(
              width = 12, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id, "maxFragLength"),
                  "max_frag_length",
                  step = 1,
                  min = 1,
                  value = 600
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the maximum fragment length for paired-end reads. '600' by default.",
                  placement = "right"
                )
              )
            )),



            # Section to set Miscellaneous options
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(
                                htmltools::h5("Miscellaneous")
                              ), htmltools::br())),


            # Input for specifying if verbose information will be generated
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyWidgets::prettyRadioButtons(shiny::NS(id, "verbose"),
                  "verbose",
                  choices = c("TRUE", "FALSE"),
                  selected = "FALSE",
                  status = "primary"
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "If set to 'TRUE', additional details about the summarization process will be provided. 'FALSE' by default.",
                  placement = "right"
                )
              )
            )),


            # Input for specifying directory for temporary files
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shinyFiles::shinyDirButton(shiny::NS(id, "directoryTmpDirSum"),
                  "Select_temporary_folder",
                  "Please, select a folder where the generated temporary files will be saved."
                  ,
                  icon = shiny::icon("upload")
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "This is the input folder designated for storing temporary files, which will be removed later. By default, a temporary folder will be automatically generated.",
                  placement = "right"
                )
              ),

              shiny::verbatimTextOutput(shiny::NS(id,"DirectoryTmpDirSum")),

            )),


            # Section to managing CPU
            shiny::fluidRow(shiny::column(12, align =
                              "center", htmltools::tags$hr(htmltools::h5(
                                "Managing CPU"
                              )), htmltools::br())),

            # Input for specifying threads number
            shiny::fluidRow(shiny::column(
              width = 9, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id, "SumNthreads"),
                  "number_of_threads",
                  step = 1 ,
                  min = 1,
                  max = parallel::detectCores(logical = TRUE) /
                    2,
                  value = 1
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the number of threads to be used for analyzing each sample. The number of threads available for selection depends on the maximum number supported by your system, the threads allocated per sample, and the number of samples being analyzed in parallel. '1' by default.",
                  placement = "right"
                )
              )
            )),
            shiny::verbatimTextOutput(shiny::NS(id,"availableSumNthreads")),

             # Input for specifying samples number
            shiny::fluidRow(shiny::column(
              width = 9, shiny::splitLayout(
                cellWidths = c("90%", "10%"),
                shiny::numericInput(shiny::NS(id,  "SummarizationSamples"),
                  "parallel_processing",
                  step = 1 ,
                  min = 1,
                  max = parallel::detectCores(logical = TRUE) /
                    1,
                  value = 2
                ),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle"),
                  "Specify the number of samples to be analyzed in parallel. The number of selectable samples depends on the total number of threads available on your system and the threads allocated per sample. The available number of threads is displayed below. '2' by default.",
                  placement = "right"
                )
              )
            )),
            shiny::verbatimTextOutput(shiny::NS(id,"availableSummarizationSamples")),


          ),

          data.step = 2,
          data.intro = "Here, you can configure the 'ADVANCED OPTIONS'. The default parameters are suitable for most common summarization operations. However, it can be beneficial to modify them to optimize the analysis."
        )
      ),

      #  The button to trigger the summarization analysis
      shiny::fluidRow(shiny::column(
        12, align = "left", htmltools::tags$hr(
          htmltools::h4("Step 3 - Run Summarization"),
          htmltools::br(),
          shiny::fluidRow(
            shiny::actionButton(shiny::NS(id,  "Summarization"),
              "Run",
              class = "btn-primary",
              icon = shiny::icon("filter")
            ),
            shinyjs::disabled(
              shiny::actionButton(shiny::NS(id, "killSummarization"),
                "Kill",
                class = "btn-warning",
                icon = shiny::icon("fire")
              )
            )
          )
        )
      ))),

      # Dropdown button for information about the summarization process
      shinyWidgets::dropdownButton(shiny::NS(id, "InfoSummarization"),
        status = "info",
        label = "Info",
        size = "sm",
        circle = FALSE,
        htmltools::h3("Summarization process"),
        htmltools::br(),
        htmltools::h5(htmltools::strong("WHEN TO PERFORM")),
        htmltools::h5("The summarization process should be performed after mapping and before identifying differentially expressed genes."),
        htmltools::br(),
        htmltools::br(),
        htmltools::h6(htmltools::strong("WHAT IT DOES")),
        htmltools::h6("This step uses the featureCounts function from the Rsubread package to assign mapped reads to specific genomic features (e.g., exons) or meta-features (e.g., genes)."),
        htmltools::br(),
        htmltools::h6("The input data for summarization analysis consists of:"),
        htmltools::h6("- one or more files in 'SAM' or 'BAM' format;"),
        htmltools::h6("- an annotation file in 'GTF' or custom 'SAF' format listing genomic features"),

        htmltools::h6("Both the read mapping and summarization process should use the same reference genome.
        The customized annotation file in 'SAF' format includes only five required columns for each feature."),
        htmltools::br(),
        htmltools::h6("Example:"),
        htmltools::h6("GeneID Chr Start End Strand"),
        htmltools::h6("497097 chr1 3204563 3207049 -"),
        htmltools::h6("497097 chr1 3411783 3411982 -"),
        htmltools::h6("497097 chr1 3660633 3661579 -"),
        htmltools::h6("100503874 chr1 3637390 3640590 -"),
        htmltools::h6("100503874 chr1 3648928 3648985 -"),
        htmltools::h6("100038431 chr1 3670236 3671869 -"),
        htmltools::br(),
        htmltools::h6("'GeneID' column includes gene identifiers, which may be numeric or character string. Chromosomal names included in the 'Chr' column must match the chromosomal names used for reference sequences to which the reads were mapped."),

        htmltools::h6("For further details, refer to:"),
        htmltools::h6("Liao Y, Smyth GK, Shi W (2019). 'The R package Rsubread is easier, faster, cheaper and better for alignment and quantification of RNA sequencing reads.' Nucleic Acids Research, 47, e47. doi:10.1093/nar/gkz114."),
        htmltools::h6("https://subread.sourceforge.net/SubreadUsersGuide.pdf"),
        htmltools::br(),
           htmltools::br(),
           htmltools::h6(htmltools::strong("OPERATIONAL INSTRUCTIONS")),
        htmltools::br(),
        htmltools::h6(htmltools::strong("Step 1")),
        htmltools::h6("- Choose the folder containing 'SAM' or 'BAM' files (extensions: .sam, .bam)."),
        htmltools::br(),
        htmltools::h6("- Select an empty folder for output files."),
        htmltools::br(),
        htmltools::h6("- Select the annotation reference file (in 'GTF' or 'SAF' format)."),
        htmltools::br(),
        htmltools::h6(htmltools::strong("Step 2 - Assigns groups")),
        htmltools::h6("Set annotation parameters: 'annotation_format', 'feature_type', 'attribute_type' and 'use_meta_features'."),
        htmltools::br(),
        htmltools::h6("- annotation_format: Specify the annotation format (GTF or SAF, default: GTF)."),
        htmltools::h6("- feature_type: Define the feature type to extract from the 3rd column of the annotation file (e.g., exon)."),
        htmltools::h6("- attribute_type: Define the attribute type to extract from the 9th column of the annotation file (e.g., gene_id)."),
        htmltools::h6("- use_meta_features: Choose whether to assign counts at the meta-feature level (e.g., gene) or at the feature level (e.g., exon)."),
        htmltools::h6(htmltools::strong("Step 3")),
        htmltools::h6("Click 'Run' to start the summarization process."),
        htmltools::br(),
        htmltools::br(),
        htmltools::h6(htmltools::strong("RESULT")),
        htmltools::br(),
        htmltools::h6("The summarization generates a count matrix in the 'Counts' folder, with filenames in the format <original_filename>.txt."),
        htmltools::h6("Summary, stats and  optionally annotation files will be saved in the output folder respectively as: 'original_filename_summary.txt, 'original_filename_stat.txt, 'original_filename_annotation.csv'"),
        htmltools::br(),
        htmltools::br(),
        htmltools::h6(htmltools::strong("ADDITIONALLY NOTES")),
        htmltools::br(),
        htmltools::h6("Advanced options are available for: 'level of summarization', 'overlap between reads and features', 'multi-mapping reads', 'fractional counting', 'read filtering', 'strandness', 'parameters specific to paired end reads', 'number of CPU threads', and 'miscellaneous."),
        htmltools::br(),
        htmltools::br(),
        shiny::column(
          5,
          shiny::actionButton(shiny::NS(id,  "helpSummarization"),
            "About summarization",
            class = "btn-primary",
            icon = shiny::icon("sliders")
          )
        ),
        htmltools::br()
      ),
      rintrojs::introBox(
        # Data table output for summarization results
        DT::DTOutput(shiny::NS(id,"SummarizatedSample")),

        data.step = 3,
        data.intro = "Here will be reported the result stored in selected folder ('Select_output_folder')."
      ),
      # Output for checking status messages
      shiny::verbatimTextOutput(shiny::NS(id,"SummarizationCheckStatusOut"))
    )
}
