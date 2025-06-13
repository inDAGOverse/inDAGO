#' UI function for filtering module in Shiny application
#' @param id Shiny module identifier

FilteringUserInterface <- function(id) {
      # Sidebar for filtering operations
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

        sidebar = bslib::sidebar(width = "23%", bslib::accordion(
          rintrojs::introBox(
            bslib::accordion_panel(
              "BASIC OPERATIONS",
              icon = bsicons::bs_icon("exclamation-diamond"),
              # select directory where to upload


              shiny::fluidRow(shiny::column(12, align =
                                "left", htmltools::tags$hr(
                                  htmltools::h5("Step 1 - Set input/output")
                                ), htmltools::br())),
              # Input folder selection
              shiny::fluidRow(shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shinyFiles::shinyDirButton(shiny::NS(id,
                    "directoryRawReadsFilteringUpload"),
                    "select_input_folder",
                    "Please select the folder where samples in FASTQ format are stored"
                    ,
                    icon = shiny::icon("upload")
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "This is the input folder containing the raw read files in FASTQ format, which can be either compressed or uncompressed. Supported file extensions include: .fastq, .fq, .fastq.gz or .fq.gz. Paired-end files must follow a naming convention where _1 and _2 differentiate R1 and R2 reads (e.g., filename1_1.fastq.gz for R1 and filename1_2.fastq.gz for R2). Avoid using '.' in filenames except for extensions.",
                    placement = "right"
                  )
                ),
                # Displays the selected input directory path
                shiny::verbatimTextOutput(shiny::NS(id,"DirFilteringUploadPath")),

              )),
              # Output folder selection
              shiny::fluidRow(shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shinyFiles::shinyDirButton(shiny::NS(id,
                    "directoryRawReadsFilteredDownload"),
                    "select_output_folder",
                    "Please select the empty folder where result will be saved"
                    ,
                    icon = shiny::icon("download")
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "This is the output folder where the generated output files will be stored.",
                    placement = "right"
                  )
                ),
                # Displays the selected output directory path
                shiny::verbatimTextOutput(shiny::NS(id,"DirFilteringDownloadPath")),

              )),



              # Section to set minimum reads length option
              shiny::fluidRow(shiny::column(12, align =
                                "left", htmltools::tags$hr(
                                  htmltools::h5("Step 2 - Set filtering options")
                              ), htmltools::br())),
              # Input for specifying the minimum reads length
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shiny::numericInput(shiny::NS(id,
                    "minLen"),
                    "minimum_read_length",
                    step = 1,
                    min = 1,
                    value = 75
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Define the minimum read length threshold. Reads equal to or exceeding this threshold will be retained for further analysis. '40' by default.",
                    placement = "right"
                  )
                )
              )),

              # Input for specifying the trimming value
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shiny::numericInput(shiny::NS(id,
                    "trimValue"),
                    "threshold_qvalue",
                    step = 1 ,
                    min = 1,
                    max = 41,
                    value = 20
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Specify a threshold value for low-quality base calls. Any base with a quality score below 'threshold_qvalue' is considered low quality. '20' by default.",
                    placement = "right"
                  )
                )
              )),

              # Checkbox to enable/disable ends trimming

              shiny::fluidRow(htmltools::tags$hr(), shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("60%", "30%", "20%"),
                  htmltools::h6("read_end_trimmer"),
                  shiny::checkboxInput(shiny::NS(id,"trim"),
                    label = "On/Off",
                    value = TRUE
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Check the box to activate 'read_end_trimmer'. 'On' by default.",
                    placement = "right"
                  )
                )
              )),

              # Conditional panel that appears if ends trimming is enabled
              shiny::conditionalPanel(condition = sprintf("input['%s'] == true", shiny::NS(id,"trim")), # Input for left ends trimming configuration
                               shiny::fluidRow(shiny::column(
                                 width = 12,
                                 shiny::splitLayout(
                                   cellWidths = c("90%", "10%"),
                                   shinyWidgets::prettyRadioButtons(shiny::NS(id,
                                     "left"),
                                     "read_left_end_trimming",
                                     choices = c("TRUE", "FALSE"),
                                     selected = "TRUE"
                                   ),
                                   bslib::tooltip(
                                     bsicons::bs_icon("question-circle"),
                                     "This option is active only if 'read_end_trimmer' is selected. If enabled, trimming will be applied to the leading ends of paired-end reads. The function trims the reads from left to right until encountering a base that is not flagged as low quality, based on the specified 'threshold_qvalue' for low-quality base calls. 'TRUE' by default.",
                                     placement = "right"
                                   )
                                 )
                               )), # Input for right ends trimming configuration
                               shiny::fluidRow(shiny::column(
                                 width = 12,
                                 shiny::splitLayout(
                                   cellWidths = c("90%", "10%"),
                                   shinyWidgets::prettyRadioButtons(shiny::NS(id,
                                     "right"),
                                     "read_right_end_trimming",
                                     choices = c("TRUE", "FALSE"),
                                     selected = "TRUE"
                                   ),
                                   bslib::tooltip(
                                     bsicons::bs_icon("question-circle"),
                                     "This option is active only if 'read_end_trimmer' is selected. If enabled, trimming
will be applied to the trailing ends of paired-end reads. The function trims the
reads from right to left until encountering a base that is not flagged as low
quality, based on the specified 'threshold_qvalue@ for low-quality base calls. 'TRUE' by default.",
                                     placement = "right"
                                   )
                                 )
                               )), ),
              # Checkbox to enable/disable window trimming
              shiny::fluidRow(htmltools::tags$hr(), shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("60%", "30%", "20%"),
                  htmltools::h6("sliding_window_trimmer"),
                  shiny::checkboxInput(shiny::NS(id,
                    "halfwidthAnalysis"),
                    label = "On/Off",
                    value = FALSE
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Check the box to activate 'Sliding_window_trimmer'. 'Off' by default.",
                    placement = "right"
                  )
                )
              )),

              # Conditional panel that appears if window trimming is enabled
              shiny::conditionalPanel(condition = sprintf("input['%s'] == true", shiny::NS(id, "halfwidthAnalysis")), # Input for left ends trimming configuration
                                 # "input.halfwidthAnalysis == '1'", # Numeric input for the sliding window size
                               shiny::fluidRow(shiny::column(
                                 width = 12, shiny::splitLayout(
                                   cellWidths = c("90%", "10%"),
                                   shiny::numericInput(shiny::NS(id,
                                     "halfwidth"),
                                     "half_window_size",
                                     step = 1 ,
                                     min = 1,
                                     value = 5
                                   ),
                                   bslib::tooltip(
                                     bsicons::bs_icon("question-circle"),
                                     "Starting at the left end of the read, low-quality nucleotides are removed by trimming the right end
using a sliding window approach. This is done because read quality typically decreases towards the
right end, and when quality becomes too low, bases are trimmed to improve mappability. Specify the window size within which the quality of bases is evaluated. The trimming process
examines the quality of nucleotides within a window, which includes half the window size to the
left and half to the right of each nucleotide (e.g., a half-window of 5 spans 5 bases to the left, the
nucleotide itself, and 5 bases to the right). Trimming occurs when the window contains a number of failing bases that exceeds the defined
threshold. The default size is set to 5. This option is active only if 'sliding_window_trimmer' is selected. '5' by default",
                                     placement = "right"
                                   )
                                 )
                               )), # Numeric input for the number of failing letters required to trigger trimming
                               shiny::fluidRow(shiny::column(
                                 width = 12, shiny::splitLayout(
                                   cellWidths = c("90%", "10%"),
                                   shiny::numericInput(shiny::NS(id,
                                     "kW"),
                                     "n_failing_letters",
                                     step = 1 ,
                                     min = 1,
                                     value = 1
                                   ),
                                   bslib::tooltip(
                                     bsicons::bs_icon("question-circle"),
                                     "This option is active only if Slidingwindow_trimmer is selected. Specify the number
of consecutive bases with failing quality scores required to trigger trimming. '1' by default",
                                     placement = "right"
                                   )
                                 )
                               )), ),

              # Checkbox to enable/disable adapter removal
              shiny::fluidRow(htmltools::tags$hr(), shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("60%", "30%", "20%"),
                  htmltools::h6("adapter_removal"),
                  shiny::checkboxInput(shiny::NS(id,
                    "Adapters"),
                    label = "On/Off",
                    value = FALSE
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Check the box to activate 'adapter_removal'. 'Off' by default.",
                    placement = "right"
                  )
                )
              )),

              # Conditional panel that appears if adapter removal is enabled
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == true", shiny::NS(id, "Adapters")), # Input for the left adapter pattern
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::textInput(shiny::NS(id,"Lpattern"), "left_adapter_string", value = ""),
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      "This option is active only if 'adapter_removal' is selected. Specify a string that
corresponds to the adapter sequence to be matched at the leading end of the
reads. By default, this field is empty, indicating no adapter sequence is specified.",
                      placement = "right"
                    )
                  )
                )),
                # Input for the right adapter pattern
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::textInput(shiny::NS(id,"Rpattern"), "right_adapter_string", value = ""),
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      "This option is active only if 'adapter_removal' is selected. Specify a string that
corresponds to the adapter sequence to be matched at the trailing end of the
reads. By default, this field is empty, indicating no adapter sequence is specified.",
                      placement = "right"
                    )
                  )
                )),


                # Numeric input for maximum allowed mismatches with the left adapter pattern
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(shiny::NS(id,
                      "max.Lmismatch"),
                      "max_num_left_mismatches",
                      step = 1,
                      min = 0,
                      value = 0
                    ),
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      "This option is active only if 'adapter_removal' is selected. Specify the maximum
number of mismatches allowed when matching the adapter sequence at the left
end of the read. '0' by default.",
                      placement = "right"
                    )
                  )
                )),

                # Numeric input for maximum allowed mismatches with the right adapter pattern
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(shiny::NS(id,
                      "max.Rmismatch"),
                      "max_num_right_mismatches",
                      step = 1 ,
                      min = 0,
                      value = 0
                    ),
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      "This option is active only if adapter removal is selected. Specify the maximum
number of mismatches allowed when matching the adapter sequence at the
right end of the read. '0' by default.",
                      placement = "right"
                    )
                  )
                ))
              ) # Closing conditional panel for adapter removal
            ),



            data.step = 1,
            data.intro = "Here, you can configure the 'BASIC OPERATIONS' related to 'Step 1 - Set input/output' and the 'Step 2 - Set filtering options'. In 'Set length option' you can set the minimum length and quality, below which reads are retained.
              Optionally, further filtering procedures can be permormed setting to 'On': read_end_trimmer, sliding_window_trimmer and adapter_removal"
          ),

          rintrojs::introBox(
            # box for advanced filtering options
            bslib::accordion_panel(
              "ADVANCED OPTIONS",
              icon = bsicons::bs_icon("sliders"),

              # Miscellaneous section header
              shiny::fluidRow(shiny::column(12, align = "left",
                                            htmltools::h6("Miscellaneous")
              , htmltools::br())),


              # Input for the number of sub-sampled reads
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shiny::numericInput(shiny::NS(id,
                    "nChunk"),
                    "subsetting",
                    step = 1 ,
                    min = 2,
                    max = 1e12,
                    value = 1e6
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Specify the number of reads to be analyzed in each process. To reduce
computational cost, the sample is divided into sub-samples, and filtering is
performed on each sub-sample sequentially. This parameter defines the size of
each sub-sample to be processed. '10^6' by default .",
                    placement = "right"
                  )
                )
              )),

              # Radio buttons for selecting the quality type for filtering
              shiny::fluidRow(shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shinyWidgets::prettyRadioButtons(shiny::NS(id,
                    "QualityTypeFiltering"),
                    "quality_scoring_scheme",
                    choices = c(
                      "Auto" = "Auto",
                      "Phred 33" = "FastqQuality" ,
                      "Phred 64" = "SFastqQuality"
                    ),
                    selected = "FastqQuality"
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Specify the quality scoring scheme used in the FASTQ file. Choose from the following options: Phred-64; Phred-33; Auto: Automatic selection of the appropriate scoring scheme. 'Phred-33' by default.",
                    placement = "right"
                  )
                )
              )),
              # Radio buttons for output compression selection
              shiny::fluidRow(shiny::column(
                width = 12,
                shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shinyWidgets::prettyRadioButtons(shiny::NS(id,
                    "compress"),
                    "output_compression",
                    choices = c("TRUE", "FALSE"),
                    selected = "TRUE"
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Specify whether the output files should be gzipped for compression. 'TRUE' by default.",
                    placement = "right"
                  )
                )
              )),
              # Slider input for the number of threads used in filtering
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                  shiny::sliderInput(shiny::NS(id,
                    "filteringThreads"),
                    "parallel_processing",
                    step = 1 ,
                    min = 1,
                    max = parallel::detectCores(logical = TRUE),
                    value = 2
                  ),
                  bslib::tooltip(
                    bsicons::bs_icon("question-circle"),
                    "Indicate the number of samples analyzed simultaneously. The number of selectable samples depends on the maximum number of threads available on your system. '2' by default.",
                    placement = "right"
                  )
                )
              )),



            ),
            data.step = 2,
            data.intro = "Here, you can configure the 'ADVANCED OPTIONS'. The default parameters are suitable for most common filtering operations. However, it can be beneficial to modify them to optimize the analysis."
          )

        ), #  The button to trigger the filtering analysis
        shiny::fluidRow(htmltools::tags$hr(
          shiny::column(
            12,
            align = "left",
            htmltools::h4("Step 3 - Run filtering analysis"),
            htmltools::br(),
            shiny::fluidRow(
              shiny::actionButton(shiny::NS(id,
                "Filtering"),
                "Run",
                class = "btn-primary",
                icon = shiny::icon("filter")
              ),
              shinyjs::disabled(
                shiny::actionButton(shiny::NS(id,
                  "killFiltering"),
                  "Kill",
                  class = "btn-warning",
                  icon = shiny::icon("fire")
                )
              )
            )
          )
        ))),

        # Dropdown button for information about the filtering process
        shinyWidgets::dropdownButton(shiny::NS(id,
          "Info filtering"),
          status = "info",
          label = "Info",
          size = "sm",
          circle = FALSE,
          htmltools::h3("Filtering Process"),
          htmltools::br(),
          htmltools::h5(htmltools::strong("WHEN TO PERFORM")),
          htmltools::h5("The filtering process should be performed after quality control and prior to the mapping process."),
          htmltools::br(),
          htmltools::br(),
          htmltools::h6(htmltools::strong("WHAT IT DOES")),
          htmltools::h6("This step addresses three essential pre-processing tasks aimed at cleaning data and reducing noise to enhance the overall analysis:"),
          htmltools::br(),
          htmltools::h6(htmltools::strong("1. Trimming Low-Quality Bases")),
          htmltools::h6("Base quality often decreases toward the end of a read, with lower quality scores indicating a higher probability of incorrect nucleotide assignment.To ensure accurate alignment to the reference genome, low-quality bases at the ends of reads are trimmed."),
          htmltools::br(),
          htmltools::h6(htmltools::strong("2. Adapter Removal")),
          htmltools::h6("Adapters are artificial DNA sequences added to facilitate attachment of DNA fragments to the sequencing flow cell. The adapter sequence must be removed if it remains in sequenced reads."),
          htmltools::br(),
          htmltools::h6("Note: While the mapping algorithm can handle unremoved adapters using 'soft-clipping', removing adapters beforehand ensures cleaner data for downstream analysis."),
          htmltools::br(),
          htmltools::h6(htmltools::strong("3. Removing Short Reads")),
          htmltools::h6("After trimming, some reads may become too short (e.g., <40 bases). These short reads often align to multiple incorrect loci on the reference genome, introducing noise. Reads shorter than a predetermined length cutoff (e.g., 40 bases) are removed to maintain data quality."),
          htmltools::br(),
          htmltools::h6("Implementing these preprocessing steps improves the reliability and accuracy of sequencing data, leading to better downstream analysis outcomes."),
          htmltools::br(),
          htmltools::br(),
          htmltools::h6(htmltools::strong("OPERATIONAL INSTRUCTIONS")),
          htmltools::br(),
          htmltools::h6(htmltools::strong("Step 1")),
          htmltools::h6("- Input folder selection: Choose the folder containing raw sequencing files (.fastq, .fq, .fastq.gz, or.fq.gz)."),
          htmltools::br(),
          htmltools::h6("- Output folder selection: Specify the directory where filtered reads will be saved."),
          htmltools::br(),
          htmltools::h6(htmltools::strong("Step 2")),
          htmltools::h6("Control the filtering parameters. Define the minimum read length threshold and relative q-value. Additional, choose if to perform 'read_end_trimmer', 'sliding_window_trimmer' or 'adapter_removal'."),
          htmltools::br(),
          htmltools::h6("- read_end_trimmer: Trim reads based on quality scores (read_end_trimmer = On)."),
          htmltools::h6("- sliding_window_trimmer: Perform trimming using sliding window metrics (sliding_window_trimmer = On)."),
          htmltools::h6("- adapter_removal: Remove adapter sequences (adapter_removal = On)."),
          htmltools::br(),
          htmltools::h6(htmltools::strong("Step 3")),
          htmltools::h6("Start the filtering analysis clicking the button 'Run'"),
          htmltools::br(),
          htmltools::br(),
          htmltools::h6(htmltools::strong("RESULTS")),
          htmltools::br(),
          htmltools::h6("Filtered reads are saved as output FASTQ files (compressed if required) in the following format:"),
          htmltools::h6("- For R1: Filtered_original_filename_1.fq.gz"),
          htmltools::h6("- For R2: Filtered_original_filename_2.fq.gz"),
          htmltools::br(),
          htmltools::h6("These filtered files serve as high-quality input for downstream analysis."),
          htmltools::br(),
          htmltools::br(),
          htmltools::h6(htmltools::strong("ADDITIONAL NOTES")),
          htmltools::br(),
          htmltools::h6(htmltools::strong("Input Requirements:")),
          htmltools::h6("- Input files must contain sorted and paired-end raw reads."),
          htmltools::h6("- Paired-end files must follow a naming convention where _1 and _2 differentiate R1 and R2 reads (e.g., filename1_1.fastq.gz for R1 and filename1_2.fastq.gz for R2). Avoid using '.' in filenames except for extensions."),
          htmltools::h6(htmltools::strong("Technical Details:")),
          htmltools::h6("- The filtering process uses functions from the ShortRead and Biostrings packages to handle FASTQ files and process quality scores."),
          htmltools::br(),
          htmltools::h6("- Optionally, it is possible to define miscellaneous advanced options."),

          shiny::column(
            5,
            shiny::actionButton(shiny::NS(id,
              "helpParameterFiltering"),
              "About parameters",
              class = "btn-primary",
              icon = shiny::icon("sliders")
            )
          ),
          htmltools::br()
        ),
        htmltools::br(),
        rintrojs::introBox(
          # Data table output for filtered results
          DT::DTOutput(shiny::NS(id,"FilteredSample")),
          data.step = 3,
          data.intro = "Here, the result will be reported in selected folder ('Select_output_folder')."
        ),
        # Output for checking status messages
        shiny::verbatimTextOutput(shiny::NS(id,"CheckStatusOut"))
      )
}
