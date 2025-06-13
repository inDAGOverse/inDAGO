#' Quality control ui
#'
#' @param id Shiny module identifier
qualityControlUserInterface <- function(id) {
  nsQuality <- shiny::NS(id)
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


    ##### SIDEBAR #####
    ## lateral bar with buttons and options
    sidebar = bslib::sidebar(
      width = "23%",
      bslib::accordion(
        ## BASIC OPERATION ACCORDION PANEL
        rintrojs::introBox(
          bslib::accordion_panel(
            title = "BASIC OPERATIONS",
            icon = bsicons::bs_icon("exclamation-diamond"),
            ## "input/output" title
            shiny::fluidRow(shiny::column(12,align="center",
                            htmltools::tags$hr(htmltools::h5("Step 1 - Set input/output")),
                            htmltools::br())),
            ## sample folder
            shiny::fluidRow(shiny::column(width = 12, shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                    shinyFiles::shinyDirButton(
                                                      id = nsQuality("QualityInputDir"),
                                                      label =  "Select input folder",
                                                      title = "Please select the folder where samples (FASTQ format) are stored",
                                                      icon = shiny::icon("upload")),
                                                    bslib::tooltip(
                                                      bsicons::bs_icon("question-circle"),
                                                      "This is the input folder containing the raw read files in FASTQ format, which can be either compressed or uncompressed. Supported file extensions include:
                                                                                                             filename1_1.fastq
                                                                                                             filename1_2.fastq
                                                                                                             filename1_1.fastq.gz
                                                                                                             filename1_2.fastq.gz
                                                                                                             filename1_1.fq
                                                                                                             filename1_2.fq
                                                                                                             filename1_1.fq.gz
                                                                                                             filename1_2.fq.gz",
                                                      placement = "right"
                                                    )), # close fluidrow
                            # Displays the selected input directory path
                            shiny::textOutput(nsQuality("QualityPathInputDir")),
            )), # close fluidRow
            htmltools::br(),
            ## result folder
            shiny::fluidRow(shiny::column(width = 12, shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                    shinyFiles::shinyDirButton(
                                                      id = nsQuality("QualityOutputDir"),
                                                      label = "Select output folder",
                                                      title = "Please select an empty folder where results (.csv format) will be generated",
                                                      icon = shiny::icon("download")),
                                                    bslib::tooltip(
                                                      bsicons::bs_icon("question-circle"),
                                                      "This is the output folder where the generated output files will be stored.",
                                                      placement = "right"
                                                    )
            ),
            shiny::textOutput(nsQuality("QualityPathOutputDir")),
            )), # close fluidrow
          ), # close accordion panel
          data.step = 1,
          data.intro = "Configure input and output settings for QC analysis."
        ), # close intro box
        ##### ADVANCED OPTIONS #####
        rintrojs::introBox(
          bslib::accordion_panel(
            title = "ADVANCED OPTIONS",
            icon = bsicons::bs_icon("sliders"),
            # Miscellaneous section header
            shiny::fluidRow(shiny::column(12,align="center",
                            htmltools::tags$hr(htmltools::h5("Miscellaneous")),
                            htmltools::br())),
            ## sequence count
            shiny::fluidRow(shiny::column(12,align="left",
                            htmltools::tags$hr(htmltools::h6(shiny::strong("Raw reads count"))),
            )),
            ## action button for sequence count
            shiny::fluidRow(shiny::column(
              width = 12,
              shiny::splitLayout(
                cellWidths = c("60%", "30%", "10%"),
                shiny::actionButton(inputId = nsQuality("RunCountFastqReads"),
                             label = "Count reads in FASTQ files",
                             class = "btn-primary",
                             shiny::icon("rocket")),
                shinyjs::disabled(
                  shiny::actionButton(
                    nsQuality("StopCountFastqReads"),
                    label = "Kill",
                    class = "btn-warning",
                    icon = shiny::icon("fire")
                  )
                ),
                bslib::tooltip(bsicons::bs_icon("question-circle"),
                        "Return the number of sequences contained in the input FASTQ files.
                                                                                 It can be useful to determine how many reads to sample during quality control analysis.",
                        placement = "right")
              ),
              shiny::textOutput(nsQuality("countingReads"))
            )), # close fluidrow counting sequences
            htmltools::br(),
            ## sequence sampling
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                   shiny::numericInput(inputId = nsQuality("SubsetFastqQuality"),
                                                                label = "Sampling raw reads",
                                                                value = 1000000, min = 1, step = 1),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "Set the number of sequence records to process in each FASTQ file.
                                                                           The greater the number of sequences analyzed, greater RAM memory size will be requested.
                                                                           '1000000' by default.",
                                                     placement = "right"
                                                   )
            ))), #close fluidrow
            ## parallel processing
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                   shiny::sliderInput(inputId = nsQuality("QualityCheckProcess"),
                                                               label = "Parallelization",
                                                               min = 1,
                                                               max = parallel::detectCores(logical = TRUE)/2,
                                                               value = 2,
                                                               step = 1),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "Parallel computation.
                                                                   Choose the number of CPU cores to analize samples in parallel.
                                                                   '2' by default",
                                                     placement = "right"
                                                   )
            ))), #close fluidrow
            ## temporary folder
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                   shinyFiles::shinyDirButton(id = nsQuality("TemporaryFolderQualityCheck"),
                                                                  label = "Select an empty temporary folder",
                                                                  title = "A temporary folder",
                                                                  icon = shiny::icon("hourglass")),                                       bslib::tooltip(
                                                                    bsicons::bs_icon("question-circle"),
                                                                    "Select an empty dedicated temporary folder to store temporary files.
                                                                                   This folder and its content will be deleted at the end of the analysis.",
                                                                    placement = "right"
                                                                  )
            ),
            # Displays the selected input directory path
            shiny::textOutput(nsQuality("TemporaryFolderPathQualityCheck")),
            )) #close fluidrow
          ), # close accordion panel
          data.step = 2,
          data.intro = "Configure advanced options."
        ) # close introbox
      ), #close accordion
      ##### action analysis button #####
      shiny::fluidRow(htmltools::tags$hr(shiny::column(12,align="center",
                              htmltools::h5("Step 2 - Run quality control analysis"),
                              htmltools::br(),
                              ## starting
                              shiny::actionButton(nsQuality("RunQualityCheck"), "Run",
                                           icon = shiny::icon("list-check"), class = "btn-primary"),
                              ## killing
                              shinyjs::disabled(
                                shiny::actionButton(nsQuality("StopQualityCheck"), "Kill", icon = shiny::icon("fire"),
                                             class = "btn-warning"))
      ))), # close fluidrow
      ## plot buttons
      rintrojs::introBox(
        shiny::fluidRow(shiny::column(12,align="center",
                        htmltools::tags$hr(htmltools::h5("Step 3 - Generate plots")),
                        htmltools::br())),
        ## Button plot 1
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::splitLayout(
            cellWidths = c("60%", "30%", "10%"),
            shinyjs::disabled(
              shiny::actionButton(nsQuality("RunBaseAveragePlot"),
                           label = "Base average quality plot",
                           class = "btn-primary",
                           shiny::icon("rocket"))
            ),
            shinyjs::disabled(
              shiny::actionButton(
                nsQuality("StopBaseAveragePlot"),
                label = "Kill",
                class = "btn-warning",
                icon = shiny::icon("fire")
              )
            ),
            bslib::tooltip(bsicons::bs_icon("question-circle"),
                    "Average quality score per base across entire read lengths.",
                    placement = "right")
          ),
          shiny::textOutput(nsQuality("firstPlot"))
        )), # close fluidrow plot 1
        htmltools::br(),
        ## button plot 2
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::splitLayout(
            cellWidths = c("60%", "30%", "10%"),
            shinyjs::disabled(
              shiny::actionButton(nsQuality("RunSequenceLengthDistribution"),
                           label = "Sequence length distribution plot",
                           class = "btn-primary",
                           shiny::icon("rocket"))
            ),
            shinyjs::disabled(
              shiny::actionButton(
                nsQuality("StopSequenceLengthDistribution"),
                label = "Kill",
                class = "btn-warning",
                icon = shiny::icon("fire")
              )
            ),
            bslib::tooltip(bsicons::bs_icon("question-circle"),
                    "Distribution of read lengths.",
                    placement = "right")
          ),
          shiny::textOutput(nsQuality("secondPlot"))
        )), # close fluidrow plot 2
        htmltools::br(),
        ## button plot 3
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::splitLayout(
            cellWidths = c("60%", "30%", "10%"),
            shinyjs::disabled(
              shiny::actionButton(nsQuality("RunGcDistribution"),
                           label = "GC distribution plot",
                           class = "btn-primary",
                           shiny::icon("rocket"))
            ),
            shinyjs::disabled(
              shiny::actionButton(
                nsQuality("StopGcDistribution"),
                label = "Kill",
                class = "btn-warning",
                icon = shiny::icon("fire")
              )
            ),
            bslib::tooltip(bsicons::bs_icon("question-circle"),
                    "A density plot reports the distribution of GC content per base", placement = "right")
          ),
          shiny::textOutput(nsQuality("thirdPlotBis"))
        )), # close fluidrow button 3
        htmltools::br(),
        ## button plot 4
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::splitLayout(
            cellWidths = c("60%", "30%", "10%"),
            shinyjs::disabled(
              shiny::actionButton(nsQuality("RunBaseQualityBoxplot"),
                           label = "Base quality box plot",
                           class = "btn-primary",
                           shiny::icon("rocket"))
            ),
            shinyjs::disabled(
              shiny::actionButton(
                nsQuality("StopBaseQualityBoxplot"),
                label = "Kill",
                class = "btn-warning",
                icon = shiny::icon("fire")
              )
            ),
            bslib::tooltip(bsicons::bs_icon("question-circle"),
                    "Box plot showing average quality per base.",
                    placement = "right")
          ),
          shiny::textOutput(nsQuality("fourthPlot"))
        )), # close fluidrow button 4
        htmltools::br(),
        ## button plot 5
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::splitLayout(
            cellWidths = c("60%", "30%", "10%"),
            shinyjs::disabled(
              shiny::actionButton(nsQuality("RunBaseCompositionAreaChart"),
                           label = "Base composition area chart",
                           class = "btn-primary",
                           shiny::icon("rocket"))
            ),
            shinyjs::disabled(
              shiny::actionButton(
                nsQuality("StopBaseCompositionAreaChart"),
                label = "Kill",
                class = "btn-warning",
                icon = shiny::icon("fire")
              )
            ),
            bslib::tooltip(bsicons::bs_icon("question-circle"),
                    "Area chart representing read base composition.",
                    placement = "right")
          ),
          shiny::textOutput(nsQuality("fifthPlot"))
        )), # close fluidrow button 5
        htmltools::br(),
        ## button plot 6
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::splitLayout(
            cellWidths = c("60%", "30%", "10%"),
            shinyjs::disabled(
              shiny::actionButton(nsQuality("RunBaseCompositionLinePlot"),
                           label = "Base composition line plot",
                           class = "btn-primary",
                           shiny::icon("rocket"))
            ),
            shinyjs::disabled(
              shiny::actionButton(
                nsQuality("StopBaseCompositionLinePlot"),
                label = "Kill",
                class = "btn-warning",
                icon = shiny::icon("fire")
              )
            ),
            bslib::tooltip(bsicons::bs_icon("question-circle"),
                    "Line plot of read base composition.",
                    placement = "right")
          ),
          shiny::textOutput(nsQuality("sixthPlot"))
        )), # close fluidrow BOTTONE 6
        data.step = 3,
        data.intro = "After running QC, view tabular results in the output folder and graphical results in the main panel."
      ), # close introbox 3
    ), #close sidebar
    ##### MAIN #####
    bslib::accordion(
      ## DROPDOWN MENU
      shinyWidgets::dropdownButton(
        inputId = nsQuality("mydropdown"),
        label = "Info",
        status = "info",
        size = "sm",
        circle = FALSE,
        htmltools::h3(shiny::strong("Quality control (QC) process")),
        htmltools::br(),
        htmltools::h5(shiny::strong("WHEN TO PERFORM:")),
        htmltools::h5("The QC step should be performed both before and after filtering to:"),
        htmltools::h5("- assess the initial quality of your reads;"),
        htmltools::h5("- determine if the filtering step improved the data quality."),
        htmltools::br(),
        htmltools::h6(shiny::strong("WHAT IT DOES:")),
        htmltools::h6("This step analyzes either a subset or the entirely of input reads, which must be provided in FASTQ format.
                                                        User can previously know the number of read of each FASTQ file (launching 'Raw read count' function in the 'ADVANCED OPTIONS' side panel) and choose the number of reads to analyze"),
        htmltools::br(),
        htmltools::h6(shiny::strong("OPERATIONAL INSTRUCTIONS:")),
        htmltools::h6(shiny::strong("Step 1")),
        htmltools::h6("- Select the input folder: Choose the folder containing your files. Files must be in FASTQ format (compressed or not) with supported extensions: .fastq, .fastq.gz, .fq, or .fq.gz."),
        htmltools::h6("- Select the output folder: Specify a folder where the output files (in .csv format) will be saved."),
        htmltools::h6(shiny::strong("Step 2")),
        htmltools::h6("- Start the process by clicking the 'Run quality control' button. Wait until the analysis is complete."),
        htmltools::h6(shiny::strong("Step 3")),
        htmltools::h6("Access Results. Use the available buttons to view and download statistics of interest."),
        htmltools::br(),
        htmltools::h6(shiny::strong("RESULTS")),
        htmltools::h6("The modules generates six QC reports, each displayed in a dedicated panel:"),
        htmltools::h6("- Base average quality: Average quality score per base across entire read lengths."),
        htmltools::h6("- Sequence length distribution: distribution of read lengths."),
        htmltools::h6("- GC Distribution: Distribution of GC content among reads."),
        htmltools::h6("- Base quality boxplot: Boxplot showing average quality per base."),
        htmltools::h6("- Base composition area chart: Area chart representing read base composition."),
        htmltools::h6("- Base composition line plot: Line plot of read base composition."),
        htmltools::br(),
        htmltools::h6(shiny::strong("ADDITIONAL NOTES")),
        htmltools::h6("You can permanently kill the analysis by clicking the 'Kill' button."),
        htmltools::h6("Download Graphical Results: Export graphical reports in your preferred format."),
        htmltools::h6("Optionally, you can configure advanced parameters in 'Miscellaneous' section, for further customization."),
        htmltools::br(),
        shiny::column(5, shiny::actionButton(
          nsQuality("helpParameterQualityCheck"), "About parameters",class = "btn-primary",icon = shiny::icon("sliders"))),
        htmltools::br()
      ), #close dropdownbutton
      htmltools::br(),
      ## background process status
      shiny::textOutput(nsQuality("mainProcessStatus")),
      ## CONDITIONAL PANELS
      shiny::conditionalPanel(
        condition = "input.RunCountFastqReads",
        ns = nsQuality,
        bslib::accordion_panel(
          title = "Read count",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Count of FASTQ records",
            placement = "right"),
          shinycssloaders::withSpinner(shiny::tableOutput(nsQuality("countFastqRecords"))),
          shiny::downloadButton(outputId = nsQuality("ReadCountLink"), label = "CSV")
        )), # close accordion panel, then conditional panel counting reads
      shiny::conditionalPanel(
        condition = "input.RunBaseAveragePlot",
        ns = nsQuality,
        bslib::accordion_panel(
          title = "Base Average Plot",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Average quality score per base along the whole read lengths",
            placement = "right"),
          bslib::card(full_screen = TRUE, bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              ##### sidebar card 1 #####
              width = "30%",
              title = "Local controls",
              position = "right",
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                 shinyWidgets::switchInput(
                    inputId = nsQuality("BAP_interactively"),
                    label = "Interactive",
                    labelWidth = "120px",
                    onStatus = "success",
                    offStatus = "default"
                  ),
                  bslib::tooltip(bsicons::bs_icon("question-circle"), "Generate interactive plot", placement = "right")
                )
              )),
              bslib::accordion_panel(
                "Download settings",
                ## format
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("BAPformat"),
                      label = "Desired format",
                      choices = c("jpeg", "png", "tiff", "eps", "svg", "pdf"),
                      selected = "jpeg"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
                  )
                )),
                ## size
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("unitsBAP"),
                      label = "units",
                      choices = c("in", "cm", "mm", "px"),
                      selected = "cm"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the unit with which to define the dimensions ('width' and 'height'). 'cm' by default", placement = "right")
                  )
                )),
                ## risoluzione
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("resBAP"),
                      label = "resolution",
                      step = 1,
                      min = 1,
                      value = 200
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the resolution. Applies only to raster output types. '200' by default", placement = "right")
                  )
                )),
                ## width
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("widthBAP"),
                      label = "width",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the width of plot. '10' by default", placement = "right")
                  )
                )),
                ## height
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("heightBAP"),
                      label = "height",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the height of plot. '10' by default", placement = "right")
                  )
                )),
                ## download
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::downloadButton(
                      outputId = nsQuality("BaseQualLink"),
                      label = "Download"),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Download the plot concerning the pre-normalized data", placement = "right")
                  )
                )),
              ) ## close accordion panel
            ), ## close sidebar
            shiny::conditionalPanel(
              condition = "input.BAP_interactively == '0'",
              ns = nsQuality,
              shinycssloaders::withSpinner(shiny::plotOutput(nsQuality("BaseAverageQuality")))
            ), ## close normal panel
            shiny::conditionalPanel(
              condition = "input.BAP_interactively == '1'",
              ns = nsQuality,
              shinycssloaders::withSpinner(plotly::plotlyOutput(nsQuality("BaseAverageQualityInteractive")))
            ) ## close interactive panel
          ) ## close layout sidebar
          ) ## close card
        )), # close accordion panel, then conditional panel plot 1 panel

      ##### PLOT 2 PANEL #####
      shiny::conditionalPanel(
        condition = "input.RunSequenceLengthDistribution",
        ns = nsQuality,
        # condition = base::sprintf("input['%s'] == true",
        #                           shiny::NS(id,"RunSequenceLengthDistribution")),
        bslib::accordion_panel(
          title = "Sequence Length Distribution",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Read lengths distribution",
            placement = "right"),
          bslib::card(full_screen = TRUE, bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              ##### sidebar card 2 #####
              width = "30%",
              title = "Local controls",
              position = "right",
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                 shinyWidgets::switchInput(
                    inputId = nsQuality("SLD_interactively"),
                    label = "Interactive",
                    labelWidth = "120px",
                    onStatus = "success",
                    offStatus = "default"
                  ),
                  bslib::tooltip(bsicons::bs_icon("question-circle"), "Generate interactive plot", placement = "right")
                )
              )),
              bslib::accordion_panel(
                "Download settings",
                ## format
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("SLDformat"),
                      label = "Desired format",
                      choices = c("jpeg", "png", "tiff", "eps", "svg", "pdf"),
                      selected = "jpeg"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
                  )
                )),
                ## size
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("unitsSLD"),
                      label = "units",
                      choices = c("in", "cm", "mm", "px"),
                      selected = "cm"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the unit with which to define the dimensions ('width' and 'height'). 'cm' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("resSLD"),
                      label = "resolution",
                      step = 1,
                      min = 1,
                      value = 200
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the resolution. Applies only to raster output types. '200' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("widthSLD"),
                      label = "width",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the width of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("heightSLD"),
                      label = "height",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the height of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::downloadButton(outputId = nsQuality("ReadLenLink"), label = "Download"),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Download the plot concerning the pre-normalized data", placement = "right")
                  )
                )),
              ) ## close accordion panel
            ), ## close sidebar
            shiny::conditionalPanel(
              condition = "input.SLD_interactively == '0'",
              ns = nsQuality,
              shinycssloaders::withSpinner(shiny::plotOutput(nsQuality("SequenceLengthDistribution")))
            ), ## close normal panel
            shiny::conditionalPanel(
              condition = "input.SLD_interactively == '1'",
              ns = nsQuality,
              shinycssloaders::withSpinner(plotly::plotlyOutput(nsQuality(
                "SequenceLengthDistributionInteractive")))
            ) ## close interactive panel
          ) ## close layout sidebar
          ) ## close card2
        )), # close accordion panel, then conditional panel plot 2

      ##### PLOT 3 PANEL #####
      shiny::conditionalPanel(
        condition = "input.RunGcDistribution",
        ns = nsQuality,
        bslib::accordion_panel(
          title = "GC Distribution",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Distribution of GC content among reads",
            placement = "right"),
          bslib::card(full_screen = TRUE, bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              ##### sidebar card 3 #####
              width = "30%",
              title = "Local controls",
              position = "right",
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                 shinyWidgets::switchInput(
                    inputId = nsQuality("GCD_interactively"),
                    label = "Interactive",
                    labelWidth = "120px",
                    onStatus = "success",
                    offStatus = "default"
                  ),
                  bslib::tooltip(bsicons::bs_icon("question-circle"), "Generate interactive plot", placement = "right")
                )
              )),
              bslib::accordion_panel(
                "Download settings",
                ## format
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("GCDformat"),
                      label = "Desired format",
                      choices = c("jpeg", "png", "tiff", "eps", "svg", "pdf"),
                      selected = "jpeg"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("unitsGCD"),
                      label = "units",
                      choices = c("in", "cm", "mm", "px"),
                      selected = "cm"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the unit with which to define the dimensions ('width' and 'height'). 'cm' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("resGCD"),
                      label = "resolution",
                      step = 1,
                      min = 1,
                      value = 200
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the resolution. Applies only to raster output types. '200' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("widthGCD"),
                      label = "width",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the width of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("heightGCD"),
                      label = "height",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the height of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::downloadButton(outputId = nsQuality("GcdContLink"),
                                   label = "Download"),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Download the plot", placement = "right")
                  )
                )),
              ) ## close accordion panel
            ), ## close sidebar
            shiny::conditionalPanel(
              condition = "input.GCD_interactively == '0'",
              ns = nsQuality,
              shinycssloaders::withSpinner(shiny::plotOutput(nsQuality("GCDcontent")))
            ), ## close normal panel
            shiny::conditionalPanel(
              condition = "input.GCD_interactively == '1'",
              ns = nsQuality,
              shinycssloaders::withSpinner(plotly::plotlyOutput(nsQuality("GCDcontentInteractive")))
            ) ## close interactive panel
          ) ## close layout sidebar
          ) ## close card 3
        )), # close accordion panel, then conditional panel plot 3 panel

      ##### PLOT 4 PANEL #####
      shiny::conditionalPanel(
        condition = "input.RunBaseQualityBoxplot",
        ns = nsQuality,
        bslib::accordion_panel(
          title = "Base Quality Boxplot",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Average quality per base",
            placement = "right"),
          bslib::card(full_screen = TRUE, bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              ##### sidebar card 4 #####
              width = "30%",
              title = "Local controls",
              position = "right",
              bslib::accordion_panel(
                "Samples",
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("BQBsamples"),
                      label = "Choose sample to display",
                      choices = character(0),
                      selected = character(0)
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
                  )
                ))
              ),
              bslib::accordion_panel(
                "Download settings",
                ## format
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("BQBformat"),
                      label = "Desired format",
                      choices = c("jpeg", "png", "tiff", "eps", "svg", "pdf"),
                      selected = "jpeg"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("unitsBQB"),
                      label = "units",
                      choices = c("in", "cm", "mm", "px"),
                      selected = "cm"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the unit with which to define the dimensions ('width' and 'height'). 'cm' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("resBQB"),
                      label = "resolution",
                      step = 1,
                      min = 1,
                      value = 200
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the resolution. Applies only to raster output types. '200' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("widthBQB"),
                      label = "width",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the width of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("heightBQB"),
                      label = "height",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the height of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::downloadButton(outputId = nsQuality("QualBoxLink"),
                                   label = "Download"),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Download the plot concerning the pre-normalized data", placement = "right")
                  )
                )),
              ) ## close accordion panel
            ), ## close sidebar
            shiny::plotOutput(nsQuality("BaseQualityBoxplot"))
          ) ## close layout sidebar
          ) ## close card4
        )), # close accordion panel, then conditional panel plot 4
      ##### PANEL PLOT 5 #####
      shiny::conditionalPanel(
        condition = "input.RunBaseCompositionAreaChart",
        ns = nsQuality,
        bslib::accordion_panel(
          title = "Base Composition Area Chart",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Read base composition",
            placement = "right"),
          bslib::card(full_screen = TRUE, bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              ##### sidebar card 5 #####
              width = "30%",
              title = "Local controls",
              position = "right",
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                 shinyWidgets::switchInput(
                    inputId = nsQuality("BCAC_interactively"),
                    label = "Interactive",
                    labelWidth = "120px",
                    onStatus = "success",
                    offStatus = "default"
                  ),
                  bslib::tooltip(bsicons::bs_icon("question-circle"), "Generate interactive plot", placement = "right")
                )
              )),
              bslib::accordion_panel(
                "Samples",
                shiny::radioButtons(nsQuality("BCACsamples"),
                             label = "Choose sample to display",
                             choices = character(0), selected = character(0)),
                bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
              ),
              bslib::accordion_panel(
                "Download settings",
                ## format
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("BCACformat"),
                      label = "Desired format",
                      choices = c("jpeg", "png", "tiff", "eps", "svg", "pdf"),
                      selected = "jpeg"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("unitsBCAC"),
                      label = "units",
                      choices = c("in", "cm", "mm", "px"),
                      selected = "cm"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the unit with which to define the dimensions ('width' and 'height'). 'cm' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("resBCAC"),
                      label = "resolution",
                      step = 1,
                      min = 1,
                      value = 200
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the resolution. Applies only to raster output types. '200' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("widthBCAC"),
                      label = "width",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the width of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("heightBCAC"),
                      label = "height",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the height of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::downloadButton(outputId = nsQuality("CompCharLink"), label = "Download"),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Download the plot concerning the pre-normalized data", placement = "right")
                  )
                )),
              ) ## close accordion panel
            ), ## close sidebar
            shiny::conditionalPanel(
              condition = "input.BCAC_interactively == '0'",
              ns = nsQuality,
              shinycssloaders::withSpinner(shiny::plotOutput(nsQuality("BaseCompositionAreaChart")))
            ), ## close normal panel
            shiny::conditionalPanel(
              condition = "input.BCAC_interactively == '1'",
              ns = nsQuality,
              shinycssloaders::withSpinner(plotly::plotlyOutput(nsQuality("BaseCompositionAreaChartInteractive")))
            ) ## close interactive panel
          ) ## close layout sidebar
          ) ## close card5
        )), # close accordion panel, then conditional panel plot 5
      ##### PANEL PLOT 6 #####
      shiny::conditionalPanel(
        condition = "input.RunBaseCompositionLinePlot",
        ns = nsQuality,
        bslib::accordion_panel(
          title = "Base Composition Line Plot",
          bslib::tooltip(
            bsicons::bs_icon("question-circle"),
            "Read base composition",
            placement = "right"),
          bslib::card(full_screen = TRUE, bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              ##### sidebar card 6 #####
              width = "30%",
              title = "Local controls",
              position = "right",
              shiny::fluidRow(shiny::column(
                width = 12, shiny::splitLayout(
                  cellWidths = c("90%", "10%"),
                 shinyWidgets::switchInput(
                    inputId = nsQuality("BCLP_interactively"),
                    label = "Interactive",
                    labelWidth = "120px",
                    onStatus = "success",
                    offStatus = "default"
                  ),
                  bslib::tooltip(bsicons::bs_icon("question-circle"), "Generate interactive plot", placement = "right")
                )
              )),
              bslib::accordion_panel(
                title = "Samples",
                shiny::radioButtons(nsQuality("BCLPsamples"),
                             label = "Choose sample to display",
                             choices = character(0), selected = character(0)),
                bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")

              ),
              bslib::accordion_panel(
                "Download settings",
                ## format
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("BCLPformat"),
                      label = "Desired format",
                      choices = c("jpeg", "png", "tiff", "eps", "svg", "pdf"),
                      selected = "jpeg"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the format to save the plot. 'jpeg' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::radioButtons(
                      inputId = nsQuality("unitsBCLP"),
                      label = "units",
                      choices = c("in", "cm", "mm", "px"),
                      selected = "cm"
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the unit with which to define the dimensions ('width' and 'height'). 'cm' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("resBCLP"),
                      label = "resolution",
                      step = 1,
                      min = 1,
                      value = 200
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the resolution. Applies only to raster output types. '200' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("widthBCLP"),
                      label = "width",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the width of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::numericInput(
                      nsQuality("heightBCLP"),
                      label = "height",
                      step = 1,
                      min = 1,
                      value = 10
                    ),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Choose the height of plot. '10' by default", placement = "right")
                  )
                )),
                shiny::fluidRow(shiny::column(
                  width = 12, shiny::splitLayout(
                    cellWidths = c("90%", "10%"),
                    shiny::downloadButton(outputId = nsQuality("CompLinLink"),
                                   label = "Download"),
                    bslib::tooltip(bsicons::bs_icon("question-circle"), "Download the plot concerning the pre-normalized data", placement = "right")
                  )
                )),
              ) ## close accordion panel
            ), ## close sidebar
            shiny::conditionalPanel(
              condition = "input.BCLP_interactively == '0'",
              ns = nsQuality,
              shinycssloaders::withSpinner(shiny::plotOutput(nsQuality("BaseCompositionLine")))
            ), ## close normal panel
            shiny::conditionalPanel(
              condition = "input.BCLP_interactively == '1'",
              ns = nsQuality,
              shinycssloaders::withSpinner(plotly::plotlyOutput(nsQuality("BaseCompositionLineInteractive")))
            ) ## close normal panel
          ) ## close layout sidebar
          ) ## close card
        )), # close accordion panel, then conditional plot 6
      htmltools::br(),
      shiny::textOutput(nsQuality("titleReassure")),
      htmltools::br(),
      DT::DTOutput(nsQuality("reassure"))
    ) # close accordion
  ) # close layout_sidebar
} ## close ui function
