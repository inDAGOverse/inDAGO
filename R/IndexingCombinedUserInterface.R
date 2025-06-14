#' Indexing combined ui
#'
#' @param id Shiny module identifier
IndexingCombinedUserInterface <- function(id) {
    ns <- shiny::NS(id)
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

        ##### SIDE PANEL
        sidebar = bslib::sidebar(
            width = "23%",
            bslib::accordion(
                rintrojs::introBox(
                    bslib::accordion_panel(
                        "BASIC OPTIONS", icon = bsicons::bs_icon("sliders"),
                        shiny::fluidRow(shiny::column(12,align="center",
                                                      htmltools::tags$hr(htmltools::h5("Step 1 - Set input/output")),
                                                      htmltools::br())),
                        ## Input folder selection
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shinyFiles::shinyFilesButton(id = ns("IndexingGenome1comb"),
                                                                                label =  "Select reference genome 1",
                                                                                title = "Please select the reference genome (FASTA format) of one organism",
                                                                                icon = shiny::icon("upload"), multiple = FALSE),
                                                                                bslib::tooltip(
                                                                                    bsicons::bs_icon("question-circle"),
                                                                   "This is the reference genome (provided in FASTA format) for organism 1, which must be indexed prior to the mapping step",
                                                                   placement = "right"
                                                               )
                        ),
                        # Displays the selected input directory path
                        shiny::verbatimTextOutput(ns("IndexingPathGenome1comb")), ##folder genome one selected
                        )), # close fluidrow
                        ## label 1
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shiny::textInput(inputId = ns("combIndexingTagGenome1"),
                                                                         label = "Tag for genome 1"),
                                                                         bslib::tooltip(
                                                                             bsicons::bs_icon("question-circle"),
                                                                   "Identifying label of the organism 1 for each reference sequence belonging to its genome",
                                                                   placement = "right"
                                                               )
                        ),
                        )), # close fluidrow
                        ## Input folder selection GENOME 2
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shinyFiles::shinyFilesButton(id = ns("IndexingGenome2comb"),label =  "Select reference genome 2",
                                                                                title = "Please select the reference genome (FASTA format) of the other organism",
                                                                                icon = shiny::icon("upload"), multiple = FALSE),
                                                                                bslib::tooltip(
                                                                                    bsicons::bs_icon("question-circle"),
                                                                   "This is the reference genome (provided in FASTA format) for organism 2, which must be indexed prior to the mapping step",
                                                                   placement = "right"
                                                               )
                        ),
                        # Displays the selected input directory path
                        shiny::verbatimTextOutput(ns("IndexingPathGenome2comb")), ##folder genome two selected
                        )), # close fluidrow
                        ## label genome 2
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shiny::textInput(inputId = ns("combIndexingTagGenome2"),
                                                                         label = "Tag for genome 2"),
                                                                         bslib::tooltip(
                                                                             bsicons::bs_icon("question-circle"),
                                                                   "Identifying label of the organism 2 for each reference sequence belonging to its genome",
                                                                   placement = "right"
                                                               )
                        ),
                        )), # close fluidrow
                        ### empty folder to store results
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shinyFiles::shinyDirButton(ns("IndexingIndexComb"),
                                                                              "Select folder genome index",
                                                                              title = "Please select an empty folder where indexed combined genome will be generated",
                                                                              icon = shiny::icon("download")),
                                                                              bslib::tooltip(
                                                                                  bsicons::bs_icon("question-circle"),
                                                                   "This is the empty folder where the indexed combined genome will be saved.",
                                                                   placement = "right"
                                                               )
                        ),
                        # Displays the selected input directory path
                        shiny::verbatimTextOutput(ns("IndexingPathIndexComb")), ## folder result selected
                        )), #close fluidrow
                    ), #close accordion panel
                    data.step = 1,
                    data.intro = "Configure input and output settings for indexing analysis"
                ), #close introbox basic parameters
                rintrojs::introBox(
                    bslib::accordion_panel(
                        "ADVANCED OPTIONS", icon = bsicons::bs_icon("sliders"),
                        ## checkbox
                        shiny::fluidRow(htmltools::tags$hr(), shiny::column(width = 12,shiny::splitLayout(cellWidths = c("60%", "30%", "20%"),
                                                                                                          htmltools::h5("Split index"),
                                                                                                          shiny::checkboxInput(
                                                                              inputId = ns("CombIndexActiveSplit"),
                                                                              label = "On/Off",
                                                                              value = FALSE
                                                                          ),
                                                                          bslib::tooltip(
                                                                              bsicons::bs_icon("question-circle"),
                                                                              "Check the box to activate 'Split index' option. 'Off' by default",
                                                                              placement = "right")))), #chiude fluidRow
                        ## conditional panel
                        shiny::conditionalPanel(
                            condition = "input.CombIndexActiveSplit == '1'",
                            ns = ns,
                            ## split index
                            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                        shiny::radioButtons(inputId = ns("IndexCombSplitIndex"),
                                                                                label = "Split index",
                                                                                choices = c("TRUE", "FALSE"), selected = "FALSE"),
                                                                                bslib::tooltip(
                                                                                    bsicons::bs_icon("question-circle"),
                                                                       "Split the index into multiple blocks",
                                                                       placement = "right"
                                                                   )
                            ))), # close fluidrow
                            ### select amount of memory
                            shinyjs::disabled(
                                shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                            shiny::sliderInput(inputId = ns("IndexingMemoryComb"),
                                                                                   label = "index_block_size",
                                                                                   # "Index block size: How big a single index-block have to be. It is reported the total RAM available (MB) on this device.",
                                                                                   value = (round(as.numeric(sub(pattern = " GiB", replacement = "", x = memuse::Sys.meminfo()$totalram)) * ((1024^3)/(1000^3))) - 4) * 1000,
                                                                                   min = 1000,
                                                                                   max = round(as.numeric(sub(pattern = " GiB", replacement = "", x = memuse::Sys.meminfo()$totalram)) * ((1024^3)/(1000^2))),
                                                                                   step = 1000,
                                                                                   post = "MB"),
                                                                                   bslib::tooltip(
                                                                                       bsicons::bs_icon("question-circle"),
                                                                           "Specify the size of each index block (in MB) to be loaded into RAM during the read mapping process. It is recommended to reserve a portion of free RAM to allow the execution of the mapping step.",
                                                                           placement = "right"
                                                                       )
                                ))) #close fluidrow
                            ) #close shinyjs
                        ), #close conditional panel
                        # Miscellaneous section header
                        shiny::fluidRow(shiny::column(12,align="center",
                                                      htmltools::tags$hr(htmltools::h5("Miscellaneous")),
                                                      htmltools::br())),
                        ## gapped index
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shiny::radioButtons(inputId = ns("IndexingGappedComb"),
                                                                            label = "gapped_index",
                                                                            choices = c("TRUE", "FALSE"), selected = "FALSE"),
                                                                            bslib::tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                   "Building a gapped index will significantly reduce memory usage, with a modest impact on read mapping time. It is recommended to use a gapped index on personal computers due to their limited memory capacity.",
                                                                   placement = "right"
                                                               )
                        ))), #close fluidrow
                        ## TH subread
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shiny::numericInput(inputId = ns("CombRipetitiveSubreads"),
                                                                            label = "Highly repetitive subreads",
                                                                            value = 100),
                                                                            bslib::tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                   "Subreads (16bp mers) will be excluded from the index if they occur more times than the selected threshold in the genome",
                                                                   placement = "right"
                                                               )
                        ))), #close fluidrow
                        ## temporary folder
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shinyFiles::shinyDirButton(id = ns("TemporaryFolderIndexComb"),
                                                                              label = "Select temporary folder",
                                                                              title = "A temporary folder",
                                                                              icon = shiny::icon("upload")),
                                                                              bslib::tooltip(
                                                                                bsicons::bs_icon("question-circle"),
                                                                   "This is the empty folder where temporary files will be generated and subsequently deleted.",
                                                                   placement = "right"
                                                               )
                        ),
                        shiny::verbatimTextOutput(ns("TemporaryFolderPrintIndexComb")), ## temporary folder selected
                        )) #close fluidrow
                    ), #close accordion panel
                    data.step = 2,
                    data.intro = "Configure advanced options"
                ) #close introbox advanced parameters
            ), #close accordion
            ## indexing genome
            shiny::fluidRow(shiny::column(12,align="center",
                                          htmltools::tags$hr(htmltools::h5("Step 2 - Run genome indexing: combined mode")),
                                          htmltools::br())),
            ## buttons
            ## running
            shiny::fluidRow(
                shiny::actionButton(inputId = ns("runIndexingCombined"), label = "Run",
                             icon = shiny::icon(name = "bookmark"),
                             class = "btn-primary"),
                ## killing
                shinyjs::disabled(
                    shiny::actionButton(ns("StopIndexingCombined"), "Kill", icon = shiny::icon("fire"),
                                 class = "btn-warning"))
            )
        ), # close sidebar
        ##### MAIN PANEL
      shinyWidgets::dropdownButton(
            inputId = ns("IndexingCombinedMydropdown"),
            label = "Info",
            size = "sm",
            status = "info",
            circle = FALSE,
            htmltools::h3(shiny::strong("Genome indexing process")),
            htmltools::br(),
            htmltools::h5(shiny::strong("WHEN TO PERFORM")),
            htmltools::h5("The genome indexing step should be performed prior to mapping and following the filtering process to create a data structure that will serve as a reference for the mapping. Here, adopting the combined approach, genome indexing create one single index obtained concatenating the selected genome 1 and the selected genome 2.
                       "),
            htmltools::br(),
            htmltools::h6(shiny::strong("WHAT IT DOES")),
            htmltools::h6("In this step, a data structure (hash table) is created to enhance the efficiency of subsequent alignment processes. This structure is generated using the provided reference genomes (FASTA file extension), which helps streamline the mapping procedure.
                       Moreover, exclusively for combined approach, a combined annotation file is generated."),
            htmltools::br(),
            htmltools::h6(shiny::strong("OPERATIONAL INSTRUCTIONS")),
            htmltools::h6(shiny::strong("Step 1")),
            htmltools::h6("- Select the first reference genome and the second reference genome (both must be provided in FASTA format)."),
            htmltools::h6("- Choose an empty folder where the indexed combined genome (binary files) will be saved."),
            htmltools::h6("- Select the format of the annotation files provided"),
            htmltools::h6(shiny::strong("Step 2")),
            htmltools::h6("- Start the analysis by clicking the 'Run' button."),
            htmltools::br(),
            htmltools::h6(shiny::strong("RESULTS")),
            htmltools::h6("A single index, consisting of two binary files, will be generated and saved in the selected folder, along with three accompanying plain text files"),
            htmltools::h6(shiny::strong("ADDITIONAL NOTES")),
            htmltools::h6("You can permanently kill the analysis by clicking the 'Kill' button."),
            htmltools::h6("Optionally, you can configure advanced parameters, including 'Split Index' and 'Miscellaneous', for further customization."),
            htmltools::br(),
            shiny::column(5, shiny::actionButton("helpParameterIndexingCombined", "About parameters",class = "btn-primary",icon = shiny::icon("sliders"))),
            htmltools::br()
        ),
      htmltools::br(),
        # text output
      shiny::verbatimTextOutput(ns("IndexingCombinedProcessstatus")),
        ## reassuring table
        DT::DTOutput(ns("IndexingCombinedReassure"))
    ) # close layout_sidebar
} # close ui function
