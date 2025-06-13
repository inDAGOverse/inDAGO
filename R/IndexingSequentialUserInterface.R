#' Indexing sequential ui
#'
#' @param id Shiny module identifier
IndexingSequentialUserInterface <- function(id) {
  nsIndSeq <- shiny::NS(id)
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
                                                   shinyFiles::shinyFilesButton(id = nsIndSeq("IndexingGenome1seq"),
                                                                    label =  "Select reference genome 1",
                                                                    title = "Please select the reference genome (FASTA format) for organism 1",
                                                                    icon = shiny::icon("upload"), multiple = FALSE),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "This is the reference genome (provided in FASTA format) for organism 1, which must be indexed prior to the mapping step",
                                                     placement = "right"
                                                   )
            ),
            # Displays the selected input directory path
            shiny::verbatimTextOutput(nsIndSeq("IndexingPathGenome1seq")) ## genome one selected
            )), #close fluidrow
            ## select output folder
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),

                                                   shinyFiles::shinyDirButton(nsIndSeq("IndexingIndex1seq"), "Select output folder for genome index 1",
                                                                  title = "Please select an empty folder where indexed genome 1 will be generated",
                                                                  icon = shiny::icon("download")),

                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "This is the empty folder where the indexed genome 1 will be saved.",
                                                     placement = "right"
                                                   )
            ),
            shiny::verbatimTextOutput(nsIndSeq("IndexingPathIndex1seq")) ## result one folder
            )), #close fluidrow
            ### selecting genome two
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                   shinyFiles::shinyFilesButton(id = nsIndSeq("IndexingGenome2seq"),label =  "Select reference genome 2",
                                                                    title = "Please select the reference genome (FASTA format) for organism 2",
                                                                    icon = shiny::icon("upload"), multiple = FALSE),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "This is the reference genome (provided in FASTA format) for organism 2, which must be indexed prior to the mapping step",
                                                     placement = "right"
                                                   )
            ),
            shiny::verbatimTextOutput(nsIndSeq("IndexingPathGenome2seq")) ## genome two selected
            )), #close fluidrow
            ### select output folder
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                   shinyFiles::shinyDirButton(nsIndSeq("IndexingIndex2seq"), "Select output folder for genome index 2", title = "Please select an empty folder where indexed genome 2 will be generated",
                                                                  icon = shiny::icon("download")),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "This is the empty folder where the indexed genome 2 will be saved.",
                                                     placement = "right"
                                                   )
            ),
            shiny::verbatimTextOutput(nsIndSeq("IndexingPathIndex2seq")) ## result two folder selected
            )) #close fluidrow
          ), #close accordion_panel
          data.step = 1,
          data.intro = "Configure input and output settings for indexing analysis"
        ), # close introBox basic parameters
        rintrojs::introBox(
          bslib::accordion_panel(
            "ADVANCED OPTIONS", icon = bsicons::bs_icon("sliders"),
            ## checkbox
            shiny::fluidRow(htmltools::tags$hr(), shiny::column(width = 12,shiny::splitLayout(cellWidths = c("60%", "30%", "20%"),
                                                                                              htmltools::h5("Split index"),
                                                              shiny::checkboxInput(
                                                                inputId = nsIndSeq("SeqIndexActiveSplit"),
                                                                label = "On/Off",
                                                                value = FALSE
                                                              ),
                                                              bslib::tooltip(
                                                                bsicons::bs_icon("question-circle"),
                                                                "Checks the box to activate 'Split index' option. 'Off' by default",
                                                                placement = "right")))), # close fluidRow

            ## conditional panel
            shiny::conditionalPanel(condition = "input.SeqIndexActiveSplit == '1'",
                             ns = nsIndSeq,
                             ## split index
                             shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                    shiny::radioButtons(inputId = nsIndSeq("IndexSeqSplitIndex"),
                                                                                 label = "Split index",
                                                                                 choices = c("TRUE", "FALSE"), selected = "FALSE"),
                                                                    bslib::tooltip(
                                                                      bsicons::bs_icon("question-circle"),
                                                                      "Split the index into multiple blocks",
                                                                      placement = "right"
                                                                    )
                             ))), #close fluidrow
                             ### select amount of memory
                             shinyjs::disabled(
                               shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                           shiny::sliderInput(inputId = nsIndSeq("IndexingMemory"),
                                                                                  label = "Index-block size",
                                                                                  value = (base::round(base::as.numeric(base::sub(pattern = " GiB", replacement = "", x = memuse::Sys.meminfo()$totalram)) * ((1024^3)/(1000^3))) - 4) * 1000,
                                                                                  min = 1000,
                                                                                  max = base::round(base::as.numeric(base::sub(pattern = " GiB", replacement = "", x = memuse::Sys.meminfo()$totalram)) * ((1024^3)/(1000^2))),
                                                                                  step = 1000,
                                                                                  post = "MB"),
                                                                      bslib::tooltip(
                                                                        bsicons::bs_icon("question-circle"),
                                                                        "Specify the size of each index block (in MB) to be loaded into RAM during the read mapping process. It is recommended to reserve a portion of free RAM to allow the execution of the mapping step.",
                                                                        placement = "right"
                                                                      )
                               ))) #close fluidrow
                             ) #close shinyjs
            ), #close conditional_panel
            # Miscellaneous section header
            shiny::fluidRow(shiny::column(12,align="center",
                            htmltools::tags$hr(htmltools::h5("Miscellaneous")),
                            htmltools::br())),
            ### gapped index
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                   shiny::radioButtons(inputId = nsIndSeq("IndexingGapped"),
                                                                label = "Gapped index",
                                                                choices = c("TRUE", "FALSE"), selected = "FALSE"),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "Building a gapped index will significantly reduce memory usage, with a modest impact on read mapping time. It is recommended to use a gapped index on personal computers due to their limited memory capacity.",
                                                     placement = "right"
                                                   )
            ))), #close fluidrow
            ## TH subread
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                        shiny::numericInput(inputId = nsIndSeq("RipetitiveSubreads"),
                                                                label = "highly_repetitive_subreads",
                                                                value = 100),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "Subreads (16bp mers) will be excluded from the index if they occur more times than the selected threshold in the genome",
                                                     placement = "right"
                                                   )
            ))), #close fluidrow
            ## one at a time or all together
            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                   shiny::radioButtons(inputId = nsIndSeq("IndexingOneAll"),
                                                                label = "simultaneous_indexing",
                                                                choices = c("TRUE", "FALSE"), selected = "FALSE"),
                                                   bslib::tooltip(
                                                     bsicons::bs_icon("question-circle"),
                                                     "Your device may not have the capacity to build two indexes simultaneously (setting: 'FALSE'; this process is less computationally intensive but requires more time). Instead, the indexes can be built simultaneously (setting: 'TRUE'; this process is less time-consuming but computationally intensive). 'FALSE' by default.",
                                                     placement = "right"
                                                   )
            ))) #close fluidrow
          ), #close accordion_panel
          data.step = 2,
          data.intro = "Configure advanced options."
        ) #close introbox advanced parameters
      ), #close accordion
      ## indexing genome
      shiny::fluidRow(htmltools::tags$hr(shiny::column(12,align="center",
                                                       htmltools::h4("Step 2 - Run genome indexing: sequential mode"), htmltools::br(), shiny::fluidRow(
                                shiny::actionButton(inputId = nsIndSeq("runIndexingSequential"), label = "Run",
                                             icon = shiny::icon(name = "bookmark"),
                                             class = "btn-primary"),
                                shinyjs::disabled(
                                  shiny::actionButton(nsIndSeq("StopIndexingSequential"), "Kill", icon = shiny::icon("fire"),
                                               class = "btn-warning")
                                )
                              )))

      ) #close fluidrow
    ), #close sidebar
    ##### MAIN PANEL
    # Dropdown button for information about the process
    shinyWidgets::dropdownButton(
      inputId = nsIndSeq("IndexingSequentialMydropdown"),
      label = "Info",
      size = "sm",
      status = "info",
      circle = FALSE,
      htmltools::h3(shiny::strong("Genome indexing process")),
      htmltools::br(),
      htmltools::h5(shiny::strong("WHEN TO PERFORM")),
      htmltools::h5("The genome indexing step should be performed prior to mapping and following the filtering process to create a data structure that will serve as a reference for the mapping. Using a sequential approach, genome indexing generates two separate indexes: one for genome 1 and another for genome 2."),
      htmltools::br(),
      htmltools::h6(shiny::strong("WHAT IT DOES")),
      htmltools::h6("In this step, a data structure (hash table) is created to enhance the efficiency of subsequent alignment processes. This structure is generated using the provided reference genomes (.fasta file extension), which helps streamline the mapping procedure."),
      htmltools::br(),
      htmltools::h6(shiny::strong("OPERATIONAL INSTRUCTIONS")),
      htmltools::h6(shiny::strong("Step 1")),
      htmltools::h6("- Select the first reference genome and the second reference genome (both must be provided in FASTA format)."),
      htmltools::h6("- Choose two separate, empty folders where the indexed genomes (binary files) will be saved."),
      htmltools::h6(shiny::strong("Step 2")),
      htmltools::h6("- Start the analysis by clicking the 'Run' button."),
      htmltools::br(),
      htmltools::h6(shiny::strong("RESULTS")),
      htmltools::h6("The index will be constituted by a single block if its size is smaller than the value specified in the INDEX BLOCK SIZE field (under the ADVANCED section). If the index exceeds this threshold, it will be split into multiple blocks, each sized according to the user-defined parameter. As output, two binary index files (one for each genome) will be saved in the selected folders, along with three accompanying plain text files."),
      htmltools::br(),
      htmltools::h6(shiny::strong("ADDITIONAL NOTES")),
      htmltools::h6("You can permanently kill the analysis by clicking the 'Kill' button."),
      htmltools::h6("Optionally, you can configure advanced parameters, including 'Split Index' and 'Miscellaneous', for further customization."),
      htmltools::br(),
      shiny::column(5, shiny::actionButton("helpParameterIndexing", "About parameters",class = "btn-primary",icon = shiny::icon("sliders"))),
      htmltools::br()
    ),
    htmltools::br(),
    # text output
    shiny::verbatimTextOutput(nsIndSeq("IndexingSequentialProcessstatus")),
    ## reassuring table
    htmltools::br(),
    shiny::textOutput(nsIndSeq("IndexingSequentialReassureTitle1")),
    DT::DTOutput(nsIndSeq("IndexingSequentialReassure1")),
    shiny::textOutput(nsIndSeq("IndexingSequentialReassureTitle2")),
    DT::DTOutput(nsIndSeq("IndexingSequentialReassure2"))
  ) #close layout_sidebar
} # close sequentialIndexingUI function
