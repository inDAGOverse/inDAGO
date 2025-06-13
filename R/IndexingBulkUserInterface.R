#' Indexing bulk ui
#'
#' @param id Shiny module identifier
IndexingBulkUserInterface <- function(id) {
    nsIndBulk <- shiny::NS(id)
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

        sidebar = bslib::sidebar(
            width = "23%",
            bslib::accordion(
              rintrojs::introBox(
                bslib::accordion_panel(
                        "BASIC OPTIONS", icon = bsicons::bs_icon("sliders"),
                        shiny::fluidRow(shiny::column(12,align="center",
                                        htmltools::tags$hr(htmltools::h5("Step 1 - Set input/output")),
                                        htmltools::br())),
                        ### UPLOADING GENOME 1
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shinyFiles::shinyFilesButton(nsIndBulk("IndexingGenomeBulk"),label =  "Select reference genome",
                                                                                title = "Please select the reference genome (FASTA format)",
                                                                                icon = shiny::icon("upload"), multiple = FALSE),
                                                               bslib::tooltip(
                                                                 bsicons::bs_icon("question-circle"),
                                                                   "This is the reference genome (provided in FASTA format) which must be indexed prior to the mapping step",
                                                                   placement = "right"
                                                               )
                        ),
                        # Displays the selected input directory path
                        shiny::verbatimTextOutput(nsIndBulk("IndexingPathGenomeBulk")), ##cartella CAMPIONI selezionata
                        )), # close fluidrow
                        ### SELECTING empty folder results
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shinyFiles::shinyDirButton(
                                                                   nsIndBulk("IndexingIndexBulk"),
                                                                   "Select folder genome index",
                                                                   title = "Please select an empty folder where indexed genome will be generated",
                                                                   icon = shiny::icon("download")),
                                                                   bslib::tooltip(
                                                                 bsicons::bs_icon("question-circle"),
                                                                   "This is the empty folder where the indexed genome will be saved.",
                                                                   placement = "right"
                                                               )
                        ),
                        # Displays the selected input directory path
                        shiny::verbatimTextOutput(nsIndBulk("IndexingPathIndexBulk")), ##cartella CAMPIONI selezionata
                        )) # close fluidrow
                    ), # close accordion_panel
                    data.step = 1,
                    data.intro = "Configure input and output settings for indexing analysis"
                ), # close introbox basic options
              rintrojs::introBox(
                bslib::accordion_panel(
                        "ADVANCED OPTIONS", icon = bsicons::bs_icon("sliders"),
                        ## checkbox
                        shiny::fluidRow(htmltools::tags$hr(), shiny::column(width = 12,shiny::splitLayout(cellWidths = c("60%", "30%", "20%"),
                                                                                                          htmltools::h5("Split index"),
                                                                                                          shiny::checkboxInput(
                                                                              inputId = nsIndBulk("BulkIndexActiveSplit"),
                                                                              label = "On/Off",
                                                                              value = FALSE
                                                                          ),
                                                                          bslib::tooltip(
                                                                            bsicons::bs_icon("question-circle"),
                                                                              "Check the box to activate 'Split index' option. 'Off' by default",
                                                                              placement = "right")))), # close fluidRow
                        ## conditional panel
                        shiny::conditionalPanel(
                            condition = "input.BulkIndexActiveSplit == '1'",
                            ns = nsIndBulk,
                            ## split index
                            shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                 shiny::radioButtons(inputId = nsIndBulk("IndexBulkSplitIndex"),
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
                                                                                          shiny::sliderInput(inputId = nsIndBulk("IndexingMemoryBulk"),
                                                                                   label = "Index-block size",
                                                                                   value = (base::round(base::as.numeric(base::sub(pattern = " GiB", replacement = "", x = memuse::Sys.meminfo()$totalram)) * ((1024^3)/(1000^3))) - 4) * 1000,
                                                                                   min = 1000,
                                                                                   max = base::round(base::as.numeric(base::sub(pattern = " GiB", replacement = "", x = memuse::Sys.meminfo()$totalram)) * ((1024^3)/(1000^2))),
                                                                                   step = 1000,
                                                                                   post = "MB"),
                                                                                   bslib::tooltip(
                                                                                     bsicons::bs_icon("question-circle"),
                                                                           "Specify the size of each index block (in MB) to be loaded into RAM during the read mapping process.
                                                                                    It is recommended to reserve a portion of free RAM to allow the execution of the mapping step.
                                                                                    The index will be constituted by a single block if its size is less than the size selected.",
                                                                           placement = "right"
                                                                       )
                                ))) # close fluidrow
                            ) # close shinyjs
                        ), # close conditional_panel
                        # Miscellaneous section header
                        shiny::fluidRow(shiny::column(12,align="center",
                                                      htmltools::tags$hr(htmltools::h5("Miscellaneous")),
                                                      htmltools::br())),
                        ## gapped index
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shiny::radioButtons(nsIndBulk("IndexingGappedBulk"),
                                                                            label = "Gapped Index",
                                                                            choices = c("TRUE", "FALSE"), selected = "FALSE"),
                                                                            bslib::tooltip(
                                                                              bsicons::bs_icon("question-circle"),
                                                                   "Building a gapped index will significantly reduce memory usage, with a modest impact on read mapping time. It is recommended to use a gapped index on personal computers due to their limited memory capacity.",
                                                                   placement = "right"
                                                               )
                        ))), # close fluidrow
                        ## TH subread
                        shiny::fluidRow(shiny::column(width = 12,shiny::splitLayout(cellWidths = c("90%", "10%"),
                                                                                    shiny::numericInput(nsIndBulk("BulkRipetitiveSubreads"),
                                                                            label = "Highly repetitive subreads",
                                                                            value = 100),
                                                                            bslib::tooltip(
                                                                              bsicons::bs_icon("question-circle"),
                                                                   "Subreads (16bp mers) will be excluded from the index if they occur more times than the selected threshold in the genome",
                                                                   placement = "right"
                                                               )
                        ))) # close fluidrow
                    ), # close accordion_panel
                    data.step = 2,
                    data.intro = "Configure advanced options"
                ) # close introbox advanced options
            ), # close accordion
            ## indexing
            shiny::fluidRow(shiny::column(12,align="center",
                                          htmltools::tags$hr(htmltools::h5("Step 2 - Run indexing analysis for bulk RNA-seq")),
                                          htmltools::br())),
            # to see class options see 'Bootstrap documentation', the CSS design system used by shiny
            shiny::fluidRow(
              shiny::actionButton(nsIndBulk("runIndexingBulk"), label = "Run",
                             icon = shiny::icon(name = "bookmark"),
                             class = "btn-primary"),
                ## kill analysis button
                shinyjs::disabled(
                  shiny::actionButton(nsIndBulk("StopIndexingBulk"), "Kill", icon = shiny::icon("fire"),
                                 class = "btn-warning"))
            ) # close fluidrow
        ), # close sidebar
        ##### MAIN PANEL #####
      shinyWidgets::dropdownButton(
            nsIndBulk("IndexingBulkMydropdown"),
            label = "Info",
            size = "sm",
            status = "info",
            circle = FALSE,
            htmltools::h3(shiny::strong("Genome indexing process")),
            htmltools::br(),
            htmltools::h5(shiny::strong("WHEN TO PERFORM")),
            htmltools::h5("The genome indexing step should be performed prior to mapping and following the filtering process to create a data structure that will serve as a reference for the mapping. Here, by adopting the bulk RNA-seq approach, genome indexing creates a single index for the requested genome."),
            htmltools::br(),
            htmltools::h6(shiny::strong("WHAT IT DOES")),
            htmltools::h6("In this step, a data structure (hash table) is created to enhance the efficiency of subsequent alignment processes. This structure is generated using the provided reference genomes (FASTA file extension), which helps streamline the mapping procedure."),
            htmltools::br(),
            htmltools::h6(shiny::strong("OPERATIONAL INSTRUCTIONS")),
            htmltools::h6(shiny::strong("Step 1")),
            htmltools::h6("- Select the reference genome (it must be provided in FASTA format)."),
            htmltools::h6("- Choose an empty folder where the indexed genome (binary files) will be saved."),
            htmltools::h6(shiny::strong("Step 2")),
            htmltools::h6("- Start the analysis by clicking the 'Run' button."),
            htmltools::br(),
            htmltools::h6(shiny::strong("RESULTS")),
            htmltools::h6("A single index, consisting of two binary files, will be generated and saved in the selected folder, along with three accompanying plain text files"),
            htmltools::h6(shiny::strong("ADDITIONAL NOTES")),
            htmltools::h6("You can permanently kill the analysis by clicking the 'Kill' button."),
            htmltools::h6("Optionally, you can configure advanced parameters, including 'Split Index' and 'Miscellaneous', for further customization."),
            htmltools::br(),
            shiny::column(5, shiny::actionButton("helpParameterIndexingBulk", "About parameters",class = "btn-primary",icon = shiny::icon("sliders"))),
            htmltools::br()
        ),
      htmltools::br(),
        # text output
      shiny:: verbatimTextOutput(nsIndBulk("IndexingBulkProcessstatus")),
        ## reassuring table showing updated list of files in the results folder
        DT::DTOutput(nsIndBulk("IndexingBulkReassure"))
    ) # close layout_sidebar
} ## close IndexingBulkUserInterface function
