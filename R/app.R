
#' inDAGO
#'
#' A Shiny app for dual and bulk RNA‑sequencing analysis.
#'
#' This function allows to launch inDAGO Shiny interface.



#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom Hmisc rcorr
#' @importFrom R.devices capturePlot
#' @importFrom R.devices eps
#' @importFrom S4Vectors FilterRules
#' @importFrom UpSetR upset
#' @importFrom bigtabulate bigtable
#' @importFrom bsicons bs_icon
#' @import bslib
#' @importFrom callr r_bg
#' @importFrom checkmate assert
#' @importFrom data.table fread
#' @importFrom dplyr all_of
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom fs path_home
#' @importFrom fs path_temp
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices jpeg
#' @importFrom grDevices pdf
#' @importFrom grDevices png
#' @importFrom grDevices replayPlot
#' @importFrom grDevices svg
#' @importFrom grDevices tiff
#' @importFrom heatmaply heatmaply
#' @importFrom htmltools HTML
#' @importFrom htmltools br
#' @importFrom htmltools h3
#' @importFrom htmltools h4
#' @importFrom htmltools h5
#' @importFrom htmltools h6
#' @importFrom htmltools strong
#' @importFrom htmltools tagList
#' @importFrom htmltools tags
#' @importFrom magrittr '%>%'
#' @importFrom matrixStats product
#' @importFrom memuse Sys.meminfo
#' @importFrom methods as
#' @import paletteer
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom pheatmap pheatmap
#' @importFrom plotly ggplotly
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#' @importFrom readr read_delim
#' @importFrom readr write_lines
#' @importFrom reshape2 melt
#' @importFrom rintrojs introBox
#' @importFrom rintrojs introjs
#' @importFrom rintrojs introjsUI
#' @importFrom seqinr read.fasta
#' @importFrom seqinr write.fasta
#' @import shiny
#' @importFrom shinyFiles getVolumes
#' @importFrom shinyFiles parseDirPath
#' @importFrom shinyFiles parseFilePaths
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom shinyFiles shinyFileChoose
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom shinyWidgets dropdownButton
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinyWidgets switchInput
#' @importFrom shinyWidgets updatePrettyRadioButtons
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs disable
#' @importFrom shinyjs disabled
#' @importFrom shinyjs enable
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @importFrom shinyjs useShinyjs
#' @importFrom bslib sidebar
#' @importFrom spsComps shinyCatch
#' @importFrom stats as.dist
#' @importFrom stats model.matrix
#' @importFrom stats prcomp
#' @importFrom stats var
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tools file_path_sans_ext
#' @importFrom upsetjs chartLabels
#' @importFrom upsetjs fromList
#' @importFrom upsetjs interactiveChart
#' @importFrom upsetjs upsetjs
#' @importFrom upsetjs renderUpsetjs
#' @importFrom upsetjs upsetjsOutput
#' @importFrom utils capture.output
#' @importFrom utils globalVariables
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom utils write.csv
#' @importFrom grDevices dev.off
#' @importFrom stats median
#' @importFrom utils head write.table
#' @importFrom shiny addResourcePath
#' @export inDAGO
#' @return No return value, called for side effects



inDAGO <- function(){
  
  # check the necessary bioconductor packages
  needed <- c(
    "BiocManager", "XVector", "ShortRead", "S4Vectors", "rtracklayer",
    "Rsubread", "Rsamtools", "Rfastp", "limma", "HTSFilter",
    "edgeR", "Biostrings", "BiocGenerics"
  )

  ## check
  missing <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = FALSE)]

  if (length(missing)) {
    msg <- paste0(
      "The following packages are not installed: ",
      paste(missing, collapse = ", "), ".\n",
      "To enable all the features of ", "inDAGO", ", install with:\n",
      "  if (!requireNamespace('BiocManager', quietly=TRUE))\n",
      "    install.packages('BiocManager')\n",
      "  BiocManager::install(c(",
      paste0("\"", missing, "\"", collapse = ", "), "))\n"
    )
    stop(msg) # stop if there are missing package
  }
  shiny::addResourcePath(
    prefix = "inDAGO",
    directoryPath = system.file("www", package = "inDAGO")
  )

##### Section3: user interface #####
ui <- bslib::page_navbar(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "sketchy",
    bg = NULL,
    fg = NULL,
    primary = "#8DB596",
    secondary = "#BEDBBB",
    success = NULL,
    info = NULL,
    warning = "#f1c40f",
    danger = "#f1948a",
    base_font = "Courier New",
    code_font = NULL,
    heading_font = "Courier New",
    font_scale = 0.9
  ),

  # insert logo and link to github
  title = htmltools::tags$a(
    href = "https://github.com/inDAGOverse/inDAGO",
    htmltools::tags$img(
      src = "inDAGO/favicon-96x96.png",
      height = "80px",
      style = "vertical-align: middle;"
    )),

  window_title = "inDAGO",

  fillable = TRUE,
  selected = "WorkflowNS",
  bslib::nav_spacer(), # Creates space between navigation items


  bslib::nav_panel(
    # Sidebar for quality check operations
    #  introBox to create pop-up text boxes
    rintrojs::introBox(
    htmltools::h6("Quality control"),
    data.step = 3,
    data.intro = "The module is designed to process paired-end reads obtained from the Illumina sequencing platforms in FASTQ format. It extracts essential information from these files and should be run both before and after the filtering step to assess the reliability of the reads and evaluate whether filtering has improved the data quality."
    ),
      qualityControlUserInterface("qualityControlAnalysisNamespacing")
    ),


  # End of nav_panel
  bslib::nav_spacer(),


  bslib::nav_panel(
    #  introBox to create pop-up text boxes
    rintrojs::introBox(
    htmltools::h6("Filtering"),
    data.step = 4,
    data.intro = "The filtering module processes raw sequencing data stored in paired-end FASTQ format, improving read quality through multiple refinement steps."
    ),
      FilteringUserInterface("Filtering")
    ), # End of nav_panel
  bslib::nav_spacer(),


  ## navigation menu for indexing
  bslib::nav_menu(
    #  introBox to create pop-up text boxes
    rintrojs::introBox(
      htmltools::h6("Indexing"),
    data.step = 5,
    data.intro = "The genome indexing module begins with reference genomes in FASTA format (which can be provided in compressed or uncompressed forms using the GNU-Linux gzip utility) and constructs a data structure that enables efficient access to the genome.This module is composed of three sub-modules, allowing users to perform dual RNA-seq analysis in either sequential or combined mode, or to conduct a classical bulk RNA-seq analysis. "
    ),
    # panel for indexing SEQUENTIAL MODE
    bslib::nav_panel(
      title = "Sequential mode",
      IndexingSequentialUserInterface("indexingSeqNameSpacing")
    ), #close nav_panel sequential indexing
    # panel for indexing Combined MODE
    bslib::nav_panel(
      title = "Combined mode",
      IndexingCombinedUserInterface("indexingCombNameSpacing"),
    ), # close nav_panel Combined MODE
    # panel for indexing Bulk MODE
    bslib::nav_panel(
      title = "Bulk mode",
      IndexingBulkUserInterface("indexingBulkNameSpacing")
    ) # close nav_panel Bulk MODE
  ), # chiude nav menu
  bslib::nav_spacer(),


  bslib::nav_menu(
    #  introBox to create pop-up text boxes
    rintrojs::introBox(
      htmltools::h6("Mapping"),
      data.step = 6,
      data.intro = "Similarly to the indexing module, the mapping process varies depending on the chosen analysis approach. In all cases, reads are aligned to the specified reference sequences, generating alignment files in SAM/BAM format, while unmapped reads are output in FASTQ format. "
    ),
    # panel for mapping Sequential MODE
    bslib::nav_panel(
                     title = "Sequential mode",
                     mappingSequentialUserInterface("mappingSeqNameSpacing")
    ), #close nav_panel sequential mode
    # panel for mapping combined MODE
    bslib::nav_panel(
                     title = "Combined mode",
                     mappingCombinedUserInterface("mappingCombNameSpacing")
    ), #close nav_panel Combined mode
    # panel for mapping bulk MODE
    bslib::nav_panel(
      title = "Bulk mode",
      mappingBulkUserInterface("mappingBulkNameSpacing")
    )# close nav_panel bulk mode
  ), # close nav_menu

  bslib::nav_spacer(),
  bslib::nav_panel(
    #  introBox to create pop-up text boxes
    rintrojs::introBox(
    htmltools::h6("Summarization"),
    data.step = 7,
    data.intro = "The summarization generates a count table that reports the number of reads mapped to each selected feature (e.g. 'exon') for each sample. Starting from this step onwards in dual RNA-seq, the organisms can be analysed independently, as the previous module has already differentiated the reads between the two organisms. To operate, the module requires alignment files and an annotation file in GTF/SAF format."
    ),
      SummarizationUserInterface("Summarization")
    ), # End of nav_panel
  bslib::nav_spacer(),


  bslib::nav_panel(
    #  introBox to create pop-up text boxes
    rintrojs::introBox(
    htmltools::h6("Exploratory Data Analysis"),
    data.step = 8,
    data.intro = "The primary objective of Exploratory Data Analysis (EDA) is to examine and interpret data in relation to the specific biological question under investigation. This module generates a series of exploratory plots that are essential for identifying patterns, detecting potential outliers, and visualizing key insights.The required inputs include count matrices generated during the summarization step for each sample, as well as a matrix indicating the group assignments for each sample. Alternatively, users can manually define group assignments though the interface."
    ),
      EDAUserInterface("EDA")
    ), # End of nav_panel
  bslib::nav_spacer(),


  bslib::nav_panel(
    #  introBox to create pop-up text boxes
    rintrojs::introBox(
    htmltools::h6("Detecting DEGs"),
    data.step = 9,
    data.intro = "The final module of the pipeline begins by utilizing the same count data form the EDA module and concludes by generating a list of differentially expressed genes along with key statistical parameters, including log2 fold change, p-value, and adjusted p-value which is output in CSV format. Other plots, such as Volcano and UpSet, are also generated."
    ),
      DEGsUserInterface("DEGs")
    ), # End of nav_panel
  bslib::nav_spacer(),

  bslib::nav_panel(
    # Sidebar for quality check operations
    htmltools::h6(em("About workflow")),
    value = "WorkflowNS",
    WorkflowUserInterface("WorkflowNS")
  ), # End of nav_panel
  bslib::nav_spacer(),

    nav_menu(
      rintrojs::introBox(h6(em("Link")),
                         data.step = 10,
                         data.intro = "More info can be found on the GitHub repository"
      ),
      nav_item(
        htmltools::tags$a(
        shiny::icon("github"),
        "github",
        href = "https://github.com/inDAGOverse/inDAGO",
        target = "_blank"
      )
    )),
  bslib::nav_spacer(),

  # ) # End of nav menu
) # End of page_navbar


##### Section4: server logic #####
server <- function(input, output, session) {

  WorkflowServerLogic("WorkflowNS")

  qualityControlServerLogic("qualityControlAnalysisNamespacing")

  FilteringServerLogic("Filtering")

  IndexingSequentialServerLogic("indexingSeqNameSpacing")

  IndexingCombinedServerLogic("indexingCombNameSpacing")

  IndexingBulkServerLogic("indexingBulkNameSpacing")

  mappingSequentialServerLogic("mappingSeqNameSpacing")

  mappingCombinedServerLogic("mappingCombNameSpacing")

  mappingBulkServerLogic("mappingBulkNameSpacing")

  SummarizationServerLogic("Summarization")

  EDAServerLogic("EDA")

  DEGsServerLogic("DEGs")


} #End of server
##### Section5: Run #####



shiny::shinyApp(ui = ui, server = server)

}



