
#' inDAGO

#' @importFrom BiocGenerics end
#' @importFrom BiocGenerics start
#' @importFrom BiocGenerics width
#' @importFrom Biostrings BStringSet
#' @importFrom Biostrings encoding
#' @importFrom Biostrings letterFrequency
#' @importFrom Biostrings quality
#' @importFrom Biostrings readDNAStringSet
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom HTSFilter HTSFilter
#' @importFrom Hmisc rcorr
#' @importFrom R.devices capturePlot
#' @importFrom R.devices eps
#' @importFrom Rfastp catfastq
#' @importFrom Rsamtools BamFile
#' @importFrom Rsamtools ScanBamParam
#' @importFrom Rsamtools filterBam
#' @importFrom Rsamtools indexBam
#' @importFrom Rsamtools scanBam
#' @importFrom Rsamtools scanBamFlag
#' @importFrom Rsamtools scanBamWhat
#' @importFrom Rsamtools sortBam
#' @importFrom Rsubread buildindex
#' @importFrom Rsubread featureCounts
#' @importFrom Rsubread subjunc
#' @importFrom S4Vectors FilterRules
#' @importFrom ShortRead FastqSampler
#' @importFrom ShortRead FastqStreamer
#' @importFrom ShortRead ShortReadQ
#' @importFrom ShortRead alphabetByCycle
#' @importFrom ShortRead countFastq
#' @importFrom ShortRead encoding
#' @importFrom ShortRead narrow
#' @importFrom ShortRead sread
#' @importFrom ShortRead trimEnds
#' @importFrom ShortRead trimLRPatterns
#' @importFrom ShortRead trimTailw
#' @importFrom ShortRead width
#' @importFrom ShortRead writeFastq
#' @importFrom ShortRead yield
#' @importFrom UpSetR upset
#' @importFrom XVector open_input_files
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
#' @importFrom edgeR cpm
#' @importFrom edgeR estimateDisp
#' @importFrom edgeR exactTest
#' @importFrom edgeR filterByExpr
#' @importFrom edgeR glmFit
#' @importFrom edgeR glmLRT
#' @importFrom edgeR glmQLFTest
#' @importFrom edgeR glmQLFit
#' @importFrom edgeR normLibSizes
#' @importFrom edgeR plotBCV
#' @importFrom edgeR plotQLDisp
#' @importFrom edgeR readDGE
#' @importFrom edgeR topTags
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
#' @importFrom limma makeContrasts
#' @importFrom limma plotMDS
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
#' @importFrom rtracklayer import
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
#' @importFrom S4Vectors isEmpty

#' @export inDAGO
#' @param ... other


inDAGO <- function(...){

  # Adds a directory of static resources to Shiny's web server, with the given path prefix.
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
    href = "https://github.com/CarmineFruggiero/inDAGO",
    htmltools::tags$img(
      src = "inDAGO/favicon-96x96.png",
      height = "80px",
      style = "vertical-align: middle;"
    )),

  window_title = "inDAGO",

  fillable = "",
  bslib::nav_spacer(), # Creates space between navigation items

  bslib::nav_panel(
      # Sidebar for quality check operations
    htmltools::h6("Quality control"),
      qualityControlUserInterface("qualityControlAnalysisNamespacing")
    ), # End of nav_panel
  bslib::nav_spacer(),


  bslib::nav_panel(
    htmltools::h6("Filtering"),
      FilteringUserInterface("Filtering")
    ), # End of nav_panel
  bslib::nav_spacer(),


  ## navigation menu for indexing
  bslib::nav_menu(
    title = htmltools::h6("Indexing"),
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
    title = htmltools::h6("Mapping"),
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
    htmltools::h6("Summarization"),
      SummarizationUserInterface("Summarization")
    ), # End of nav_panel
  bslib::nav_spacer(),


  bslib::nav_panel(
    htmltools::h6("Exploratory Data Analysis"),
      EDAUserInterface("EDA")
    ), # End of nav_panel
  bslib::nav_spacer(),


  bslib::nav_panel(
    htmltools::h6("Detecting DEGs"),
      DEGsUserInterface("DEGs")
    ), # End of nav_panel
  bslib::nav_spacer(),


    nav_menu(title = h5("Link"),
      nav_item(
        htmltools::tags$a(
        shiny::icon("github"),
        "github",
        href = "https://github.com/CarmineFruggiero/inDAGO",
        target = "_blank"
      )
    )),
  bslib::nav_spacer(),

  # ) # End of nav menu
) # End of page_navbar


##### Section4: server logic #####
server <- function(input, output, session) {

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



