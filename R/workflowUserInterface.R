#' UI function for workflow module in Shiny application
#' @param id Shiny module identifier

WorkflowUserInterface <- function(id) {

  shiny::fluidPage(
    rintrojs::introjsUI(),

    shiny::div(
      style = "
        # height: 100vh;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        background-color: #f9f9f9;
        gap: 20px;
        text-align: center;
        padding: 0 20px;
        margin: auto;",

      shiny::actionButton(
        inputId = shiny::NS(id,"helpWorkflow"),
        label = "Need help with the steps?",
        icon = shiny::icon("question-circle"),
        class = "btn btn-outline-primary btn-lg"
      ),
      shiny::fluidRow(column(
        width = 12,
        htmltools::tags$h1("Welcome to inDAGO", style = "font-family: 'Courier New', monospace; font-weight: bold; color: #4a6c4a;"),
        htmltools::div(
          style = "
          font-size: 1.2rem;
          font-family: 'Courier New';
          color: #666;
          width: 100%;
          line-height: 1.5;
          text-align: left;
        ",
          htmltools::tags$h5(htmltools::strong("A Shiny app that makes dual and bulk RNA-seq analysis easy, without writing code!")),
          htmltools::br(),
          htmltools::tags$h5("What you'll find inside inDAGO:"),
          htmltools::tags$h6(
            "- an open source, cross-platform graphical user interface, designed with biologists in mind, to perform bulk and dual RNA-seq;"
          ),
          htmltools::tags$h6(
            "- step by step guidance to move through analysis stages, download intermediate results, and generate publication-ready plots;"
          ),
          htmltools::br(),
          htmltools::tags$h6("Dual RNA-seq can run in two ways:"),
          htmltools::tags$h6("- sequential: reads are mapped separately onto each genome;"),
          htmltools::tags$h6("- combined: reads are aligned onto a single merged genome."),
          htmltools::br(),
          htmltools::tags$h6("A laptop with 16 GB of RAM is sufficient to perform each step in a reasonable time."),
          htmltools::br(),
          htmltools::tags$h5(htmltools::strong("Let inDAGO handle the coding, so you can focus on biology!"))
        )
      )),
      htmltools::br(),
      htmltools::br(),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          htmltools::tags$div(
            style = "width: 100%; text-align: center;",
            htmltools::tags$h5("Bulk RNA-seq workflow"),
            htmltools::br(),
          rintrojs::introBox(
            htmltools::tags$div(
              style = "width: 100%; aspect-ratio: 3/2; overflow: hidden;",
              htmltools::tags$img(
                src = "inDAGO/BULKpipeline.png",
                style = "
                  width: 100%;
                  height: 100%;
                  object-fit: contain;
                  border-radius: 10px;
                  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.15);
                "
              )
            ),
            data.step = 1,
            data.intro = "The inDAGO pipeline for bulk RNA-seq analysis consists of seven key steps that trace the full analytical process, ultimately leading to the identification of differentially expressed genes (DEGs) between experimental conditions. It starts with Step 1, where quality control is performed on raw reads, followed by Step 2, which involves filtering these reads to remove low-quality sequences. In Step 3, a single reference genome in FASTA format is indexed to prepare for Step 4, where the filtered reads are aligned to the reference. Step 5 summarizes the mapped reads, typically in SAM or BAM format, according to the biological units (e.g., genes) of the organism. In Step 6, the summarized reads are explored through statistical analysis. Finally, Step 7 identifies the differentially expressed genes."
          )
        )
        ),
        shiny::column(
          width = 6,
          htmltools::tags$div(
            style = "width: 100%; text-align: center;",
            htmltools::tags$h5("Dual RNA-seq workflow"),
            htmltools::br(),
          rintrojs::introBox(
            htmltools::tags$div(
              style = "width: 100%; aspect-ratio: 3/2; overflow: hidden;",
              htmltools::tags$img(
                src = "inDAGO/DUALpipeline.png",
                style = "
                  width: 100%;
                  height: 100%;
                  object-fit: contain;
                  border-radius: 10px;
                  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.15);
                "
              )
            ),
            data.step = 2,
            data.intro = "The inDAGO pipeline for dual RNA-seq analysis offers both sequential and combined approaches. The pipeline consists of seven steps, with steps 1, 2, 5, 6 and 7 being common to both approaches, while steps 3 and 4 are approach-specific. Step 1: quality control of raw mixed reads (organism A + organism B in FASTQ format). Step 2: filtering of raw mixed reads. Step 3: indexing of reference sequences (FASTA format). In the sequential approach, separate indexing is done for each organism, while in the combined approach, a single concatenated genome is indexed. Step 4, 4.1, and 4.2: alignment of filtered reads to the reference genomes. In the sequential approach, a double mapping step is performed (one for each organism), while in the com-bined approach, a single mapping is followed by in silico read discrimination. Step 5: mapped reads (in SAM/BAM format) are summarized according to biological units (e.g., genes) of the organisms. Step 6: summarized reads (in CSV format) are explored through statistical analysis. Step 7: differentially expressed genes (DEGs) are identified."
          )
        )
      )
      ),
      htmltools::br(),
    )
  )
}
