#' Indexing bulk server logic
#'
#' @param id Shiny module identifier
IndexingBulkServerLogic <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        # documentazione interattiva dei parametri
      shiny::observeEvent(eventExpr = input$helpParameterIndexingBulk,
                     rintrojs::introjs(
                         session = session,
                         options = list(
                             "showBullets" = "TRUE", #Show introduction bullets or not
                             "showProgress" = "TRUE",
                             "showStepNumbers" = "FALSE",
                             "nextLabel" = "Next",
                             "prevLabel" = "Prev",
                             "skipLabel" = "Skip")
                     ))
        ## mostra solo ciò che è opportuno
        shinyjs::hide("IndexingBulkReassure")
        ## attiva pannello per la scomposizione dell'indice
        shiny::observeEvent(eventExpr = input$IndexBulkSplitIndex, {
            if (input$IndexBulkSplitIndex == TRUE) {
                shinyjs::enable("IndexingMemoryBulk")
            } else {
                shinyjs::disable("IndexingMemoryBulk")
            }
        })
        ## selezione dati in ingresso
        volumes <- c(Home = fs::path_home(), "Rinstallation" = R.home(), getVolumes()())
        ##### GENOMA 1 #####
        shinyFiles::shinyFileChoose(input, "IndexingGenomeBulk", roots = volumes, session = session)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$IndexingGenomeBulk value:\n\n")
            base::print(input$IndexingGenomeBulk)
        })
        ## print to browser
        output$IndexingPathGenomeBulk <- shiny::renderPrint({
            if (base::is.integer(input$IndexingGenomeBulk)) {
                base::cat("No file has been selected")
            } else {
                base::cat(
                    base::paste0("Uploaded genome is ",
                                 shinyFiles::parseFilePaths(volumes, input$IndexingGenomeBulk)[1,4]
                    )
                )
            }
        })
        ##### CARTELLA RISULTATI
        shinyFiles::shinyDirChoose(input, "IndexingIndexBulk", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$IndexingGenomeBulk value:\n\n")
            base::print(input$IndexingGenomeBulk)
        })
        output$IndexingPathIndexBulk <- shiny::renderPrint({
            if (base::is.integer(input$IndexingIndexBulk)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Indexed genome will be stored in ",
                                 shinyFiles::parseDirPath(volumes, input$IndexingIndexBulk)
                    )
                )
            }
        })
        ##useful objects
        genome_file_bulk <- shiny::reactive(base::as.character(shinyFiles::parseFilePaths(volumes, input$IndexingGenomeBulk)[1,4]))
        out_path_bulk <- shiny::reactive(shinyFiles::parseDirPath(roots = volumes, selection = input$IndexingIndexBulk))
        outseq <- shiny::reactiveValues()
        AutoUpdatingBulkInd <- shiny::reactiveTimer(2000)
        ##### CLICK RUN 1 #####
        shiny::observeEvent(eventExpr = input$runIndexingBulk, {
            output$IndexingBulkProcessstatus <- shiny::renderPrint({
              shiny::req(outseq$genome)
                AutoUpdatingBulkInd()
                if (outseq$genome$is_alive()) {
                    outseq$genome$print()
                    base::cat("This process could be time-consuming\nFeel free to take a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
                } else {
                    shinyjs::enable("runIndexingBulk")
                    shinyjs::disable("StopIndexingBulk")
                    base::cat("Genome indexing has been finished")
                }
            })
        })
        ##### CLICK RUN 2 #####
        shiny::observeEvent(input$runIndexingBulk, {
            #controlla che le cartelle ci siano come dato in ingresso
            if (base::is.integer(input$IndexingIndexBulk) | base::is.integer(input$IndexingGenomeBulk)) {
              shiny::showNotification(base::paste("Please select the required inputs"),
                                 duration = 10, type = "error")
            }# chiude ciclo if
            # validazione dati necessari che l'utente deve inserire
          shiny::validate(
            shiny::need(expr = parseDirPath(volumes, input$IndexingIndexBulk), message = character(0)),
            shiny::need(expr = input$IndexingGenomeBulk, message = character(0)),
            )
            # rendi disponibile il blocco e non una nuova analisi
          shinyjs::disable("runIndexingBulk")
          shinyjs::enable("StopIndexingBulk")
          shiny::showNotification(base::paste("Genome indexing analysis has been started"), duration = 10,
                             type = "message")
            ## show reassuring messages
          shiny::observe(
                if(base::length(base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndexBulk))) == 0) {
                    AutoUpdatingBulkInd()
                } else {
                    shinyjs::show("IndexingBulkReassure")
                }
            )
            ##analisi
            outseq$genome <- callr::r_bg(
                func = IndexingBulk,
                args = list(
                    basename = base::file.path(out_path_bulk(), "genome_reference_index"),
                    reference = genome_file_bulk(),
                    gappedIndex = base::as.logical(input$IndexingGappedBulk),
                    indexSplit = input$IndexBulkSplitIndex,
                    memory = input$IndexingMemoryBulk,
                    TH_subread = input$BulkRipetitiveSubreads
                )
            )
        }) #chiude observeevent
        ### dai la possibilità di bloccare il programma
        shiny::observeEvent(eventExpr = input$StopIndexingBulk, {
          shinyjs::enable("runIndexingBulk")
          shinyjs::disable("StopIndexingBulk")
            while(outseq$genome$is_alive()) {
                outseq$genome$kill()
            }
            if (outseq$genome$get_exit_status() != 0) {
                output$IndexingBulkProcessstatus <- shiny::renderPrint({
                    base::cat("Genome indexing was killed!")
                })
            }
          shiny::showNotification(paste("Genome indexing was killed!"),
                             duration = 10, type = "warning")
        })#chiude ObservEvent
        ## MESSAGGIO DI RASSICURAZIONE BASATO SUI RISULTATI RIVERSATI NELLA CARTELLA DEI RISULTATI
        ## oggetto che si aggiornano sul contenuto della cartella
        reassuringBulkInd <- shiny::reactivePoll(
            intervalMillis = 1000, session = session,
            checkFunc = function() {
                # this function returns the most recent modification time in the folder
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndexBulk),
                                    full.names = TRUE)
                # per evitare che emetta errore quando la directory e' vuota
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    base::max(info$mtime)
                }
            }, valueFunc = function() {
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndexBulk),
                                    full.names = TRUE)
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    info$size <- info$size / 1e6
                    info$files <- base::basename(base::rownames(info))
                    info <- dplyr::select(info, c("files","size","mtime","ctime")) %>% dplyr::rename(.,"size (mb)"  = "size","mtime (y-m-d_h-m-s)" = "mtime", "ctime (y-m-d_h-m-s)" = "ctime")
                    base::rownames(info) <- NULL
                    info }
            }
        )
        ## tabella che appare all'utente
        output$IndexingBulkReassure <- DT::renderDT(reassuringBulkInd())
    } # close function inside moduleServer
    ) # close moduleServer
} # close IndexingBulkServerLogic function
