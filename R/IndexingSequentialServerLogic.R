#' Indexing sequential server logic
#'
#' @param id Shiny module identifier
IndexingSequentialServerLogic <- function(id) {
  shiny::moduleServer(id, function(input,output,session) {
    ## interactive documentation
      shiny::observeEvent(eventExpr = input$helpParameterIndexing,
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
    ## showing/hiding
    shinyjs::hide("IndexingSequentialReassureTitle1")
    shinyjs::hide("IndexingSequentialReassureTitle2")
    shinyjs::hide("IndexingSequentialReassure1")
    shinyjs::hide("IndexingSequentialReassure2")

    shiny::observeEvent(eventExpr = input$IndexSeqSplitIndex, {
      if (input$IndexSeqSplitIndex == TRUE) {
        shinyjs::enable("IndexingMemory")
      } else {
        shinyjs::disable("IndexingMemory")
      }
    })

    ## input selection
    volumes <- c(Home = fs::path_home(), "Rinstallation" = R.home(), getVolumes()())
    ##### GENOMe 1 #####
    shinyFiles::shinyFileChoose(input, "IndexingGenome1seq", roots = volumes, session = session)
    ## print to console to see how the value of the shinyFiles
    ## button changes after clicking and selection
    shiny::observe({
      base::cat("\ninput$IndexingGenome1seq value:\n\n")
      base::print(input$IndexingGenome1seq)
    })
    ## print to browser
    output$IndexingPathGenome1seq <- shiny::renderPrint({
      if (base::is.integer(input$IndexingGenome1seq)) {
          base::cat("No file has been selected")
      } else {
          base::cat(
              base::paste0("Uploaded genome is ",
                           shinyFiles::parseFilePaths(volumes, input$IndexingGenome1seq)[1,4]
          )
        )
      }
    })
    shinyFiles::shinyDirChoose(input, "IndexingIndex1seq", roots = volumes, session = session,
                   restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
    ## print to console to see how the value of the shinyFiles
    ## button changes after clicking and selection
    shiny::observe({
        base::cat("\ninput$IndexingGenome1seq value:\n\n")
        base::print(input$IndexingGenome1seq)
    })
    output$IndexingPathIndex1seq <- shiny::renderPrint({
      if (base::is.integer(input$IndexingIndex1seq)) {
          base::cat("No directory has been selected")
      } else {
          base::cat(
              base::paste0("Indexed genome will be stored in ",
                           shinyFiles::parseDirPath(volumes, input$IndexingIndex1seq)
          )
        )
      }
    })

    ##### GENOMe 2 #####
    shinyFiles::shinyFileChoose(input, "IndexingGenome2seq", roots = volumes, session = session)
    ## print to console to see how the value of the shinyFiles
    ## button changes after clicking and selection
    shiny::observe({
        base::cat("\ninput$IndexingGenome2seq value:\n\n")
        base::print(input$IndexingGenome2seq)
    })
    ## print to browser
    output$IndexingPathGenome2seq <- shiny::renderPrint({
      if (base::is.integer(input$IndexingGenome2seq)) {
          base::cat("No file has been selected")
      } else {
          base::cat(
              base::paste0("Uploaded genome is ",
                           shinyFiles::parseFilePaths(volumes, input$IndexingGenome2seq)[1,4]
          )
        )
      }
    })
    shinyFiles::shinyDirChoose(input, "IndexingIndex2seq", roots = volumes, session = session,
                   restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
    ## print to console to see how the value of the shinyFiles
    ## button changes after clicking and selection
    shiny::observe({
        base::cat("\ninput$IndexingGenome2seq value:\n\n")
        base::print(input$IndexingGenome2seq)
    })
    output$IndexingPathIndex2seq <- shiny::renderPrint({
      if (base::is.integer(input$IndexingIndex2seq)) {
          base::cat("No directory has been selected")
      } else {
          base::cat(
              base::paste0("Indexed genome will be stored in ",
                           shinyFiles::parseDirPath(volumes, input$IndexingIndex2seq)
          )
        )
      }
    })
    ##### running analysis
    # path
    out_path1seq <- shiny::reactive(shinyFiles::parseDirPath(roots = volumes, selection = input$IndexingIndex1seq))
    genome_file1_seq <- shiny::reactive(base::as.character(shinyFiles::parseFilePaths(volumes, input$IndexingGenome1seq)[1,4]))
    out_path2seq <- shiny::reactive(parseDirPath(roots = volumes, selection = input$IndexingIndex2seq))
    genome_file2_seq <- shiny::reactive(base::as.character(shinyFiles::parseFilePaths(volumes, input$IndexingGenome2seq)[1,4]))
    ## genome indexing progress
    AutoUpdatingSeqIndex <- shiny::reactiveTimer(2000)
    shiny::observeEvent(eventExpr = input$runIndexingSequential, {
      if(base::as.logical(input$IndexingOneAll)) {
        ## progressive analysis
        output$IndexingSequentialProcessstatus <- shiny::renderPrint({
            shiny::req(outseq$genomeprogressive)
          AutoUpdatingSeqIndex()
          if (outseq$genomeprogressive$is_alive()) {
            outseq$genomeprogressive$print()
            cat("This process could be time-consuming\nFeel free to take a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
          } else {
            ## parallel analysis
            shinyjs::disable("StopIndexingSequential")
            shinyjs::enable("runIndexingSequential")
            base::cat("Genome indexing has been finished!")
          }
        })
      } else {
        output$IndexingSequentialProcessstatus <- shiny::renderPrint({
            shiny::req(outseq$genome1)
            shiny::req(outseq$genome2)
          AutoUpdatingSeqIndex()
          if (outseq$genome1$is_alive() | outseq$genome2$is_alive()) {
            outseq$genome1$print()
            outseq$genome2$print()
            base::cat("This process could be time-consuming\nTake a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
          } else {
            shinyjs::disable("StopIndexingSequential")
            shinyjs::enable("runIndexingSequential")
            base::cat("Genome indexing is complete!")
          }
        })
      }
    })

    outseq <- shiny::reactiveValues()
    shiny::observeEvent(input$runIndexingSequential, {
      # input validation 1
      if (base::is.integer(input$IndexingIndex1seq) | base::is.integer(input$IndexingIndex2seq) | base::is.integer(input$IndexingGenome1seq) | base::is.integer(input$IndexingGenome2seq)) {
          shiny::showNotification(paste("Please select the required inputs"),
                         duration = 10, type = "error")
      }# close if statement
      # input validation 2
        shiny::validate(
            shiny::need(expr = parseDirPath(volumes, input$IndexingIndex1seq), message = character(0)),
            shiny::need(expr = parseDirPath(volumes, input$IndexingIndex2seq), message = character(0)),
            shiny::need(expr = input$IndexingGenome1seq, message = character(0)),
            shiny::need(expr = input$IndexingGenome2seq, message = character(0))
      )
      # killing analysis
        shinyjs::disable("runIndexingSequential")
        shinyjs::enable("StopIndexingSequential")
        shiny::showNotification(base::paste("Genome indexing analysis has been started"), duration = 10,
                       type = "message")
      ## show reassuring messages
        shiny::observe(
        if(length(list.files(parseDirPath(volumes, input$IndexingIndex1seq))) == 0) {
          AutoUpdatingSeqIndex()
        } else {
          shinyjs::show("IndexingSequentialReassureTitle1")
          shinyjs::show("IndexingSequentialReassure1")
          shinyjs::show("IndexingSequentialReassureTitle2")
          shinyjs::show("IndexingSequentialReassure2")
        }
      )
      ##
        shiny::observe(
        if(length(list.files(parseDirPath(volumes, input$IndexingIndex2seq))) == 0) {
          AutoUpdatingSeqIndex()
        } else {
          shinyjs::show("IndexingSequentialReassureTitle2")
          shinyjs::show("IndexingSequentialReassure2")
        }
      )
      # analysis
      if(base::as.logical(input$IndexingOneAll)) {
        tryCatch({
          outseq$genomeprogressive <- callr::r_bg(
            func = IndexingSequentialProgressive,
            args = list(
              gappedIndex = as.logical(input$IndexingGapped),
              indexSplit = input$IndexSeqSplitIndex,
              memory = input$IndexingMemory,
              TH_subread = input$RipetitiveSubreads,
              outfolder1 = file.path(out_path1seq(), "genomeOne_reference_index"),
              outfolder2 = file.path(out_path2seq(), "genomeTwo_reference_index"),
              refgen1 = genome_file1_seq(),
              refgen2 = genome_file2_seq()
            )
          )
        }, error = function(e) {
            shiny::showNotification(
            ui = "error occurs - check if files with the name of the samples already exist in selected folder",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }
        ) # close trycatch
      } else {
          base::tryCatch({
          outseq$genome1 <- callr::r_bg(
            func = IndexingSequentialParallel,
            args = list(
              basename = file.path(out_path1seq(), "genome_reference_index"),
              reference = genome_file1_seq(),
              gappedIndex = as.logical(input$IndexingGapped),
              indexSplit = input$IndexSeqSplitIndex,
              memory = input$IndexingMemory,
              TH_subread = input$RipetitiveSubreads
            )
          )
        }, error = function(e) {
            shiny::showNotification(
            ui = "error occurs - check if files with the name of the samples already exist in selected folder",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }
        ) # close trycatch
          

          base::tryCatch({
          outseq$genome2 <- callr::r_bg(
            func = IndexingSequentialParallel,
            args = list(
              basename =file.path(out_path2seq(), "genome_reference_index"),
              reference = genome_file2_seq(),
              gappedIndex = as.logical(input$IndexingGapped),
              indexSplit = input$IndexSeqSplitIndex,
              memory = input$IndexingMemory,
              TH_subread = input$RipetitiveSubreads
            )#close list argument
          )#close callr
        }, error = function(e) {
            shiny::showNotification(
            ui = "error occurs - check if files with the name of the samples already exist in selected folder",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }
        ) # close trycatch
      } #close else
    }) #close observeevent
    ### killing analysis
    shiny::observeEvent(eventExpr = input$StopIndexingSequential, {
        shinyjs::enable("runIndexingSequential")
        shinyjs::disable("StopIndexingSequential")
      if(base::as.logical(input$IndexingOneAll)) {
        while(outseq$genomeprogressive$is_alive()) {
          outseq$genomeprogressive$kill()
        }
        if (outseq$genomeprogressive$get_exit_status() != 0) {
          output$IndexingSequentialProcessstatus <- shiny::renderPrint({
            base::cat("Genome indexing was killed!")
          })
        }
          shiny::showNotification(base::paste("Genome indexing process has been killed!"),
                         duration = 10, type = "warning")
      } else {
        while(outseq$genome1$is_alive() | outseq$genome2$is_alive()) {
          outseq$genome1$kill()
          outseq$genome2$kill()
        }
        if (outseq$genome1$get_exit_status() != 0 | outseq$genome2$get_exit_status() != 0) {
          output$IndexingSequentialProcessstatus <- shiny::renderPrint({
            base::cat("Genome indexing was killed!")
          })
        }
          shiny::showNotification(paste("Genome indexing process has been killed!"),
                         duration = 10, type = "warning")
      }
    })#close ObservEvent
    ## reassuring table
    ## reassuring titles
    output$IndexingSequentialReassureTitle1 <- shiny::renderText({
      base::paste("Updated list of results in",
            parseDirPath(volumes, input$IndexingIndex1seq), sep = " ")})
    output$IndexingSequentialReassureTitle2 <- renderText({
      paste("Updated list of results in",
            shinyFiles::parseDirPath(volumes, input$IndexingIndex2seq), sep = " ")})
    ## updating reassuring object
    reassuringSeqInd1 <- shiny::reactivePoll(
      intervalMillis = 1000, session = session,
      checkFunc = function() {
        # this function returns the most recent modification time in the folder
        files <- base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndex1seq),
                            full.names = TRUE)
        # per evitare che emetta errore quando la directory e' vuota
        if (!length(files) == 0) {
          info <- base::file.info(files)
          base::max(info$mtime)
        }
      }, valueFunc = function() {
        files <- base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndex1seq),
                            full.names = TRUE)
        if (!length(files) == 0) {
          info <- base::file.info(files)
          info$size <- info$size / 1e6
          info$files <- base::basename(base::rownames(info))
          info <- dplyr::select(info, c("files","size","mtime","ctime")) %>% dplyr::rename(.,"size (mb)"  = "size","mtime (y-m-d_h-m-s)" = "mtime", "ctime (y-m-d_h-m-s)" = "ctime")
          base::rownames(info) <- NULL
          info }
      }
    )
    reassuringSeqInd2 <- reactivePoll(
      intervalMillis = 1000, session = session,
      checkFunc = function() {
        # this function returns the most recent modification time in the folder
        files <- base::list.files(parseDirPath(volumes, input$IndexingIndex2seq),
                            full.names = TRUE)
        # avoiding error print where folder is empty
        if (!base::length(files) == 0) {
          info <- base::file.info(files)
          base::max(info$mtime)
        }
      }, valueFunc = function() {
        files <- base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndex2seq),
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
    ## reassuring table
    output$IndexingSequentialReassure1 <- DT::renderDT(reassuringSeqInd1())
    output$IndexingSequentialReassure2 <- DT::renderDT(reassuringSeqInd2())
  } # close server logic function
  ) # close moduleServer
} # close server function
