#' Mapping sequential server logic
#'
#' @param id Shiny module identifier
mappingSequentialServerLogic <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        ## interactive documentation
        shiny::observeEvent(eventExpr = input$helpParameterMappingSequential,
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
        shinyjs::hide("MappingSequentialReassure")
        shinyjs::hide("MappingSequentialReassure2")
        ## paths
        volumes <- c(Home = fs::path_home(), "Rinstallation" = R.home(), getVolumes()())
        ## FIRST genome index folder selected
        shinyFiles::shinyDirChoose(input, "SeqIndex1", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$SeqIndex1 value:\n\n")
            base::print(input$SeqIndex1)
        })
        output$MappingPathIndexOneSequential <- shiny::renderPrint({
            if (base::is.integer(input$SeqIndex1)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Indexed genome will be stored in ",
                           shinyFiles::parseDirPath(volumes, input$SeqIndex1)
                    )
                )
            }
        })
        ## SECOND genome index folder selected
        shinyFiles::shinyDirChoose(input, "SeqIndex2", roots = volumes, session = session,
                       restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$SeqIndex2 value:\n\n")
            base::print(input$SeqIndex2)
        })
        output$MappingPathIndexTwoSequential <- shiny::renderPrint({
            if (base::is.integer(input$SeqIndex2)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Indexed genome will be stored in ",
                           shinyFiles::parseDirPath(volumes, input$SeqIndex2)
                    )
                )
            }
        })
        # reads folder selected
        shinyFiles::shinyDirChoose(input, "SequentialReads", roots = volumes, session = session,
                       restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$SequentialReads value:\n\n")
            base::print(input$SequentialReads)
        })
        output$MappingPathProcessedReadsSequential <- shiny::renderPrint({
            if (base::is.integer(input$SequentialReads)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("The processed reads are stored in ",
                           shinyFiles::parseDirPath(volumes, input$SequentialReads)
                    )
                )
            }
        })
        ## first BAM folder selected
        shinyFiles::shinyDirChoose(input, "SeqBam1", roots = volumes, session = session,
                       restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$SeqBam1 value:\n\n")
            base::print(input$SeqBam1)
        })
        output$MappingPathOutputOneSequential <- shiny::renderPrint({
            if (base::is.integer(input$SeqBam1)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("First alignment output will be stored in ",
                           shinyFiles::parseDirPath(volumes, input$SeqBam1)
                    )
                )
            }
        })
        ## first BAM folder selected
        shinyFiles::shinyDirChoose(input, "SeqBam2", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$SeqBam2 value:\n\n")
            base::print(input$SeqBam2)
        })
        output$MappingPathOutputTwoSequential <- shiny::renderPrint({
            if (base::is.integer(input$SeqBam2)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Seond alignment output will be stored in ",
                           shinyFiles::parseDirPath(volumes, input$SeqBam2)
                    )
                )
            }
        })
        ## temporary folder
        shinyFiles::shinyDirChoose(input, "MappingTemporaryFolderSequential", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## useful objects
        dirMappingTempFilePathSequential <- shiny::reactiveVal()
        outmapseq <- shiny::reactiveValues()
        ## SLIDER INPUT UPDATING
        ## update slider input for parallel workers
        shiny::observeEvent(eventExpr = input$MappingThreadsSequential, {
            threads <- input$MappingThreadsSequential
            shiny::updateSliderInput(session = session,
                              inputId = "MappingProcessSequential",
                              max = base::as.integer(base::as.integer(parallel::detectCores(logical = TRUE))/threads))
        })
        ## update slider input for subjunc threads
        shiny::observeEvent(eventExpr = input$MappingProcessSequential, {
            workers <- input$MappingProcessSequential
            shiny::updateSliderInput(session = session,
                              inputId = "MappingThreadsSequential",
                              max = base::as.integer(base::as.integer(parallel::detectCores(logical = TRUE))/workers))
        })
        ## starting analysis
        ## ALIGNMENT 1
        AutoUpdatingSeqAlign <- shiny::reactiveTimer(2000)
        shiny::observeEvent(eventExpr = input$runMappingSequential, {
            output$MappingSequentialProcessstatus <- shiny::renderPrint({
                shiny::req(outmapseq$mapping)
                AutoUpdatingSeqAlign()
                if (outmapseq$mapping$is_alive()) {
                    outmapseq$mapping$print()
                    base::cat("This process could be time-consuming\nFeel free to take a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
                } else {
                    shinyjs::enable("runMappingSequential")
                    shinyjs::disable("StopMappingSequential")
                    base::cat("Genome mapping has been finished")
                }
            })
        })
        ## ALIGNMENT 2
        shiny::observeEvent(input$runMappingSequential, {
            # INPUT VALIDATION 1
            if (base::is.integer(input$SeqIndex1) | base::is.integer(input$SeqIndex2) | base::is.integer(input$SequentialReads) | base::is.integer(input$SeqBam1) | base::is.integer(input$SeqBam2)) {
                shiny::showNotification(base::paste("Please select requested folders"),
                                 duration = 10, type = "error")
            }# close if statement
            # input validation 2
            shiny::validate(
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$SeqIndex1), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$SeqIndex2), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$SequentialReads), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$SeqBam1), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$SeqBam2), message = character(0))
            )
            # enabling/disabling
            shinyjs::disable("runMappingSequential")
            shinyjs::enable("StopMappingSequential")
            shiny::showNotification(base::paste("Genome mapping analysis has been started"), duration = 10,
                             type = "message")
            ## temporary folder
            if (base::is.integer(input$MappingTemporaryFolderSequential) == FALSE){
                dir <- shinyFiles::parseDirPath(volumes, input$MappingTemporaryFolderSequential)
                ## Set the value by calling with an argument
                dirMappingTempFilePathSequential(dir)
            } else {
                if (file.exists(base::file.path(fs::path_temp(),"TempDirSum_3738"))){
                    dir <- base::file.path(fs::path_temp(),"TempDirSum_3738")
                    dirMappingTempFilePathSequential(dir)
                } else {
                    dir.create(base::file.path(fs::path_temp(),"TempDirSum_3738"))
                    dir <- base::file.path(fs::path_temp(),"TempDirSum_3738")
                    dirMappingTempFilePathSequential(dir)
                }
            }
            ## paths
            processedReadsPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$SequentialReads)
            firstIndexPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$SeqIndex1)
            secondIndexPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$SeqIndex2)
            firstOutputPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$SeqBam1)
            secondOutputPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$SeqBam2)
            lista <- base::sub(pattern = "_1.fastq$|_1.fq$|_2.fastq$|_2.fq$|_1.fastq.gz$|_1.fq.gz$|_2.fastq.gz$|_2.fq.gz$",
                         replacement = "",
                         x = base::list.files(path = processedReadsPath,
                                        pattern = "_1.fastq$|_1.fq$|_2.fastq$|_2.fq$|_1.fastq.gz$|_1.fq.gz$|_2.fastq.gz$|_2.fq.gz$"))
            lista <- base::unique(lista)
            ## reassuring
            shiny::observe(
                if(base::length(base::list.files(shinyFiles::parseDirPath(volumes, input$SeqBam1))) == 0) {
                    AutoUpdatingSeqAlign()
                } else {
                    shinyjs::show("MappingSequentialReassure")
                }
            )
            ##
            shiny::observe(
                if(base::length(base::list.files(shinyFiles::parseDirPath(volumes, input$SeqBam2))) == 0) {
                    AutoUpdatingSeqAlign()
                } else {
                    shinyjs::show("MappingSequentialReassure2")
                }
            )
            ## analisi
            outmapseq$mapping <- callr::r_bg(
                func = SequentialAlignment,
                args = list(
                    lalista = lista,
                    nodes = input$MappingProcessSequential,
                    readsPath = processedReadsPath,
                    GenomeFirstIndex = base::file.path(firstIndexPath, "genome_reference_index"),
                    GenomeSecondIndex = base::file.path(secondIndexPath, "genome_reference_index"),
                    outBam1 = firstOutputPath,
                    outBam2 = secondOutputPath,
                    threads = base::as.integer(input$MappingThreadsSequential),
                    outFormat = base::as.character(input$MappingFormatSequential),
                    phredScore = base::as.integer(input$MappingSeqPhredScore),
                    maxExtractedSubreads = base::as.integer(input$MappingNumberSubreadsSequential),
                    consensusVote = base::as.integer(input$MappingConsensusThresholdSequential),
                    mismatchMax = base::as.integer(input$MappingMaxMismatchSequential),
                    uniqueOnly = base::as.logical(input$MappingUniqueOnlySequential),
                    maxMultiMapped = base::as.integer(input$MappingMultiMapMaxSequential),
                    indelLength = base::as.integer(input$MappingIndelLengthSequential),
                    fragmentMinLength = base::as.integer(input$MappingMinFragLengthSequential),
                    fragmentMaxLength = base::as.integer(input$MappingMaxFragLengthSequential),
                    matesOrientation = base::as.character(input$MappingPeOrientationSequential),
                    readOrderConserved = base::as.logical(input$MappingKeepOrderSequential),
                    coordinatesSorting = base::as.logical(input$MappingSortByCoordinatesSequential),
                    allJunctions = base::as.logical(input$MappingAllJunctionsSequential),
                    tempfolder = base::file.path(dirMappingTempFilePathSequential()),
                    readsAlignedBlock = base::as.integer(input$SubsetBam)
                )
            )
        }) ## close observeevent
        ## killing analysis
        shiny::observeEvent(eventExpr = input$StopMappingSequential, {
            shinyjs::enable("runMappingSequential")
            shinyjs::disable("StopMappingSequential")
            ## killing
            while(outmapseq$mapping$is_alive()) {
                outmapseq$mapping$kill()
            }
            ## deleting temporary folder
            if (base::file.exists(dirMappingTempFilePathSequential()) && base::is.integer(input$MappingTemporaryFolderSequential) == TRUE) {
                base::unlink(dirMappingTempFilePathSequential(), recursive = TRUE)
            }
            if (outmapseq$mapping$get_exit_status() != 0) {
                output$MappingSequentialProcessstatus <- shiny::renderPrint({
                    base::cat("Genome mapping was killed!")
                })
                shiny::showNotification(base::paste("Genome indexing process has been killed!"),
                                 duration = 10, type = "warning")
            }
        })
        ## reassuring table
        ## updating object
        reassuringSeqAlign1 <- shiny::reactivePoll(
            intervalMillis = 1000, session = session,
            checkFunc = function() {
                # this function returns the most recent modification time in the folder
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$SeqBam1),
                                    full.names = TRUE)
                # per evitare che emetta errore quando la directory e' vuota
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    max(info$mtime)
                }
            }, valueFunc = function() {
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$SeqBam1),
                                    full.names = TRUE)
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    info$size <- info$size / 1e6
                    info$files <- base::basename(base::rownames(info))
                    info <- dplyr::select(info, c(
                        "files",
                        "size","mtime","ctime")) %>% dplyr::rename("List of first alignment results" = "files","size (mb)"  = "size","mtime (y-m-d_h-m-s)" = "mtime", "ctime (y-m-d_h-m-s)" = "ctime")
                    base::rownames(info) <- NULL
                    info }
            }
        )
        ##
        reassuringSeqAlign2 <- shiny::reactivePoll(
            intervalMillis = 1000, session = session,
            checkFunc = function() {
                # this function returns the most recent modification time in the folder
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$SeqBam2),
                                    full.names = TRUE)
                # avoiding error printing when directory is empty
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    max(info$mtime)
                }
            }, valueFunc = function() {
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$SeqBam2),
                                    full.names = TRUE)
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    info$size <- info$size / 1e6
                    info$files <- base::basename(base::rownames(info))
                    info <- dplyr::select(info, c(
                        "files",
                        "size","mtime","ctime")) %>% dplyr::rename("List of second alignment results" = "files","size (mb)"  = "size","mtime (y-m-d_h-m-s)" = "mtime", "ctime (y-m-d_h-m-s)" = "ctime")
                    base::rownames(info) <- NULL
                    info }
            }
        )
        ## reassuring table
        output$MappingSequentialReassure <- DT::renderDT(reassuringSeqAlign1())
        output$MappingSequentialReassure2 <- DT::renderDT(reassuringSeqAlign2())
    } # close function inside moduleServer
    ) # close moduleServer
} # close mappingSequentialServer function
