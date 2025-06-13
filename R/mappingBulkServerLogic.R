#' Mapping bulk server logic
#'
#' @param id Shiny module identifier
mappingBulkServerLogic <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        shiny::observeEvent(eventExpr = input$helpParameterMappingBulk,
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
        ## hide/show
        shinyjs::hide("MappingBulkReassure")
        ## paths
        volumes <- c(Home = fs::path_home(), "Rinstallation" = R.home(), getVolumes()())
        ## genome index folder selected
        shinyFiles::shinyDirChoose(input, "BulkIndex", roots = volumes, session = session,
                       restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$BulkIndex value:\n\n")
            print(input$BulkIndex)
        })
        output$MappingPathIndexBulk <- shiny::renderPrint({
            if (base::is.integer(input$BulkIndex)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Indexed genome is stored in ",
                           shinyFiles::parseDirPath(volumes, input$BulkIndex)
                    )
                )
            }
        })
        # reads folder selected
        shinyFiles::shinyDirChoose(input, "BulkReads", roots = volumes, session = session,
                       restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$BulkReads value:\n\n")
            print(input$BulkReads)
        })
        output$MappingPathProcessedReadsBulk <- shiny::renderPrint({
            if (base::is.integer(input$BulkReads)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("The processed reads are stored in ",
                           shinyFiles::parseDirPath(volumes, input$BulkReads)
                    )
                )
            }
        })
        ## first BAM folder selected
        shinyFiles::shinyDirChoose(input, "BulkBam", roots = volumes, session = session,
                       restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$BulkBam value:\n\n")
            base::print(input$BulkBam)
        })
        output$MappingPathOutputBulk <- shiny::renderPrint({
            if (base::is.integer(input$BulkBam)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("The mapping results are stored in ",
                           shinyFiles::parseDirPath(volumes, input$BulkBam)
                    )
                )
            }
        })
        ## temporary folder
        shinyFiles::shinyDirChoose(input, "MappingTemporaryFolderBulk", roots = volumes, session = session,
                       restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        ## print path
        shiny::observe({
            base::cat("\ninput$MappingTemporaryFolderBulk value:\n\n")
            base::print(input$MappingTemporaryFolderBulk)
        })
        output$MappingPathTempBulk <- shiny::renderPrint({
            if (base::is.integer(input$MappingTemporaryFolderBulk)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Temporary files are transiently stored in ",
                           shinyFiles::parseDirPath(volumes, input$MappingTemporaryFolderBulk)
                    )
                )
            }
        })

        ## useful objects
        dirMappingTempFilePathSequential <- shiny::reactiveVal()
        outmapseq <- shiny::reactiveValues()
        ## SLIDER INPUT UPDATING
        ## vengono osservate le azioni dell'utente prima dell'analisi
        ## update slider input for parallel workers
        shiny::observeEvent(eventExpr = input$MappingThreadsBulk, {
            threads <- input$MappingThreadsBulk
            shiny::updateSliderInput(session = session,
                              inputId = "MappingProcessBulk",
                              max = base::as.integer(base::as.integer(parallel::detectCores(logical = TRUE))/threads))
        })
        ## update slider input for subjunc threads
        shiny::observeEvent(eventExpr = input$MappingProcessBulk, {
            workers <- input$MappingProcessBulk
            shiny::updateSliderInput(session = session,
                              inputId = "MappingThreadsBulk",
                              max = base::as.integer(base::as.integer(parallel::detectCores(logical = TRUE))/workers))
        })
        ## analysis
        AutoUpdatingBulkAlign <- shiny::reactiveTimer(2000)
        shiny::observeEvent(eventExpr = input$RunBulk, {
            output$MappingBulkProcessstatus <- shiny::renderPrint({
                req(outmapseq$mapping)
                AutoUpdatingBulkAlign()
                if (outmapseq$mapping$is_alive()) {
                    outmapseq$mapping$print()
                    base::cat("This process could be time-consuming\nFeel free to take a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
                } else {
                    shinyjs::enable("RunBulk")
                    shinyjs::disable("StopMappingBulk")
                    base::cat("Genome mapping has been finished!")
                }
            })
        })
        ## analysis complementary
        shiny::observeEvent(input$RunBulk, {
            if (base::is.integer(input$BulkIndex) | base::is.integer(input$BulkReads) | base::is.integer(input$BulkBam)) {
                shiny::showNotification(paste("Please select the required folders"),
                                 duration = 10, type = "error")
            }# close if statement
            # input validation
            shiny::validate(
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$BulkIndex), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$BulkReads), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$BulkBam), message = character(0))
            )
            # enable/disable
            shinyjs::disable("RunBulk")
            shinyjs::enable("StopMappingBulk")
            showNotification(paste("Genome mapping analysis has been started"), duration = 10,
                             type = "message")
            ## temporary folder generation
            if (base::is.integer(input$MappingTemporaryFolderBulk) == FALSE){
                dir <- shinyFiles::parseDirPath(volumes, input$MappingTemporaryFolderBulk)
                ## Set the value by calling with an argument
                dirMappingTempFilePathSequential(dir)
            } else {
                if (base::file.exists(base::file.path(fs::path_temp(),"TempDirSum_3738"))){
                    dir <- base::file.path(fs::path_temp(),"TempDirSum_3738")
                    dirMappingTempFilePathSequential(dir)
                } else {
                    dir.create(base::file.path(fs::path_temp(),"TempDirSum_3738"))
                    dir <- base::file.path(fs::path_temp(),"TempDirSum_3738")
                    dirMappingTempFilePathSequential(dir)
                }
            }
            ## paths
            processedReadsPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$BulkReads)
            IndexPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$BulkIndex)
            OutputPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$BulkBam)
            lista <- base::sub(pattern = "_1.fastq$|_1.fq$|_2.fastq$|_2.fq$",
                         replacement = "",
                         x = base::list.files(path = processedReadsPath,
                                        pattern = "_1.fastq$|_1.fq$|_2.fastq$|_2.fq$"))
            lista <- base::unique(lista)
            ## reassuring table
            shiny::observe(
                if(base::length(base::list.files(shinyFiles::parseDirPath(volumes, input$BulkBam))) == 0) {
                    AutoUpdatingBulkAlign()
                } else {
                    shinyjs::show("MappingBulkReassure")
                }
            )
            ## analysis
            outmapseq$mapping <- callr::r_bg(
                func = BulkAlignment,
                args = list(
                    lalista = lista,
                    nodes = input$MappingProcessBulk,
                    readsPath = processedReadsPath,
                    GenomeIndex = base::file.path(IndexPath, "genome_reference_index"),
                    outBam = OutputPath,
                    threads = base::as.integer(input$MappingThreadsBulk),
                    outFormat = base::as.character(input$MappingFormatBulk),
                    phredScore = base::as.integer(input$MappingBulkPhredScore),
                    maxExtractedSubreads = base::as.integer(input$MappingNumberSubreadsBulk),
                    consensusVote = base::as.integer(input$MappingConsensusThresholdBulk),
                    mismatchMax = base::as.integer(input$MappingMaxMismatchBulk),
                    uniqueOnly = as.logical(input$MappingUniqueOnlyBulk),
                    maxMultiMapped = base::as.integer(input$MappingMultiMapMaxBulk),
                    indelLength = base::as.integer(input$MappingIndelLengthBulk),
                    fragmentMinLength = base::as.integer(input$MappingMinFragLengthBulk),
                    fragmentMaxLength = base::as.integer(input$MappingMaxFragLengthBulk),
                    matesOrientation = base::as.character(input$MappingPeOrientationBulk),
                    readOrderConserved = as.logical(input$MappingKeepOrderBulk),
                    coordinatesSorting = as.logical(input$MappingSortByCoordinatesBulk),
                    allJunctions = as.logical(input$MappingAllJunctionsBulk),
                    tempfolder = base::file.path(dirMappingTempFilePathSequential())
                )
            )
        }) ## close observeevent analysis
        ## killing
        shiny::observeEvent(eventExpr = input$StopMappingBulk, {
            shinyjs::enable("RunBulk")
            shinyjs::disable("StopMappingBulk")
            ## killing
            while(outmapseq$mapping$is_alive()) {
                outmapseq$mapping$kill()
            }
            ## deleting temporary folder
            if (base::file.exists(dirMappingTempFilePathSequential()) && base::is.integer(input$MappingTemporaryFolderBulk) == TRUE) {
                base::unlink(dirMappingTempFilePathSequential(), recursive = TRUE)
            }## reassuring message
            if (outmapseq$mapping$get_exit_status() != 0) {
                output$MappingBulkProcessstatus <- shiny::renderPrint({
                    base::cat("Genome mapping was killed!")
                })
            }
            shiny::showNotification(paste("Genome indexing process has been killed!"),
                             duration = 10, type = "warning")
        })
        ## reassuring table
        ## updating object
        reassuringBulkAlign <- shiny::reactivePoll(
            intervalMillis = 1000, session = session,
            checkFunc = function() {
                # this function returns the most recent modification time in the folder
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$BulkBam),
                                    full.names = TRUE)
                # avoid error print when directory is empty
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    base::max(info$mtime)
                }
            }, valueFunc = function() {
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$BulkBam),
                                    full.names = TRUE)
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    info$size <- info$size / 1e6
                    info$files <- base::basename(base::rownames(info))
                    info <- dplyr::select(info, c(
                        "files",
                        "size","mtime","ctime")) %>% dplyr::rename("List of alignment results" = "files","size (mb)"  = "size","mtime (y-m-d_h-m-s)" = "mtime", "ctime (y-m-d_h-m-s)" = "ctime")
                    base::rownames(info) <- NULL
                    info }
            }
        )
        ## reassuring table
        output$MappingBulkReassure <- DT::renderDT(reassuringBulkAlign())
    } # close function inside moduleServer
    ) # close moduleServer
} # close mappingSequentialServer function