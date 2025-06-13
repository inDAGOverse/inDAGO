#' Mapping combined server logic
#'
#' @param id Shiny module identifier
mappingCombinedServerLogic <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        ## interactive documentation
        shiny::observeEvent(eventExpr = input$helpParameterMappingCombined,
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
        ##### reactive values
        AutoUpdatingCombAlign <- shiny::reactiveTimer(2000)
        ## hiding/showing
        shinyjs::hide("MappingCombinedReassure")
        ## paths
        volumes <- c(Home = fs::path_home(), "Rinstallation" = R.home(), getVolumes()())
        ## genome index folder selected
        shinyFiles::shinyDirChoose(input, "CombIndex", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$CombIndex value:\n\n")
            print(input$CombIndex)
        })
        output$MappingPathConcatenatedIndexCombined <- shiny::renderPrint({
            if (base::is.integer(input$CombIndex)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Indexed concatenated genome is stored in ",
                           shinyFiles::parseDirPath(volumes, input$CombIndex)
                    )
                )
            }
        })
        ## reads folder selected
        shinyFiles::shinyDirChoose(input, "CombinedReads", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$CombinedReads value:\n\n")
            print(input$CombinedReads)
        })
        output$MappingPathReadsCombined <- shiny::renderPrint({
            if (base::is.integer(input$CombinedReads)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Processed reads are stored in ",
                           shinyFiles::parseDirPath(volumes, input$CombinedReads)
                    )
                )
            }
        })
        ## BAM folder selected
        shinyFiles::shinyDirChoose(input, "CombBam", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$CombBam value:\n\n")
            print(input$CombBam)
        })
        output$MappingPathOutBamCombined <- shiny::renderPrint({
            if (base::is.integer(input$CombBam)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Mapping results are stored in ",
                           shinyFiles::parseDirPath(volumes, input$CombBam)
                    )
                )
            }
        })
        ## temporary folder selected
        shinyFiles::shinyDirChoose(input, "MappingTemporaryFolderCombined", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$MappingTemporaryFolderCombined value:\n\n")
            base::print(input$MappingTemporaryFolderCombined)
        })
        output$MappingPathTempCombined <- shiny::renderPrint({
            if (base::is.integer(input$MappingTemporaryFolderCombined)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Temporary files are transiently stored in ",
                           shinyFiles::parseDirPath(volumes, input$MappingTemporaryFolderCombined)
                    )
                )
            }
        })
        ## useful objects
        outmapseq <- shiny::reactiveValues()
        start_time <- shiny::reactiveVal()
        dirMappingTempFilePathSequential <- shiny::reactiveVal()
        ## SLIDER INPUT UPDATING
        ## update slider input for parallel workers
        shiny::observeEvent(eventExpr = input$MappingThreadsCombined, {
            threads <- input$MappingThreadsCombined
            shiny::updateSliderInput(session = session,
                              inputId = "MappingProcessCombined",
                              max = base::as.integer(base::as.integer(parallel::detectCores(logical = TRUE))/threads))
        })
        ## update slider input for subjunc threads
        shiny::observeEvent(eventExpr = input$MappingProcessCombined, {
            workers <- input$MappingProcessCombined
            shiny::updateSliderInput(session = session,
                              inputId = "MappingThreadsCombined",
                              max = base::as.integer(base::as.integer(parallel::detectCores(logical = TRUE))/workers))
        })
        ## analysis with reassuring messages
        shiny::observeEvent(eventExpr = input$RunMappingCombined, {
            output$MappingCombinedProcessstatus <- shiny::renderPrint({
                req(outmapseq$mapping)
                if (outmapseq$mapping$is_alive()) {
                    outmapseq$mapping$print()
                    invalidateLater(5000)
                    base::cat("This process could be time-consuming\nFeel free to take a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
                } else {
                    shinyjs::enable("RunMappingCombined")
                    shinyjs::disable("StopMappingCombined")
                    #check end time of process
                    end_time <- base::Sys.time()
                    execution_time <- end_time - start_time()
                    # Convert to hours, minutes, and seconds
                    execution_time_sec <- base::as.numeric(execution_time, units = "secs")
                    hours <- base::floor(execution_time_sec / 3600)
                    minutes <- base::floor((execution_time_sec %% 3600) / 60)
                    seconds <- execution_time_sec %% 60
                    # Print the result
                    base::cat(base::paste(shiny::isolate("The genome mapping process has been finished!")),
                        "\nExecution Time: ", hours, "hours", minutes, "minutes",
                        round(seconds, 2), "seconds\n")
                }
            })
        })
        ## analysis
        shiny::observeEvent(input$RunMappingCombined, {
            ## input validation 1
            ## index genome folder
            if (base::is.integer(input$CombIndex) | base::is.integer(input$CombinedReads) | base::is.integer(input$CombBam)) {
                shiny::showNotification(base::paste("Please select the required folders"),
                                 duration = 10, type = "error")
            }# close if statement
            # input validation 2
            shiny::validate(
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$CombIndex), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$CombinedReads), message = character(0)),
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$CombBam), message = character(0))
            )
            ##
            shinyjs::enable("StopMappingCombined")
            shinyjs::disable("RunMappingCombined")
            ## temporary folder generation
            if (base::is.integer(input$MappingTemporaryFolderCombined) == FALSE){
                dir <- shinyFiles::parseDirPath(volumes, input$MappingTemporaryFolderCombined)
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
            ## start time to monitor execution
            start_time(base::Sys.time())
            ## reassuring table
            shiny::observe(
                if(base::length(base::list.files(shinyFiles::parseDirPath(volumes, input$CombBam))) == 0) {
                    AutoUpdatingCombAlign()
                } else {
                    shinyjs::show("MappingCombinedReassure")
                }
            )
            ## paths
            combIndexPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$CombIndex)
            processedReadsPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$CombinedReads)
            lista <- base::sub(pattern = "_1.fastq$|_1.fq$|_2.fastq$|_2.fq$|_1.fastq.gz$|_1.fq.gz$|_2.fastq.gz$|_2.fq.gz$",
                         replacement = "",
                         x = base::list.files(path = processedReadsPath,
                                        pattern = "_1.fastq$|_1.fq$|_2.fastq$|_2.fq$|_1.fastq.gz$|_1.fq.gz$|_2.fastq.gz$|_2.fq.gz$"))
            lista <- base::unique(lista)
            OutputPath <- shinyFiles::parseDirPath(roots = volumes, selection = input$CombBam)

            ## background analysis
            outmapseq$mapping <- callr::r_bg(
                func = CombinedAlignment,
                args = list(
                    ### arguments
                    GenomeConcIndex = base::file.path(combIndexPath, "genome_reference_index"),
                    readsPath = processedReadsPath,
                    lalista = lista,
                    outBam = OutputPath,
                    nodes = input$MappingProcessCombined,
                    threads = base::as.integer(input$MappingThreadsCombined),
                    outFormat = base::as.character(input$MappingFormatCombined),
                    phredScore = base::as.integer(input$MappingCombPhredScore),
                    maxExtractedSubreads = base::as.integer(input$MappingNumberSubreadsCombined),
                    consensusVote = base::as.integer(input$MappingConsensusThresholdCombined),
                    mismatchMax = base::as.integer(input$MappingMaxMismatchCombined),
                    uniqueOnly = base::as.logical(input$MappingUniqueOnlyCombined),
                    maxMultiMapped = base::as.integer(input$MappingMultiMapMaxCombined),
                    indelLength = base::as.integer(input$MappingIndelLengthCombined),
                    fragmentMinLength = base::as.integer(input$MappingMinFragLengthCombined),
                    fragmentMaxLength = base::as.integer(input$MappingMaxFragLengthCombined),
                    matesOrientation = base::as.character(input$MappingPeOrientationCombined),
                    readOrderConserved = base::as.logical(input$MappingKeepOrderCombined),
                    coordinatesSorting = base::as.logical(input$MappingSortByCoordinatesCombined),
                    allJunctions = base::as.logical(input$MappingAllJunctionsCombined),
                    readsAlignedBlock = base::as.integer(input$SubsetBamCombined),
                    tempfolder = base::file.path(dirMappingTempFilePathSequential())
                ) ## close argument list
            ) ## close callr:r_bg
        }) ## close observe event "RUN-MAPPING"
        ## KILLING ANALYSIS
        shiny::observeEvent(eventExpr = input$StopMappingCombined, {
            shinyjs::enable("RunMappingCombined")
            shinyjs::disable("StopMappingCombined")
            ## killing
            while(outmapseq$mapping$is_alive()) {
                outmapseq$mapping$kill()
            }
            ## delete temporary folder
            if (base::file.exists(dirMappingTempFilePathSequential()) && base::is.integer(input$MappingTemporaryFolderCombined) == TRUE) {
                base::unlink(dirMappingTempFilePathSequential(), recursive = TRUE)
            }

            ## reassuring message
            if (outmapseq$mapping$get_exit_status() != 0) {
                output$MappingCombinedProcessstatus <- shiny::renderPrint({
                    base::cat("Genome mapping was killed!")
                })
            }
            shiny::showNotification(base::paste("Genome indexing process has been killed!"),
                             duration = 10, type = "warning")
        }) ## close observeEvent killing analysis
        ## reassuring table
        ## updating object
        reassuringCombAlign <- shiny::reactivePoll(
            intervalMillis = 1000, session = session,
            checkFunc = function() {
                # this function returns the most recent modification time in the folder
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$CombBam),
                                    full.names = TRUE)
                # avoid print error when directory is empty
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    base::max(info$mtime)
                }
            }, valueFunc = function() {
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$CombBam),
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
        ## reassurin table
        output$MappingCombinedReassure <- DT::renderDT(reassuringCombAlign())
    } # close function inside moduleServer
    ) # close moduleServer
} # close mappingSequentialServer function
