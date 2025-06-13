#' Indexing combined server logic
#'
#' @param id Shiny module identifier
IndexingCombinedServerLogic <- function(id) {
    shiny::moduleServer(id, function(input,output,session) {
        ## interactive documentation
        shiny::observeEvent(eventExpr = input$helpParameterIndexingCombined,
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
        shinyjs::hide("IndexingCombinedReassure")
        ## split index panel activation
        shiny::observeEvent(eventExpr = input$IndexCombSplitIndex, {
            if (input$IndexCombSplitIndex == TRUE) {
                shinyjs::enable("IndexingMemoryComb")
            } else {
                shinyjs::disable("IndexingMemoryComb")
            }
        })
        ##### reactive values
        ## starting analysis time
        start_time <- shiny::reactiveVal()
        ## input selection
        volumes <- c(Home = fs::path_home(), "Rinstallation" = R.home(), getVolumes()())
        ##### GENOME 1
        shinyFiles::shinyFileChoose(input, "IndexingGenome1comb", roots = volumes, session = session)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$IndexingGenome1comb value:\n\n")
            base::print(input$IndexingGenome1comb)
        })
        ## print to browser
        output$IndexingPathGenome1comb <- renderPrint({
            if (base::is.integer(input$IndexingGenome1comb)) {
                base::cat("No file has been selected")
            } else {
                base::cat(
                    base::paste0("Uploaded genome is ",
                            shinyFiles::parseFilePaths(volumes, input$IndexingGenome1comb)[1,4]
                    )
                )
            }
        })
        ##### GENOMe 2
        shinyFiles::shinyFileChoose(input, "IndexingGenome2comb", roots = volumes, session = session)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$IndexingGenome2comb value:\n\n")
            base::print(input$IndexingGenome2comb)
        })
        ## print to browser
        output$IndexingPathGenome2comb <- shiny::renderPrint({
            if (base::is.integer(input$IndexingGenome2comb)) {
                base::cat("No file has been selected")
            } else {
                base::cat(
                    base::paste0("Uploaded genome is ",
                                 shinyFiles::parseFilePaths(volumes, input$IndexingGenome2comb)[1,4]
                    )
                )
            }
        })
        ## folder result
        shinyFiles::shinyDirChoose(input, "IndexingIndexComb", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$IndexingIndexComb value:\n\n")
            base::print(input$IndexingIndexComb)
        })
        output$IndexingPathIndexComb <- shiny::renderPrint({
            if (base::is.integer(input$IndexingIndexComb)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Indexed genome will be stored in ",
                                 shinyFiles::parseDirPath(volumes, input$IndexingIndexComb)
                    )
                )
            }
        })
        ## temporary folder
        shinyFiles::shinyDirChoose(input, "TemporaryFolderIndexComb", roots = volumes, session = session,
                       restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
        ## print to console to see how the value of the shinyFiles
        ## button changes after clicking and selection
        shiny::observe({
            base::cat("\ninput$TemporaryFolderIndexComb value:\n\n")
            base::print(input$TemporaryFolderIndexComb)
        })
        output$TemporaryFolderPrintIndexComb <- shiny::renderPrint({
            if (base::is.integer(input$TemporaryFolderIndexComb)) {
                base::cat("No directory has been selected")
            } else {
                base::cat(
                    base::paste0("Temporary files will be stored and then deleted in ",
                                 shinyFiles::parseDirPath(volumes, input$TemporaryFolderIndexComb)
                    )
                )
            }
        })
        ## useful objects
        genome_file1_comb <- shiny::reactive(base::as.character(shinyFiles::parseFilePaths(volumes, input$IndexingGenome1comb)[1,4]))
        out_path_comb <- shiny::reactive(shinyFiles::parseDirPath(roots = volumes, selection = input$IndexingIndexComb))
        genome_file2_comb <- shiny::reactive(base::as.character(shinyFiles::parseFilePaths(volumes, input$IndexingGenome2comb)[1,4]))
        outseq <- shiny::reactiveValues()
        AutoUpdatingCombInd <- shiny::reactiveTimer(2000)
        dirIndexTemFilePath <- shiny::reactiveVal()
        ##### CLICK RUN 1 ##### process information
        shiny::observeEvent(eventExpr = input$runIndexingCombined, {
            output$IndexingCombinedProcessstatus <- shiny::renderPrint({
                req(outseq$genomes)
                if (outseq$genomes$is_alive()) {
                    shiny::invalidateLater(5000)
                    outseq$genomes$print()
                    base::cat("This process could be time-consuming\nFeel free to take a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
                } else {
                    shinyjs::enable("runIndexingCombined")
                    shinyjs::disable("StopIndexingCombined")
                    #check end time of process
                    end_time <- base::Sys.time()
                    execution_time <- end_time - start_time()
                    # Convert to hours, minutes, and seconds
                    execution_time_sec <- base::as.numeric(execution_time, units="secs")
                    hours <- base::floor(execution_time_sec / 3600)
                    minutes <- base::floor((execution_time_sec %% 3600) / 60)
                    seconds <- execution_time_sec %% 60
                    # Print the result
                    base::cat(base::paste(shiny::isolate("The genome indexing process has been finished!")),
                        "\nExecution Time: ", hours, "hours", minutes, "minutes",
                        base::round(seconds, 2), "seconds\n")
                    ## deleting temporary folder
                    if (base::file.exists(dirIndexTemFilePath())) {
                        base::unlink(dirIndexTemFilePath(),recursive = TRUE)
                    }
                }
            })
        })
        ##### CLICK RUN 2 ##### where analysis run
        shiny::observeEvent(input$runIndexingCombined, {
            # input validations 1
            if (base::is.integer(input$IndexingIndexComb) | base::is.integer(input$IndexingGenome1comb) | base::is.integer(input$IndexingGenome2comb)) {
                shiny::showNotification(base::paste("Please select the required inputs"),
                                 duration = 10, type = "error")
            }# close if statement
            # input validations 2
            shiny::validate(
                shiny::need(expr = shinyFiles::parseDirPath(volumes, input$IndexingIndexComb), message = character(0)),
                shiny::need(expr = input$IndexingGenome1comb, message = character(0)),
                shiny::need(expr = input$IndexingGenome2comb, message = character(0))
            )
            # showing/hiding
            shinyjs::disable("runIndexingCombined")
            shinyjs::enable("StopIndexingCombined")
            ## temporary folder creation
            if (base::is.integer(input$TemporaryFolderIndexComb) == FALSE){
                dir <- shinyFiles::parseDirPath(volumes, input$TemporaryFolderIndexComb)
                ## Set the value by calling with an argument
                dirIndexTemFilePath(dir)
            } else {
                if (base::file.exists(base::file.path(fs::path_temp(),"TempDirSum_3738"))){
                    dir <- base::file.path(fs::path_temp(),"TempDirSum_3738")
                    dirIndexTemFilePath(dir)
                } else {
                    base::dir.create(base::file.path(fs::path_temp(),"TempDirSum_3738"))
                    dir <- file.path(fs::path_temp(),"TempDirSum_3738")
                    dirIndexTemFilePath(dir)
                }
            }
            ## running notification
            shiny::showNotification(base::paste("Genome indexing analysis has been started"), duration = 10,
                             type = "message")
            ##
            shiny::observe(
                if(base::length(base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndexComb))) == 0) {
                    AutoUpdatingCombInd()
                } else {
                    shinyjs::show("IndexingCombinedReassure")
                }
            )
            #start time to monitor execution
            start_time(base::Sys.time())
            ##analysis
            outseq$genomes <- callr::r_bg(
                func = IndexingComb,
                args = list(
                    gen1 = genome_file1_comb(),
                    gen2 = genome_file2_comb(),
                    outfolder = out_path_comb(),
                    basename = base::file.path(out_path_comb(), "genome_reference_index"),
                    reference = base::file.path(dirIndexTemFilePath(), "CombinedGenome.fasta"),
                    gappedIndex = base::as.logical(input$IndexingGappedComb),
                    indexSplit = input$IndexCombSplitIndex,
                    memory = input$IndexingMemoryComb,
                    TH_subread = input$CombRipetitiveSubreads,
                    tempfolder = base::file.path(dirIndexTemFilePath()),
                    tag1 = base::as.character(input$combIndexingTagGenome1),
                    tag2 = base::as.character(input$combIndexingTagGenome2)
                )
            )
        }) #close observeevent
        ### killing process
        shiny::observeEvent(eventExpr = input$StopIndexingCombined, {
            shinyjs::enable("runIndexingCombined")
            shinyjs::disable("StopIndexingCombined")
            while(outseq$genomes$is_alive()) {
                outseq$genomes$kill()
            }
            ## monitoring analysis
            if (outseq$genomes$get_exit_status() != 0) {
                output$IndexingCombinedProcessstatus <- shiny::renderPrint({
                    base::cat("Genome indexing was killed!")
                })
                ## removing temporary folder
                if (base::file.exists(dirIndexTemFilePath()) && base::is.integer(input$TemporaryFolderIndexComb) == TRUE) {
                    base::unlink(dirIndexTemFilePath(), recursive = TRUE)
                }
            }
            shiny::showNotification(base::paste("Genome indexing process has been killed!"),
                             duration = 10, type = "warning")
        })#close ObservEvent
        ## reassuring table
        ## updating object
        reassuringCombInd1 <- shiny::reactivePoll(
            intervalMillis = 1000, session = session,
            checkFunc = function() {
                # this function returns the most recent modification time in the folder
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndexComb),
                                    full.names = TRUE)
                # avoiding error when the directory is empty
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    max(info$mtime)
                }
            }, valueFunc = function() {
                files <- base::list.files(shinyFiles::parseDirPath(volumes, input$IndexingIndexComb),
                                    full.names = TRUE)
                if (!base::length(files) == 0) {
                    info <- base::file.info(files)
                    info$size <- info$size / 1e6
                    info$files <- base::basename(base::rownames(info))
                    info <- dplyr::select(info, c("files","size","mtime","ctime")) %>% dplyr::rename("Indexed genome files" = "files","size (mb)"  = "size","mtime (y-m-d_h-m-s)" = "mtime", "ctime (y-m-d_h-m-s)" = "ctime")
                    base::rownames(info) <- NULL
                    info }
            }
        )
        ## table
        output$IndexingCombinedReassure <- DT::renderDT(reassuringCombInd1())
    } # close server logic function
    ) # close moduleServer
} # close server function
