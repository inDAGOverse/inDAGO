#' Quality control server logic
#'
#' @param id Shiny module identifier
qualityControlServerLogic <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ## interactive parameters
    shiny::observeEvent(eventExpr = input$helpParameterQualityCheck,
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
    ## reactive values
    AutoUpdating <- shiny::reactiveTimer(2000)
    ## pre-analysis
    pQuality <- shiny::reactiveValues() # processo
    reactQuality <- shiny::reactiveVal() # risultati
    dirQualityCheckTempFilePath <- shiny::reactiveVal() # cartella temporanea
    ## starting time
    start_time <- shiny::reactiveVal()
    start_time_g1 <- shiny::reactiveVal() ## g1
    start_time_g2 <- shiny::reactiveVal() ## g2
    start_time_g3 <- shiny::reactiveVal() ## g3
    start_time_g3bis <- shiny::reactiveVal() ## g3bis
    start_time_g4 <- shiny::reactiveVal() ## g4
    start_time_g5 <- shiny::reactiveVal() ## g5
    start_time_g6 <- shiny::reactiveVal() ## g6
    ## plot 1
    pPlotOne <- shiny::reactiveValues() # process
    pPlotlyOne <- shiny::reactiveValues() # process
    reactPlotOne <- shiny::reactiveVal() # result
    reactPlotlyOne <- shiny::reactiveVal() # result
    ## plot 2
    pPlotTwo <- shiny::reactiveValues() # processo
    pPlotlyTwo <- shiny::reactiveValues() # processo
    reactPlotTwo <- shiny::reactiveVal() # risultati
    reactPlotlyTwo <- shiny::reactiveVal() # risultati
    ## counting sequences (ex plot3)
    pPlotThree <- shiny::reactiveValues() # process
    pPlotlyThree <- shiny::reactiveValues() # process
    reactPlotThree <- shiny::reactiveVal() # result
    reactPlotlyThree <- shiny::reactiveVal() # result
    ## plot3 (ex 3bis)
    pPlotThreeBis <- shiny::reactiveValues() # process
    pPlotlyThreeBis <- shiny::reactiveValues() # process
    reactPlotThreeBis <- shiny::reactiveVal() # results
    reactPlotlyThreeBis <- shiny::reactiveVal() # results
    ## plot4
    pPlotFour <- shiny::reactiveValues() # process
    reactPlotFour <- shiny::reactiveVal() # results
    ## plot5
    pPlotFive <- shiny::reactiveValues() # process
    reactPlotFive <- shiny::reactiveVal() # results
    ## plot6
    pPlotSix <- shiny::reactiveValues() # process
    reactPlotSix <- shiny::reactiveVal() # results
    ## hide/show
    shinyjs::hide("titleReassure")
    shinyjs::hide("reassure")
    ## input data
    ## paths
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    ## input folder
    shinyFiles::shinyDirChoose(input, "QualityInputDir", roots = volumes, session = session,
                   restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
    ## print console
    shiny::observe({
      base::cat("\ninput$QualityInputDir value:\n\n")
      base::print(input$QualityInputDir)
    })
    ## print interface
    output$QualityPathInputDir <- shiny::renderPrint({
      if (base::is.integer(input$QualityInputDir)) {
        base::cat("No directory has been selected")
      } else {
        base::cat(
          base::paste0("The input reads are stored in ",
                 shinyFiles::parseDirPath(volumes, input$QualityInputDir))
        )
      }
    })
    ## result folder
    shinyFiles::shinyDirChoose(input, "QualityOutputDir", roots = volumes, session = session,
                   restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
    ## print 1
    shiny::observe({
      base::cat("\ninput$QualityOutputDir value:\n\n")
      base::print(input$QualityOutputDir)
    })
    ## print 2
    output$QualityPathOutputDir <- shiny::renderPrint({
      if (base::is.integer(input$QualityOutputDir)) {
        base::cat("No directory has been selected")
      } else {
        shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
      }
    })
    ## temporary folder selected
    shinyFiles::shinyDirChoose(input, "TemporaryFolderQualityCheck", roots = volumes, session = session,
                   restrictions = base::system.file(package = "base"), allowDirCreate = FALSE)
    ## print to console to see how the value of the shinyFiles
    ## button changes after clicking and selection
    shiny::observe({
      base::cat("\ninput$TemporaryFolderQualityCheck value:\n\n")
      base::print(input$TemporaryFolderQualityCheck)
    })
    output$TemporaryFolderPathQualityCheck <- shiny::renderPrint({
      if (base::is.integer(input$TemporaryFolderQualityCheck)) {
        base::cat("No directory has been selected")
      } else {
        base::cat(
          base::paste0("Selected temporary folder is ",
                 shinyFiles::parseDirPath(volumes, input$TemporaryFolderQualityCheck)
          )
        )
      }
    })
    ## analysis
    shiny::observeEvent(input$RunQualityCheck, {
      ## temporary folder generation
      if (base::is.integer(input$TemporaryFolderQualityCheck) == FALSE){
        dir <- shinyFiles::parseDirPath(volumes, input$TemporaryFolderQualityCheck)
        ## Set the value by calling with an argument
        dirQualityCheckTempFilePath(dir)
      } else {
        if (base::file.exists(base::file.path(fs::path_temp(),"TempDirSum_3738"))){
          dir <- base::file.path(fs::path_temp(),"TempDirSum_3738")
          dirQualityCheckTempFilePath(dir)
        } else {
          dir.create(base::file.path(fs::path_temp(),"TempDirSum_3738"))
          dir <- base::file.path(fs::path_temp(),"TempDirSum_3738")
          dirQualityCheckTempFilePath(dir)
        }
      }
      ## input validation 1
      if (base::is.integer(input$QualityInputDir) | base::is.integer(input$QualityOutputDir)) {
        shiny::showNotification(base::paste("Please select the required folders"), duration = 10,
                         type = "error")
      } else {
        ## input validation 2
        shiny::validate(
          shiny::need(expr = shinyFiles::parseDirPath(volumes, input$QualityInputDir), message = character(0)
          ),
          shiny::need(expr = shinyFiles::parseDirPath(volumes, input$QualityOutputDir), message = character(0)
          )
        )
        ## hide/show
        shiny::observe(
          if(base::length(base::list.files(shinyFiles::parseDirPath(volumes, input$QualityOutputDir))) == 0) {
            AutoUpdating()
          } else {
            shinyjs::show("titleReassure")
            shinyjs::show("reassure")
          }
        )
        ## enable/disable
        shinyjs::disable("RunQualityCheck")
        shinyjs::enable("StopQualityCheck")
        #start time to monitor execution
        start_time(base::Sys.time())
        ## reassuring
        shiny::showNotification(base::paste("QC analysis has been started"), duration = 10,
                         type = "message")
        output$mainProcessStatus <- shiny::renderPrint({
          pQuality$result$print()
          base::cat("This process could be time-consuming\nFeel free to take a coffee\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'")
        })
        ## handling unusual condition, error or warning
        base::tryCatch({
          ## background process
          pQuality$result <- callr::r_bg(
            func = QualityCheckAnalysis,
            args = list(
              directoryInput = shinyFiles::parseDirPath(volumes, input$QualityInputDir),
              inputFormat = ".fastq|.fq|.fq.gz|.fastq.gz",
              Nodes = input$QualityCheckProcess,
              ReadsNumber = base::as.integer(input$SubsetFastqQuality),
              directoryOutput = shinyFiles::parseDirPath(volumes, input$QualityOutputDir),
              tempFolder = base::file.path(dirQualityCheckTempFilePath())
            ),
            supervise = TRUE,
            error = base::getOption("callr.error", "error")
          ) #close r_bg
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
        ## saving result
        shiny::observe(
          if (shiny::isolate(pQuality$result$poll_io(0)["process"]) != "ready") {
            AutoUpdating()
          } else if (shiny::isolate(pQuality$result$poll_io(0)["process"]) == "ready") {
            print("quality control has been finished") #control
            reactQuality(10) #result
            shinyjs::disable("StopQualityCheck") #enable/disable
            shinyjs::enable("RunQualityCheck")
            shinyjs::enable("RunBaseAveragePlot")
            shinyjs::enable("RunSequenceLengthDistribution")
            shinyjs::enable("RunGcDistribution")
            shinyjs::enable("RunBaseQualityBoxplot")
            shinyjs::enable("RunBaseCompositionAreaChart")
            shinyjs::enable("RunBaseCompositionLinePlot")
            #check end time of process
            end_time <- base::Sys.time()
            execution_time <- end_time - start_time()
            # Convert to hours, minutes, and seconds
            execution_time_sec <- as.numeric(execution_time, units="secs")
            hours <- base::floor(execution_time_sec / 3600)
            minutes <- base::floor((execution_time_sec %% 3600) / 60)
            seconds <- execution_time_sec %% 60
            ## print process end
            output$mainProcessStatus <- shiny::renderPrint({
              base::cat(base::paste(shiny::isolate("QC analysis has been finished!")),
                  "\nExecution Time: ", hours, "hours", minutes, "minutes",
                  base::round(seconds, 2), "seconds\n")
            })
            ## print "samples" for plot 4
            shiny::updateRadioButtons(session = session, inputId = "BQBsamples",
                               label = "Choose samples to display",
                               choices = sapply(
                                 1:magrittr::'%>%'(base::list.files(shinyFiles::parseDirPath(volumes, input$QualityInputDir),
                                                              pattern = ".fastq|.fastq.gz|.fq|.fq.gz"), base::length()),
                                 function (x) {
                                   base::list.files(shinyFiles::parseDirPath(volumes, input$QualityInputDir))[x]
                                 }),
                               selected = NULL)
            ## print "samples" for plot 5
            shiny::updateRadioButtons(session = session, inputId = "BCACsamples",
                               label = "Choose samples to display",
                               choices = sapply(
                                 1:magrittr::'%>%'(base::list.files(shinyFiles::parseDirPath(volumes, input$QualityInputDir),
                                                              pattern = ".fastq|.fastq.gz|.fq|.fq.gz"), base::length()),
                                 function (x) {
                                   base::list.files(shinyFiles::parseDirPath(volumes, input$QualityInputDir))[x]
                                 }),
                               selected = NULL)
            ## print "samples" for plot 6
            shiny::updateRadioButtons(session = session, inputId = "BCLPsamples",
                               label = "Choose samples to display",
                               choices = sapply(
                                 1:magrittr::'%>%'(base::list.files(shinyFiles::parseDirPath(volumes, input$QualityInputDir),
                                                              pattern = ".fastq|.fastq.gz|.fq|.fq.gz"), base::length()),
                                 function (x) {
                                   base::list.files(shinyFiles::parseDirPath(volumes, input$QualityInputDir))[x]
                                 }),
                               selected = NULL)
          } # end else-if
        )  # end observe
      } # end else
    }) # close observeevent

    ##### killing quality control main analysis #####
    shiny::observeEvent(eventExpr = input$StopQualityCheck, {
      shinyjs::enable("RunQualityCheck")
      shinyjs::disable("StopQualityCheck")
      base::print("interrupting")
      while (pQuality$result$is_alive()) {
        pQuality$result$kill()
      }
      ## delete temp. folder
      if (base::file.exists(dirQualityCheckTempFilePath()) && base::is.integer(input$TemporaryFolderQualityCheck) == TRUE) {
        base::unlink(dirQualityCheckTempFilePath(), recursive = TRUE)
      }
      base::print("interrupted")
      if (pQuality$result$get_exit_status() != 0) {
        output$mainProcessStatus <- shiny::renderPrint({
          base::cat("The quality control process has been killed!")
        })
        shiny::showNotification(base::paste("The quality control process has been killed!"),
                         duration = 10, type = "warning")
      }
    })
    ## plot1
    shiny::observeEvent(input$RunBaseAveragePlot, {
      shinyjs::disable("RunQualityCheck")
      shinyjs::disable("RunBaseAveragePlot")
      shinyjs::enable("StopBaseAveragePlot")
      output$firstPlot <- shiny::renderPrint({
        base::cat("Plotting")
      })
      #start time to monitor execution
      start_time_g1(base::Sys.time())
      ## plot1 process
      base::tryCatch({
        pPlotOne$result <- callr::r_bg(
          func = BaseAverageQualityPlot,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) #close r_bg
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
      ## getting result
      shiny::observe(
        if (shiny::isolate(pPlotOne$result$poll_io(0)["process"]) != "ready") {
          base::print("plot1 running")
          AutoUpdating()
        } else if (shiny::isolate(pPlotOne$result$poll_io(0)["process"]) == "ready") {
          shinyjs::enable("RunBaseAveragePlot")
          shinyjs::disable("StopBaseAveragePlot")
          reactPlotOne(pPlotOne$result$get_result())
          output$BaseAverageQuality <- renderPlot(reactPlotOne())
          base::print("plot1 done")
          #check end time of process
          end_time <- base::Sys.time()
          execution_time <- end_time - start_time_g1()
          # Convert to hours, minutes, and seconds
          execution_time_sec <- as.numeric(execution_time, units="secs")
          hours <- base::floor(execution_time_sec / 3600)
          minutes <- base::floor((execution_time_sec %% 3600) / 60)
          seconds <- execution_time_sec %% 60
          ## printing
          output$firstPlot <- shiny::renderPrint({
            base::cat(base::paste(shiny::isolate("Done")),
                " (execution time: ", hours, "hours", minutes, "minutes",
                base::round(seconds, 2), "seconds)")                    })
          ## download 1
          output$BaseQualLink <- shiny::downloadHandler(
            filename = function() {
              base::paste("BaseAverageQuality_", base::Sys.Date(), ".", input$BAPformat, sep = "")
            },
            content = function(file) {
              ggplot2::ggsave(file, reactPlotOne(), device = input$BAPformat, width = input$widthBAP,
                     height = input$heightBAP, units = input$unitsBAP, dpi = input$resBAP)
            })
        }
      ) #close observe
      ## interactive
      base::tryCatch({
        pPlotlyOne$result <- callr::r_bg(
          func = BaseAverageQualityPlotly,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) #close r_bg
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
      ## getting result
      shiny::observe(
        if (shiny::isolate(pPlotlyOne$result$poll_io(0)["process"]) != "ready") {
          base::print("plotly1 running")
          AutoUpdating()
        } else if (shiny::isolate(pPlotlyOne$result$poll_io(0)["process"]) == "ready") {
          reactPlotlyOne(pPlotlyOne$result$get_result())
          output$BaseAverageQualityInteractive <- renderPlotly(reactPlotlyOne())
          base::print("plotly1 done")
        }
      ) #close observe interactive
    }) #close observeevent
    ## killing
    shiny::observeEvent(eventExpr = input$StopBaseAveragePlot, {
      shinyjs::enable("RunBaseAveragePlot")
      shinyjs::disable("StopBaseAveragePlot")
      print("interrupting plot one")
      while (pPlotOne$result$is_alive() | pPlotlyOne$result$is_alive()) {
        pPlotOne$result$kill()
        pPlotlyOne$result$kill()
      }
      base::print("interrupted plot one")
      if (pPlotOne$result$get_exit_status() != 0 | pPlotlyOne$result$get_exit_status() != 0) {
        output$firstPlot <- shiny::renderPrint({
          base::cat("Killed!")
        })
        shiny::showNotification(base::paste("Analysis killed!"),
                         duration = 10, type = "warning")
      }
    }) #close observeevent
    ## plot2
    shiny::observeEvent(input$RunSequenceLengthDistribution, {
      shinyjs::disable("RunQualityCheck")
      shinyjs::disable("RunSequenceLengthDistribution")
      shinyjs::enable("StopSequenceLengthDistribution")
      output$secondPlot <- shiny::renderPrint({
        base::cat("Plotting")
      })
      #start time to monitor execution
      start_time_g2(base::Sys.time())
      ##
      base::tryCatch({
        pPlotTwo$result <- callr::r_bg(
          func = SequenceLengthDistributionPlot,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) #close r_bg
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
      ##
      shiny::observe(
        if (shiny::isolate(pPlotTwo$result$poll_io(0)["process"]) != "ready") {
          base::print("plot2 running")
          AutoUpdating()
        } else if (shiny::isolate(pPlotTwo$result$poll_io(0)["process"]) == "ready") {
          shinyjs::enable("RunSequenceLengthDistribution")
          shinyjs::disable("StopSequenceLengthDistribution")
          reactPlotTwo(pPlotTwo$result$get_result())
          output$SequenceLengthDistribution <- renderPlot(reactPlotTwo())
          print
          #check end time of process
          end_time <- base::Sys.time()
          execution_time <- end_time - start_time_g2()
          # Convert to hours, minutes, and seconds
          execution_time_sec <- as.numeric(execution_time, units="secs")
          hours <- base::floor(execution_time_sec / 3600)
          minutes <- base::floor((execution_time_sec %% 3600) / 60)
          seconds <- execution_time_sec %% 60
          ##
          output$secondPlot <- shiny::renderPrint({
            base::cat(base::paste(shiny::isolate("Done ")),
                "(execution time: ", hours, "hours", minutes, "minutes",
                base::round(seconds, 2), "seconds)")
          })
          ## download 2
          output$ReadLenLink <- shiny::downloadHandler(
            filename = function() {
              base::paste("ReadLengthComposition_", base::Sys.Date(), ".", input$SLDformat, sep = "")
            },
            content = function(file) {
              ggplot2::ggsave(file, reactPlotTwo(), device = input$SLDformat, width = input$widthSLD,
                     height = input$heightSLD, units = input$unitsSLD, dpi = input$resSLD)
            })
        }
      ) #close observe
      ##
      base::tryCatch({
        pPlotlyTwo$result <- callr::r_bg(
          func = SequenceLengthDistributionPlotly,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) #close r_bg
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
      ##
      shiny::observe(
        if (shiny::isolate(pPlotlyTwo$result$poll_io(0)["process"]) != "ready") {
          base::print("plotly2 running")
          AutoUpdating()
          # invalidateLater(2000, session)
        } else if (shiny::isolate(pPlotlyTwo$result$poll_io(0)["process"]) == "ready") {
          reactPlotlyTwo(pPlotlyTwo$result$get_result())
          output$SequenceLengthDistributionInteractive <- renderPlotly(reactPlotlyTwo())
          base::print("plotly2 done")
        }
      ) #close observe
    }) #close observeevent
    ## killing
    shiny::observeEvent(eventExpr = input$StopSequenceLengthDistribution, {
      shinyjs::enable("RunSequenceLengthDistribution")
      shinyjs::disable("StopSequenceLengthDistribution")
      print("interrupting plot two")
      while (pPlotTwo$result$is_alive() | pPlotlyTwo$result$is_alive()) {
        pPlotTwo$result$kill()
        pPlotlyTwo$result$kill()
      }
      base::print("interrupted plot two")
      if (pPlotTwo$result$get_exit_status() != 0 | pPlotlyTwo$result$get_exit_status() != 0) {
        output$secondPlot <- shiny::renderPrint({
          base::cat("Killed!")
        })
        shiny::showNotification(base::paste("Analysis killed!"),
                         duration = 10, type = "warning")
      }
    }) #close observeevent
    ## plot3
    shiny::observeEvent(input$RunCountFastqReads, {
      if(base::is.integer(input$QualityInputDir)) {
        shiny::showNotification(base::paste("Please select the input data folder"), duration = 10,
                         type = "error")
      } else {
        ## input validation
        shiny::validate(
          shiny::need(expr = shinyFiles::parseDirPath(volumes, input$QualityInputDir), message = character(0))
        )
        ##
        shinyjs::disable("RunCountFastqReads")
        shinyjs::enable("StopCountFastqReads")
        output$countingReads <- shiny::renderPrint({
          base::cat("Counting")
        })
        #start time to monitor execution
        start_time_g3(base::Sys.time())
        ##
        base::tryCatch({
          pPlotThree$result <- callr::r_bg(
            func = counting_Reads,
            args = list(
              input_data = shinyFiles::parseDirPath(volumes, input$QualityInputDir)
            ),
            supervise = TRUE,
            error = base::getOption("callr.error", "error")
          ) #close r_bg
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
        ##
        shiny::observe(
          if (shiny::isolate(pPlotThree$result$poll_io(0)["process"]) != "ready") {
            print("read count running")
            AutoUpdating()
          } else if (shiny::isolate(pPlotThree$result$poll_io(0)["process"]) == "ready") {
            shinyjs::enable("RunCountFastqReads")
            shinyjs::disable("StopCountFastqReads")
            reactPlotThree(pPlotThree$result$get_result())
            output$countFastqRecords <- renderTable(reactPlotThree())
            base::print("counting done")
            #check end time of process
            end_time <- base::Sys.time()
            execution_time <- end_time - start_time_g3()
            # Convert to hours, minutes, and seconds
            execution_time_sec <- as.numeric(execution_time, units="secs")
            hours <- base::floor(execution_time_sec / 3600)
            minutes <- base::floor((execution_time_sec %% 3600) / 60)
            seconds <- execution_time_sec %% 60
            ##
            output$countingReads <- shiny::renderPrint({
              base::cat(base::paste(shiny::isolate("Done ")),
                  "(execution time: ", hours, "hours", minutes, "minutes",
                  base::round(seconds, 2), "seconds)")                    })
            ## download 3
            output$ReadCountLink <- shiny::downloadHandler(
              filename = function() {
                base::paste("Read_count_", base::Sys.Date(), ".csv", sep = "")
              },
              content = function(file) {
                utils::write.csv(x = reactPlotThree(),file = file,
                          row.names = FALSE,quote = FALSE)
              })
          }
        ) #close observe
      } #close if-else
    }) #close observeevent
    ##### killing counting sequences #####
    shiny::observeEvent(eventExpr = input$StopCountFastqReads, {
      shinyjs::enable("RunCountFastqReads")
      shinyjs::disable("StopCountFastqReads")
      print("interrupting counting")
      while (pPlotThree$result$is_alive() | pPlotlyThree$result$is_alive()) {
        pPlotThree$result$kill()
        pPlotlyThree$result$kill()
      }
      base::print("interrupted counting")
      if (pPlotThree$result$get_exit_status() != 0 | pPlotlyThree$result$get_exit_status() != 0) {
        output$countingReads <- shiny::renderPrint({
          base::cat("Killed!")
        })
        shiny::showNotification(base::paste("Analysis killed!"),
                         duration = 10, type = "warning")
      }
    }) #close observeevent

    ##### plot3 (ex 3bis) #####
    shiny::observeEvent(input$RunGcDistribution, {
      shinyjs::disable("RunQualityCheck")
      shinyjs::disable("RunGcDistribution")
      shinyjs::enable("StopGcDistribution")
      output$thirdPlotBis <- shiny::renderPrint({
        base::cat("Plotting")
      })
      #start time to monitor execution
      start_time_g3bis(base::Sys.time())
      ##
      base::tryCatch({
        pPlotThreeBis$result <- callr::r_bg(
          func = GCcontentDistributionPlot,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) #close r_bg
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
      ##
      shiny::observe(
        if (shiny::isolate(pPlotThreeBis$result$poll_io(0)["process"]) != "ready") {
          print("plot3bis running")
          AutoUpdating()
        } else if (shiny::isolate(pPlotThreeBis$result$poll_io(0)["process"]) == "ready") {
          shinyjs::enable("RunGcDistribution")
          shinyjs::disable("StopGcDistribution")
          ##
          reactPlotThreeBis(pPlotThreeBis$result$get_result())
          output$GCDcontent <- renderPlot(reactPlotThreeBis())
          print("plot3bis done")
          #check end time of process
          end_time <- base::Sys.time()
          execution_time <- end_time - start_time_g3bis()
          # Convert to hours, minutes, and seconds
          execution_time_sec <- as.numeric(execution_time, units="secs")
          hours <- base::floor(execution_time_sec / 3600)
          minutes <- base::floor((execution_time_sec %% 3600) / 60)
          seconds <- execution_time_sec %% 60
          ##
          output$thirdPlotBis <- shiny::renderPrint({
            base::cat(base::paste(shiny::isolate("Done ")),
                "(execution time: ", hours, "hours", minutes, "minutes",
                base::round(seconds, 2), "seconds)")
          })
          ## download 3
          output$GcdContLink <- shiny::downloadHandler(
            filename = function() {
              base::paste("GcDistribution_", base::Sys.Date(), ".", input$GCDformat, sep = "")
            },
            content = function(file) {
              ggplot2::ggsave(file, reactPlotThreeBis(), device = input$GCDformat, width = input$widthGCD,
                     height = input$heightGCD, units = input$unitsGCD, dpi = input$resGCD)
            })
        }
      ) # observe

      ##
      base::tryCatch({
        pPlotlyThreeBis$result <- callr::r_bg(
          func = GCcontentDistributionPlotly,
          args = list(input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) # r_bg
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
      ) #  trycatch
      ##
      shiny::observe(
        if (shiny::isolate(pPlotlyThreeBis$result$poll_io(0)["process"]) != "ready") {
          print("plotly3BIS running")
          AutoUpdating()
        }
        else if (shiny::isolate(pPlotlyThreeBis$result$poll_io(0)["process"]) == "ready") {
          reactPlotlyThreeBis(pPlotlyThreeBis$result$get_result())
          output$GCDcontentInteractive <- renderPlotly(reactPlotlyThreeBis())
          base::print("plotly3 done")
        }
      ) # observe

    }) # observeevent
    ## killing
    shiny::observeEvent(eventExpr = input$StopGcDistribution, {
      shinyjs::enable("RunGcDistribution")
      shinyjs::disable("StopGcDistribution")
      base::print("interrupting plot three")
      while (pPlotThreeBis$result$is_alive() | pPlotlyThreeBis$result$is_alive()) {
        pPlotlyThreeBis$result$kill()
        pPlotThreeBis$result$kill()
      }
      base::print("interrupted plot three BIS")
      if (pPlotlyThreeBis$result$get_exit_status() != 0 | pPlotThreeBis$result$get_exit_status() != 0) {
        output$thirdPlotBis <- shiny::renderPrint({
          base::cat("Killed!")
        })
        shiny::showNotification(base::paste("Analysis killed!"),
                         duration = 10, type = "warning")
      }
    }) # observeevent
    #start time to monitor execution
    start_time_g4(base::Sys.time())
    ## plot4
    shiny::observeEvent(input$RunBaseQualityBoxplot, {

      shinyjs::disable("RunQualityCheck")
      shinyjs::disable("RunBaseQualityBoxplot")
      shinyjs::enable("StopBaseQualityBoxplot")
      output$fourthPlot <- shiny::renderPrint({
        base::cat("Plotting")
      })
      ##
      base::tryCatch({
        pPlotFour$result <- callr::r_bg(
          func = BaseQualityBoxplotPlot,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) # r_bg
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
      ) #  trycatch
      ##
      shiny::observe(
        if (shiny::isolate(pPlotFour$result$poll_io(0)["process"]) != "ready") {
          base::print("plot4 running")
          AutoUpdating()
        } else if (shiny::isolate(pPlotFour$result$poll_io(0)["process"]) == "ready") {
          shinyjs::enable("RunBaseQualityBoxplot")
          shinyjs::disable("StopBaseQualityBoxplot")
          reactPlotFour(pPlotFour$result$get_result())
          output$BaseQualityBoxplot <- renderPlot(reactPlotFour()[[input$BQBsamples]])
          base::print("plot4 done")
          #check end time of process
          end_time <- base::Sys.time()
          execution_time <- end_time - start_time_g4()
          # Convert to hours, minutes, and seconds
          execution_time_sec <- as.numeric(execution_time, units="secs")
          hours <- base::floor(execution_time_sec / 3600)
          minutes <- base::floor((execution_time_sec %% 3600) / 60)
          seconds <- execution_time_sec %% 60
          ##
          output$fourthPlot <- shiny::renderPrint({
            base::cat(base::paste(shiny::isolate("Done ")),
                "(execution time: ", hours, "hours", minutes, "minutes",
                base::round(seconds, 2), "seconds)")                    })
          ## download 4
          output$QualBoxLink <- shiny::downloadHandler(
            filename = function() {
              base::paste("BaseQualityBoxplot_", base::Sys.Date(), ".", input$BQBformat, sep = "")
            },
            content = function(file) {
              ggplot2::ggsave(file, reactPlotFour()[[input$BQBsamples]], device = input$BQBformat, width = input$widthBQB,
                     height = input$heightBQB, units = input$unitsBQB, dpi = input$resBQB)
            })
        }
      ) # observe

    }) #observeevent
    ## killing
    shiny::observeEvent(eventExpr = input$StopBaseQualityBoxplot, {
      shinyjs::enable("RunBaseQualityBoxplot")
      shinyjs::disable("StopBaseQualityBoxplot")
      print("interrupting plot four")
      while (pPlotFour$result$is_alive()) {
        pPlotFour$result$kill()
      }
      base::print("interrupted plot four")
      if (pPlotFour$result$get_exit_status() != 0) {
        output$fourthPlot <- shiny::renderPrint({
          base::cat("Killed!")
        })
        shiny::showNotification(base::paste("Analysis killed!"),
                         duration = 10, type = "warning")
      }
    }) #close observeevent
    ## plot 5
    shiny::observeEvent(input$RunBaseCompositionAreaChart, {
      shinyjs::disable("RunQualityCheck")
      shinyjs::disable("RunBaseCompositionAreaChart")
      shinyjs::enable("StopBaseCompositionAreaChart")
      output$fifthPlot <- shiny::renderPrint({
        base::cat("Plotting")
      })
      #start time to monitor execution
      start_time_g5(base::Sys.time())
      ##
      base::tryCatch({
        pPlotFive$result <- callr::r_bg(
          func = BaseCompositionAreaChartPlot,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) # r_bg
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
      ) #  trycatch
      ##
      shiny::observe(
        if (shiny::isolate(pPlotFive$result$poll_io(0)["process"]) != "ready") {
          base::print("plot5 running")
          AutoUpdating()
        } else if (shiny::isolate(pPlotFive$result$poll_io(0)["process"]) == "ready") {
          shinyjs::enable("RunBaseCompositionAreaChart")
          shinyjs::disable("StopBaseCompositionAreaChart")
          reactPlotFive(pPlotFive$result$get_result())
          output$BaseCompositionAreaChart <- renderPlot(reactPlotFive()[[input$BCACsamples]])
          output$BaseCompositionAreaChartInteractive <- renderPlotly(
            plotly::ggplotly(reactPlotFive()[[input$BCACsamples]])
          )
          base::print("plot5 done")
          #check end time of process
          end_time <- base::Sys.time()
          execution_time <- end_time - start_time_g5()
          # Convert to hours, minutes, and seconds
          execution_time_sec <- as.numeric(execution_time, units="secs")
          hours <- base::floor(execution_time_sec / 3600)
          minutes <- base::floor((execution_time_sec %% 3600) / 60)
          seconds <- execution_time_sec %% 60
          ##
          output$fifthPlot <- shiny::renderPrint({
            base::cat(base::paste(shiny::isolate("Done ")),
                "(execution time: ", hours, "hours", minutes, "minutes",
                base::round(seconds, 2), "seconds)")
          })
          ## download 5
          output$CompCharLink <- shiny::downloadHandler(
            filename = function() {
              base::paste("CompositionAreaChart_", base::Sys.Date(), ".", input$BCACformat, sep = "")
            },
            content = function(file) {
              ggplot2::ggsave(file, reactPlotFive()[[input$BCACsamples]], device = input$BCACformat, width = input$widthBCAC,
                     height = input$heightBCAC, units = input$unitsBCAC, dpi = input$resBCAC)
            })
        }
      ) # observe
    }) # observeevent
    ## killing
    shiny::observeEvent(eventExpr = input$StopBaseCompositionAreaChart, {
      shinyjs::enable("RunBaseCompositionAreaChart")
      shinyjs::disable("StopBaseCompositionAreaChart")
      print("interrupting plot five")
      while (pPlotFive$result$is_alive()) {
        pPlotFive$result$kill()
      }
      base::print("interrupted plot five")
      if (pPlotFive$result$get_exit_status() != 0) {
        output$fifthPlot <- shiny::renderPrint({
          base::cat("Killed!")
        })
        shiny::showNotification(base::paste("Analysis killed!"),
                         duration = 10, type = "warning")
      }
    }) # observeevent

    ## plot6
    shiny::observeEvent(input$RunBaseCompositionLinePlot, {
      shinyjs::disable("RunQualityCheck")
      shinyjs::disable("RunBaseCompositionLinePlot")
      shinyjs::enable("StopBaseCompositionLinePlot")
      output$sixthPlot <- shiny::renderPrint({
        base::cat("Plotting")
      })
      #start time to monitor execution
      start_time_g6(base::Sys.time())
      ##
      base::tryCatch({
        pPlotSix$result <- callr::r_bg(
          func = BaseCompositionLinePlot,
          args = list(
            input_data = shinyFiles::parseDirPath(volumes, input$QualityOutputDir)
          ),
          supervise = TRUE,
          error = base::getOption("callr.error", "error")
        ) # r_bg
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
      ) #  trycatch
      ##
      shiny::observe(
        if (shiny::isolate(pPlotSix$result$poll_io(0)["process"]) != "ready") {
          print("plot6 running")
          AutoUpdating()
        } else if (shiny::isolate(pPlotSix$result$poll_io(0)["process"]) == "ready") {
          shinyjs::enable("RunBaseCompositionLinePlot")
          shinyjs::disable("StopBaseCompositionLinePlot")
          reactPlotSix(pPlotSix$result$get_result())
          output$BaseCompositionLine <- renderPlot(reactPlotSix()[[input$BCLPsamples]])
          output$BaseCompositionLineInteractive <- renderPlotly(
            plotly::ggplotly(reactPlotSix()[[input$BCLPsamples]])
          )
          print("plot6 done")
          #check end time of process
          end_time <- base::Sys.time()
          execution_time <- end_time - start_time_g6()
          # Convert to hours, minutes, and seconds
          execution_time_sec <- as.numeric(execution_time, units="secs")
          hours <- base::floor(execution_time_sec / 3600)
          minutes <- base::floor((execution_time_sec %% 3600) / 60)
          seconds <- execution_time_sec %% 60
          ##
          output$sixthPlot <- shiny::renderPrint({
            base::cat(base::paste(shiny::isolate("Done ")),
                "(execution time: ", hours, "hours", minutes, "minutes",
                base::round(seconds, 2), "seconds)")
          })
          ## download 6
          output$CompLinLink <- shiny::downloadHandler(
            filename = function() {
              base::paste("CompositionLinePlot_", base::Sys.Date(), ".", input$BCLPformat, sep = "")
            },
            content = function(file) {
              ggplot2::ggsave(file, reactPlotSix()[[input$BCLPsamples]], device = input$BCLPformat, width = input$widthBCLP,
                     height = input$heightBCLP, units = input$unitsBCLP, dpi = input$resBCLP)
            })
        }
      ) # observe

    }) # observeevent
    ## killing
    shiny::observeEvent(eventExpr = input$StopBaseCompositionLinePlot, {
      shinyjs::enable("RunBaseCompositionLinePlot")
      shinyjs::disable("StopBaseCompositionLinePlot")
      print("interrupting plot six")
      while (pPlotSix$result$is_alive()) {
        pPlotSix$result$kill()
      }
      base::print("interrupted plot six")
      if (pPlotSix$result$get_exit_status() != 0) {
        output$sixthPlot <- shiny::renderPrint({
          base::cat("Killed!")
        })
        shiny::showNotification(base::paste("Analysis killed!"),
                         duration = 10, type = "warning")
      }
    }) # observeevent
    ## reassuring table
    reassuring <- reactivePoll(
      intervalMillis = 100, session = session,
      checkFunc = function() {
        # this function returns the most recent modification time in the folder
        files <- base::list.files(shinyFiles::parseDirPath(volumes, input$QualityOutputDir),
                            full.names = TRUE)
        #
        if (!base::length(files) == 0) {
          info <- base::file.info(files)
          base::max(info$mtime)
        }
      },
      valueFunc = function() {
        files <- base::list.files(shinyFiles::parseDirPath(volumes, input$QualityOutputDir),
                            full.names = TRUE)
        if (!base::length(files) == 0) {
          info <- base::file.info(files)
          info$size <- info$size / 1e6
          info$files <- base::basename(base::rownames(info))
          info <- dplyr::select(info, c("files","size","mtime")) %>% dplyr::rename(.,"size (mb)"  = "size","mtime (y-m-d_h-m-s)" = "mtime")
          base::rownames(info) <- NULL
          info }
      }
    )
    output$titleReassure <- shiny::renderText("Tabular results saved in the output folder")
    output$reassure <- DT::renderDT(reassuring())
  }) ## close moduleServer
} ## close server function
