#' Server function for Summarization module in Shiny application
#' @param id Shiny module identifier

SummarizationServerLogic <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
  #Summarization
  shiny::observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      shiny::showNotification(
        "Welcome to the dark side!",
        action = NULL,
        duration = 5,
        closeButton = TRUE,
        id = NULL,
        type = "message",
        session = shiny::getDefaultReactiveDomain()
      )
    }
  })

  # Summarization: Set up introductory steps for user guidance
  shiny::observeEvent(input$helpSummarization, rintrojs::introjs(
    session,
    options = list(
      "showBullets" = "TRUE",
      "showProgress" = "TRUE",
      "showStepNumbers" = "FALSE",
      "nextLabel" = "Next",
      "prevLabel" = "Prev",
      "skipLabel" = "Skip"
    )
  ))

  # Define available directories for file selection
  volumes <- c(Home = fs::path_home(),
               "R Installation" = R.home(),
               shinyFiles::getVolumes()())

  # Summarization: Allow user to select the directory for bam files
  shinyFiles::shinyDirChoose(
    input,
    "directorySumSamUpload",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )

  # Observe directory selection and print to console
  shiny::observe({
    cat("\ninput$directorySumSamUpload value:\n\n")
    print(input$directorySumSamUpload)
  })

  # Reactive value to store the summarization upload path
  DirSumUploadSamPath <- shiny::reactiveVal()
  shiny::observe({
    dir <- shinyFiles::parseDirPath(volumes, input$directorySumSamUpload)
    DirSumUploadSamPath(dir)

  })

  # Output the selected directory path for the bam files
  output$DirSumUploadSamPath <- shiny::renderPrint({
    if (is.integer(input$directorySumSamUpload)) {
      cat("No input directory has been selected")
    } else {
      shinyFiles::parseDirPath(volumes, input$directorySumSamUpload)
    }
  })

  # Summarization: Allow user to select the directory for summarization output
  shinyFiles::shinyDirChoose(
    input,
    "directorySumDownload",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )

  # Observe directory selection and print to console
  shiny::observe({
    cat("\ninput$directorySumDownload value:\n\n")
    print(input$directorySumDownload)
  })
  # Reactive value to store the summarization download path
  dirSumDownloadPath <- shiny::reactiveVal()
  shiny::observe({
    dir <- shinyFiles::parseDirPath(volumes, input$directorySumDownload)
    dirSumDownloadPath(dir)

      })
  # Output the selected directory path for the summarization output
  output$DirSumDownloadPath <- shiny::renderPrint({
    if (is.integer(input$directorySumDownload)) {
      cat("No output directory has been selected")
    } else {
      shinyFiles::parseDirPath(volumes, input$directorySumDownload)
    }
  })

  # Summarization: Allow user to select the reference annotation file
  shinyFiles::shinyFileChoose(
    input,
    "fileSumRefUpload",
    roots = volumes,
    session = session
    #,
    #restrictions = system.file(package = "base")
  )
  # Observe file selection and print to console
  shiny::observe({
    cat("\ninput$fileSumRefUpload value:\n\n")
    print(input$fileSumRefUpload)
  })
  # Reactive value to store the rederence annotation file
  fileSumUploadRefPath <- shiny::reactiveVal()
  shiny::observe({
    File <- as.character(shinyFiles::parseFilePaths(volumes, input$fileSumRefUpload)$datapath)
    fileSumUploadRefPath(File)

  })

  # Output the selected annotation file path
  output$FileSumUploadRefPath <- shiny::renderPrint({
    if (is.integer(input$fileSumRefUpload)) {
      cat("No input file has been selected")
    } else {
      cat(
        paste0("Uploaded genome is ",
               as.character(shinyFiles::parseFilePaths(volumes, input$fileSumRefUpload)$datapath)
        )
      )
    }
  })

  # Summarization: Allow user to select the directory for save temporary summarization output
  shinyFiles::shinyDirChoose(
    input,
    "directoryTmpDirSum",
    roots = volumes,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$directoryTmpDirSum value:\n\n")
    print(input$directoryTmpDirSum)
  })

  # Reactive value to store temporary path
  dirSumTemFilePath <- shiny::reactiveVal()
  # Output the selected temporary path
  output$DirectoryTmpDirSum <- shiny::renderPrint({
    if (is.integer(input$directoryTmpDirSum)) {
      cat("No temporary directory has been selected")
    } else {
      shinyFiles::parseDirPath(volumes, input$directoryTmpDirSum)
    }
  })

  # update useMetaFeatures option
  shiny::observeEvent(input$isGTFAnnotationFile,{
    if (input$isGTFAnnotationFile == FALSE) {

      shinyWidgets::updatePrettyRadioButtons(session, inputId = "useMetaFeatures", label = NULL, choices = NULL, selected = TRUE)
      shinyjs::disable("useMetaFeatures")
    }
    else{
      shinyjs::enable("useMetaFeatures")
    }
  })

  # update fraction option
  shiny::observeEvent(c(input$countMultiMappingReads, input$allowMultiOverlap), {
    if (input$countMultiMappingReads == TRUE ||
        input$allowMultiOverlap == TRUE) {
      shinyjs::enable("fraction")
    } else if (input$countMultiMappingReads == FALSE &&
               input$allowMultiOverlap == FALSE) {
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "fraction",
        label = NULL,
        choices = NULL,
        selected = FALSE,
        inline = FALSE
      )
      shinyjs::disable("fraction")
    }
  })

  # update countMultiMappingReads option
  shiny::observeEvent(input$primaryOnly, {
    if (input$primaryOnly == FALSE) {
      shinyjs::enable("countMultiMappingReads")
    } else{
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "countMultiMappingReads",
        label = NULL,
        choices = NULL,
        selected = FALSE,
        inline = FALSE
      )
      shinyjs::disable("countMultiMappingReads")
    }
  })

  # update minFragLength option
  shiny::observeEvent(input$checkFragLength, {
    if (input$checkFragLength == TRUE) {
      shinyjs::enable("minFragLength")
      shinyjs::enable("maxFragLength")
    } else{
      shinyjs::disable("minFragLength")
      shinyjs::disable("maxFragLength")
    }
  })

  # update GTF.featureType option
  shiny::observeEvent(input$ActionAddGTF.featureType, {
    if (input$AddGTF.featureType != "NULL") {
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "GTF.featureType",
        label = NULL,
        choices = c(
          "exon",
          "gene",
          "transcript",
          "CDS",
          input$AddGTF.featureType
        ),
        selected = "exon",
        inline = FALSE
      )
    } else{
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "GTF.featureType",
        label = NULL,
        choices = c("exon", "gene", "transcript", "CDS"),
        selected = "exon",
        inline = FALSE
      )
    }
  })

  # update GTF.attrType option
  shiny::observeEvent(input$ActionAddGTF.attrType, {
    if (input$AddGTF.attrType != "NULL") {
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "GTF.attrType",
        label = NULL,
        choices = c(
          "gene_id",
          "transcript_id",
          "protein_id",
          "gene",
          "CDS",
          input$AddGTF.attrType
        ),
        selected = "gene_id",
        inline = FALSE
      )
    } else{
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "GTF.attrType",
        label = NULL,
        choices = c("gene_id", "transcript_id", "protein_id", "gene", "CDS"),
        selected = "gene_id",
        inline = FALSE
      )
    }
  })

  # update SumNthreads and SummarizationSamples options
  shiny::observeEvent(c(input$SumNthreads, input$SummarizationSamples), {
    shiny::updateNumericInput(
      session,
      inputId = "SumNthreads",
      label = NULL,
      value = NULL,
      min = 1,
      max = floor(parallel::detectCores(logical = TRUE) / input$SummarizationSamples),
      step = NULL
    )

    shiny::updateNumericInput(
      session,
      inputId = "SummarizationSamples",
      label = NULL,
      value = NULL,
      min = 1,
      max = floor(parallel::detectCores(logical = TRUE) / input$SumNthreads),
      step = NULL
    )

    output$availableSummarizationSamples <- shiny::renderPrint(cat(paste(
      "still available", floor((parallel::detectCores(logical = TRUE) / input$SumNthreads) - input$SummarizationSamples
      )
    )))
    output$availableSumNthreads <- shiny::renderPrint(cat(paste(
      "still available", floor((parallel::detectCores(logical = TRUE) / input$SummarizationSamples) - input$SumNthreads
      )
    )))

  })

  # generate a list of bam file analyzed in summarization analysis
  ListSummarization <- shiny::reactiveVal()
  shiny::observe({
    shiny::req(DirSumUploadSamPath())
    l <- list.files(DirSumUploadSamPath(), pattern = "*.sam$|*.bam$")
    ListSummarization(l)
  })




  # reactiveVal to save execution time
  start_time_summarization <- shiny::reactiveVal()


  # Reactive value to store summarization_process
  rvSum <- shiny::reactiveValues()
  # run summarization() in a background process
  shiny::observeEvent(input$Summarization, {
    # Check if directories are selected
    if (is.integer(input$directorySumSamUpload) ||
        is.integer(input$directorySumDownload) ||
        is.integer(input$fileSumRefUpload)) {
      shiny::showNotification(
        "Please select all required input in 'Step 1 - Set input/output'",
        closeButton = TRUE,
        duration = 10,
        type = "error"
      )
    } else{

      #start time to monitor execution
      start_time_summarization(Sys.time())

      shinyjs::disable("Summarization")
      shinyjs::enable("killSummarization")

      if (is.integer(input$directoryTmpDirSum) == FALSE) {
        dir <- shinyFiles::parseDirPath(volumes, input$directoryTmpDirSum)
        dirSumTemFilePath(dir)
      } else{
        if (file.exists(file.path(fs::path_temp(), "TempDirSum_3738"))) {
          dir <- file.path(fs::path_temp(), "TempDirSum_3738")
          dirSumTemFilePath(dir)
        } else {
          dir.create(file.path(fs::path_temp(), "TempDirSum_3738"))
          dir <- file.path(fs::path_temp(), "TempDirSum_3738")
          dirSumTemFilePath(dir)
        }
      }

      tryCatch({
      rvSum$Summarization_process <- callr::r_bg(
        func = Summarization
        ,
        args = list(
          NodesSum = input$SummarizationSamples,
          Xsum =  ListSummarization(),
          UploadPathSum = file.path(DirSumUploadSamPath(), "/"),
          DownloadPathSum = file.path(dirSumDownloadPath(), "/"),
          annot.ext = file.path(fileSumUploadRefPath()),
          isGTFAnnotationFile = input$isGTFAnnotationFile,
          GTF.featureType = input$GTF.featureType,
          GTF.attrType = input$GTF.attrType,
          useMetaFeatures = input$useMetaFeatures,
          allowMultiOverlap = input$allowMultiOverlap,
          largestOverlap = input$largestOverlap,
          minOverlap = input$minOverlap,
          fracOverlap = input$fracOverlap / 100,
          fracOverlapFeature = input$fracOverlapFeature / 100,
          countMultiMappingReads = input$countMultiMappingReads,
          fraction = input$fraction,
          minMQS = input$minMQS,
          primaryOnly = input$primaryOnly,
          ignoreDup = input$ignoreDup,
          strandSpecific = input$strandSpecific,
          requireBothEndsMapped = input$requireBothEndsMapped,
          checkFragLength = input$checkFragLength,
          minFragLength = input$minFragLength,
          maxFragLength = input$maxFragLength,
          countChimericFragments = input$countChimericFragments,
          autosort = input$autosort,
          verbose = input$verbose,
          tmpDir = file.path(dirSumTemFilePath(), "/"),
          nthreads = input$SumNthreads


        )
        ,
        supervise = TRUE
        ,
        error = getOption("callr.error", "error")
      )

      }
      , error = function(e) {
        shiny::showNotification(
          "error occours - check if files with the name of the samples already exist in selected folder",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # finisce tryCatch

      shiny::showNotification(
        "Summarization analysis has been starded",
        action = NULL,
        duration = 10,
        closeButton = TRUE,
        id = NULL,
        type = "message"
      )
    }
  })



  # Output the status of the summarization process
  shiny::observeEvent(input$Summarization, {
    if (is.integer(input$directorySumSamUpload) ||
        is.integer(input$directorySumDownload) ||
        is.integer(input$fileSumRefUpload)) {

    } else{
      output$SummarizationCheckStatusOut <- shiny::renderPrint({
        shiny::req(input$Summarization)

        if (shiny::isolate(rvSum$Summarization_process$is_alive()) == TRUE) {
          shiny::invalidateLater(5000)
          rvSum$Summarization_process$print()
          cat(
            shiny::isolate(
              "This process could be time-consuming\nFeel free to take a coffee break\n\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'"
            )
          )

        } else {



          #check end time of process
          end_time_summarization <- Sys.time()
          execution_time_summarization <- end_time_summarization - start_time_summarization()

          # Convert to hours, minutes, and seconds
          execution_time_sec_summarization <- as.numeric(execution_time_summarization, units="secs")
          hours_summarization <- floor(execution_time_sec_summarization / 3600)
          minutes_summarization <- floor((execution_time_sec_summarization %% 3600) / 60)
          seconds_summarization <- execution_time_sec_summarization %% 60

          # Print the result
          cat(paste(shiny::isolate("The summarization analysis has been completed!\n")),"Execution Time: ", hours_summarization, "hours", minutes_summarization, "minutes", round(seconds_summarization, 2), "seconds")

          shinyjs::enable("Summarization")
          shinyjs::disable("killSummarization")
          #remove temp Dir
          if (file.exists(dirSumTemFilePath()) &&
              is.integer(input$directoryTmpDirSum) == TRUE) {
             unlink(dirSumTemFilePath(), recursive = TRUE)
          }

        }

      })
    }
  })

  # Interrupt the summarization process if the kill button is clicked
  shiny::observeEvent(input$killSummarization, {
    # stop the slow Summarization() function
    rvSum$Summarization_process$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
    shinyjs::enable("Summarization")
    shinyjs::disable("killSummarization")
    output$SummarizationCheckStatusOut <- shiny::renderPrint({
      shiny::invalidateLater(5000)
      if (shiny::isolate(rvSum$Summarization_process$is_alive()) == FALSE) {
        cat(shiny::isolate("Summarization analysis has been killed!"))
        #remove temp Dir
        if (file.exists(dirSumTemFilePath()) &&
            is.integer(input$directoryTmpDirSum) == TRUE) {
          unlink(dirSumTemFilePath(), recursive = TRUE)
        }

      }
    })
    shiny::showNotification(
      "Summarization analysis was killed!",
      action = NULL,
      duration = 10,
      closeButton = TRUE,
      id = NULL,
      type = "warning"
    )
  })

  shiny::observe({
    shiny::req(dirSumDownloadPath())

    # Check if the download directory is empty to avoid overwriting files
    if (length(list.files(dirSumDownloadPath())) > 1) {
      shiny::showNotification(
        "The folder already contains BAM files - select an empty folder",
        action = NULL,
        duration = NULL,
        closeButton = TRUE,
        id = NULL,
        type = "warning",
        session = shiny::getDefaultReactiveDomain()
      )
    }
    # Set up a reactive polling mechanism to check for new files in the download directory
    dataSummarization <- shiny::reactivePoll(
      5000,
      session,
      checkFunc = function() {
        # this function returns the most recent modification time in the folder
        files <- list.files(dirSumDownloadPath(), full.names = TRUE)
        # per evitare che emetta errore quando la directory e' vuota
        if (!length(files) == 0) {
          info <- file.info(files)
          max(info$mtime)
        }
      },
      valueFunc = function() {
        # this function returns the content of the most recent file in the folder
        files <- list.files(dirSumDownloadPath(), full.names = TRUE)
        if (!length(files) == 0) {
          info <- file.info(files)
          info$size <- info$size / 1e9
          info$files <- basename(rownames(info))
          info <- dplyr::select(info, c("files", "size", "mtime")) %>% dplyr::rename(.,
            "size (gb)"  = "size",
            "mtime (y-m-d_h-m-s)" = "mtime"
          )
          rownames(info) <- NULL
          info
        }
      }
    )
    # Render a DataTable to show the contents of the summarization output folder
    output$SummarizatedSample <- DT::renderDT({
      dataSummarization()
    })
  })

  } # close function inside moduleServer
  ) # close moduleServer
} # close ServerLogic function
