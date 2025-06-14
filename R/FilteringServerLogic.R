#' Server function for filtering module in Shiny application
#' @param id Shiny module identifier

FilteringServerLogic <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
  #Pre-processing

  # Pre-processing: Set up introductory steps for user guidance
  shiny::observeEvent(input$helpParameterFiltering,
               rintrojs::introjs(
                 session,
                 options = list(
                   "showBullets" = "TRUE",
                   #Show introduction bullets or not
                   "showProgress" = "TRUE",
                   #
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

  # Filtering: Allow user to select the directory for raw reads
  shinyFiles::shinyDirChoose(
    input,
    "directoryRawReadsFilteringUpload",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )

  # Observe directory selection and print to console
  shiny::observe({
    cat("\ninput$directoryRawReadsFilteringUpload value:\n\n")
    print(input$directoryRawReadsFilteringUpload)
  })

  # Reactive value to store the filtering upload path
  dirFilteringUploadPath <- shiny::reactiveVal()
  shiny::observe({
    dir <- shinyFiles::parseDirPath(volumes, input$directoryRawReadsFilteringUpload)
    dirFilteringUploadPath(dir)

  })

  # Output the selected directory path for the raw reads
  output$DirFilteringUploadPath <- shiny::renderPrint({
    if (is.integer(input$directoryRawReadsFilteringUpload)) {
      cat("No directory has been selected")
    } else {
      shinyFiles::parseDirPath(volumes, input$directoryRawReadsFilteringUpload)
    }
  })

  # Filtering: Allow user to select the directory for filtered output
  shinyFiles::shinyDirChoose(
    input,
    "directoryRawReadsFilteredDownload",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )

  # Observe directory selection and print to console
  shiny::observe({
    cat("\ninput$directoryRawReadsFilteredDownload value:\n\n")
    print(input$directoryRawReadsFilteredDownload)
  })

  # Reactive value to store the filtering download path
  dirFilteringDownloadPath <- shiny::reactiveVal()
  shiny::observe({
    dir <- shinyFiles::parseDirPath(volumes, input$directoryRawReadsFilteredDownload)
    dirFilteringDownloadPath(dir)

  })

  # Output the selected directory path for the filtered output
  output$DirFilteringDownloadPath <- shiny::renderPrint({
    if (is.integer(input$directoryRawReadsFilteredDownload)) {
      cat("No directory has been selected")
    } else {
      shinyFiles::parseDirPath(volumes, input$directoryRawReadsFilteredDownload)
    }
  })

  # List files in the raw reads directory and prepare filtering options
  ListFIlter <- shiny::reactiveVal()
  shiny::observe({
    shiny::req(dirFilteringUploadPath())
    l <- list.files(dirFilteringUploadPath(),
                    "*fastq$|*fq$|*fastq.gz$|*fq.gz$")
    Sample <- sub(
      '_1.fastq|_1.fq|_1.fastq.gz|_1.fq.gz|_2.fastq|_2.fq|_2.fastq.gz|_2.fq.gz',
      "",
      l
    ) %>% unique(.)
    gr <- function(x) {
      l[grep(x, l)]
    }
    listFilter <- lapply(Sample, gr)
    ListFIlter(listFilter)

  })

  # shiny::reactiveVal to save execution time
  start_time_filtering <- shiny::reactiveVal()

  # shiny::reactiveVal to save path for temporary files
  logFile <- shiny::reactiveVal()


  # Reactive value to store Filtering_process
  rvFil <- shiny::reactiveValues()
  # Run the filtering process in a background
  shiny::observeEvent(input$Filtering, {
    # Check if directories are selected
    if (is.integer(input$directoryRawReadsFilteringUpload) ||
        is.integer(input$directoryRawReadsFilteredDownload)) {
      shiny::showNotification(
        "Please select all required input in 'Step 1 - Set input/output'",
        closeButton = TRUE,
        duration = 10,
        type = "error"
      )
    } else {

    if (length(list.files(
      dirFilteringDownloadPath(),
      "*fastq$|*fq$|*fastq.gz$|*fq.gz$"
    )) > 1) {

    } else {
      #start time to monitor execution
      start_time_filtering(Sys.time())

      shinyjs::disable("Filtering")
      shinyjs::enable("killFiltering")

      tryCatch({
        rvFil$Filtering_process <- callr::r_bg(
          func = Filtering
          ,
          args = list(
            Nodes = input$filteringThreads,
            X =  ListFIlter(),
            UploadPath = file.path(dirFilteringUploadPath(), "/"),
            DownloadPath = file.path(dirFilteringDownloadPath(), "/"),
            qualityType = input$QualityTypeFiltering,
            minLen = input$minLen,
            trim = input$trim,
            trimValue = input$trimValue,
            n = input$nChunk,
            Adapters = input$Adapters,
            Lpattern = input$Lpattern,
            Rpattern = input$Rpattern,
            max.Lmismatch = input$max.Lmismatch,
            max.Rmismatch = input$max.Rmismatch,
            kW = input$kW,
            left = input$left,
            right = input$right,
            halfwidthAnalysis = input$halfwidthAnalysis,
            halfwidth = input$halfwidth,
            compress = input$compress
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
      })   # End of tryCatch
      shiny::showNotification(
        "Filtering analysis has been started",
        action = NULL,
        duration = 10,
        closeButton = TRUE,
        id = NULL,
        type = "message"
      )
    }
    }

  })


  # Output the status of the filtering process
  shiny::observeEvent(input$Filtering, {
    if (is.integer(input$directoryRawReadsFilteringUpload) ||
        is.integer(input$directoryRawReadsFilteredDownload)) {
      # No action needed if directories are not selected
    } else {

       if (length(list.files(
      dirFilteringDownloadPath(),
      "*fastq$|*fq$|*fastq.gz$|*fq.gz$"
    )) > 1) {

    } else {
      output$CheckStatusOut <- shiny::renderPrint({
        shiny::req(input$Filtering)

        if (shiny::isolate(rvFil$Filtering_process$is_alive()) == TRUE) {
          # autoInvalidate()
          shiny::invalidateLater(5000)
          rvFil$Filtering_process$print()
          cat(
            shiny::isolate(
              "This process could be time-consuming\nFeel free to take a coffee break\n\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'"
            )
          )

        } else {

          #check end time of process
          end_time_filtering <- Sys.time()
          execution_time_filtering <- end_time_filtering - start_time_filtering()

          # Convert to hours, minutes, and seconds
          execution_time_sec_filtering <- as.numeric(execution_time_filtering, units="secs")
          hours_filtering <- floor(execution_time_sec_filtering / 3600)
          minutes_filtering <- floor((execution_time_sec_filtering %% 3600) / 60)
          seconds_filtering <- execution_time_sec_filtering %% 60


          # Print the result
          cat(paste(shiny::isolate("The filtering analysis has been completed!\n")),"Execution Time: ", hours_filtering, "hours", minutes_filtering, "minutes", round(seconds_filtering, 2), "seconds\n")


          # shinyjs::enable and shinyjs::disable button
          shinyjs::enable("Filtering")
          shinyjs::disable("killFiltering")


        }

      })
    }
    }
  })

  # Interrupt the filtering process if the kill button is clicked
  shiny::observeEvent(input$killFiltering, {
    # stop the slow Filtering() function
    rvFil$Filtering_process$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
    shinyjs::enable("Filtering")
    shinyjs::disable("killFiltering")
    output$CheckStatusOut <- shiny::renderPrint({
      shiny::invalidateLater(5000)
      # autoInvalidate()
      if (shiny::isolate(rvFil$Filtering_process$is_alive()) == FALSE) {
        cat(shiny::isolate("The filtering analysis has been killed!"))

        unlink(logFile())

      }
    })
    shiny::showNotification(
      "The filtering analysis has been killed!",
      action = NULL,
      duration = 10,
      closeButton = TRUE,
      id = NULL,
      type = "warning"
    )



  })

  shiny::observe({
  # Check if the download directory is empty to avoid overwriting files
  if (length(list.files(
    dirFilteringDownloadPath(),
    "*fastq$|*fq$|*fastq.gz$|*fq.gz$"
  )) > 1) {
    shiny::showNotification(
      "The folder already contains FASTQ files - select an empty folder",
      action = NULL,
      duration = NULL,
      closeButton = TRUE,
      id = NULL,
      type = "warning",
      session = getDefaultReactiveDomain()
    )
  }
  })



  shiny::observeEvent(input$Filtering, {
    shiny::req(dirFilteringUploadPath())
    shiny::req(dirFilteringDownloadPath())

    if (length(list.files(dirFilteringDownloadPath(), "*fastq$|*fq$|*fastq.gz$|*fq.gz$")) > 1) {
      shiny::showNotification(
        "The folder already contains FASTQ files - select an empty folder",
        type = "error",
        duration = NULL
      )
    } else {


      # Reactive polling to check for new files
      dataFiltering <- shiny::reactivePoll(
        5000,
        session,
        checkFunc = function() {
          files <- list.files(dirFilteringDownloadPath(), "*fastq$|*fq$|*fastq.gz$|*fq.gz$", full.names = TRUE)
          if (length(files) > 0) {
            return(max(file.info(files)$mtime))
          }
          return(Sys.time())  # Avoid NULL return
        },
        valueFunc = function() {
          files <- list.files(dirFilteringDownloadPath(), "*fastq$|*fq$|*fastq.gz$|*fq.gz$", full.names = TRUE)

          if (length(files) == 0) {
            return(data.frame(
              files = character(),
              `size (gb)` = numeric(),
              `mtime (y-m-d_h-m-s)` = character(),
              Processed = numeric(),
              Retained = numeric(),
              Discarded = numeric(),
              `Discarded_%` = character()
            ))
          }


          info <- file.info(files)
          info$size <- info$size / 1e9
          info$files <- basename(rownames(info))
          info <- dplyr::select(info, c("files", "size", "mtime")) %>%
            dplyr::rename(
              "size (gb)" = "size",
              "mtime (y-m-d_h-m-s)" = "mtime"
            )


          empty_df_with_na <- data.frame(
            col1 = NA,  # Column 1 (NA value)
            col2 = NA,  # Column 2 (NA value)
            col3 = NA,  # Column 3 (NA value)
            col4 = NA   # Column 4 (NA value)
          )


          info <-  cbind(info,empty_df_with_na)


          fileslog <- list.files(dirFilteringDownloadPath(), "*.log$", full.names = TRUE)
          logFile(fileslog)

          if (length(fileslog) > 0) {
            resultSum <- sapply(fileslog, function(i) {
              df <- utils::read.table(i)
              name <- unique(df$V1)

              processStats <- function(name) {
                df1sum <- df %>% filter(V1 == name) %>% select(2:4) %>% apply(., 2, sum)
                percentDiscarded <- paste0(round(as.numeric(df1sum[3]) * 100 / as.numeric(df1sum[1]), 2), "%")
                return(c(name, df1sum, percentDiscarded))
              }

              df1 <- processStats(name[[1]])
              df2 <- processStats(name[[2]])

              df <- rbind(df1, df2) %>%
                `colnames<-`(c("files", "Processed", "Retained", "Discarded", "Discarded_%"))


              return(df)
            }, simplify = FALSE, USE.NAMES = TRUE) %>% do.call(rbind, .)

            info <- dplyr::left_join(info[1:3], data.frame(resultSum), by = "files")

          }
          return(info)


        }
      )


      output$FilteredSample <- DT::renderDT(
        dataFiltering(),
        selection = 'none',
        editable = FALSE,
        rownames = FALSE,
        colnames = c("files",
                     "size (gb)",
                     "mtime (y-m-d_h-m-s)",
                     "Processed",
                     "Retained",
                     "Discarded",
                     "Discarded_%"),
        extensions = 'Buttons',
        options = list(
          paging = FALSE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'csv', title = "FilteringResult")
          )),

        class = "display"
      )


    }
  })

  } # close function inside moduleServer
  ) # close moduleServer
} # close IndexingBulkServerLogic function
