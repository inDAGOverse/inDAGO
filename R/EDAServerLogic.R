#' Server function for EDA module in Shiny application
#' @param id Shiny module identifier


    EDAServerLogic <- function(id) {
      shiny::moduleServer(id, function(input, output, session) {

    # Event triggered when 'Help Exploration' button is clicked
    shiny::observeEvent(input$helpExploration, introjs(
      session,
      options = list(
        "showBullets" = "TRUE", # Enable introduction bullets
        "showProgress" = "TRUE",# Show progress bar
        "showStepNumbers" = "FALSE", # Hide step numbers
        "nextLabel" = "Next", # Label for the 'Next' button
        "prevLabel" = "Prev", # Label for the 'Previous' button
        "skipLabel" = "Skip" # Label for the 'Skip' button
      )
    ))


    # Pre-processing - Directory selection
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

    # Select directory for pre-processing
    shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session,
                   restrictions = system.file(package = "base"), allowDirCreate = FALSE)

    # Print directory value to console
    shiny::observe({
      cat("\ninput$directory value:\n\n")
      print(input$directory)
    })
    # Output the selected directory path to the browser
    output$directorypath <- shiny::renderPrint({
      if (is.integer(input$directory)) {
        cat("No directory has been selected")
      } else {
        shinyFiles::parseDirPath(volumes, input$directory)
      }
    })


    # Import raw counts - Set working directory
    shinyFiles::shinyDirChoose(input, "directory_raw_count", roots = volumes, session = session,
                   restrictions = system.file(package = "base"), allowDirCreate = FALSE)

    # Print selected directory for raw counts to the console
    shiny::observe({
      cat("\ninput$directory_raw_count value:\n\n")
      print(input$directory_raw_count)
    })
    # Output the selected directory for raw counts to the browser
    output$directory_raw_count_path <- shiny::renderPrint({
      if (is.integer(input$directory_raw_count)) {
        cat("No directory has been selected yet")
      } else {
        cat(
          paste0("Selected directory is: ",
                 shinyFiles::parseDirPath(volumes, input$directory_raw_count)))
      }
    })


    # Save working directory in a reactive variable
    WDirEX <- shiny::reactiveVal()
    shiny::observe({
      p <- shinyFiles::parseDirPath(volumes, input$directory_raw_count)
      WDirEX(p)
    })

    # Set sample matrix for selection based on files in the directory
    shiny::observeEvent(input$directory_raw_count,{
      req(WDirEX())
     tryCatch({

      l <- list.files(WDirEX(), pattern = "*.tab")

      if(is.null(l))
        l <- character(0)
      # Can also set the label and select items
      updateSelectInput(session, "selectpn",
                        label = paste("Number of imported matrices:", length(l)),
                        choices = l,
                        selected = head(l, 1))
      sample <-paste0(WDirEX(),"/",l[1])
      # Update numeric input based on the selected file's columns
      updateNumericInput(session, "colIDgene",   max = ncol(read_delim(sample,delim = "\t")))
      updateNumericInput(session, "colCounts",  max = ncol(read_delim(sample,delim = "\t")))

      shiny::showNotification("Table successfully loaded in Step 1 Result - Import count matrices",  closeButton = TRUE, duration = 5,  type = "message")

      }, error = function(e) {
        shiny::showNotification(
          "error occurs in loading raw counts tables",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      })

    })


    # Load and save selected matrix as a reactive value
    matrixnp <- shiny::reactiveVal()
    shiny::observe({
      req(input$selectpn)
      req(WDirEX())
      req(input$directory_raw_count)

      tryCatch({


        if( input$selectpn != 0 ) {
          path <- paste0(WDirEX(),"/",input$selectpn)
          m <- read_delim(path,
                          col_names = FALSE,
                          skip = input$skip_preN,
                          delim = "\t") %>% `colnames<-`(paste0("col",seq_len(ncol(.)))) %>% .[,c(input$colIDgene, input$colCounts)]
        }

        matrixnp(m)

      }, error = function(e) {
      })
    })



    # Render and display the matrix in a table format

    shiny::observe({
      req(input$directory_raw_count)
      req(matrixnp())
    output$ShowTable <- renderTable({


      if(input$disp == "head") {

        return(head(matrixnp(), n = 20))# Display first 20 rows

      }
      else {

        return(matrixnp()) # Display entire matrix

      }

    }, rownames = TRUE)
})


    # print message if table is not loaded
    output$ShowTable <- shiny::renderPrint(cat("Perform step 1 to see matrices"))


    # Assign groups based on samples
    df <- shiny::reactiveValues(group = data.frame())

    # Initialize group assignment
    shiny::observeEvent(input$assign_group, {
      shiny::req(WDirEX())
      # check if the WDirEX() is proprierly assigned
      spsComps::shinyCatch({
        if (is.null(WDirEX())) warning("Warning: The groups are not properly assigned.")
      }, blocking_level = "warning")
      files <- list.files(WDirEX(),pattern = ".tab") %>% tools::file_path_sans_ext(.)
      df$group <- data.frame(as.character(files),as.character(rep("NA",length(files))))

      # Notify when group table is loaded
      shiny::showNotification("Table loaded in the panel: Step 2 Result - Generate the Matrix",  closeButton = TRUE, duration = 10,  type = "message")

    })



    # To be able to use the output.item in conditionalPanel
    output$groupingMessage <- shiny::renderPrint(cat("Please complete steps 1 and 2 to generate the matrix"))


    # Render group assignment table

    shiny::observe({
      shiny::req(df$group)
      shiny::req(WDirEX())
      shiny::req(input$assign_group)

    output$grouping <- DT::renderDT(
      df$group,selection = 'none', editable = TRUE,
      rownames = TRUE,
      extensions = 'Buttons',
      colnames =  c("Samples","Groups"),
      options = list(
        paging = FALSE,
        searching = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list(targets = c(1,2), width = '200px')),
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('csv')
      ),

      class = "display"
    )

    output$groupingMessage <- NULL

    })



    # Edit and update group assignments
    shiny::observeEvent(input$grouping_cell_edit, {
      spsComps::shinyCatch({
        if (is.null(df$group)) warning("Warning: The matrix is not properly generated")
      }, blocking_level = "warning")
      df$group[input$grouping_cell_edit$row,input$grouping_cell_edit$col] <- input$grouping_cell_edit$value
    })

    # Save group assignment to a reactive variable

    gr <- shiny::reactiveVal()
    shiny::observeEvent(input$save_group, {
      # check if the df$group is proprierly assigned
      spsComps::shinyCatch({
        if (is.null(df$group)) warning("Warning: The groups are not properly saved")
      }, blocking_level = "warning")

      gr <- df$group
      gr <- as.data.frame(gr)
      colnames(gr) <- c("Samples", "Groups")
      gr(gr)
      # Notify the user when the groups are saved
      shiny::showNotification(
        "Assigned groups have been saved",
        closeButton = TRUE,
        duration = 10,
        type = "message"
      )
    })


    # Render the saved group assignments
    # Display the saved group table
    shiny::observeEvent(gr(), {
      shiny::req(gr())
      # check if the gr() is proprierly assigned
      spsComps::shinyCatch({
        if (is.null(gr())) warning("Warning: The groups are not properly saved")
      }, blocking_level = "warning")

      output$groups_assigned <- renderTable(gr())
    })


    # import pre-grouped table
    shinyFileChoose(
      input,
      "assignGroupByTable",
      roots = volumes,
      session = session
    )

    # Print selected file path to console
    shiny::observe({
      cat("\ninput$assignGroupByTable value:\n\n")
      print(input$assignGroupByTable)
    })

    # Save the path of the pre-grouped table
    assignGroupByTablePath <- shiny::reactiveVal()
    shiny::observe({
      File <- as.character(parseFilePaths(volumes, input$assignGroupByTable)$datapath)
      assignGroupByTablePath(File)
    })

    # Output selected file path to the browser
    output$AssignGroupByTablePath <- shiny::renderPrint({
      if (is.integer(input$assignGroupByTable)) {
        cat("No input file has been selected yet")
      } else {
        cat(
          paste0("Uploaded groups matrix is ",

                 as.character(parseFilePaths(volumes, input$assignGroupByTable)$datapath))
        )

      }
    })

    # Load and display the imported pre-grouped table

    # Loading the imported pre-grouped table and displaying it
    AssignTable <- shiny::reactiveVal()
    shiny::observeEvent(input$assignGroupByTable, {
      shiny::req(assignGroupByTablePath())
      # check if the assignGroupByTablePath() is proprierly assigned
      spsComps::shinyCatch({
        if (is.null(assignGroupByTablePath())) warning("Warning: The matrix is not properly loaded")
      }, blocking_level = "warning")

      data <- data.table::fread(
        file.path(assignGroupByTablePath()),
        header = TRUE,
        stringsAsFactors = FALSE
      )
      AssignTable(data)

      # Notify when the pre-grouped table is loaded
      shiny::showNotification(
        "Table has been loaded in card: Step 2 Result - Load the Matrix",
        closeButton = TRUE,
        duration = 10,
        type = "message"
      )
    })


    # Render the imported table
    shiny::observeEvent(AssignTable(),{
      spsComps::shinyCatch({
        if (is.null(AssignTable())) warning("Warning: The matrix is not properly loaded")
      }, blocking_level = "warning")
      output$ImportedTable <- renderTable(AssignTable())
      })


    #to be able to use the output.item in conditionalPanel
    output$ImportedTable <- shiny::renderPrint(cat("Please complete steps 1 and 2 to load the matrix."))



     # Notify when the pre-grouped table is loaded
    shiny::observeEvent(AssignTable(), {
      shiny::showNotification("Table has been loaded in card: Step 2 - Load the Matrix",  closeButton = TRUE, duration = 10,  type = "message")})




    # Output a message when data has not been elaborated yet
    output$RunAnalysisExpNote <- shiny::renderPrint({cat("Data has not been processed yet")})



    # Reactive variables to store pre-normalized and post-normalized matrices
    PreNor <- shiny::reactiveValues()
    PostNor <- shiny::reactiveValues()
    totalGenes <- shiny::reactiveVal()
    keptGenesPre <- shiny::reactiveVal()
    keptGenesPost <- shiny::reactiveVal()

    # Reactive value to store exploration analysis process
    rvExp <- shiny::reactiveValues()

    # shows a warning message to perform the previous steps
     output$infoNormMessage <- shiny::renderPrint(cat("Please complete steps 1, 2, and 3 to proceed with the analysis"))


     # reactiveVal to save execution time
     start_time_RunAnalysisExp <- shiny::reactiveVal()


     # Event triggered when "Run Analysis" button is clicked
    shiny::observeEvent(input$RunAnalysisExp,{

    # Check if the required steps (directories or group assignments) are completed
      if (is.integer(input$directory_raw_count) ||   is.integer(input$directory_deg) || (is.null(AssignTable()$Groups) && is.null(gr()$Groups))) {
        shiny::showNotification("Please complete the previous steps before proceeding.",  closeButton = TRUE, duration = NULL ,  type = "error")
      } else {

        #start time to monitor execution
        start_time_RunAnalysisExp(Sys.time())

    # Disable the RunAnalysisExp button and enable the killRunAnalysisExp button
        shinyjs::disable("RunAnalysisExp")
        shinyjs::enable("killRunAnalysisExp")

           # Show a running message
        output$RunAnalysisExpNote <- shiny::renderPrint({cat("Running ...")})


        # to avoid opening conditional panel before that the plot is ready
        output$ConditionalRunAnalysisExp <- shiny::renderText({"Running"})
        outputOptions(output, "ConditionalRunAnalysisExp", suspendWhenHidden = FALSE)

         # Use assigned table if available, otherwise use reactive group
        if (! is.null(AssignTable())){
          gr <- AssignTable()
        } else {
          gr <- gr()
        }
        # Run the exploration analysis in a background process
        tryCatch({
        rvExp$Exploration_process <- callr::r_bg(
        func = GetEdgerY,
        args = list(
          gr = gr,
          WDpn = WDirEX(),
          colIDgene = input$colIDgene,
          colCounts = input$colCounts,
          skip_preN = input$skip_preN,
          filterMethod = input$filterMethodPreProcess,
          min_count = input$min_count,
          min_total_count = input$min_total_count,
          large_n =input$large_n ,
          min_prop = input$min_prop,
          normMethod = input$normMethodExp
        ) ,supervise = TRUE ,error = getOption("callr.error", "error"))


        output$infoNormMessage <- NULL


        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the status of the exploration process
        shiny::observe({
          # Check if the process is still alive
          if(shiny::isolate(rvExp$Exploration_process$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)
            # If the process has finished, handle the results
          } else if(shiny::isolate(rvExp$Exploration_process$is_alive()) == FALSE) {
            # Try to capture the result from the background process
            tryCatch({

               shiny::isolate({
                 # Store pre and post-normalized matrices
              PreNor$y <- rvExp$Exploration_process$get_result()$ypre
              PreNor$logcounts <- rvExp$Exploration_process$get_result()$yPreLogcounts
              PostNor$y <- rvExp$Exploration_process$get_result()$ypost
              PostNor$logcounts <- rvExp$Exploration_process$get_result()$yPostLogcounts
              totalGenes(rvExp$Exploration_process$get_result()$totalGenes)
              keptGenesPre(rvExp$Exploration_process$get_result()$keptGenesPre)
              keptGenesPost(rvExp$Exploration_process$get_result()$keptGenesPost)

              # Show success notification
              shiny::showNotification("Samples have been processed. The table is now loaded in the panel: Step 3 Result - Data Processing ",  closeButton = TRUE, duration = 10,  type = "message")

              # Enable and disable buttons
              shinyjs::enable("RunAnalysisExp")
              shinyjs::disable("killRunAnalysisExp")


              # Update SelectInput
              updateSelectInput(session, "selectInfoExp", choices = c("samples","count"))


              ## Update color palettes for PCA, MDS, correlation plot, bar plot, box plot, heatmap, and saturation plot
              palettesD <- paletteer::palettes_d_names
              palettesC <- paletteer::palettes_c_names
              palettesFilteredGroup <- dplyr::filter(palettesD, length >= length(unique(PreNor$y$samples$group)),type == "qualitative")
              palettesFilteredSample <- dplyr::filter(palettesD, length >= length(unique(PreNor$y$samples$files)), type == "qualitative")


              #pca

              shiny::updateSelectizeInput(session, "PCApalette", choices = c(paste(palettesFilteredGroup$package, palettesFilteredGroup$palette, sep="::")))

              #mds

              shiny::updateSelectizeInput(session, "MDSpalette", choices = c(paste(palettesFilteredGroup$package, palettesFilteredGroup$palette, sep="::")))

              #corr plot

              shiny::updateSelectizeInput(session, "Corrpalette", choices = c(paste(palettesC$package, palettesC$palette, sep="::")))

              # barplot

              shiny::updateSelectizeInput(session, "selectColorBarplotExp", choices = c(paste(palettesFilteredGroup$package, palettesFilteredGroup$palette, sep="::")))

              #boxplot

              shiny::updateSelectizeInput(session, "selectColorBoxplotExp", choices = c(paste(palettesFilteredGroup$package, palettesFilteredGroup$palette, sep="::")))


              #heatmap

              shiny::updateSelectizeInput(session, "ColorPanelHeatmap", choices = c(paste(palettesC$package, palettesC$palette, sep="::")))

              # Saturationplot

              shiny::updateSelectizeInput(session, "paletteSatPlot", choices = c(paste(palettesFilteredSample$package, palettesFilteredSample$palette, sep="::")))



              #check end time of process
              end_time_RunAnalysisExp <- Sys.time()
              execution_time_RunAnalysisExp <- end_time_RunAnalysisExp - start_time_RunAnalysisExp()

              # Convert to hours, minutes, and seconds
              execution_time_sec_RunAnalysisExp <- as.numeric(execution_time_RunAnalysisExp, units="secs")
              hours_RunAnalysisExp <- floor(execution_time_sec_RunAnalysisExp / 3600)
              minutes_RunAnalysisExp <- floor((execution_time_sec_RunAnalysisExp %% 3600) / 60)
              seconds_RunAnalysisExp <- execution_time_sec_RunAnalysisExp %% 60


              # Update analysis note to "Done!"
              output$RunAnalysisExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunAnalysisExp, "hours", minutes_RunAnalysisExp, "minutes", round(seconds_RunAnalysisExp, 2), "seconds)")})

               }) #End of isolated


            }  , error = function(e) {

              # Handle errors
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )}
            )
          }
           })

    }# closing else
})


    # Event triggered when the kill button is clicked, stopping the exploration process
    shiny::observeEvent(input$killRunAnalysisExp, {
      # Kill the running process
      rvExp$Exploration_process$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

      # Enable and disable buttons
      shinyjs::enable("RunAnalysisExp")
      shinyjs::disable("killRunAnalysisExp")

      # Show "Killed" message if the process has been terminated
      if (shiny::isolate(rvExp$Exploration_process$is_alive()) == FALSE){
        output$RunAnalysisExpNote <- shiny::renderPrint({cat("Killed!")})
      }
    })


    # Update the displayed pre-normalization information based on the selected info type (samples or count)
    shiny::observeEvent(c(input$RunAnalysisExp,input$selectInfoExp),{

      shiny::req(PreNor$y)
      shiny::req(totalGenes())
      shiny::req(keptGenesPre())

      if (input$selectInfoExp == "samples") {
        data <- as.matrix(PreNor$y$samples)
        output$infoPreNorm <- DT::renderDT(
          data,
          selection = 'none',
          editable = FALSE,
          rownames = FALSE,
                 extensions = 'Buttons',
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = list(c(15,30, 50, 100, -1),
                              c('15', '30', '50','100', 'All')),
            searching = TRUE,
            fixedColumns = TRUE,
            #       columnDefs = list(list( targets = c(1,2), width = '200px')),
            autoWidth = TRUE,
            ordering = TRUE,
                 dom = 'Bfrtip',
                 buttons = c('csv','pageLength')
          ),

          class = "display"

        )

      } else if (input$selectInfoExp == "count") {
        data <- as.matrix(PreNor$y$count)
        output$infoPreNorm <- DT::renderDT(
          data,
          selection = 'none',
          editable = FALSE,
          rownames = TRUE,
                 extensions = 'Buttons',
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = list(c(15,30, 50, 100, -1),
                              c('15', '30', '50','100', 'All')),
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
                     dom = 'Bfrtip',
                      buttons = c('csv','pageLength')
          ),
          class = "display"
        )

      }


      output$InfoFilteringPre <- shiny::renderText({

        paste("Total genes:", totalGenes(), "|", "Kept genes:", keptGenesPre())


      })

    })


    # Update the displayed post-normalization information based on the selected info type (samples or count)
    shiny::observeEvent(c(input$RunAnalysisExp,input$selectInfoExp),{

      shiny::req(PostNor$y)
      shiny::req(totalGenes())
      shiny::req(keptGenesPost())


      if (input$selectInfoExp == "samples") {
        data <- as.matrix(PostNor$y$samples)
        output$infoPostNorm <- DT::renderDT(
          data,
          selection = 'none',
          editable = FALSE,
          rownames = FALSE,
                 extensions = 'Buttons',
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = list(c(15,30, 50, 100, -1),
                              c('15', '30', '50','100', 'All')),
            searching = TRUE,
            fixedColumns = TRUE,
            #       columnDefs = list(list( targets = c(1,2), width = '200px')),
            autoWidth = TRUE,
            ordering = TRUE,
                 dom = 'Bfrtip',
                     buttons = c('csv','pageLength')
          ),

          class = "display"
        )
      } else if (input$selectInfoExp == "count") {
        data <- as.matrix(PostNor$y$count)
        output$infoPostNorm <- DT::renderDT(
          data,
          selection = 'none',
          editable = FALSE,
          rownames = TRUE,
                 extensions = 'Buttons',
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = list(c(15,30, 50, 100, -1),
                              c('15', '30', '50','100', 'All')),
            searching = TRUE,
            fixedColumns = TRUE,
            #       columnDefs = list(list( targets = c(1,2), width = '200px')),
            autoWidth = TRUE,
            ordering = TRUE,
                    dom = 'Bfrtip',
                      buttons = c('csv','pageLength')
          ),
          class = "display"
        )
      }


      output$InfoFilteringPost <- shiny::renderText({

        paste("Total genes:", totalGenes(), "|", "Kept genes:", keptGenesPost())

      })

    })



    # Run MDS analysis and manage related UI elements and background operations

    # Reactive values to store background operations for MDS plots
    rvmdsPreplot <- shiny::reactiveValues()
    rvmdsPostplot <- shiny::reactiveValues()
    rvmdsPreplottly <- shiny::reactiveValues()
    rvmdsPostplottly <- shiny::reactiveValues()

    # Reactive values to save the result of MDS plots
    mdsPreplot <- shiny::reactiveVal()
    mdsPostplot <- shiny::reactiveVal()
    mdsPreplottly <- shiny::reactiveVal()
    mdsPostplottly <- shiny::reactiveVal()


    #start time to monitor execution
    start_time_RunMDSExp <- shiny::reactiveVal()

    # Output message to indicate the status of the MDS process (initial state is "Undone")
    output$RunMDSExpNote <- shiny::renderPrint({cat("Undone")})

    # Observe event when MDS is triggered by either 'Run' or 'Refresh' button
    shiny::observeEvent(c(input$RunMDSExp, input$RefreshRunMDSExp),{
      shiny::req(input$RunMDSExp) # Ensure 'Run' button is pressed

      # Validate the presence of pre- and post-normalization data
      if (is.null(PreNor$y) && is.null(PostNor$y)) {
        shiny::showNotification("Run MDS error: perform step 3",  closeButton = TRUE, duration = NULL ,  type = "error")

      } else {

        shiny::req(PreNor$y)
        shiny::req(PostNor$y)

        #start time to monitor execution
        start_time_RunMDSExp(Sys.time())


        # Disable/shinyjs::enable buttons to reflect the process state
        shinyjs::disable("RunMDSExp")
        shinyjs::enable("killRunMDSExp")
        shinyjs::disable("RefreshRunMDSExp")

        # Update output message to indicate the MDS process is running
        output$RunMDSExpNote <- shiny::renderPrint({cat("Running ...")})



        # to avoid opening conditional panel before that the plot is ready
        output$ConditionalRunMDS <- shiny::renderText("Running")
        shiny::outputOptions(output, 'ConditionalRunMDS', suspendWhenHidden = FALSE)


        # Run pre-normalization MDS plot in a background process using callr
        tryCatch({
          rvmdsPreplot$result <- callr::r_bg(
            func = mdsPlot,
            args = list(
              x = PreNor$y,
              Sample = PreNor$y$samples$files,
              Group = PreNor$y$samples$group,
              title = "MDS pre-normalization",
              palette = input$MDSpalette,
              maxOverlaps = input$MDSMaxOverlaps,
              sizeLabel = input$MDSsizeLabel,
              top = input$MDStop,
              gene.selection = input$MDSgene_selection
              ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # ENd of tryCatch

       # Observe the state of the background process for pre-norm ggplot
        shiny::observe({

          if(shiny::isolate(rvmdsPreplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvmdsPreplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                mdsPreplot(rvmdsPreplot$result$get_result())

                output$MDSpreNorm <- shiny::renderPlot({

                  plot(mdsPreplot())
                }
                ,
                width ="auto", height = "auto", res = 96
                )


              }) #End of isolated
              #

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) #End of tryCatch
          }

        })

        # Run pre-normalization MDS plot (Plotly) in a background process using callr
        tryCatch({
          rvmdsPreplottly$result <- callr::r_bg(
            func = mdsPlottly,
            args = list(
              x = PreNor$y,
              Sample = PreNor$y$samples$files,
              Group = PreNor$y$samples$group,
              title = "MDS pre-normalization",
              palette = input$MDSpalette,
              top = input$MDStop,
              gene.selection = input$MDSgene_selection
              ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the state of the background process for pre-norm Plotly
        shiny::observe({

          if(shiny::isolate(rvmdsPreplottly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvmdsPreplottly$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                mdsPreplottly(rvmdsPreplottly$result$get_result())

                output$MDSpreNormPlottly <- plotly::renderPlotly({

                  print(mdsPreplottly())}
                )

              }) #End of isolated


            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) #ENd of tryCatch
          }
        })


        # Run post-normalization MDS plot in a background process
        tryCatch({
          rvmdsPostplot$result <- callr::r_bg(
            func = mdsPlot,
            args = list(
              x = PostNor$y,
              Sample = PostNor$y$samples$files,
              Group = PostNor$y$samples$group,
              title = "MDS post-normalization",
              palette = input$MDSpalette,
              maxOverlaps = input$MDSMaxOverlaps,
              sizeLabel = input$MDSsizeLabel,
              top = input$MDStop,
              gene.selection = input$MDSgene_selection
              ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the state of the background process for post-norm ggplot
        shiny::observe({

          if(shiny::isolate(rvmdsPostplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvmdsPostplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                mdsPostplot(rvmdsPostplot$result$get_result())

                output$MDSpostNorm <- shiny::renderPlot({

                  plot(mdsPostplot())
                }
                ,
                width ="auto", height = "auto", res = 96
                )




              }) #ENd of isolated


            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) #ENd of tryCatch
          }

        })

        # Run post-normalization MDS plot (Plotly) in a background process
        tryCatch({
          rvmdsPostplottly$result <- callr::r_bg(
            func = mdsPlottly,
            args = list(
              x = PostNor$y,
              Sample = PostNor$y$samples$files,
              Group = PostNor$y$samples$group,
              title = "MDS post-normalization",
              palette = input$MDSpalette,
              top = input$MDStop,
              gene.selection = input$MDSgene_selection
              ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the state of the background process for post-norm Plotly
        shiny::observe({

          if(shiny::isolate(rvmdsPostplottly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvmdsPostplottly$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                mdsPostplottly(rvmdsPostplottly$result$get_result())

                output$MDSpostNormPlottly <- plotly::renderPlotly({

                  print(mdsPostplottly())}
                )




              }) #End of isolated

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) #End of tryCatch
          }
        })



        #check end time of process
        end_time_RunMDSExp <- Sys.time()
        execution_time_RunMDSExp <- end_time_RunMDSExp - start_time_RunMDSExp()

        # Convert to hours, minutes, and seconds
        execution_time_sec_RunMDSExp <- as.numeric(execution_time_RunMDSExp, units="secs")
        hours_RunMDSExp <- floor(execution_time_sec_RunMDSExp / 3600)
        minutes_RunMDSExp <- floor((execution_time_sec_RunMDSExp %% 3600) / 60)
        seconds_RunMDSExp <- execution_time_sec_RunMDSExp %% 60


        # Update analysis note to "Done!"
        output$RunMDSExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunMDSExp, "hours", minutes_RunMDSExp, "minutes", round(seconds_RunMDSExp, 2), "seconds)")})


        # Adjust buttons when the process is done
        shinyjs::disable("RunMDSExp")
        shinyjs::disable("killRunMDSExp")
        shinyjs::enable("RefreshRunMDSExp")



      }
    })

    # Interrupt MDS process if the kill button is clicked
    shiny::observeEvent(input$killRunMDSExp, {

      # Terminate all running MDS processes
      rvmdsPreplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvmdsPostplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvmdsPreplottly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvmdsPostplottly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

      # Adjust buttons upon process termination
      shinyjs::enable("RunMDSExp")
      shinyjs::disable("killRunMDSExp")

      # Display "Killed" message if all processes are terminated
      if (shiny::isolate(rvmdsPreplot$result$is_alive()) == FALSE && shiny::isolate(rvmdsPostplot$result$is_alive()) == FALSE && shiny::isolate(rvmdsPreplottly$result$is_alive()) == FALSE && shiny::isolate(rvmdsPostplottly$result$is_alive()) == FALSE ){
        output$RunMDSExpNote <- shiny::renderPrint({cat("Killed!")})
      }
    })


  # Download handler for pre-normalization MDS plot
    output$MDSLinkpre <- shiny::downloadHandler(
      filename = function() {
        paste("MDS_pre_"
              , Sys.Date(), ".", input$MDSformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (mdsPreplot()), width = input$widthMDS, height = input$heightMDS,units = input$unitsMDS, device = input$MDSformat, dpi = input$resMDS)
      })


    # Download handler for post-normalization MDS plot
    output$MDSLinkpost <- shiny::downloadHandler(
      filename = function() {
        paste("MDS_post_"
              , Sys.Date(), ".", input$MDSformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (mdsPostplot()), width = input$widthMDS, height = input$heightMDS,units = input$unitsMDS, device = input$MDSformat, dpi = input$resMDS)
      })



    # show value
    mdsInfoPre <- shiny::eventReactive(c(input$RunMDSExp,input$RefreshRunMDSExp),{

      shiny::req(PreNor$y)
      mdsinfo(PreNor$y, top = input$MDStop, gene.selection = input$MDSgene_selection)

    })

    mdsInfoPost <- shiny::eventReactive(c(input$RunMDSExp,input$RefreshRunMDSExp),{
      shiny::req(PostNor$y)
      mdsinfo(PostNor$y, top = input$MDStop, gene.selection = input$MDSgene_selection)
    })


    shiny::observeEvent(input$selectMDSexp,{

      output$MDSValuePre <- shiny::renderPrint({
        shiny::req(input$RunMDSExp)
        shiny::req(mdsInfoPre())
        mdsInfoPre()[input$selectMDSexp]
      })
      output$MDSValuePost <- shiny::renderPrint({
        shiny::req(input$RunMDSExp)
        shiny::req(mdsInfoPost())
        mdsInfoPost()[input$selectMDSexp]
      })
    })





   # Run PCA analysis and manage related UI elements and background operations

    # Reactive values to store background operations for PCA plots
    rvpcaPreplot <- shiny::reactiveValues()
    rvpcaPostplot <- shiny::reactiveValues()
    rvpcaPreplottly <- shiny::reactiveValues()
    rvpcaPostplottly <- shiny::reactiveValues()

      # Reactive values to save the result of PCA plots
    pcaPreplot <- shiny::reactiveVal()
    pcaPostplot <- shiny::reactiveVal()
    pcaPreplottly <- shiny::reactiveVal()
    pcaPostplottly <- shiny::reactiveVal()

    #start time to monitor execution
    start_time_RunPCAExp <- shiny::reactiveVal()

    # Output message to indicate the status of the PCA process (initial state is "Undone")
    output$RunPCAExpNote <- shiny::renderPrint({cat("Undone")})


    # Observe event when PCA is triggered by either 'Run' or 'Refresh' button
    shiny::observeEvent(c(input$RunPCAExp, input$RefreshRunPCAExp),{
      shiny::req(input$RunPCAExp)
      # Validate the presence of pre- and post-normalization data
      if (is.null(PreNor$y) && is.null(PostNor$y)) {
        shiny::showNotification("Run PCA error: perform step 3",  closeButton = TRUE, duration = NULL ,  type = "error")

      } else {

        shiny::req(PreNor$y)
        shiny::req(PostNor$y)

        #start time to monitor execution
        start_time_RunPCAExp(Sys.time())

        # Disable/enable buttons to reflect the process state
        shinyjs::disable("RunPCAExp")
        shinyjs::enable("killRunPCAExp")
        shinyjs::disable("RefreshRunPCAExp")
        # Update output message to indicate the PCA process is running
        output$RunPCAExpNote <- shiny::renderPrint({cat("Running ...")})

        # to avoid opening conditional panel before that the plot is ready
        output$ConditionalRunPCA <- shiny::renderText("Running")
        shiny::outputOptions(output, 'ConditionalRunPCA', suspendWhenHidden = FALSE)


        # Run pre-normalization PCA plot in a background process using callr
        tryCatch({
          rvpcaPreplot$result <- callr::r_bg(
            func = pcaPlot,
            args = list(
              logcounts = PreNor$logcounts,
              Sample = PreNor$y$samples$files,
              Group = PreNor$y$samples$group,
              title = "PCA pre-normalization",
              palette = input$PCApalette,
              maxOverlaps = input$PCAMaxOverlaps,
              sizeLabel = input$PCAsizeLabel,
              center = input$PCAcenter,
              scale = input$PCAscale
            ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the state of the background process for pre-norm ggplot
        shiny::observe({

          if(shiny::isolate(rvpcaPreplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvpcaPreplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                pcaPreplot(rvpcaPreplot$result$get_result())

                output$PCApreNorm <- shiny::renderPlot({

                  plot(pcaPreplot())
                }
                ,
                width ="auto", height = "auto", res = 96
                )


              }) #End of isolated
              #

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }

        })

        # Run pre-normalization PCA plot (Plotly) in a background process using callr
        tryCatch({
          rvpcaPreplottly$result <- callr::r_bg(
            func = pcaPlottly,
            args = list(
              logcounts = PreNor$logcounts,
              Sample = PreNor$y$samples$files,
              Group = PreNor$y$samples$group,
              title = "PCA pre-normalization",
              palette = input$PCApalette,
              center = input$PCAcenter,
              scale = input$PCAscale
            ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # finisce tryCatch

        # Observe the state of the background process for pre-norm Plotly
        shiny::observe({

          if(shiny::isolate(rvpcaPreplottly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvpcaPreplottly$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                pcaPreplottly(rvpcaPreplottly$result$get_result())

                output$PCApreNormPlottly <- plotly::renderPlotly({

                  print(pcaPreplottly())}
                )




              }) #End of isolated

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) #End of tryCatch
          }
        })

        # Run post-normalization PCA plot in a background process
        tryCatch({
          rvpcaPostplot$result <- callr::r_bg(
            func = pcaPlot,
            args = list(
              logcounts = PostNor$logcounts,
              Sample = PostNor$y$samples$files,
              Group = PostNor$y$samples$group,
              title = "PCA post-normalization",
              palette = input$PCApalette,
              maxOverlaps = input$PCAMaxOverlaps,
              sizeLabel = input$PCAsizeLabel,
              center = input$PCAcenter,
              scale = input$PCAscale
            ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the state of the background process for post-norm ggplot
        shiny::observe({

          if(shiny::isolate(rvpcaPostplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvpcaPostplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                pcaPostplot(rvpcaPostplot$result$get_result())

                output$PCApostNorm <- shiny::renderPlot({

                  plot(pcaPostplot())
                }
                ,
                width ="auto", height = "auto", res = 96
                )




              }) #End of isolated
              #

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) #End of tryCatch
          }

        })

        # Run post-normalization PCA plot (Plotly) in a background process
        tryCatch({
          rvpcaPostplottly$result <- callr::r_bg(
            func = pcaPlottly,
            args = list(
              logcounts = PostNor$logcounts,
              Sample = PostNor$y$samples$files,
              Group = PostNor$y$samples$group,
              title = "PCA post-normalization",
              palette = input$PCApalette,
              center = input$PCAcenter,
              scale = input$PCAscale
            ),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the state of the background process for post-norm Plotly
        shiny::observe({

          if(shiny::isolate(rvpcaPostplottly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvpcaPostplottly$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                pcaPostplottly(rvpcaPostplottly$result$get_result())

                output$PCApostNormPlottly <- plotly::renderPlotly({

                  print(pcaPostplottly())}
                )




              }) #End of isolated
              #

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) #End of tryCatch
          }
        })


        #check end time of process
        end_time_RunPCAExp <- Sys.time()
        execution_time_RunPCAExp <- end_time_RunPCAExp - start_time_RunPCAExp()

        # Convert to hours, minutes, and seconds
        execution_time_sec_RunPCAExp <- as.numeric(execution_time_RunPCAExp, units="secs")
        hours_RunPCAExp <- floor(execution_time_sec_RunPCAExp / 3600)
        minutes_RunPCAExp <- floor((execution_time_sec_RunPCAExp %% 3600) / 60)
        seconds_RunPCAExp <- execution_time_sec_RunPCAExp %% 60


        # Update analysis note to "Done!"
        output$RunPCAExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunPCAExp, "hours", minutes_RunPCAExp, "minutes", round(seconds_RunPCAExp, 2), "seconds)")})



        # Adjust buttons when the process is done
        shinyjs::disable("RunPCAExp")
        shinyjs::disable("killRunPCAExp")
        shinyjs::enable("RefreshRunPCAExp")

      }
    })


    # Interrupt PCA process if the kill button is clicked
    shiny::observeEvent(input$killRunPCAExp, {
      # Terminate all running PCA processes
      rvpcaPreplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvpcaPostplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvpcaPreplottly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvpcaPostplottly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

      # Adjust buttons upon process termination
      shinyjs::enable("RunPCAExp")
      shinyjs::disable("killRunPCAExp")

      # Display "Killed" message if all processes are terminated
      if (shiny::isolate(rvpcaPreplot$result$is_alive()) == FALSE && shiny::isolate(rvpcaPostplot$result$is_alive()) == FALSE && shiny::isolate(rvpcaPreplottly$result$is_alive()) == FALSE && shiny::isolate(rvpcaPostplottly$result$is_alive()) == FALSE ){
        output$RunPCAExpNote <- shiny::renderPrint({cat("Killed!")})
      }
    })



    # Download handler for pre-normalization PCA plot
    output$PCALinkpre <- shiny::downloadHandler(
      filename = function() {
        paste("PCA_pre_"
              , Sys.Date(), ".", input$PCAformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (pcaPreplot()), width = input$widthPCA, height = input$heightPCA,units = input$unitsPCA, device = input$PCAformat, dpi = input$resPCA)
      })

    # Download handler for post-normalization PCA plot
    output$PCALinkpost <- shiny::downloadHandler(
      filename = function() {
        paste("PCA_post_"
              , Sys.Date(), ".", input$PCAformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (pcaPostplot()), width = input$widthPCA, height = input$heightPCA,units = input$unitsPCA, device = input$PCAformat, dpi = input$resPCA)
      })



    # show value
    pcaInfoPre <- shiny::eventReactive(c(input$RunPCAExp,input$RefreshRunPCAExp),{

      shiny::req(PreNor$logcounts)
      pcainfo(PreNor$logcounts, center = input$PCAcenter, scale = input$PCAscale)

    })

    pcaInfoPost <- shiny::eventReactive(c(input$RunPCAExp,input$RefreshRunPCAExp),{
      shiny::req(PostNor$logcounts)
      pcainfo(PostNor$logcounts, center = input$PCAcenter, scale = input$PCAscale)
    })


    shiny::observeEvent(input$selectPCAExp,{

      output$PCAValuePre <- shiny::renderPrint({
        shiny::req(input$RunPCAExp)
        shiny::req(pcaInfoPre())
        pcaInfoPre()[input$selectPCAExp]
      })
      output$PCAValuePost <- shiny::renderPrint({
        shiny::req(input$RunPCAExp)
        shiny::req(pcaInfoPost())
        pcaInfoPost()[input$selectPCAExp]
      })
    })


    # Run Box plot analysis and manage related UI elements and background operations
    # Reactive values to store background operations for Box plotplots
    rvBOXPlotPreExp <- shiny::reactiveValues()
    rvBOXPlotPostExp <- shiny::reactiveValues()
    # Reactive values to save the result of Box plotplots
    BOXPlotPreExp <- shiny::reactiveVal()
    BOXPlotPostExp <- shiny::reactiveVal()

    #start time to monitor execution
    start_time_RunBoxplotExp <- shiny::reactiveVal()

    # Default note before running the Box plotanalysis
    output$RunBoxplotExpNote <- shiny::renderPrint({cat("Undone")})

    # Observe the button events for running or refreshing the Box plotanalysis
    shiny::observeEvent(c(input$RunBoxplotExp, input$RefreshBoxplotExp),{
      shiny::req(input$RunBoxplotExp)

    # If the normalization data is missing, show an error notification
      if (is.null(PreNor$y) && is.null(PostNor$y)) {
        shiny::showNotification("Run Box plot error: perform step 3",  closeButton = TRUE, duration = NULL ,  type = "error")

      } else {



        shiny::req(PreNor$y)
        shiny::req(PostNor$y)

        #start time to monitor execution
        start_time_RunBoxplotExp(Sys.time())

        # Disable and enable relevant UI buttons for process control
        shinyjs::disable("RunBoxplotExp")
        shinyjs::enable("killRunBoxplotExp")
        shinyjs::disable("RefreshBoxplotExp")

        # Indicate that the Box plot process is running
        output$RunBoxplotExpNote <- shiny::renderPrint({cat("Running ...")})

        # to avoid opening conditional panel before that the plot is ready
        output$ConditionalRunBox <- shiny::renderText("Running")
        shiny::outputOptions(output, 'ConditionalRunBox', suspendWhenHidden = FALSE)

        # Pre-normalization Box plot process
        tryCatch({
          rvBOXPlotPreExp$result <- callr::r_bg(
            func = boxplotExp,
            args = list(
              x = PreNor$y,
              y = PreNor$logcounts,
              palette = input$selectColorBoxplotExp,
              main = "Box plot pre-normalization",
              selectOrder = input$selectOrderBoxplotExp),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the background process for pre-normalization Boxplot
        shiny::observe({

          if(shiny::isolate(rvBOXPlotPreExp$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvBOXPlotPreExp$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                BOXPlotPreExp(rvBOXPlotPreExp$result$get_result())

                output$BOXpreNorm <- shiny::renderPlot({
                  plot(BOXPlotPreExp())
                },width ="auto", height = "auto",res = 96)

                output$BOXpreNormPlottly <- plotly::renderPlotly({
                  plotly::ggplotly(BOXPlotPreExp())
                })

              }) # End of isolated

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }

        })

        # Post-normalization Box plotprocess
        tryCatch({
          rvBOXPlotPostExp$result <- callr::r_bg(
            func = boxplotExp,
            args = list(
              x = PostNor$y,
              y = PostNor$logcounts,
              palette = input$selectColorBoxplotExp,
              main = "Box plot post-normalization",
              selectOrder = input$selectOrderBoxplotExp),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the background process for post-normalization Boxplot
        shiny::observe({

          if(shiny::isolate(rvBOXPlotPostExp$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvBOXPlotPostExp$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                BOXPlotPostExp(rvBOXPlotPostExp$result$get_result())


                output$BOXpostNorm <- shiny::renderPlot({
                  plot(BOXPlotPostExp())
                }, width ="auto", height = "auto",res = 96)
                output$BOXpostNormPlottly <- plotly::renderPlotly({
                  plotly::ggplotly(BOXPlotPostExp())
                })

              }) # End of isolated


            } ,error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }
        })




        #check end time of process
        end_time_RunBoxplotExp <- Sys.time()
        execution_time_RunBoxplotExp <- end_time_RunBoxplotExp - start_time_RunBoxplotExp()

        # Convert to hours, minutes, and seconds
        execution_time_sec_RunBoxplotExp <- as.numeric(execution_time_RunBoxplotExp, units="secs")
        hours_RunBoxplotExp <- floor(execution_time_sec_RunBoxplotExp / 3600)
        minutes_RunBoxplotExp <- floor((execution_time_sec_RunBoxplotExp %% 3600) / 60)
        seconds_RunBoxplotExp <- execution_time_sec_RunBoxplotExp %% 60


        # Update analysis note to "Done!"
        output$RunBoxplotExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunBoxplotExp, "hours", minutes_RunBoxplotExp, "minutes", round(seconds_RunBoxplotExp, 2), "seconds)")})



        # Enable or disable buttons as necessary after the process is completed
        shinyjs::disable("RunBoxplotExp")
        shinyjs::disable("killRunBoxplotExp")
        shinyjs::enable("RefreshBoxplotExp")


      }
    })

    # Download handler for pre-normalization Boxplot
    output$BoxplotLinkpre <- shiny::downloadHandler(
      filename = function() {
        paste("Box_pre_"
              , Sys.Date(), ".", input$Boxplotformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (BOXPlotPreExp()), width = input$widthBoxplot, height = input$heightBoxplot,units = input$unitsBoxplot, device = input$Boxplotformat, dpi = input$resBoxplot)
      })

    # Download handler for post-normalization Boxplot
    output$BoxplotLinkpost <- shiny::downloadHandler(
      filename = function() {
        paste("Box_post_"
              , Sys.Date(), ".", input$Boxplotformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (BOXPlotPostExp()), width = input$widthBoxplot, height = input$heightBoxplot,units = input$unitsBoxplot, device = input$Boxplotformat, dpi = input$resBoxplot)
      })





    # Run Barplot analysis and manage related UI elements and background operations
    # Reactive values to store background operations for Barplot plots
    rvBARPlotPreExp <- shiny::reactiveValues()
    rvBARPlotPostExp <- shiny::reactiveValues()

    # Reactive values to save the result of Barplot plots
    BARPlotPreExp <- shiny::reactiveVal()
    BARPlotPostExp <- shiny::reactiveVal()

    #start time to monitor execution
    start_time_RunBarplotExp <- shiny::reactiveVal()

    # Default note before running the Barplot analysis
    output$RunBarplotExpNote <- shiny::renderPrint({cat("Undone")})

    # Observe the button events for running or refreshing the Barplot analysis
    shiny::observeEvent(c(input$RunBarplotExp, input$RefreshBarplotExp),{
      shiny::req(input$RunBarplotExp)

      # If the normalization data is missing, show an error notification
      if (is.null(PreNor$y) && is.null(PostNor$y)) {
        shiny::showNotification("Run Barplot error: perform step 3",  closeButton = TRUE, duration = NULL ,  type = "error")

      } else {



      shiny::req(PreNor$y)
      shiny::req(PostNor$y)

      #start time to monitor execution
      start_time_RunBarplotExp(Sys.time())

      # Disable and enable relevant UI buttons for process control
      shinyjs::disable("RunBarplotExp")
      shinyjs::enable("killRunBarplotExp")
      shinyjs::disable("RefreshBarplotExp")
      # Indicate that the Barplot process is running
      output$RunBarplotExpNote <- shiny::renderPrint({cat("Running ...")})

      # to avoid opening conditional panel before that the plot is ready
      output$ConditionalRunBar <- shiny::renderText("Running")
      shiny::outputOptions(output, 'ConditionalRunBar', suspendWhenHidden = FALSE)

      # Pre-normalization Barplot process
      tryCatch({
        rvBARPlotPreExp$result <- callr::r_bg(
          func = barplotExp,
          args = list(
            x = PreNor$y,
            palette = input$selectColorBarplotExp,
            main = "Barplot pre-normalization",
            selectOrder = input$selectOrderBarplotExp,
            effecLibSize = "FALSE"),
          supervise = TRUE ,error = getOption("callr.error", "error"))
      }
      , error = function(e) {
        shiny::showNotification(
          "error occurs",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # End of tryCatch

      # Observe the background process for pre-normalization Barplot
    shiny::observe({

      if(shiny::isolate(rvBARPlotPreExp$result$is_alive()) == TRUE)  {
        shiny::invalidateLater(2000, session)

      } else if(shiny::isolate(rvBARPlotPreExp$result$is_alive()) == FALSE) {

        tryCatch({

           shiny::isolate({

       BARPlotPreExp(rvBARPlotPreExp$result$get_result())

            output$PlotBARplotPre <- shiny::renderPlot({
              plot(BARPlotPreExp())
            },
            width ="auto", height = "auto",res = 96
            )

            output$PlotBARplotPrePlottly <- plotly::renderPlotly({
              plotly::ggplotly(BARPlotPreExp())
            })



           }) # End of isolated


        }  , error = function(e) {
          shiny::showNotification(
            "An error has occurred or the process was killed",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
          }) # End of tryCatch
      }

    })



    # Post-normalization Barplot process
    tryCatch({
      rvBARPlotPostExp$result <- callr::r_bg(
        func = barplotExp,
        args = list(
          x = PostNor$y,
          palette = input$selectColorBarplotExp,
          main = "Barplot post-normalization",
          selectOrder = input$selectOrderBarplotExp,
          effecLibSize = "TRUE"),
        supervise = TRUE ,error = getOption("callr.error", "error"))
    }
    , error = function(e) {
      shiny::showNotification(
        "error occurs",
        action = NULL,
        duration = NULL,
        closeButton = TRUE,
        id = NULL,
        type = "error"
      )
    }) # End of tryCatch

    # Observe the background process for post-normalization Barplot
    shiny::observe({

      if(shiny::isolate(rvBARPlotPostExp$result$is_alive()) == TRUE)  {
        shiny::invalidateLater(2000, session)

      } else if(shiny::isolate(rvBARPlotPostExp$result$is_alive()) == FALSE) {

        tryCatch({

          shiny::isolate({

            BARPlotPostExp(rvBARPlotPostExp$result$get_result())

            output$PlotBARplotPost <- shiny::renderPlot({
              plot(BARPlotPostExp())
            }
            ,width ="auto", height = "auto",res = 96
            )
            output$PlotBARplotPostPlottly <- plotly::renderPlotly({
              plotly::ggplotly(BARPlotPostExp())
            })

          }) # End of isolated


        } ,error = function(e) {
          shiny::showNotification(
            "An error has occurred or the process was killed",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch
      }

    })


    #check end time of process
    end_time_RunBarplotExp <- Sys.time()
    execution_time_RunBarplotExp <- end_time_RunBarplotExp - start_time_RunBarplotExp()

    # Convert to hours, minutes, and seconds
    execution_time_sec_RunBarplotExp <- as.numeric(execution_time_RunBarplotExp, units="secs")
    hours_RunBarplotExp <- floor(execution_time_sec_RunBarplotExp / 3600)
    minutes_RunBarplotExp <- floor((execution_time_sec_RunBarplotExp %% 3600) / 60)
    seconds_RunBarplotExp <- execution_time_sec_RunBarplotExp %% 60


    # Update analysis note to "Done!"
    output$RunBarplotExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunBarplotExp, "hours", minutes_RunBarplotExp, "minutes", round(seconds_RunBarplotExp, 2), "seconds)")})



    shinyjs::disable("RunBarplotExp")
    shinyjs::disable("killRunBarplotExp")
    shinyjs::enable("RefreshBarplotExp")



}
    })

    # Interrupt the Barplot process if the kill button is clicked
    shiny::observeEvent(input$killRunBarplotExp, {
      # Terminate the background processes for pre- and post-normalization Barplot
      rvBARPlotPreExp$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvBARPlotPostExp$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

      shinyjs::enable("RunBarplotExp")
      shinyjs::disable("killRunBarplotExp")

      # If both processes are terminated, update the status note
      if (shiny::isolate(rvBARPlotPreExp$result$is_alive()) == FALSE && shiny::isolate(rvBARPlotPostExp$result$is_alive()) == FALSE ){
        output$RunBarplotExpNote <- shiny::renderPrint({cat("Killed!")})
      }
    })

    # Download handler for pre-normalization Barplot
    output$BarplotLinkpre <- shiny::downloadHandler(
      filename = function() {
        paste("Barplot_pre_"
              , Sys.Date(), ".", input$Barplotformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (BARPlotPreExp()), width = input$widthBarplot, height = input$heightBarplot,units = input$unitsBarplot, device = input$Barplotformat, dpi = input$resBarplot)
      })


    # Download handler for post-normalization Barplot
    output$BarplotLinkpost <- shiny::downloadHandler(
      filename = function() {
        paste("Barplot_post_"
              , Sys.Date(), ".", input$Barplotformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (BARPlotPostExp()), width = input$widthBarplot, height = input$heightBarplot,units = input$unitsBarplot, device = input$Barplotformat, dpi = input$resBarplot)
      })







    # heatmap most variable expression
    # Reactive values for background operations
    # These reactive values are used to store the background processes and the results
    # for both pre- and post-normalization heatmaps (ggplot and plotly).

    rvHeatmapMVExpPreplot <- shiny::reactiveValues() # For pre-normalization heatmap (ggplot)
    rvHeatmapMVExpPreplotly <- shiny::reactiveValues() # For pre-normalization heatmap (plotly)
    rvHeatmapMVExpPostplot <- shiny::reactiveValues() # For post-normalization heatmap (ggplot)
    rvHeatmapMVExpPostplotly <- shiny::reactiveValues() # For post-normalization heatmap (plotly)

    # Reactive values to store the results of the heatmap plots
    HeatmapMVExpPreplot <- shiny::reactiveVal() # Stores the ggplot pre-normalization heatmap
    HeatmapMVExpPreplotly <- shiny::reactiveVal()  # Stores the plotly pre-normalization heatmap
    HeatmapMVExpPostplot <- shiny::reactiveVal()  # Stores the ggplot post-normalization heatmap
    HeatmapMVExpPostplotly <- shiny::reactiveVal()  # Stores the plotly post-normalization heatmap


    #start time to monitor execution
    start_time_RunHeatmapExp <- shiny::reactiveVal()

    # Initial message for heatmap process
    output$RunHeatmapExpNote <- shiny::renderPrint({cat("Undone")})

    # Observing event when Heatmap button is clicked or refresh button is used
    shiny::observeEvent(c(input$RunHeatmapExp, input$RefreshRunHeatmapExp),{
      shiny::req(input$RunHeatmapExp)

      # If logcounts are not available, show error notification
      if (is.null(PreNor$logcounts) && is.null(PostNor$logcounts)) {
        shiny::showNotification("Run Exp.heatmap error: perform step 3",  closeButton = TRUE, duration = NULL ,  type = "error")

      } else {
        # Ensure pre- and post-normalization logcounts are available
        shiny::req(PreNor$logcounts)
        shiny::req(PostNor$logcounts)

        #start time to monitor execution
        start_time_RunHeatmapExp(Sys.time())

        # Disable/enable UI buttons during heatmap execution
        shinyjs::disable("RunHeatmapExp")
        shinyjs::enable("killRunHeatmapExp")
        shinyjs::disable("RefreshRunHeatmapExp")
        # Update the status note to show "Running..."
        output$RunHeatmapExpNote <- shiny::renderPrint({cat("Running ...")})

        # to avoid opening conditional panel before that the plot is ready
        output$ConditionalRunHeatmap <- shiny::renderText("Running")
        shiny::outputOptions(output, 'ConditionalRunHeatmap', suspendWhenHidden = FALSE)


        # Generate pre-normalization heatmap (ggplot)
        tryCatch({
          rvHeatmapMVExpPreplot$result <- callr::r_bg(
            func = HeatmapExp,
            args = list(
              x = PreNor$logcounts,
              ColorPanel= input$ColorPanelHeatmap,
              cutree_rows = input$cutree_rowsHeatmap,
              cutree_cols= input$cutree_colsHeatmap,
              scale = input$scaleHeatmap,
              cluster = input$clusterHeatmap,
              show_names = input$show_namesHeatmap,
              NumGenes = input$NumGenesHeatmap),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) #  End of tryCatch

        # Observe the status of the pre-normalization ggplot process
        shiny::observe({

          if(shiny::isolate(rvHeatmapMVExpPreplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapMVExpPreplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                HeatmapMVExpPreplot(rvHeatmapMVExpPreplot$result$get_result())

                output$HeatmapPreNormExp <- shiny::renderPlot({

                  print(HeatmapMVExpPreplot())
                }
                ,
                width ="auto", height = "auto", res = 96
                )

              })# End of isolated

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }

        })

        # Generate pre-normalization heatmap (plotly)
        tryCatch({
          rvHeatmapMVExpPreplotly$result <- callr::r_bg(
            func = HeatmapExpPlotly,
            args = list(
              x = PreNor$logcounts,
              ColorPanel= input$ColorPanelHeatmap,
              cluster = input$clusterHeatmap,
              scale = input$scaleHeatmap,
              show_names = input$show_namesHeatmap,
              NumGenes = input$NumGenesHeatmap),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the status of the pre-normalization plotly process
        shiny::observe({

          if(shiny::isolate(rvHeatmapMVExpPreplotly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapMVExpPreplotly$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                HeatmapMVExpPreplotly(rvHeatmapMVExpPreplotly$result$get_result())




                output$HeatmapPreNormExpPlotly <- plotly::renderPlotly({

                  print(HeatmapMVExpPreplotly())}
                )




              }) # End of isolated

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }
        })




        # Generate post-normalization heatmap (ggplot)
        tryCatch({
          rvHeatmapMVExpPostplot$result <- callr::r_bg(
            func = HeatmapExp,
            args = list(
              x = PostNor$logcounts,
              ColorPanel= input$ColorPanelHeatmap,
              cutree_rows = input$cutree_rowsHeatmap,
              cutree_cols= input$cutree_colsHeatmap,
              scale = input$scaleHeatmap,
              cluster = input$clusterHeatmap,
              show_names = input$show_namesHeatmap,
              NumGenes = input$NumGenesHeatmap),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the status of the post-normalization ggplot process
        shiny::observe({

          if(shiny::isolate(rvHeatmapMVExpPostplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapMVExpPostplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                HeatmapMVExpPostplot(rvHeatmapMVExpPostplot$result$get_result())

                output$HeatmapPostNormExp <- shiny::renderPlot({

                  print(HeatmapMVExpPostplot())}
                  ,
                  width ="auto", height = "auto",res = 96
                )

              }) # End of isolated

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }

        })


        # Generate post-normalization heatmap (plotly)
        tryCatch({
          rvHeatmapMVExpPostplotly$result <- callr::r_bg(
            func = HeatmapExpPlotly,
            args = list(
              x = PostNor$logcounts,
              ColorPanel= input$ColorPanelHeatmap,
              cluster = input$clusterHeatmap,
              scale = input$scaleHeatmap,
              show_names = input$show_namesHeatmap,
              NumGenes = input$NumGenesHeatmap),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the status of the post-normalization plotly process
        shiny::observe({

          if(shiny::isolate(rvHeatmapMVExpPostplotly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapMVExpPostplotly$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                HeatmapMVExpPostplotly(rvHeatmapMVExpPostplotly$result$get_result())



                output$HeatmapPostNormExpPlotly <- plotly::renderPlotly({

                  print(HeatmapMVExpPostplotly())}
                )


              }) # End of isolated
              #

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }
        })



        #check end time of process
        end_time_RunHeatmapExp <- Sys.time()
        execution_time_RunHeatmapExp <- end_time_RunHeatmapExp - start_time_RunHeatmapExp()

        # Convert to hours, minutes, and seconds
        execution_time_sec_RunHeatmapExp <- as.numeric(execution_time_RunHeatmapExp, units="secs")
        hours_RunHeatmapExp <- floor(execution_time_sec_RunHeatmapExp / 3600)
        minutes_RunHeatmapExp <- floor((execution_time_sec_RunHeatmapExp %% 3600) / 60)
        seconds_RunHeatmapExp <- execution_time_sec_RunHeatmapExp %% 60


        # Update analysis note to "Done!"
        output$RunHeatmapExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunHeatmapExp, "hours", minutes_RunHeatmapExp, "minutes", round(seconds_RunHeatmapExp, 2), "seconds)")})



         # Update UI buttons
        shinyjs::disable("RunHeatmapExp")
        shinyjs::disable("killRunHeatmapExp")
        shinyjs::enable("RefreshRunHeatmapExp")





      } # closing else


      })




    # Interrupt the Heatmap process if the kill button is clicked
    shiny::observeEvent(input$killRunHeatmapExp, {
      # Kill all background processes (pre- and post-normalization ggplot and plotly)
      rvHeatmapMVExpPreplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvHeatmapMVExpPreplotly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvHeatmapMVExpPostplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvHeatmapMVExpPostplotly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

       # Enable/disable UI buttons
      shinyjs::enable("RunHeatmapExp")
      shinyjs::disable("killRunHeatmapExp")

      # Check if all processes are dead and update the status note
      if (shiny::isolate(rvHeatmapMVExpPreplot$result$is_alive()) == FALSE && shiny::isolate(rvHeatmapMVExpPreplotly$result$is_alive()) == FALSE && shiny::isolate(rvHeatmapMVExpPostplot$result$is_alive()) == FALSE && shiny::isolate(rvHeatmapMVExpPostplotly$result$is_alive()) == FALSE ){
        output$RunHeatmapExpNote <- shiny::renderPrint({cat("Killed!")})
      }
    })
    #





    # Download pre-normalization heatmap
    output$HeatmapLinkpre <- shiny::downloadHandler(
      filename = function() {
        paste("Heatmap_pre_normalization"
              , Sys.Date(), ".", input$Heatmapformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (HeatmapMVExpPreplot()), width = input$widthHeatmap, height = input$heightHeatmap,units = input$unitsHeatmap, device = input$Heatmapformat, dpi = input$resHeatmap)
      })


    # Download post-normalization heatmap
    output$HeatmapLinkpost <- shiny::downloadHandler(
      filename = function() {
        paste("Heatmap_post_normalization"
              , Sys.Date(), ".", input$Heatmapformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (HeatmapMVExpPostplot()), width = input$widthHeatmap, height = input$heightHeatmap,units = input$unitsHeatmap, device = input$Heatmapformat, dpi = input$resHeatmap)
      })







    #Run CorrelationPlot


    # Reactive values for storing background operation results
    rvHeatmapCORExpPreplot <- shiny::reactiveValues() # For pre-normalization heatmap (ggplot)
    rvHeatmapCORExpPreplotly <- shiny::reactiveValues()  # For pre-normalization heatmap (plotly)
    rvHeatmapCORExpPostplot <- shiny::reactiveValues() # For post-normalization heatmap (ggplot)
    rvHeatmapCORExpPostplotly <- shiny::reactiveValues() # For post-normalization heatmap (plotly)

    # Reactive values to hold the results of the correlation plots
    HeatmapCORExpPreplot <- shiny::reactiveVal() # Holds the pre-normalization ggplot
    HeatmapCORExpPreplotly <- shiny::reactiveVal() # Holds the pre-normalization plotly
    HeatmapCORExpPostplot <- shiny::reactiveVal()  # Holds the post-normalization ggplot
    HeatmapCORExpPostplotly <- shiny::reactiveVal() # Holds the post-normalization plotly

    #start time to monitor execution
    start_time_RunCorrPlotExp <- shiny::reactiveVal()

    # Note to indicate the status of the correlation plot execution
    output$RunCorrPlotExpNote <- shiny::renderPrint({cat("Undone")})


    # Observe event for the run and refresh buttons for correlation plot
    shiny::observeEvent(c(input$RunCorrPlotExp, input$RefreshRunCorrPlotExp),{
      shiny::req(input$RunCorrPlotExp)
      # Check if required logcounts are available
      if (is.null(PreNor$logcounts) && is.null(PostNor$logcounts)) {
        shiny::showNotification("Run Cor.heatmap error: perform step 3",  closeButton = TRUE, duration = NULL ,  type = "error")

      } else {

        shiny::req(PreNor$logcounts)
        shiny::req(PostNor$logcounts)

        #start time to monitor execution
        start_time_RunCorrPlotExp(Sys.time())

        # Disable the Run button and enable the Kill button during processing
        shinyjs::disable("RunCorrPlotExp")
        shinyjs::enable("killRunCorrPlotExp")
        shinyjs::disable("RefreshRunCorrPlotExp")

        # Update the status note to indicate running state
        output$RunCorrPlotExpNote <- shiny::renderPrint({cat("Running ...")})

        # to avoid opening conditional panel before that the plot is ready
        output$ConditionalRunCor <- shiny::renderText("Running")
        shiny::outputOptions(output, 'ConditionalRunCor', suspendWhenHidden = FALSE)


        # Pre-normalization ggplot
        tryCatch({
          rvHeatmapCORExpPreplot$result <- callr::r_bg(
            func = CorrPlotHeatmap,
            args = list(
              x = PreNor$logcounts,
              Color = input$Corrpalette,
              type = input$typeCorrPlot,
              display = input$displayCorrPlot,
              round_number = input$round_numberCorrPlot,
              cutree_rows = input$cutree_rowsCorrPlot,
              cutree_cols = input$cutree_colsCorrPlot,
              scale = input$scaleCorrPlot,
              cluster = input$clusterCorrPlot,
              show_names = input$show_namesCorrPlot,
              NumGenes = input$NumGenesCorrPlot),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the status of the pre-normalization ggplot process
        shiny::observe({

          if(shiny::isolate(rvHeatmapCORExpPreplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapCORExpPreplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                HeatmapCORExpPreplot(rvHeatmapCORExpPreplot$result$get_result())

                output$PlotCorrplotPre <- shiny::renderPlot({

                  print(HeatmapCORExpPreplot())
                }
                ,
                width ="auto", height = "auto", res = 96
                )

              }) # End of isolated
              #
            #
            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }

        })



        # Pre-normalization plotly
        tryCatch({
          rvHeatmapCORExpPreplotly$result <- callr::r_bg(
            func = CorrPlotHeatmaply,
            args = list(
              x = PreNor$logcounts,
              Color = input$Corrpalette,
              type = input$typeCorrPlot,
              scale = input$scaleCorrPlot,
              cluster = input$clusterCorrPlot,
              show_names = input$show_namesCorrPlot,
              NumGenes = input$NumGenesCorrPlot),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the status of the pre-normalization plotly process
        shiny::observe({

          if(shiny::isolate(rvHeatmapCORExpPreplotly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapCORExpPreplotly$result$is_alive()) == FALSE) {

            tryCatch({

            shiny::isolate({

              HeatmapCORExpPreplotly(rvHeatmapCORExpPreplotly$result$get_result())




              output$PlotCorrplotPrePlotly <- plotly::renderPlotly({

                print(HeatmapCORExpPreplotly())}
              )



            }) # End of isolate


            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            })  # End of tryCatch
          }
        })




        # Post-normalization ggplot
        tryCatch({
          rvHeatmapCORExpPostplot$result <- callr::r_bg(
            func = CorrPlotHeatmap,
            args = list(
              x = PostNor$logcounts,
              Color = input$Corrpalette,
              type = input$typeCorrPlot,
              display = input$displayCorrPlot,
              round_number = input$round_numberCorrPlot,
              cutree_rows = input$cutree_rowsCorrPlot,
              cutree_cols = input$cutree_colsCorrPlot,
              scale = input$scaleCorrPlot,
              cluster = input$clusterCorrPlot,
              show_names = input$show_namesCorrPlot,
              NumGenes = input$NumGenesCorrPlot),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        })  # End of tryCatch

        # Observe the status of the post-normalization ggplot process
        shiny::observe({

          if(shiny::isolate(rvHeatmapCORExpPostplot$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapCORExpPostplot$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                HeatmapCORExpPostplot(rvHeatmapCORExpPostplot$result$get_result())

                output$PlotCorrplotPost <- shiny::renderPlot({

                  print(HeatmapCORExpPostplot())}
                  ,
                  width ="auto", height = "auto",res = 96
                )


              })  # End of isolate


            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            })  # End of tryCatch
          }

        })


        # Post-normalization plotly
        tryCatch({
          rvHeatmapCORExpPostplotly$result <- callr::r_bg(
            func = CorrPlotHeatmaply,
            args = list(
              x = PostNor$logcounts,
              Color = input$Corrpalette,
              type = input$typeCorrPlot,
              scale = input$scaleCorrPlot,
              cluster = input$clusterCorrPlot,
              show_names = input$show_namesCorrPlot,
              NumGenes = input$NumGenesCorrPlot),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # End of tryCatch

        # Observe the status of the post-normalization plotly process
        shiny::observe({

          if(shiny::isolate(rvHeatmapCORExpPostplotly$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvHeatmapCORExpPostplotly$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                HeatmapCORExpPostplotly(rvHeatmapCORExpPostplotly$result$get_result())



                output$PlotCorrplotPostPlotly <- plotly::renderPlotly({

                  print(HeatmapCORExpPostplotly())}
                )


              }) # End of isolate

            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch
          }
        })



        #check end time of process
        end_time_RunCorrPlotExp <- Sys.time()
        execution_time_RunCorrPlotExp <- end_time_RunCorrPlotExp - start_time_RunCorrPlotExp()

        # Convert to hours, minutes, and seconds
        execution_time_sec_RunCorrPlotExp <- as.numeric(execution_time_RunCorrPlotExp, units="secs")
        hours_RunCorrPlotExp <- floor(execution_time_sec_RunCorrPlotExp / 3600)
        minutes_RunCorrPlotExp <- floor((execution_time_sec_RunCorrPlotExp %% 3600) / 60)
        seconds_RunCorrPlotExp <- execution_time_sec_RunCorrPlotExp %% 60


        # Update analysis note to "Done!"
        output$RunCorrPlotExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunCorrPlotExp, "hours", minutes_RunCorrPlotExp, "minutes", round(seconds_RunCorrPlotExp, 2), "seconds)")})




        # Disable the Run button and enable the Refresh button
        shinyjs::disable("RunCorrPlotExp")
        shinyjs::disable("killRunCorrPlotExp")
        shinyjs::enable("RefreshRunCorrPlotExp")

      }

    })



    # Interrupt the MDS process if the kill button is clicked
    shiny::observeEvent(input$killRunCorrPlotExp, {
      rvHeatmapCORExpPreplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvHeatmapCORExpPreplotly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvHeatmapCORExpPostplot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
      rvHeatmapCORExpPostplotly$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

      # Enable/disable buttons
      shinyjs::enable("RunCorrPlotExp")
      shinyjs::disable("killRunCorrPlotExp")

      # Check if all processes have been killed
      if (shiny::isolate(rvHeatmapCORExpPreplot$result$is_alive()) == FALSE && shiny::isolate(rvHeatmapCORExpPreplotly$result$is_alive()) == FALSE && shiny::isolate(rvHeatmapCORExpPostplot$result$is_alive()) == FALSE && shiny::isolate(rvHeatmapCORExpPostplotly$result$is_alive()) == FALSE ){
        output$RunCorrPlotExpNote <- shiny::renderPrint({cat("Killed!")})
      }
    })
    #


    # Download handler for pre-normalization correlation plot
    output$CorrPlotLinkpre <- shiny::downloadHandler(
      filename = function() {
        paste("Corrplot_pre_normalization"
              , Sys.Date(), ".", input$CorrPlotformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (HeatmapCORExpPreplot()), width = input$widthCorrPlot, height = input$heightCorrPlot,units = input$unitsCorrPlot, device = input$CorrPlotformat, dpi = input$resCorrPlot)
      })


    # Download handler for post-normalization correlation plot
    output$CorrPlotLinkpost <- shiny::downloadHandler(
      filename = function() {
        paste("Corrplot_post_normalization"
              , Sys.Date(), ".", input$CorrPlotformat, sep = ""
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = (HeatmapCORExpPostplot()), width = input$widthCorrPlot, height = input$heightCorrPlot,units = input$unitsCorrPlot, device = input$CorrPlotformat, dpi = input$resCorrPlot)
      })







    #Saturation Plot

    # Reactive value for background operation, storing the results of the saturation plot process
    rvSATPlotExp <- shiny::reactiveValues()

    # Reactive value to store the final result of the saturation plot
    SATPlotExp <- shiny::reactiveVal()

    #start time to monitor execution
    start_time_RunSatPlotExp <- shiny::reactiveVal()

    # Initial note to indicate the status of the saturation plot execution
    output$RunSatPlotExpNote <- shiny::renderPrint({cat("Undone")})

    # Observe the event for the run and refresh buttons for the saturation plot
    shiny::observeEvent(c(input$RunSatPlotExp, input$RefreshRunSatPlotExp),{
      shiny::req(input$RunSatPlotExp)

    # Check if pre-normalized logcounts are available
      if (is.null(PreNor$logcounts)) {
        shiny::showNotification("Run Sat. plot error: perform step 3",  closeButton = TRUE, duration = NULL ,  type = "error")

      } else {

        shiny::req(PreNor$logcounts)

        #start time to monitor execution
        start_time_RunSatPlotExp(Sys.time())

        # Disable the Run button and enable the Kill button during processing
        shinyjs::disable("RunSatPlotExp")
        shinyjs::enable("killRunSatPlotExp")
        shinyjs::disable("RefreshRunSatPlotExp")

        # Update the status note to indicate running state
        output$RunSatPlotExpNote <- shiny::renderPrint({cat("Running ...")})


        # to avoid opening conditional panel before that the plot is ready
        output$ConditionalRunSat <- shiny::renderText("Running")
        shiny::outputOptions(output, 'ConditionalRunSat', suspendWhenHidden = FALSE)


        # Run the saturation plot function in the background
        tryCatch({
          rvSATPlotExp$result <- callr::r_bg(
            func = Saturation,
            args = list(
              matrix = PreNor$logcounts,
              method = input$methodSatPlot,
              max_reads = input$max_readsSatPlot,
              palette = input$paletteSatPlot),
            supervise = TRUE ,error = getOption("callr.error", "error"))
        }
        , error = function(e) {
          shiny::showNotification(
            "error occurs",
            action = NULL,
            duration = NULL,
            closeButton = TRUE,
            id = NULL,
            type = "error"
          )
        }) # finisce tryCatch

        # Observe the status of the saturation plot process
        shiny::observe({

          if(shiny::isolate(rvSATPlotExp$result$is_alive()) == TRUE)  {
            shiny::invalidateLater(2000, session)

          } else if(shiny::isolate(rvSATPlotExp$result$is_alive()) == FALSE) {

            tryCatch({

              shiny::isolate({

                SATPlotExp(rvSATPlotExp$result$get_result())

                output$SaturationPlot <- shiny::renderPlot({
                  plot(SATPlotExp())
                },
                width ="auto", height = "auto",res = 96
                )

                output$SaturationPlotly <- plotly::renderPlotly({
                  plotly::ggplotly(SATPlotExp())
                })





              }) # End of isolated


            }  , error = function(e) {
              shiny::showNotification(
                "An error has occurred or the process was killed",
                action = NULL,
                duration = NULL,
                closeButton = TRUE,
                id = NULL,
                type = "error"
              )
            }) # End of tryCatch


          } # End of else

        })




        #check end time of process
        end_time_RunSatPlotExp <- Sys.time()
        execution_time_RunSatPlotExp <- end_time_RunSatPlotExp - start_time_RunSatPlotExp()

        # Convert to hours, minutes, and seconds
        execution_time_sec_RunSatPlotExp <- as.numeric(execution_time_RunSatPlotExp, units="secs")
        hours_RunSatPlotExp <- floor(execution_time_sec_RunSatPlotExp / 3600)
        minutes_RunSatPlotExp <- floor((execution_time_sec_RunSatPlotExp %% 3600) / 60)
        seconds_RunSatPlotExp <- execution_time_sec_RunSatPlotExp %% 60


        # Update analysis note to "Done!"
        output$RunSatPlotExpNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunSatPlotExp, "hours", minutes_RunSatPlotExp, "minutes", round(seconds_RunSatPlotExp, 2), "seconds)")})


        # Disable the Run button and enable the Refresh button
        shinyjs::disable("RunSatPlotExp")
        shinyjs::disable("killRunSatPlotExp")
        shinyjs::enable("RefreshRunSatPlotExp")

      }
    })

    # Interrupt the saturation plot process if the kill button is clicked
    shiny::observeEvent(input$killRunSatPlotExp, {
      # stop the slow barplotExp() function
      rvSATPlotExp$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

      # Enable the Run button and disable the Kill button
      shinyjs::enable("RunSatPlotExp")
      shinyjs::disable("killRunSatPlotExp")

      # Check if the process has been successfully killed
      if (shiny::isolate(rvSATPlotExp$result$is_alive()) == FALSE){
        output$RunSatPlotExpNote <- shiny::renderPrint({cat("Killed!")})
      }
    })


    # Download handler for the saturation plot
    output$SatPlotLink <- shiny::downloadHandler(
      filename = function() {
        paste("Saturation"
              , Sys.Date(), ".", input$SatPlotformat, sep = ""
        )
      },
      content = function(file) {
        # Save the saturation plot using ggplot2::ggsave with specified parameters
        ggplot2::ggsave(file, plot = (SATPlotExp()), width = input$widthSatPlot, height = input$heightSatPlot,units = input$unitsSatPlot, device = input$SatPlotformat, dpi = input$resSatPlot)
      })

      } # close function inside moduleServer
      ) # close moduleServer
    } # close ServerLogic function
