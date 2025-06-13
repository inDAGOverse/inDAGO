#' Server function for DEGs module in Shiny application
#' @param id Shiny module identifier
DEGsServerLogic <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

  shiny::observeEvent(input$helpExploration,{
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
  )})


  #Pre-processing

  volumes <- c(Home = fs::path_home(),
               "R Installation" = R.home(),
               getVolumes()())
  # by setting `allowDirCreate = FALSE` a user will not be able to create a new directory


  # Set the shinyDirChoose UI element for raw counts directory
  shinyFiles::shinyDirChoose(
    input,
    "directory_raw_count",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$directory_raw_count value:\n\n")
    print(input$directory_raw_count)
  })
  ## print to browser
  output$directory_raw_count_path <- shiny::renderPrint({
    if (is.integer(input$directory_raw_count)) {
      cat("No directory has been selected yet")
    } else {
      cat(paste0(
        "Selected directory is: ",
        shinyFiles::parseDirPath(volumes, input$directory_raw_count)
      ))
    }
  })


  # saving working directory in variable

  WDpn <- shiny::reactiveVal()
  shiny::observe({
    shiny::req(input$directory_raw_count)
    p <- shinyFiles::parseDirPath(volumes, input$directory_raw_count)

    WDpn(p)
  })

  #set output DEG directory

  # by setting allowDirCreate = FALSE a user will not be able to create a new directory
  shinyFiles::shinyDirChoose(
    input,
    "directory_deg",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"),
    allowDirCreate = FALSE
  )
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$directory_deg value:\n\n")
    print(input$directory_deg)
  })
  ## print to browser
  output$directory_deg_path <- shiny::renderPrint({
    if (is.integer(input$directory_deg)) {
      cat("No directory has been selected yet")
    } else {
      cat(paste0(
        "Selected directory is: ",
        shinyFiles::parseDirPath(volumes, input$directory_deg)
      ))
    }
  })

  # saving deg directory in variable

  WDdeg <- shiny::reactiveVal()
  shiny::observe({
    #   shiny::req(input$directory_deg)
    p <- shinyFiles::parseDirPath(volumes, input$directory_deg)
    WDdeg(p)
  })


  # DEGs matrix for merging

  shinyFiles::shinyDirChoose(input, "MergeDegsDir", roots = volumes, session = session,
                 restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$MergeDegsDir value:\n\n")
    print(input$MergeDegsDir)
  })



  # saving deg directory in variable

  MergeDegsPath <- shiny::reactiveVal()
  shiny::observe({
    p <- shinyFiles::parseDirPath(volumes, input$MergeDegsDir)
    MergeDegsPath(p)
  })

  ## print to browser
  output$MergeDegsPath <- shiny::renderPrint({
    if (is.integer(input$MergeDegsDir)) {
      cat("No directory has been selected yet")
    } else {
      cat(
        paste0("Selected directory is: ",
               shinyFiles::parseDirPath(volumes, input$MergeDegsDir)))
    }
  })



  # File selection for GTF (Gene Transfer Format) file
  shinyFiles::shinyFileChoose(
    input,
    "MergeDegsgtf",
    roots = volumes,
    session = session
  )
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$MergeDegsgtf value:\n\n")
    print(input$MergeDegsgtf)
  })

  MergeDegsgtfPath <- shiny::reactiveVal()
  shiny::observe({
    File <- as.character(shinyFiles::parseFilePaths(volumes, input$MergeDegsgtf)$datapath)
    MergeDegsgtfPath(File)

  })

  ## print to browser
  output$MergeDegsgtfPath <- shiny::renderPrint({
    if (is.integer(input$MergeDegsgtf)) {
      cat("No input file has been selected yet")
    } else {
      cat(paste0(
        "The GTF file path is:",

        as.character(shinyFiles::parseFilePaths(volumes, input$MergeDegsgtf)$datapath)
      ))

    }
  })


  # Volcano matrix path

  # File selection for Volcano matrix
  shinyFiles::shinyFileChoose(
    input,
    "VolcanoDEGmatrix",
    roots = volumes,
    session = session
  )
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$VolcanoDEGmatrix value:\n\n")
    print(input$VolcanoDEGmatrix)
  })

  VolcanoDEGmatrixPath <- shiny::reactiveVal()
  shiny::observe({
    File <- as.character(shinyFiles::parseFilePaths(volumes, input$VolcanoDEGmatrix)$datapath)
    VolcanoDEGmatrixPath(File)

  })

  ## print to browser
  output$VolcanoDEGmatrixPath <- shiny::renderPrint({
    if (is.integer(input$VolcanoDEGmatrix)) {
      cat("No input file has been selected yet")
    } else {
      cat(
        paste0("The uploaded table is:",

               as.character(shinyFiles::parseFilePaths(volumes, input$VolcanoDEGmatrix)$datapath))
      )

    }
  })


  # Upset folder path
  #UpsetDEGfolder

  # Directory selection for UpSet DEG folder
  shinyFiles::shinyDirChoose(input, "UpsetDEGfolder", roots = volumes, session = session,
                 restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$UpsetDEGfolder value:\n\n")
    print(input$UpsetDEGfolder)
  })


  # saving deg directory in variable

  UpsetDEGfolderPath <- shiny::reactiveVal()
  shiny::observe({
    p <- shinyFiles::parseDirPath(volumes, input$UpsetDEGfolder)
    UpsetDEGfolderPath(p)
  })

  ## print to browser
  output$UpsetDEGfolderPath <- shiny::renderPrint({
    if (is.integer(input$UpsetDEGfolder)) {
      cat("No directory has been selected yet")
    } else {
      cat(
        paste0("Selected directory is: ",
               shinyFiles::parseDirPath(volumes, input$UpsetDEGfolder)))
    }
  })


  # Set the samples for the select input based on the raw count directory
  shiny::observeEvent(input$directory_raw_count, {
    shiny::req(WDpn())
    spsComps::shinyCatch({
      if (is.null(WDpn())) warning("Warning: The raw count directory is not correctly assigned. Please check the directory path.")
    }, blocking_level = "warning")
    l <- list.files(WDpn(), pattern = ".tab")
    # Can use character(0) to remove all choices
    if (is.null(l))
      l <- character(0)
    # Can also set the label and select items
    shiny::updateSelectInput(
      session,
      "selectpn",
      label = paste("Number of matrices:", length(l)),
      choices = l,
      selected = head(l, 1)
    )
    sample <- paste0(WDpn(), "/", l[1])
    shiny::updateNumericInput(session, "colIDgene", max = ncol(readr::read_delim(sample, delim = "\t")))
    shiny::updateNumericInput(session, "colCounts", max = ncol(readr::read_delim(sample, delim = "\t")))

    shiny::showNotification(
      "Table successfully loaded in 'Step 1 Result - Import count matrices'",
      closeButton = TRUE,
      duration = 5,
      type = "message"
    )

  })

  # Save the matrix and show it to the user


  matrixnp <- shiny::reactiveVal()

  shiny::observe({
    shiny::req(input$selectpn)
    shiny::req(input$directory_raw_count)
    if (!is.integer(input$directory_raw_count)) {
    if (input$selectpn != 0) {
      path <- paste0(WDpn(), "/", input$selectpn)
      m <- readr::read_delim(
        path,
        col_names = FALSE,
        skip = input$skip_preN,
        delim = "\t"
      ) %>% `colnames<-`(paste0("col", seq_len(ncol(.)))) %>% .[, c(input$colIDgene, input$colCounts)]
    }

    matrixnp(m)
    }
  })


  # Render the matrix table to show the user

  shiny::observe({
    shiny::req(input$directory_raw_count)
    shiny::req(matrixnp())
  output$ShowTable <- shiny::renderTable({
    if (!is.integer(input$directory_raw_count)) {
    if (input$disp == "head") {
      return(head(matrixnp(), n = 20))
    }
    else {
      return(matrixnp())
    }
      }
  }, rownames = TRUE)
  })


  # print message if table is not loaded
  output$ShowTable <- shiny::renderPrint(cat("Complete Step 1 to view the matrices."))



  # Assigning groups for the samples
  df <- shiny::reactiveValues(group = data.frame())

  shiny::observeEvent(input$assign_group, {
    shiny::req(WDpn())
# check if the WDpn() is proprierly assigned
    spsComps::shinyCatch({
      if (is.null(WDpn())) warning("Warning: The groups have not been properly assigned.")
    }, blocking_level = "warning")

    files <- list.files(WDpn(), pattern = ".tab") %>% tools::file_path_sans_ext(.)
    df$group <- data.frame(as.character(files),as.character(rep("NA",length(files))))

    # Notify the user when the group table is loaded
    shiny::showNotification(
      "Table loaded in the panel 'Step 2 Result - Generate the matrix'",
      closeButton = TRUE,
      duration = 10,
      type = "message"
    )
  })

  # To be able to use the output.item in conditionalPanel
  output$groupingMessage <- shiny::renderPrint(cat("Please complete steps 1 and 2 to generate the matrix"))


  # Render the group assignment table
  shiny::observe({
    shiny::req(df$group)
    shiny::req(WDpn())
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
      columnDefs = list(list( targets = c(1,2), width = '200px')),
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv')
    ),

    class = "display"
  )
  output$groupingMessage <- NULL

  })


  # Updating the group data table when editing a cell
  shiny::observeEvent(input$grouping_cell_edit, {
    # check if the df$group is proprierly assigned
    spsComps::shinyCatch({
      if (is.null(df$group)) warning("Warning: The matrix has not been generated correctly.")
    }, blocking_level = "warning")

    df$group[input$grouping_cell_edit$row, input$grouping_cell_edit$col] <- input$grouping_cell_edit$value
  })




  # Saving the assigned group data
  gr <- shiny::reactiveVal()
  shiny::observeEvent(input$save_group, {
    # check if the df$group is proprierly assigned
    spsComps::shinyCatch({
      if (is.null(df$group)) warning("Warning: The groups have not been saved correctly")
    }, blocking_level = "warning")


    gr <- df$group
    gr <- as.data.frame(gr)
    colnames(gr) <- c("Samples", "Groups")
    gr(gr)
    # Notify the user when the groups are saved
    shiny::showNotification(
      "The assigned groups have been saved",
      closeButton = TRUE,
      duration = 10,
      type = "message"
    )
    # shinyjs::enable the button to set the contrasts
    shinyjs::enable("SetContrast")

  })
  # Display the saved group table
  shiny::observeEvent(gr(), {
    shiny::req(gr())
    # check if the gr() is proprierly assigned
    spsComps::shinyCatch({
      if (is.null(gr())) warning("Warning: The groups have not been saved correctly")
    }, blocking_level = "warning")

    output$groups_assigned <- shiny::renderTable(gr())
  })


  # File selection for importing a pre-grouped table
  shinyFiles::shinyFileChoose(input,
                  "assignGroupByTable",
                  roots = volumes,
                  session = session)
  ## print to console to see how the value of the shinyFiles
  ## button changes after clicking and selection
  shiny::observe({
    cat("\ninput$assignGroupByTable value:\n\n")
    print(input$assignGroupByTable)
  })

  assignGroupByTablePath <- shiny::reactiveVal()
  shiny::observe({
    File <- as.character(shinyFiles::parseFilePaths(volumes, input$assignGroupByTable)$datapath)
    assignGroupByTablePath(File)

  })




  # Display the path of the imported pre-grouped table
  output$AssignGroupByTablePath <- shiny::renderPrint({
    if (is.integer(input$assignGroupByTable)) {
      cat("No input file has been selected yet")
    } else {
      cat(paste0(
        "The uploaded group matrix is:",

        as.character(
          shinyFiles::parseFilePaths(volumes, input$assignGroupByTable)$datapath
        )
      ))

    }
  })

  # Loading the imported pre-grouped table and displaying it
  AssignTable <- shiny::reactiveVal()
  shiny::observeEvent(input$assignGroupByTable, {
    shiny::req(assignGroupByTablePath())
    # check if the assignGroupByTablePath() is proprierly assigned
    spsComps::shinyCatch({
      if (is.null(assignGroupByTablePath())) warning("Warning: The matrix has not been loaded correctly.")
    }, blocking_level = "warning")

    data <- data.table::fread(
      file.path(assignGroupByTablePath()),
      header = TRUE,
      stringsAsFactors = FALSE
    )
    AssignTable(data)

    # Notify when the pre-grouped table is loaded
      shiny::showNotification(
        "Table has been loaded in panel: step 2 Result - Loading matrix",
        closeButton = TRUE,
        duration = 10,
        type = "message"
      )
      # shinyjs::enable the button to set the contrasts
      shinyjs::enable("SetContrast")
  })



  shiny::observeEvent(AssignTable(), {
    # check if the AssignTable() is proprierly assigned
    spsComps::shinyCatch({
      if (is.null(AssignTable())) warning("Warning: The matrix has not been loaded correctly.")
    }, blocking_level = "warning")
    output$ImportedTable <- shiny::renderTable(AssignTable())
  })

  #to be able to use the output.item in conditionalPanel
  output$ImportedTable <- shiny::renderPrint(cat("Please complete steps 1 and 2 to load the matrix."))



  # Select contrast

  output$SetContrastNote <- shiny::renderPrint({
    cat("Undone")
  })

  shiny::observeEvent(input$SetContrast, {
    if (is.integer(input$directory_raw_count) ||
        (is.null(AssignTable()$Groups) && is.null(gr()$Groups))) {
      shiny::showNotification(
        "Please complete the previous steps to view the available contrasts",
        closeButton = TRUE,
        duration = NULL ,
        type = "error"
      )

    } else {
      output$SetContrastNote <- shiny::renderPrint({
        cat("Ready for contrasts selection")
      })

      shiny::showNotification(
        "The contrasts are ready to be configured in the ' Step 3 - Defining contrasts'",
        closeButton = TRUE,
        duration = 5 ,
        type = "message"
      )
      # Cancel the message
      output$ContrastMessage <- NULL

      # to manage conditional panel
      output$ShowpanelContrast <- shiny::renderText("Running")
      shiny::outputOptions(output, 'ShowpanelContrast', suspendWhenHidden = FALSE)
    }
      })

  # shows a warning message to perform the previous steps
  output$ContrastMessage <- shiny::renderPrint(cat("Please complete steps 1, 2, and 3 to proceed with the analysis."))


  shiny::observeEvent(input$save_data_contrast, {
    shiny::req(input$SetContrast)
    if (is.integer(input$directory_raw_count) ||
        (is.null(AssignTable()$Groups) && is.null(gr()$Groups))) {
      shiny::showNotification(
        "Please complete the previous steps to view the available contrasts",
        closeButton = TRUE,
        duration = NULL ,
        type = "error"
      )
    } else {
      output$SetContrastNote <- shiny::renderPrint({
        cat("Saved")
      })

      shiny::showNotification(
        "The contrasts have been saved",
        closeButton = TRUE,
        duration = 5 ,
        type = "message"
      )

    }
  })


  # Reactive expression to generate the list of possible test conditions (contrasts)
  choicesTest <- shiny::reactive({
    if (!is.null(AssignTable())) {
      gr <- AssignTable()$Groups
      unique(gr)
    } else {
      gr <- gr()$Groups
      unique(gr)
    }
  })
  # Reactive expression to generate the list of possible baseline conditions
  choicesBaseline <- shiny::reactive({
    if (!is.null(AssignTable())) {
      gr <- AssignTable()$Groups
      unique(gr)
    } else {
      gr <- gr()$Groups
      unique(gr)
    }
  })


  # Initial data with a single row to represent the first contrast pair (Test vs Baseline)
  DFdata <- shiny::reactiveVal(
    data.frame(
      ID = 1,
      Test = 1,
      Baseline = 1,
      stringsAsFactors = FALSE
    )
  )

  # A separate reactive value to store the saved contrast data
  saved_data <- shiny::reactiveVal(
    data.frame(
      ID = integer(0),
      Test = character(0),
      Baseline = character(0),
      stringsAsFactors = FALSE
    )
  )

  # Function to store the current values of contrast selections (Test and Baseline) before adding/removing rows
  store_current_inputs <- function() {
    current_data <- DFdata()
    DFdata(lapply(1:nrow(current_data), function(i) {
      id <- current_data$ID[i]
      current_data$Test[i] <- input[[paste0("select1_", id)]]
      current_data$Baseline[i] <- input[[paste0("select2_", id)]]
      current_data[i, ]
    }) %>% do.call(rbind, .))
  }


  # Render the dynamic user interface for selecting contrasts (Test vs Baseline)
  output$Resultcontrast <- shiny::renderUI({
    num_rows <- nrow(DFdata())
    lapply(1:num_rows, function(i) {
      row <- DFdata()[i, ]
      htmltools::tagList(
        htmltools::h6(paste("Contrast", row$ID)),
        shiny::selectInput(
          paste0(shiny::NS(id,"select1_"), row$ID),
          label = "Test",
          choices = choicesTest(),
          selected = row$Test
        ),
        shiny::selectInput(
          paste0(shiny::NS(id,"select2_"), row$ID),
          label = "Baseline",
          choices = choicesBaseline(),
          selected = row$Baseline
        ),
        htmltools::br()
      )
    })
  })


  # Add a new row to the contrast table when the "Add Row" button is clicked
  shiny::observeEvent(input$add_row_contrast, {
    # check if the AssignTable() is proprierly assigned
    current_data <- DFdata()
    new_id <- ifelse(nrow(current_data) > 0, max(current_data$ID) + 1, 1)
    new_row <- data.frame(
      ID = new_id,
      Test = choicesTest()[1],
      Baseline = choicesBaseline()[1],
      stringsAsFactors = FALSE
    )
    DFdata(rbind(current_data, new_row))
  })

  # Remove the last row from the contrast table when the "Remove Last Row" button is clicked
  shiny::observeEvent(input$remove_last_row_contrast, {
    store_current_inputs()  # Store current input values before removing a row

    # Retrieve the current contrast data
    current_data <- DFdata()
    if (nrow(current_data) > 1) {
      # Ensure that at least one row remains before removing the last row
      updated_data <- current_data[-nrow(current_data), ]
      DFdata(updated_data) # Update the data with the remaining rows
    }
  })
  # Save the current contrast data (Test and Baseline selections) when the "Save Data" button is clicked
  shiny::observeEvent(input$save_data_contrast, {
    current_data <- DFdata() # Retrieve the current contrast data
    updated_data <- lapply(1:nrow(current_data), function(i) {
      id <- current_data$ID[i]
      data.frame(
        ID = as.integer(id),
        Test = input[[paste0("select1_", id)]],
        Baseline = input[[paste0("select2_", id)]],
        stringsAsFactors = FALSE
      )
    }) %>% do.call(rbind, .)

    saved_data(updated_data)
  })

  # Render and display the saved contrast data in a data table format
  output$saved_data_table <-  DT::renderDT(
    saved_data(),
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      paging = FALSE,
      searching = TRUE,
      fixedColumns = TRUE,
      columnDefs = list(list(
        targets = c(1, 2), width = '200px'
      )),
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = 'csv'
    )
  )



  # Reactive value for background operation, storing the results of the DEGs analysis
  rvDegsAnalysis <- shiny::reactiveValues()
  # Initial note to indicate the status of the merging DEGs execution

  #start time to monitor execution
  start_time_RunDEGs <- shiny::reactiveVal()

  # Render the process note
  output$RunDEGsNote <- shiny::renderPrint({
    cat("Undone")
  })


  # shows a warning message to perform the previous steps
  output$DEGsMessage <- shiny::renderPrint(cat("Please complete steps 1, 2, 3 and 4 to proceed with the analysis"))



  # Observe event triggered when the "Run DEGs" button is clicked
  shiny::observeEvent(input$RunDEGs, {
    # Check if required inputs are not set, show an error notification if true

    if (is.integer(input$directory_raw_count)) {
      shiny::showNotification(
        "Please select the input folder.",
        closeButton = TRUE,
        duration = NULL ,
        type = "error")
      } else if (is.integer(input$directory_deg)) {
        shiny::showNotification(
          "Please select the output folder.",
          closeButton = TRUE,
          duration = NULL ,
          type = "error")

      } else if (S4Vectors::isEmpty(saved_data())) {
        shiny::showNotification(
          "Please save the contrast.",
          closeButton = TRUE,
          duration = NULL ,
          type = "error")
      } else if (is.integer(input$directory_raw_count) ||
                 is.integer(input$directory_deg) ||
                 S4Vectors::isEmpty(saved_data()) ||
                 (is.null(AssignTable()$Groups) && is.null(gr()$Groups))) {

      shiny::showNotification(
        "Please complete the previous steps.",
        closeButton = TRUE,
        duration = NULL ,
        type = "error")
    } else {
      # Determine whether to use AssignTable() or gr() based on availability
      if (!is.null(AssignTable())) {
        gr <- AssignTable()
      } else {
        gr <- gr()
      }


      data_test <- as.data.frame(saved_data())
      num_rows <- nrow(data_test)
     check_test <- lapply(1:num_rows, function(i) {
        test <- data_test$Test[[i]]
        baseline <- data_test$Baseline[[i]]
        if (test == baseline) {
          print("TRUE")
        } else {
          print("FALSE")
        }
      })

      if ("FALSE" %in% check_test){



      #start time to monitor execution
      start_time_RunDEGs(Sys.time())

      # Render the process note
      output$RunDEGsNote <- shiny::renderPrint({
        cat("Running ...")
      })

      shinyjs::disable("RunDEGs")
      shinyjs::enable("KillRunDEGs")

      tryCatch({
        rvDegsAnalysis$result <- callr::r_bg(
          func = EdgerDEG,
          args = list(
            gr = gr,
            WD_samples = WDpn(),
            WD_DEGs = WDdeg(),
            colIDgene = input$colIDgene,
            colCounts = input$colCounts,
            skip_preN = input$skip_preN,
            grContrast = as.data.frame(saved_data()),
            model = as.character(input$modelDEG),
            filter = as.character(input$filterDEG),
            min_count = as.numeric(input$DEGsmin_count),
            min_total_count = as.numeric(input$DEGsmin_total_count),
            large_n = as.numeric(input$DEGslarge_n),
            min_prop = as.numeric(input$DEGsmin_prop),
            normMethod = as.character(input$normMethod),
            adjustPvalue = as.character(input$adjustPvalue),
            Th_logFC = as.numeric(input$Th_logFC),
            Th_Pvalue = as.numeric(input$Th_Pvalue)
          ),
          supervise = TRUE ,
          error = getOption("callr.error", "error")
        )
      }
      , error = function(e) {
        shiny::showNotification(
          "Error occurs",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # finisce tryCatch



      # Observe the status of the RunDEGs process
      shiny::observe({
        if (shiny::isolate(rvDegsAnalysis$result$poll_io(0)["process"]) != "ready")  {
          shiny::invalidateLater(2000, session)

        } else if (shiny::isolate(rvDegsAnalysis$result$poll_io(0)["process"]) == "ready") {



          output$RunDEGsNote <- shiny::renderPrint({cat("Done!")})


            # Disable the Run button and enable the Refresh button
            shinyjs::enable("RunDEGs")
            shinyjs::disable("killRunDEGs")

        }
      })


      } # End of check_test
     else {

       shiny::showNotification(
         "At least one contrast present the same 'Test' and 'Baseline'",
         closeButton = TRUE,
         duration = NULL ,
         type = "error")

    }

    } # End of else

  })



  # Output the status of the filtering process
  shiny::observeEvent(input$RunDEGs, {
    if (is.integer(input$directory_raw_count) ||
        is.integer(input$directory_deg) ||
        S4Vectors::isEmpty(saved_data()) ||
        (is.null(AssignTable()$Groups) && is.null(gr()$Groups))) {
      # No action needed if directories are not selected
    } else {

      data_test <- as.data.frame(saved_data())
      num_rows <- nrow(data_test)
      check_test <- lapply(1:num_rows, function(i) {
        test <- data_test$Test[[i]]
        baseline <- data_test$Baseline[[i]]
        if (test == baseline) {
          print("TRUE")
        } else {
          print("FALSE")
        }
      })

      if ("FALSE" %in% check_test){


shiny::observe({
      if (shiny::isolate(rvDegsAnalysis$result$is_alive()) == TRUE && shiny::isolate(rvDegsAnalysis$result$poll_io(0)["process"]) != "ready") {
        shiny::invalidateLater(3000)
      output$RunDEGsoutput <- shiny::renderPrint({
        rvDegsAnalysis$result$print()
        cat(
          shiny::isolate(
            "This process could be time-consuming\nFeel free to take a coffee break\n\n   ( (\n    ) )\n ..........\n |        |]\n  \\      /\n   '----'"
          )
        )
      })

      # Cancel the message
      output$DEGsMessage <- NULL

      # to manage conditional panel
      output$ShowpanelDEGs <- shiny::renderText("Running")
      shiny::outputOptions(output, 'ShowpanelDEGs', suspendWhenHidden = FALSE)

        } else if (shiny::isolate(rvDegsAnalysis$result$is_alive()) == FALSE) {


            # Get resulting plot from background analysis
            record_plotBCV <- rvDegsAnalysis$result$get_result()$record_plotBCV
            record_plotQLDisp <- rvDegsAnalysis$result$get_result()$record_plotQLDisp
            log_t_k_a_b_c <- rvDegsAnalysis$result$get_result()$log[1:5]
            log_d <- rvDegsAnalysis$result$get_result()$log$d
            # Display log in shiny



            #check end time of process
            end_time_RunDEGs <- Sys.time()
            execution_time_RunDEGs <- end_time_RunDEGs - start_time_RunDEGs()

            # Convert to hours, minutes, and seconds
            execution_time_sec_RunDEGs <- as.numeric(execution_time_RunDEGs, units="secs")
            hours_RunDEGs <- floor(execution_time_sec_RunDEGs / 3600)
            minutes_RunDEGs <- floor((execution_time_sec_RunDEGs %% 3600) / 60)
            seconds_RunDEGs <- execution_time_sec_RunDEGs %% 60

            # Print the result
            output$RunDEGsoutput <- shiny::renderPrint({
              cat(shiny::isolate("DEGs analysis has been completed! - checks the results in the selected output folder\n\nInformation about process events:\n"))
              cat("Execution Time: ", hours_RunDEGs, "hours", minutes_RunDEGs, "minutes", round(seconds_RunDEGs, 2), "seconds\n")
              writeLines(unlist(lapply(log_t_k_a_b_c, paste, collapse=" ")))
                  cat(paste(log_d, collapse = "\n"))
            })


            # Display resulting plot in shiny
            output$plotBCV <- shiny::renderPlot(record_plotBCV)
            output$plotQLDisp <- shiny::renderPlot(record_plotQLDisp)


         # enable and disable actionButton
          shinyjs::enable("RunDEGs")
          shinyjs::disable("KillRunDEGs")

        }
}) # End of observe



    } # End of check_test
    }

  })

  # Interrupt the filtering process if the kill button is clicked
  shiny::observeEvent(input$KillRunDEGs, {
    # stop the slow Filtering() function
    rvDegsAnalysis$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
    shinyjs::enable("RunDEGs")
    shinyjs::disable("KillRunDEGs")
    if (shiny::isolate(rvDegsAnalysis$result$is_alive()) == FALSE) {
      shiny::invalidateLater(5000)
      output$RunDEGsoutput <- shiny::renderPrint({
      cat(shiny::isolate("DEG analysis has been killed!"))
        })

      # Display resulting plot in shiny
      output$plotBCV <- shiny::renderPlot(NULL)
      output$plotQLDisp <- shiny::renderPlot(NULL)

      output$RunDEGsNote <- shiny::renderPrint({
        cat("Killed!")
      })

      shiny::showNotification(
        "DEG analysis has been killed!",
        action = NULL,
        duration = 10,
        closeButton = TRUE,
        id = NULL,
        type = "warning"
      )
      }
  })



  # Observe event triggered when the "Check Empty GTF column" button is clicked
  shiny::observeEvent(input$CheckEmptyGTFcol, {
    # Check if the user has selected a GTF file; show an error if not
    if (is.integer(input$MergeDegsgtf)) {
      shiny::showNotification(
        "Please select GTF file",
        closeButton = TRUE,
        duration = NULL ,
        type = "error"
      )
    } else {
      shiny::showNotification(
        "Checking for empty columns in the 'GTF' file to ensure proper data formatting.",
        closeButton = NULL,
        duration = 10 ,
        type = "message"
      )
      # Perform metadata check on the GTF file
      check <- checkMetadata(gtfPath = MergeDegsgtfPath(),
                             typeFilter = input$typeFilterDegs)

      # Display the results of the check
      print(check)
      # Update the checkbox options with the results
      shiny::updateCheckboxGroupInput(inputId = "ColumnsMergeDegs",
                               label = "Chosen columns in GTF",
                               choices = check)
      shiny::showNotification(
        "Updated selectable columns in the 'GTF' file",
        closeButton = TRUE,
        duration = 5 ,
        type = "message"
      )
    }

  })


  #Merge DEGs

  # Reactive value for background operation, storing the results of the merging DEGs
  rvMergeDegs <- shiny::reactiveValues()
  # Reactive value to store the final result of the merging DEGs
  MergeDegsResult <- shiny::reactiveVal()
  # Initial note to indicate the status of the merging DEGs execution


  #start time to monitor execution
  start_time_RunMergeDegs <- shiny::reactiveVal()

  output$RunMergeDegsNote <- shiny::renderPrint({
    cat("Undone")
  })

  # Observe event triggered when the "Run Merge DEGs" button is clicked
  shiny::observeEvent(input$RunMergeDegs, {
    # Check if the DEGs directory is set; show an error if not
    if (is.integer(input$MergeDegsDir)) {
      shiny::showNotification(
        "Please select the folder containing the DEG tables",
        closeButton = TRUE,
        duration = NULL ,
        type = "error"
      )
    } else {
      # Disable the Run button and enable the Kill button during processing



      #start time to monitor execution
      start_time_RunMergeDegs(Sys.time())

      shinyjs::disable("RunMergeDegs")
      shinyjs::enable("killRunMergeDegs")

      # Update the status note to indicate running state
      output$RunMergeDegsNote <- shiny::renderPrint({
        cat("Running ...")
      })


      # Run the DEGs merge operation in the background using callr
      tryCatch({
        rvMergeDegs$result <- callr::r_bg(
          func = getDegMerged,
          args = list(
            # ID = input$IDMergeDegs,
            path = MergeDegsPath(),
            gtfPath = MergeDegsgtfPath(),
            columns = input$ColumnsMergeDegs,
            collapseName = input$collapseNameDegs,
            typeFilter = input$typeFilterDegs,
            selectUpDown = input$selectUpDownDegs
          ),
          supervise = TRUE ,
          error = getOption("callr.error", "error")
        )
      }
      , error = function(e) {
        shiny::showNotification(
          "Error occurs",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # finisce tryCatch

      # Observe the status of the merging process
      shiny::observe({
        if (shiny::isolate(rvMergeDegs$result$is_alive()) == TRUE)  {
          shiny::invalidateLater(2000, session)

        } else if (shiny::isolate(rvMergeDegs$result$poll_io(0)["process"]) == "ready") {
          tryCatch({
            shiny::isolate({
              MergeDegsResult(rvMergeDegs$result$get_result())

              # Render merged DEGs table
              output$Merged_DEGs_out <- DT::renderDT(
                MergeDegsResult(),
                selection = 'none',
                editable = FALSE,
                rownames = FALSE,
                # deve essere TRUE, si potrebbe risolvere considerando che DT inizia a leggere a partire da 0
                extensions = 'Buttons',
                options = list(
                  searching = TRUE,
                  fixedColumns = TRUE,
                  paging = TRUE,
                  pageLength = 5,
                  lengthMenu = list(
                    c(5, 15, 30, 50, 100, -1),
                    c('5','15', '30', '50', '100', 'All')
                  ),
                  autoWidth = TRUE,
                  ordering = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('csv', 'pageLength')
                ),

                class = "display"
              )

              shiny::showNotification(
                "The merged DEG table is displayed in the 'Merge DEG' panel.",
                closeButton = TRUE,
                duration = 5 ,
                type = "message"
              )


            }) # End of isolated


          }  ,error = function(e) {
            shiny::showNotification(
              "An error has occurred or the process was killed",
              action = NULL,
              duration = NULL,
              closeButton = TRUE,
              id = NULL,
              type = "error"
            )
          }) # End of tryCatch

          # Enable the "Run" button and disable the "Kill" button
          shinyjs::enable("RunMergeDegs")
          shinyjs::disable("killRunMergeDegs")



          #check end time of process
          end_time_RunMergeDegs <- Sys.time()
          execution_time_RunMergeDegs <- end_time_RunMergeDegs - start_time_RunMergeDegs()

          # Convert to hours, minutes, and seconds
          execution_time_sec_RunMergeDegs <- as.numeric(execution_time_RunMergeDegs, units="secs")
          hours_RunMergeDegs <- floor(execution_time_sec_RunMergeDegs / 3600)
          minutes_RunMergeDegs <- floor((execution_time_sec_RunMergeDegs %% 3600) / 60)
          seconds_RunMergeDegs <- execution_time_sec_RunMergeDegs %% 60


          # Update analysis note to "Done!"
          output$RunMergeDegsNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunMergeDegs, "hours", minutes_RunMergeDegs, "minutes", round(seconds_RunMergeDegs, 2), "seconds)")})



        }
      })


    } # end of else

  })


  # Interrupt the DEGs merging process if the "Kill" button is clicked
  shiny::observeEvent(input$killRunMergeDegs, {
    # stop the slow barplotExp() function
    rvMergeDegs$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

    # Enable the Run button and disable the Kill button
    shinyjs::enable("RunMergeDegs")
    shinyjs::disable("killRunMergeDegs")

    # Check if the process has been successfully killed
    if (shiny::isolate(rvMergeDegs$result$is_alive()) == FALSE) {
      output$RunMergeDegsNote <- shiny::renderPrint({
        cat("Killed!")
      })
    }
  })



  #volcano plot

  # Reactive value for background operation, storing the results of the saturation plot process
  rvVolcanoPlot <- shiny::reactiveValues()
  rvVolcanoPlottly <- shiny::reactiveValues()
  # Reactive value to store the final result of the saturation plot
  VolcanoPlotResult <- shiny::reactiveVal()
  VolcanoPlottlyResult <- shiny::reactiveVal()

  #start time to monitor execution
  start_time_RunVolcano <- shiny::reactiveVal()

  # Initial note to indicate the status of the volcano plot execution
  output$RunVolcanoNote <- shiny::renderPrint({
    cat("Undone")
  })

  # Observe event triggered when "Run Volcano" or "Refresh Run Volcano" is clicked
  shiny::observeEvent(c(input$RunVolcano, input$RefreshRunVolcano), {
    shiny::req(input$RunVolcano)
    if (is.integer(input$VolcanoDEGmatrix)) {
      shiny::showNotification(
        "Please set the path for the input DEG file",
        closeButton = TRUE,
        duration = NULL ,
        type = "error"
      )
    } else {

      #start time to monitor execution
      start_time_RunVolcano(Sys.time())

      # Disable the Run button and enable the Kill button during processing
      shinyjs::disable("RunVolcano")
      shinyjs::enable("killRunVolcano")
      shinyjs::disable("RefreshRunVolcano")

      # Update the status note to indicate running state
      output$RunVolcanoNote <- shiny::renderPrint({
        cat("Running ...")
      })



      # Run volcano plot in the background using callr
      tryCatch({
        rvVolcanoPlot$result <- callr::r_bg(
          func = volcanoPlot,
          args = list(
            x = VolcanoDEGmatrixPath(),
            palettePoint = input$VolcanoPalettePoint,
            maxOverlaps = input$VolcanoMaxOverlaps,
            sizeLabel = input$VolcanoSizeLabel,
            Th_logFC = input$VolcanoTh_logFC,
            Th_Pvalue = input$VolcanoTh_Pvalue,
            subsetGenes = input$VolcanoSubsetGenes,
            st_significance = as.character(input$Volcano_st_significance)
          ),
          supervise = TRUE ,
          error = getOption("callr.error", "error")
        )
      }
      , error = function(e) {
        shiny::showNotification(
          "Error occurs",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # finisce tryCatch

      # Observe the status of the volcano plot process
      shiny::observe({
        if (shiny::isolate(rvVolcanoPlot$result$is_alive()) == TRUE)  {
          shiny::invalidateLater(2000, session)

        } else if (shiny::isolate(rvVolcanoPlot$result$is_alive()) == FALSE && shiny::isolate(rvVolcanoPlot$result$poll_io(0)["process"]) == "ready") {
          tryCatch({
            shiny::isolate({
              VolcanoPlotResult(rvVolcanoPlot$result$get_result())

              output$VolcanoPlot <- shiny::renderPlot({
                print(VolcanoPlotResult())
              }, width = "auto", height = "auto", res = 96)


            }) # End of isolated


            #check end time of process
            end_time_RunVolcano <- Sys.time()
            execution_time_RunVolcano <- end_time_RunVolcano - start_time_RunVolcano()

            # Convert to hours, minutes, and seconds
            execution_time_sec_RunVolcano <- as.numeric(execution_time_RunVolcano, units="secs")
            hours_RunVolcano <- floor(execution_time_sec_RunVolcano / 3600)
            minutes_RunVolcano <- floor((execution_time_sec_RunVolcano %% 3600) / 60)
            seconds_RunVolcano <- execution_time_sec_RunVolcano %% 60


            # Update analysis note to "Done!"
            output$RunVolcanoNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunVolcano, "hours", minutes_RunVolcano, "minutes", round(seconds_RunVolcano, 2), "seconds)")})


            # Disable the Run button and enable the Refresh button
            shinyjs::disable("RunVolcano")
            shinyjs::disable("killRunVolcano")
            shinyjs::enable("RefreshRunVolcano")


          }  , error = function(e) {
            shiny::showNotification(
              "An error occurred while generating the volcano plot",
              action = NULL,
              duration = NULL,
              closeButton = TRUE,
              id = NULL,
              type = "error"
            )
          }) # End of tryCatch




        }
      })


      # Run the volcano plot function in the background using callr
      tryCatch({
        rvVolcanoPlottly$result <- callr::r_bg(
          func = volcanoPlottly,
          args = list(
            x = VolcanoDEGmatrixPath(),
            palettePoint = input$VolcanoPalettePoint,
            Th_logFC = input$VolcanoTh_logFC,
            Th_Pvalue = input$VolcanoTh_Pvalue,
            subsetGenes = input$VolcanoSubsetGenes,
            st_significance = input$Volcano_st_significance
          ),
          supervise = TRUE ,
          error = getOption("callr.error", "error")
        )
      }
      , error = function(e) {
        shiny::showNotification(
          "An error occurred while generating the volcano plot",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # finisce tryCatch

      # Observe the status of the volcano plot process
      shiny::observe({
        if (shiny::isolate(rvVolcanoPlottly$result$poll_io(0)["process"]) != "ready")  {
          shiny::invalidateLater(2000, session)

        } else if (shiny::isolate(rvVolcanoPlottly$result$poll_io(0)["process"]) == "ready") {
          tryCatch({
            shiny::isolate({
              VolcanoPlottlyResult(rvVolcanoPlottly$result$get_result())

              output$VolcanoPlottly <- plotly::renderPlotly({
                VolcanoPlottlyResult()
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




    } # End of else
  })



  # Interrupt the volcano plot process if the kill button is clicked
  shiny::observeEvent(input$killRunVolcano, {
    # stop the slow barplotExp() function
    rvVolcanoPlot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
    rvVolcanoPlottly$result$kill()
    # Enable the Run button and disable the Kill button
    shinyjs::enable("RunVolcano")
    shinyjs::disable("killRunVolcano")

    # Check if the process has been successfully killed
    if (shiny::isolate(rvVolcanoPlot$result$is_alive()) == FALSE &&
        shiny::isolate(rvVolcanoPlottly$result$is_alive()) == FALSE) {
      output$RunVolcanoNote <- shiny::renderPrint({
        cat("Killed!")
      })
    }
  })




  # Download handler for the volcano plot
  output$VolcanoLink <- shiny::downloadHandler(
    filename = function() {
      paste("Volcano"
            , Sys.Date(), ".", input$Volcanoformat, sep = "")
    },
    content = function(file) {
      # Save the volcano plot using ggsave with specified parameters
      ggsave(
        file,
        plot = (VolcanoPlotResult()),
        width = input$widthVolcano,
        height = input$heightVolcano,
        units = input$unitsVolcano,
        device = input$Volcanoformat,
        dpi = input$resVolcano
      )
    }
  )




  #Upset plot

  # Reactive value for background operation, storing the results of the Upset plot process
  rvUpsetPlot <- shiny::reactiveValues()
  rvUpsetjsPlot <- shiny::reactiveValues()

  # Reactive value to store the final result of the Upset plot
  UpsetPlotResult <- shiny::reactiveVal()
  UpsetjsPlotResult <- shiny::reactiveVal()


  #start time to monitor execution
  start_time_RunUpSet <-  shiny::reactiveVal()

  # Initial note to indicate the status of the Upset plot execution
  output$RunUpSetNote <- shiny::renderPrint({
    cat("Undone")
  })


  # Observe the event for the run and refresh buttons for the Upset plot
  shiny::observeEvent(c(input$RunUpSet, input$RefreshRunUpSet), {
    shiny::req(input$RunUpSet)
    if (is.integer(input$UpsetDEGfolder)) {
      shiny::showNotification(
        "Please set the path for the input DEG folder",
        closeButton = TRUE,
        duration = NULL ,
        type = "error"
      )

    } else if (length(list.files(file.path(UpsetDEGfolderPath()), pattern = "*.csv")) < 2 ) {

      shiny::showNotification(
        "Error: At least two contrasts must be selected for analysis",
        action = NULL,
        duration = NULL,
        closeButton = TRUE,
        id = NULL,
        type = "error"
      )

    } else {

      #start time to monitor execution
      start_time_RunUpSet(Sys.time())

      # Disable the Run button and enable the Kill button during processing
      shinyjs::disable("RunUpSet")
      shinyjs::enable("killRunUpSet")
      shinyjs::disable("RefreshRunUpSet")

      # Update the status note to indicate running state
      output$RunUpSetNote <- shiny::renderPrint({
        cat("Running ...")
      })


      # Run the Upset plot function in the background using callr
      tryCatch({
        rvUpsetPlot$result <- callr::r_bg(
          func = UpSetPlot,
          args = list(
            WD_samples = file.path(UpsetDEGfolderPath()),
            Th_logFC = input$UpSetTh_logFC,
            Th_Pvalue = input$UpSetTh_Pvalue,
            collapseName = input$UpSet_collapseName,
            nintersects = input$UpSet_nintersects,
            scale = input$UpSet_scale,
            st_significance = as.character(input$UpSet_st_significance)
          ),
          supervise = TRUE ,
          error = getOption("callr.error", "error")
        )
      }
      , error = function(e) {
        shiny::showNotification(
          "Error occurs",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # finisce tryCatch

      # Observe the status of the Upset plot process
      shiny::observe({
        if (shiny::isolate(rvUpsetPlot$result$poll_io(0)["process"]) != "ready")  {
          shiny::invalidateLater(2000, session)

        } else if (shiny::isolate(rvUpsetPlot$result$poll_io(0)["process"]) == "ready") {
          tryCatch({
            shiny::isolate({
              UpsetPlotResult(rvUpsetPlot$result$get_result())

              output$UpSetPlot <- shiny::renderPlot({
                UpsetPlotResult()
              }, width = "auto", height = "auto", res = 96)


            }) # End of isolated




            #check end time of process
            end_time_RunUpSet <- Sys.time()
            execution_time_RunUpSet <- end_time_RunUpSet - start_time_RunUpSet()

            # Convert to hours, minutes, and seconds
            execution_time_sec_RunUpSet <- as.numeric(execution_time_RunUpSet, units="secs")
            hours_RunUpSet <- floor(execution_time_sec_RunUpSet / 3600)
            minutes_RunUpSet <- floor((execution_time_sec_RunUpSet %% 3600) / 60)
            seconds_RunUpSet <- execution_time_sec_RunUpSet %% 60


            # Update analysis note to "Done!"
            output$RunUpSetNote <- shiny::renderPrint({cat(paste("Done! "),"(Execution Time: ", hours_RunUpSet, "hours", minutes_RunUpSet, "minutes", round(seconds_RunUpSet, 2), "seconds)")})




            # Disable the Run button and enable the Refresh button
            shinyjs::disable("RunUpSet")
            shinyjs::disable("killRunUpSet")
            shinyjs::enable("RefreshRunUpSet")

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

      # Run the Upsetjs plot function in the background using callr
      tryCatch({
        rvUpsetjsPlot$result <- callr::r_bg(
          func = UpsetjsPlot,
          args = list(
            WD_samples = file.path(UpsetDEGfolderPath()),
            Th_logFC = input$UpSetTh_logFC,
            Th_Pvalue = input$UpSetTh_Pvalue,
            collapseName = input$UpSet_collapseName,
            nintersects = input$UpSet_nintersects,
            st_significance = as.character(input$UpSet_st_significance)
          ),
          supervise = TRUE ,
          error = getOption("callr.error", "error")
        )
      }
      , error = function(e) {
        shiny::showNotification(
          "Error occurs",
          action = NULL,
          duration = NULL,
          closeButton = TRUE,
          id = NULL,
          type = "error"
        )
      }) # finisce tryCatch

      # Observe the status of the Upsetjs plot process
      shiny::observe({
        if (shiny::isolate(rvUpsetjsPlot$result$is_alive()) == TRUE)  {
          shiny::invalidateLater(2000, session)

        } else if (shiny::isolate(rvUpsetjsPlot$result$poll_io(0)["process"]) == "ready") {
          tryCatch({
            shiny::isolate({
              UpsetjsPlotResult(rvUpsetjsPlot$result$get_result())

              output$UpSetjsPlot <- upsetjs::renderUpsetjs({
                UpsetjsPlotResult()
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


    } # End of else
  })


  # Interrupt the Upset plot process if the kill button is clicked
  shiny::observeEvent(input$killRunUpSet, {
    # stop the Upset functions
    rvUpsetPlot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).
    rvUpsetjsPlot$result$kill() #Terminate the process. It also terminate all of its child processes, except if they have created a new process group (on Unix), or job object (on Windows). It returns TRUE if the process was terminated, and FALSE if it was not (because it was already finished/dead when processx tried to terminate it).

    # Enable the Run button and disable the Kill button
    shinyjs::enable("RunUpSet")
    shinyjs::disable("killRunUpSet")

    # Check if the process has been successfully killed
    if (shiny::isolate(rvUpsetPlot$result$is_alive())  == FALSE &&
        shiny::isolate(rvUpsetjsPlot$result$is_alive())  == FALSE) {
      output$RunUpSetNote <- shiny::renderPrint({
        cat("Killed!")
      })
    }
  })


  #  download Upset pre-norm

  output$UpSetLink <- shiny::downloadHandler(
    filename = function() {
      paste("UpSet", ".", input$UpSetformat, sep = "")
    },
    content = function(file) {
      if (input$UpSetformat == "jpeg") {
        grDevices::jpeg(
          file,
          width = input$widthUpSet,
          height = input$heightUpSet,
          units = input$unitsUpSet,
          res = input$resUpSet
        )
        print(UpsetPlotResult())
        grDevices::dev.off()
      } else if (input$UpSetformat == "png") {
        grDevices::png(
          file,
          width = input$widthUpSet,
          height = input$heightUpSet,
          units = input$unitsUpSet,
          res = input$resUpSet
        )
        print(UpsetPlotResult())
        grDevices::dev.off()
      } else if (input$UpSetformat == "tiff") {
        grDevices::tiff(
          file,
          width = input$widthUpSet,
          height = input$heightUpSet,
          units = input$unitsUpSet,
          res = input$resUpSet
        )
        print(UpsetPlotResult())
        grDevices::dev.off()
      } else if (input$UpSetformat == "tiff") {
        grDevices::tiff(
          file,
          width = input$widthUpSet,
          height = input$heightUpSet,
          units = input$unitsUpSet,
          res = input$resUpSet
        )
        print(UpsetPlotResult())
        grDevices::dev.off()
      } else if (input$UpSetformat == "eps") {
        R.devices::eps(file,
            width = input$widthUpSet,
            height = input$heightUpSet)
        print(UpsetPlotResult())
        grDevices::dev.off()
      } else if (input$UpSetformat == "svg") {
        grDevices::svg(file,
            width = input$widthUpSet,
            height = input$heightUpSet)
        print(UpsetPlotResult())
        grDevices::dev.off()
      } else if (input$UpSetformat == "pdf") {
        grDevices::pdf(file,
            width = input$widthUpSet,
            height = input$heightBoxplot)
        print(UpsetPlotResult())
        grDevices::dev.off()
      }
    }
  )

  } # close function inside moduleServer
  ) # close moduleServer
} # close ServerLogic function
