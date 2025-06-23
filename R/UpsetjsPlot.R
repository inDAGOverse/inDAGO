#' UpsetjsPlot
#'
#' Create an interactive UpSet plot of overlapping DEGs using "UpsetJS".
#'
#' This function reads DEG CSV files from a directory, filters genes by log-FC and
#' p-value thresholds (adjusted or raw), optionally simplifies file names, and
#' visualizes the intersections of gene sets using the "UpsetJS" package.
#'
#' @param WD_samples Character. Directory containing DEG result CSV files.
#' @param Th_logFC Numeric. Absolute log₂ fold-change threshold to include a gene.
#' @param Th_Pvalue Numeric. P-value threshold for significance (0 < Th_Pvalue ≤ 1).
#' @param collapseName Logical. If TRUE, strip method/model prefixes from file names when labeling sets.
#' @param nintersects Integer. Maximum number of intersections to display.
#' @param st_significance Character. Which p-value to use: "adjustPvalue" (FDR or FWER) or "PValue".
#'
#' @return An interactive "UpsetJS" object.
#'
#' @details
#' 1. Lists all CSV files in "WD_samples" and reads each into a data frame.
#' 2. Checks for duplicate IDs and selects "ID", "logFC", and either "adjustPvalue" or "PValue".
#' 3. Filters each set by "|logFC| ≥ Th_logFC" and p-value < "Th_Pvalue".
#' 4. Renames each gene-ID list to the (optionally collapsed) file name.
#' 5. Feeds the list of gene sets into "upsetjs::upsetjs()"
#'
UpsetjsPlot <- function(WD_samples, Th_logFC, Th_Pvalue, collapseName, nintersects, st_significance) {

  # List all files in the specified directory and remove file extensions
  l <- magrittr::`%>%`(list.files(WD_samples, pattern = ".*csv"), tools::file_path_sans_ext(.))

  # Loop over each file and process it to create a list of gene sets based on the thresholds and significance measure
  listFileUpset <- sapply(l, function(i) {

    # Read each CSV file into a data frame
    df <- utils::read.csv(paste0(file.path(WD_samples), "/", i, ".csv"))


    colnames(df)[1] <- "ID"

    if (any(duplicated(df[[1]]))) {
      stop(paste("Il dataframe ha valori duplicati inella prima colonna che descrive l identificatore"))
    }


    # Check if the significance measure is based on adjusted p-values (e.g., FDR or FWER)
    if (st_significance == "adjustPvalue") {

      # Select the columns for gene ID, logFC, and the appropriate adjusted p-value (FDR or FWER) and rename them
      if ("FDR" %in% colnames(df)) {
        df <- magrittr::`%>%`(dplyr::select(df, "ID", "logFC", "FDR"), `colnames<-`(c("ID", "logFC", "adjustPvalue")))
      } else if ("FWER" %in% colnames(df)) {
        df <- magrittr::`%>%`(dplyr::select(df, "ID", "logFC", "FWER"), `colnames<-`(c("ID", "logFC", "adjustPvalue")))
      }

      # Optionally collapse the file name by removing specific patterns
      if (collapseName == "TRUE") {
        i <- gsub("filterByExpr_|HTSFilter_|exactTest_|glmQLFTest_|glmLRT_", "", i)
      }

      # Filter the data frame for genes meeting the logFC and raw P-value thresholds and select only the "ID" column
      # Rename the selected "ID" column to the simplified file name (or original name if not collapsed)
      df <- magrittr::`%>%`(magrittr::`%>%`(subset(df, abs(logFC) >= Th_logFC & adjustPvalue < Th_Pvalue), dplyr::select("ID")), `names<-`(i))


    } else if (st_significance == "PValue") {
      # If significance measure is raw p-value, select the appropriate columns and rename them
      df <- dplyr::select(df, "ID", "logFC", "PValue")

      # Optionally collapse the file name by removing specific patterns
      if (collapseName == "TRUE") {
        i <- gsub("filterByExpr_|HTSFilter_|exactTest_|glmQLFTest_|glmLRT_", "", i)
      }

      # Filter the data frame for genes meeting the logFC and raw P-value thresholds and select only the "ID" column
      # Rename the selected "ID" column to the simplified file name (or original name if not collapsed)
      df <- magrittr::`%>%`(magrittr::`%>%`(subset(df, abs(logFC) >= Th_logFC & PValue < Th_Pvalue), dplyr::select("ID")), `names<-`(i))
    }

  }, simplify = TRUE, USE.NAMES = FALSE)  # Simplify the result to a list of gene ID data frames

  # Create the UpSet plot using the upsetjs library, from the list of gene sets
  magrittr::`%>%`(
    magrittr::`%>%`(
      magrittr::`%>%`(
        magrittr::`%>%`(
          upsetjs::upsetjs(),                               # Initialize the UpSetJS plot object
          upsetjs::fromList(listFileUpset)                  # Convert the list of gene sets to UpSetJS format
        ),
        upsetjs::generateDistinctIntersections(limit = nintersects)  # Generate unique intersections with a limit on the number displayed
      ),
      upsetjs::chartLabels(combination.name = "Genes intersected", set.name = "Genes per contrast")  # Add labels to the chart
    ),
    upsetjs::interactiveChart()                             # Make the chart interactive for enhanced visualization
  )

}
