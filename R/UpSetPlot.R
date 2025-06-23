#' UpSetPlot
#'
#' Generate an UpSet plot of overlapping DEGs across multiple contrasts.
#'
#' This function reads DEG CSV files from a directory, filters genes by log-FC and p-value
#' thresholds (adjusted or raw), optionally simplifies file names, and visualizes the
#' intersections of gene sets using an UpSet plot.
#' @param WD_samples Character. Directory containing DEG result CSV files.
#' @param Th_logFC Numeric. Absolute log₂ fold-change threshold to include a gene.
#' @param Th_Pvalue Numeric. P-value threshold for significance (0 < Th_Pvalue ≤ 1).
#' @param collapseName Logical. If TRUE, strip method/model prefixes from file names when labeling sets.
#' @param nintersects Integer. Maximum number of intersections to display.
#' @param st_significance Character. Which p-value to use: "adjustPvalue" (FDR or FWER) or "PValue".
#' @param scale Numeric. Text scaling factor for plot labels and annotations.
#'
#' @return An UpSet plot.
#'
#' @details
#' 1. Validates thresholds (Th_logFC ≥ 0, 0 < Th_Pvalue ≤ 1).
#' 2. Lists all CSV files in WD_samples and reads each into a data frame.
#' 3. Checks for duplicate IDs and standardizes to columns ID, logFC, and adjustPvalue or PValue.
#' 4. Filters each set of results by |logFC| ≥ Th_logFC and p-value < Th_Pvalue.
#' 5. Renames each gene-ID column to the (optionally collapsed) file name.
#' 6. Converts the list of filtered ID sets to an UpSetR input and calls UpSetR::upset().
#'
UpSetPlot <- function(WD_samples, Th_logFC, Th_Pvalue, collapseName, nintersects, st_significance, scale) {

  # To prevent it from saving a default PDF file "Rplots.pdf" in your working folder.
  if (!interactive()) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Check if the threshold values for logFC or P-value are invalid (logFC should be >= 0, P-value should be between 0 and 1)
  if (Th_logFC < 0 | Th_Pvalue < 0 | Th_Pvalue > 1) {
    print("Invalid threshold values: Th_logFC < 0 | Th_Pvalue < 0 | Th_Pvalue > 1")
  } else {



    # Create a list of file names in the specified working directory without file extensions
    l <- magrittr::`%>%`(list.files(WD_samples,pattern = ".*csv"), tools::file_path_sans_ext(.))

    # Loop over the list of files to create a list of data frames containing only the gene IDs that pass the filtering criteria
    listFileUpset <- sapply(l, function(i) {

      # Read each CSV file
      df <- utils::read.csv(paste0(file.path(WD_samples), "/", i, ".csv"))

      colnames(df)[1] <- "ID"

      if (any(duplicated(df[[1]]))) {
        stop(paste("Il dataframe ha valori duplicati inella prima colonna che descrive l identificatore"))
      }

      # Check if the significance metric is based on adjusted p-value (e.g., FDR or FWER)
      if (st_significance == "adjustPvalue") {

        # Select the appropriate columns for ID, logFC, and adjusted p-value (FDR or FWER) and rename them
        if ("FDR" %in% colnames(df)) {
          df <- magrittr::`%>%`(dplyr::select(df, "ID", "logFC", "FDR"), `colnames<-`(c("ID", "logFC", "adjustPvalue")))
        } else if ("FWER" %in% colnames(df)) {
          df <- magrittr::`%>%`(dplyr::select(df, "ID", "logFC", "FWER"), `colnames<-`(c("ID", "logFC", "adjustPvalue")))
        }

        # Optionally collapse the file name by removing specific patterns (simplifying the name)
        if (collapseName == "TRUE") {
          i <- gsub("filterByExpr_|HTSFilter_|exactTest_|glmQLFTest_|glmLRT_", "", i)
        }

        # Filter the data frame for genes meeting the logFC and adjusted p-value thresholds and select only the "ID" column
        # Rename the selected "ID" column to the simplified file name (or original name if not collapsed)

        df <- magrittr::`%>%`(magrittr::`%>%`(subset(df, abs(logFC) >= Th_logFC & adjustPvalue < Th_Pvalue), dplyr::select("ID")), `names<-`(i))

        # Check if the significance metric is based on raw p-value
      } else if (st_significance == "PValue") {

        # Select the appropriate columns for ID, logFC, and raw P-value and rename them
        df <- dplyr::select(df, "ID", "logFC", "PValue")

        # Optionally collapse the file name by removing specific patterns (simplifying the name)
        if (collapseName == "TRUE") {
          i <- gsub("filterByExpr_|HTSFilter_|exactTest_|glmQLFTest_|glmLRT_", "", i)
        }

        # Filter the data frame for genes meeting the logFC and raw P-value thresholds and select only the "ID" column
        # Rename the selected "ID" column to the simplified file name (or original name if not collapsed)
        df <- magrittr::`%>%`(magrittr::`%>%`(subset(df, abs(logFC) >= Th_logFC & PValue < Th_Pvalue), dplyr::select("ID")), `names<-`(i))
      }

    }, simplify = TRUE, USE.NAMES = FALSE)  # Simplify the result to a list of gene ID data frames


    # Create an UpSet plot using the list of gene sets from the files
    plot <-  UpSetR::upset(
      UpSetR::fromList(listFileUpset),  # Convert the list of gene sets to a format suitable for UpSetR
      order.by = "freq",                # Order intersections by frequency of occurrence
      point.size = 1.5,                 # Set the size of the points in the intersection plot
      line.size = 0.5,                  # Set the size of the lines connecting the points
      mainbar.y.label = "Genes intersected",  # Label for the y-axis of the intersection plot
      sets.x.label = "Genes per contrast",    # Label for the x-axis of the set size bar plot
      text.scale = scale,               # Adjust text scale for labels and annotations
      nintersects = nintersects,        # Limit the number of intersections displayed
      sets.bar.color = "gray23",        # Set the color of the set size bars
      nsets = 50                         # Number of sets to look at
    )

   return(plot)

  }
}



