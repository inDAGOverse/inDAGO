#' volcanoPlottly
#'
#' Create an interactive volcano plot of differential expression results using "Plotly".
#'
#' This function reads a CSV of DEGs, classifies genes as up/down/no change based on
#' log-fold change and p-value thresholds, and renders an interactive volcano plot
#' via "plotly::ggplotly()".
#'
#' @param x Character. File path to a CSV containing DEG results, with at least columns "ID", "logFC", and one of "PValue", "FDR", or "FWER".
#' @param palettePoint Character. Name of a discrete palette from the "paletteer" package, supplying colors for "UP", "DOWN", and "NO".
#' @param Th_logFC Numeric. Absolute log₂ fold-change threshold to call a gene "UP" or "DOWN".
#' @param Th_Pvalue Numeric. P-value threshold to call significance (uses "FDR"/"FWER" if "st_significance = "adjustPvalue"", otherwise raw "PValue").
#' @param subsetGenes Integer or "Inf". If numeric, only the top "subsetGenes" genes by p-value are included in the plot.
#' @param st_significance Character. Which p-value column to use: "adjustPvalue" (FDR or FWER) or "PValue".
#'
#' @return A Plotly object ("plotly::ggplotly") representing the interactive volcano plot.
#'
#' @details
#' 1. Reads the input CSV and checks for duplicate IDs.
#' 2. Standardizes columns to "ID", "logFC", and "adjustPvalue" or "PValue".
#' 3. Optionally subsets to the top N genes by p-value.
#' 4. Classifies each gene as "UP", "DOWN", or "NO" based on thresholds.
#' 5. Plots points with manual fill, size, and alpha scales, adds threshold lines,
#'    and converts to an interactive Plotly graph.
#'
volcanoPlottly <- function(x, palettePoint, Th_logFC, Th_Pvalue, subsetGenes, st_significance) {


  # 'x' in input is the path of DEGs dataframe
  # Convert the input data to a data frame
  x <- as.data.frame(utils::read.csv(file.path(x)))

  colnames(x)[1] <- "ID"



  if (any(duplicated(x[[1]]))) {
    stop(paste("Il dataframe ha valori duplicati inella prima colonna che desrive l identificatore"))
  }


  # Retrieve the color palette for the points from the paletteer package
  col <- paletteer::paletteer_d(palettePoint)

  # Assign colors for "UP", "DOWN", and "NO" based on the palette
  cols <- c("UP" = col[[1]], "DOWN" = col[[2]], "NO" = col[[3]])

  # Set sizes for the points based on their differential expression status
  sizes <- c("UP" = 2, "DOWN" = 2, "NO" = 0.5)

  # Set transparency (alpha) values for the points
  alphas <- c("UP" = 1, "DOWN" = 1, "NO" = 0.5)

  # Check if significance should be based on the adjusted p-value (e.g., FDR or FWER)
  if (st_significance == "adjustPvalue") {

    # Select the appropriate column for adjusted p-values (either FDR or FWER)
    if ("FDR" %in% colnames(x)) {
      # Rename columns to a standardized format
      x <- magrittr::`%>%`(dplyr::select(x, "ID", "logFC", "FDR"), `colnames<-`(c("ID", "logFC", "adjustPvalue")))
    } else if ("FWER" %in% colnames(x)) {
      x <- magrittr::`%>%`(dplyr::select(x, "ID", "logFC", "FWER"), `colnames<-`(c("ID", "logFC", "adjustPvalue")))
    }

    # If a subset of genes is requested, select the top ones based on the adjusted p-value
    if (subsetGenes != Inf) {
      x <- magrittr::`%>%`(x[order(x$adjustPvalue),], head(as.numeric(subsetGenes)))
    }

    # Initialize all genes as "NO" differential expression
    x$diffExp <- "NO"

    # Classify genes as "UP" or "DOWN" based on thresholds for logFC and adjusted p-value
    x$diffExp[x$logFC >= Th_logFC & x$adjustPvalue < Th_Pvalue] <- "UP"
    x$diffExp[x$logFC <= -Th_logFC & x$adjustPvalue < Th_Pvalue] <- "DOWN"

    # Create a volcano plot using ggplot2
    plotly::ggplotly(ggplot2::ggplot(x, ggplot2::aes(x = logFC, y = -log10(adjustPvalue),
                                                     fill = diffExp, group = diffExp, size = diffExp, alpha = diffExp, label = ID)) +
                       ggplot2::geom_point(shape = 21, colour = "black") +  # Use shape 21 (filled circle) with a black border

                       # Modify the plot's appearance by adjusting the fill color, size, and alpha (transparency) of the points
                       ggplot2::scale_fill_manual(values = cols) +
                       ggplot2::scale_size_manual(values = sizes) +
                       ggplot2::scale_alpha_manual(values = alphas) +

                       # Add vertical lines at the log fold change thresholds
                       ggplot2::geom_vline(xintercept = c(-Th_logFC, Th_logFC), col = "grey", linetype = 2) +

                       # Add a horizontal line at the p-value threshold
                       ggplot2::geom_hline(yintercept = -log10(Th_Pvalue), col = "grey", linetype = 2) +

                       # Add a title to the plot and center it
                       ggplot2::ggtitle("Volcano Plot") +
                       ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +

                       # Set Y-axis limits and remove extra space on the axis
                       ggplot2::scale_y_continuous(limits = function(x){c(min(x), (max(x)+0.5))}) +

                       # Use a minimal theme for the plot
                       ggplot2::theme_minimal()
    )
  } else if (st_significance == "PValue") {
    # If using raw p-values for significance, select the PValue column
    x <- dplyr::select(x, "ID", "logFC", "PValue")

    # Initialize all genes as "NO" differential expression
    x$diffExp <- "NO"

    # Classify genes as "UP" or "DOWN" based on thresholds for logFC and raw p-value
    x$diffExp[x$logFC >= Th_logFC & x$PValue < Th_Pvalue] <- "UP"
    x$diffExp[x$logFC <= -Th_logFC & x$PValue < Th_Pvalue] <- "DOWN"

    # Create a volcano plot using ggplot2
    plotly::ggplotly(ggplot2::ggplot(x, ggplot2::aes(x = logFC, y = -log10(PValue),
                                                     fill = diffExp, group = diffExp, size = diffExp, alpha = diffExp, label = ID)) +
                       ggplot2::geom_point(shape = 21, colour = "black") +  # Use shape 21 (filled circle) with a black border

                       # # Add labels to points with ggrepel to avoid overlap
                       # ggrepel::geom_text_repel(max.overlaps = as.numeric(maxOverlaps), size = as.numeric(sizeLabel)) +

                       # Modify the plot's appearance by adjusting the fill color, size, and alpha (transparency) of the points
                       ggplot2::scale_fill_manual(values = cols) +
                       ggplot2::scale_size_manual(values = sizes) +
                       ggplot2::scale_alpha_manual(values = alphas) +

                       # Add vertical lines at the log fold change thresholds
                       ggplot2::geom_vline(xintercept = c(-Th_logFC, Th_logFC), col = "grey", linetype = 2) +

                       # Add a horizontal line at the p-value threshold
                       ggplot2::geom_hline(yintercept = -log10(Th_Pvalue), col = "grey", linetype = 2) +

                       # Add a title to the plot and center it
                       ggplot2::ggtitle("Volcano Plot") +
                       ggplot2::theme(plot.title =   ggplot2::element_text(hjust = 0.5, face = "bold")) +

                       # Set Y-axis limits and remove extra space on the axis
                       ggplot2::scale_y_continuous(limits = function(x){c(min(x), (max(x)+0.5))}) +

                       # Use a minimal theme for the plot
                       ggplot2::theme_minimal()
    )
  }
}
