#' boxplotExp
#'
#' Generate a boxplot of log-CPM expression values per sample, colored by group.
#'
#' This function orders samples by group or sample name, and produces a ggplot2 boxplot
#' with a horizontal line at the overall median.
#'
#' @param x A DGEList object from "edgeR".
#' @param y Numeric matrix of log-CPM values (genes × samples), e.g., from edgeR::cpm().
#' @param palette Character. Name of a discrete palette from the paletteer package.
#' @param main Character. Title for the boxplot.
#' @param selectOrder Character. Either "Groups" (order samples by group) or "Samples" (order by sample name).
#'
#' @return A ggplot object showing per-sample boxplots of log-CPM values.
#'
#' @details
#' 1. Extract sample metadata (Samples, Groups) from "x$samples".
#' 2. Order columns of y by group or sample name per "selectOrder".
#' 3. Melt the ordered matrix to long format and join with metadata.
#' 4. Plot boxplots with no outliers, colored by group, and include a dashed line at the overall median.
#'
boxplotExp <- function(x, y, palette, main, selectOrder){

  # 'x' is the DGEList object
  # 'y' is the logcounts matrix from edgeR::cpm
  # Extract the first two columns from the edger DGEList object and rename them as "Samples" and "Groups"
  gPre <- magrittr::`%>%`(x$samples[,1:2], `colnames<-`(c("Samples","Groups")))

  # Order the expression data based on the selected order ("Groups" or "Samples")
  if (selectOrder == "Groups") {
    # If order by "Groups" is selected, sort the columns of 'y' by group
    lPre <- as.matrix(y[,order(gPre$Groups)])

  } else if (selectOrder == "Samples") {
    # If order by "Samples" is selected, sort the columns of 'y' by sample names
    lPre <- as.matrix(y[,order(gPre$Samples)])
  }

  # Reshape the sorted expression data for plotting: melt the matrix into long format
  meltLpre <- reshape2::melt(lPre, variable.name = "Samples", value.name = "Value")

  # Join the melted expression data with the sample metadata (Groups and Samples) based on the "Samples" column
  JoinMeltLpre <- dplyr::left_join(meltLpre, gPre, by = "Samples")

  # Generate a box plot using ggplot2
  plot <- ggplot2::ggplot(data = JoinMeltLpre, mapping = ggplot2::aes(x = Samples, y = Value, color = Groups)) +

    # Add a box plot with thick lines and no outliers shown
    ggplot2::geom_boxplot(linewidth = 1, outlier.shape = NA) +

    # Set the plot title
    ggplot2::ggtitle(main) +

    # Add a horizontal dashed line at the median value of the entire dataset
    ggplot2::geom_hline(yintercept = median(JoinMeltLpre$Value), linetype = 2, colour = "black") +

    # Label the X-axis as "Samples"
    ggplot2::xlab("Samples") +

    # Label the Y-axis as "Log2 counts per million"
    ggplot2::ylab("Log2 counts per million") +

    # Add a legend for the "Groups" with the appropriate color scheme
    ggplot2::guides(color = ggplot2::guide_legend("Groups")) +

    # Apply the minimal theme for a minimalistic theme with no background annotations
    ggplot2::theme_minimal() +

    # Use the specified color palette for groups from the paletteer package
    paletteer::scale_color_paletteer_d(palette) +

    # Customize the theme elements (titles, axis labels, and X-axis text)
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "black", size = 20, face = "bold.italic", hjust = 0.5),
      axis.title.x = ggplot2::element_text(color = "black", size = 14, face = "bold"),
      axis.title.y = ggplot2::element_text(color = "black", size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(
        angle = 90,   # Rotate the X-axis labels by 90 degrees
        vjust = 0.5,  # Adjust vertical justification
        hjust = 1     # Adjust horizontal justification
      )
    )

  return(plot)


}
