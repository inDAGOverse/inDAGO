#' barplotExp
#'
#' Create a barplot of library sizes per sample, optionally using effective library sizes.
#'
#' This function extracts library size information from an "edgeR" "DGEList", computes
#' effective library sizes if requested, orders samples by group or name, and plots
#' library sizes (in millions) colored by group.
#'
#' @param x A DGEList object from "edgeR".
#' @param palette Character. Name of a discrete color palette from the "paletteer" package.
#' @param main Character. Title for the barplot.
#' @param selectOrder Character. Either "Groups" (order samples by group) or "Samples" (order by sample name).
#' @param effecLibSize Logical. If TRUE, use effective library size (norm factors × raw size); otherwise use raw size.
#'
#' @return A "ggplot" object showing per-sample barplots of library size in millions.
#'
#' @details
#' 1. Extracts or computes (effecLibSize = TRUE) the library size for each sample.
#' 2. Orders samples by group or sample name per selectOrder.
#' 3. Plots bar heights as library size (×10⁶) with white fill and colored borders.
#'
#'

barplotExp <- function(x, palette, main, selectOrder, effecLibSize){


  # 'x' is the DGEList object
  # Check if the effective library size should be used
  if (effecLibSize == FALSE ) {
    # If FALSE, select the first three columns: Samples, Groups, and original library size
    data <- magrittr::`%>%`(x$samples[,1:3], `colnames<-`(c("Samples", "Groups", "lib.size")))
  } else if (effecLibSize == TRUE) {
    # If TRUE, calculate the effective library size as the product of columns 3 and 4
    data <- magrittr::`%>%`(cbind(x$samples, apply(x$samples[,3:4], 1, matrixStats::product))[,c(1,2,5)],
                            `colnames<-`(c("Samples", "Groups", "lib.size")))
  }

  # Order the data based on the selected order ("Groups" or "Samples")
  if (selectOrder == "Groups") {
    # Order by Groups and set Samples as a factor with levels corresponding to the new order
    dataOrdered <- as.data.frame(data[order(data$Groups),])
    dataOrdered$Samples <- factor(dataOrdered$Samples, levels = dataOrdered$Samples)
  } else if (selectOrder == "Samples") {
    # Order by Samples and set Samples as a factor with levels corresponding to the new order
    dataOrdered <- as.data.frame(data[order(data$Samples),])
    dataOrdered$Samples <- factor(dataOrdered$Samples, levels = dataOrdered$Samples)
  }

  # Convert library size to numeric
  dataOrdered$lib.size <- as.numeric(dataOrdered$lib.size)

  # Create a bar plot with ggplot2
  plot <- ggplot2::ggplot(data = dataOrdered, ggplot2::aes(x = Samples, y = lib.size / 1e6, color = Groups, width = 0.8)) +
    # Add bars with white fill, colored borders, and linewidth
    ggplot2::geom_bar(stat = "identity", fill = "white", linewidth = 1) +

    # Set Y-axis limits and remove extra space on the axis
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +

    # Set plot title
    ggplot2::ggtitle(main) +

    # Set X and Y axis labels
    ggplot2::xlab("Samples") +
    ggplot2::ylab("Library size (millions)") +

    # Add a legend for the "Groups"
    ggplot2::guides(color = ggplot2::guide_legend("Groups")) +

    # Apply the minimal theme for clean aesthetics
    ggplot2::theme_minimal() +

    # Use the specified color palette from the paletteer package
    paletteer::scale_color_paletteer_d(palette) +

    # Customize text and label appearance
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "black", size = 20, face = "bold.italic", hjust = 0.5),
      axis.title.x = ggplot2::element_text(color = "black", size = 14, face = "bold"),
      axis.title.y = ggplot2::element_text(color = "black", size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(
        angle = 90,   # Rotate X-axis labels by 90 degrees
        vjust = 0.5,  # Adjust vertical justification for X-axis labels
        hjust = 1     # Adjust horizontal justification for X-axis labels
      )
    )

  return(plot)
}
