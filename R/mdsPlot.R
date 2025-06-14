#' mdsPlot
#'
#' Generate a multidimensional scaling (MDS) plot based on expression data.
#'
#' This function performs MDS analysis using limma's  "plotMDS() " and visualizes
#' the sample relationships in two dimensions using  "ggplot2 " and  "ggrepel ".
#'
#' @param x DGEList object from edgeR.
#' @param Sample A character vector of sample labels (one per column in  "x ").
#' @param Group A factor or character vector specifying the group/class of each sample.
#' @param title Plot title as a character string.
#' @param palette Name of a palette from the  "paletteer " package for coloring groups.
#' @param maxOverlaps Maximum number of overlapping labels allowed by  "geom_text_repel ".
#' @param sizeLabel Numeric value for label font size.
#' @param top Integer. Number of top most variable genes to include in MDS.
#' @param gene.selection Method for gene selection: one of "pairwise", "common", or "logFC".
#'
#' @return A  "ggplot " object representing the MDS plot.

mdsPlot <- function(x, Sample, Group, title, palette, maxOverlaps, sizeLabel, top, gene.selection){

  # 'x' is the input edger DGEList object
  # Perform MDS analysis on the input matrix
  mdsinfo <- limma::plotMDS(x= x, top = as.numeric(top), gene.selection = gene.selection, method = "logFC", plot = FALSE)

  # Extract the eigenvalues from the MDS result (used to calculate variance explained by each dimension)
  eigenValues <- mdsinfo$eigen.values

  # Extract the coordinates of samples in the first two MDS dimensions (dim1 and dim2)
  dim1 <- mdsinfo$x
  dim2 <- mdsinfo$y

  # Calculate the percentage of variance explained by the first two MDS dimensions
  mdsVarPer <- round(eigenValues/sum(eigenValues) * 100, 1)

  # Create a tibble to store the coordinates along with sample and group information
  data <- tibble::tibble(dim1 = dim1, dim2 = dim2, Sample = Sample, Group = Group)

  # Generate the MDS plot using ggplot2
  plot <- ggplot2::ggplot(data, mapping = ggplot2::aes(
    x = dim1,   # X-axis represents the first MDS dimension
    y = dim2,   # Y-axis represents the second MDS dimension
    label = Sample,  # Labels are the sample names
    color = Group    # Color points by the Group
  )) +

    # Add points representing samples, with black border
    ggplot2::geom_point(color = "black") +

    # Ensure labels and points do not get clipped outside plot bounds
    ggplot2::coord_cartesian(clip = "off") +

    # Use ggrepel to prevent label overlaps and to adjust label appearance
    ggrepel::geom_text_repel(
      fontface = "bold",          # Bold font for sample labels
      max.overlaps = as.numeric(maxOverlaps),  # Maximum allowed overlaps for labels
      size = as.numeric(sizeLabel),  # Size of the sample labels
      xlim = c(-Inf, Inf),  # No limits for the X-axis
      ylim = c(-Inf, Inf)   # No limits for the Y-axis
    ) +

    # Label the X-axis with the percentage of variance explained by MDS dimension 1
    ggplot2::xlab(paste("MDS 1 ", mdsVarPer[1], "%", sep = "")) +

    # Label the Y-axis with the percentage of variance explained by MDS dimension 2
    ggplot2::ylab(paste("MDS 2 ", mdsVarPer[2], "%", sep = "")) +

    # Set the title of the plot with the provided title argument
    ggplot2::ggtitle(title) +

    # Center and bold the title
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +

    # Apply the specified color palette to groups using the paletteer package
    paletteer::scale_color_paletteer_d(palette) +

    # Use the minimal theme for a minimalistic theme with no background annotations
    ggplot2::theme_minimal()

  return(plot)

}
