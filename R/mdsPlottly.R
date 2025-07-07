#' mdsPlottly
#'
#' Generate an interactive MDS plot using Plotly based on expression data.
#'
#' This function computes multidimensional scaling (MDS) coordinates with limma's  "plotMDS() "
#' and then renders an interactive scatterplot via  "plotly::ggplotly() ".
#'
#' @param x A DGEList object from edgeR.
#' @param Sample Character vector. Sample names corresponding to columns of  "x ".
#' @param Group Factor or character vector. Group or condition for each sample.
#' @param title Character. Title for the plot.
#' @param palette Character. Name of a discrete palette from the  "paletteer " package.
#' @param top Integer. Number of top most variable genes (by logFC) to include in MDS.
#' @param gene.selection Character. Gene selection method: one of  ""pairwise" ",  ""common" ", or  ""logFC" ".
#'
#' @return A Plotly object ( "plotly::ggplotly ") representing the interactive MDS scatterplot.
#'
#' @details
#' 1. Compute MDS on the input data with  "limma::plotMDS() ".
#' 2. Extract eigenvalues and first two dimensions for variance annotation.
#' 3. Build a ggplot2 scatterplot with axis labels showing percent variance explained.
#' 4. Convert the ggplot to an interactive Plotly graph.
#'
mdsPlottly <- function(x, Sample, Group, title, palette, top, gene.selection){

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

  # Create the MDS plot using ggplot2, and convert it to a plotly interactive plot
  plotly <- plotly::ggplotly(
    ggplot2::ggplot(data, mapping = ggplot2::aes(
      x = dim1,    # X-axis represents the first MDS dimension
      y = dim2,    # Y-axis represents the second MDS dimension
      label = Sample,  # Labels are the sample names, used in the interactive plot for hover information
      color = Group    # Color points by the Group
    )) +

      # Add points representing samples
      ggplot2::geom_point() +

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
  )

  return(plotly)
}
