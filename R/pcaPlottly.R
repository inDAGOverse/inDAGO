#' pcaPlottly
#'
#' Create an interactive PCA scatter plot using Plotly from log-expression data.
#'
#' This function performs Principal Component Analysis (PCA) on a log-count matrix
#' and generates an interactive plot of the first two principal components via
#'  "plotly::ggplotly()".
#'
#' @param logcounts Numeric matrix of log-CPM values (genes × samples), e.g., from edgeR::cpm.
#' @param Sample Character vector of sample names corresponding to the columns of  "logcounts ".
#' @param Group Factor or character vector denoting group/condition for each sample.
#' @param title Character. Title for the PCA plot.
#' @param palette Character. Name of a discrete color palette from the  "paletteer" package.
#' @param center Logical. If TRUE, center variables (genes) before PCA.
#' @param scale Logical. If TRUE, scale variables to unit variance before PCA.
#'
#' @return A Plotly object ( "plotly::ggplotly ") representing the interactive PCA scatterplot.
#'
#' @details
#' 1. Transposes the  "logcounts " matrix so samples are rows.
#' 2. Runs PCA with  "stats::prcomp() ", using centering and scaling as specified.
#' 3. Computes percent variance explained by PC1 and PC2.
#' 4. Builds a ggplot2 scatterplot and converts it to an interactive Plotly graph.
#'
pcaPlottly <- function(logcounts, Sample, Group, title, palette, center, scale){

  # 'logcounts' is the matrix from edgeR::cpm
  # Transpose the logcount matrix so that rows represent samples and columns represent genes.
  logcount_t <- t(logcounts)

  # Perform Principal Component Analysis (PCA) on the transposed matrix.
  # 'center = TRUE' centers the data by subtracting the mean of each gene.
  # 'scale. = FALSE' does not scale the variance of each gene.
  pca <- stats::prcomp(logcount_t, center = as.logical(center), scale. = as.logical(scale))

  # Extract the standard deviation of each principal component.
  sdev <- pca$sdev

  # Extract the coordinates (scores) for the first two principal components (PC1 and PC2).
  dim1 <- pca$x[,1]  # Principal Component 1
  dim2 <- pca$x[,2]  # Principal Component 2

  # Calculate the percentage of variance explained by the first two principal components.
  pcaVarPer <- round(sdev^2 / sum(sdev^2) * 100, 1)

  # Create a tibble to store the PCA dimensions along with sample and group information.
  data <- tibble::tibble(dim1 = dim1, dim2 = dim2, Sample = Sample, Group = Group)

  # Generate an interactive PCA plot using ggplot2 and convert it to a plotly object for interactivity.
  plotly <- plotly::ggplotly(
    ggplot2::ggplot(data, ggplot2::aes(
      x = dim1,   # X-axis represents Principal Component 1
      y = dim2,   # Y-axis represents Principal Component 2
      label = Sample,  # Labels are the sample names (will appear as hover text)
      color = Group    # Color points by the group
    )) +

      # Add points representing samples.
      ggplot2::geom_point() +

      # Label the X-axis with the percentage of variance explained by PC1.
      ggplot2::xlab(paste("PCA 1 ", pcaVarPer[1], "%", sep = "")) +

      # Label the Y-axis with the percentage of variance explained by PC2.
      ggplot2::ylab(paste("PCA 2 ", pcaVarPer[2], "%", sep = "")) +

      # Set the title of the plot with the provided title argument.
      ggplot2::ggtitle(title) +

      # Apply the minimal theme for a clean plot appearance.
      ggplot2::theme_minimal() +

      # Center and bold the title.
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +

      # Apply the specified color palette to groups using the paletteer package.
      paletteer::scale_color_paletteer_d(palette)
  )

  return(plotly)
}
