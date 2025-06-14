#' pcaPlot
#'
#' Create a PCA scatter plot from log-expression data with sample labels.
#'
#' This function performs Principal Component Analysis (PCA) on a log-count matrix
#' and visualizes the first two principal components using ggplot2 and ggrepel.
#' Each point represents a sample, colored by group, with hover labels.
#'
#' @param logcounts Numeric matrix of log-CPM values (genes × samples), e.g., from edgeR::cpm.
#' @param Sample Character vector of sample names corresponding to the columns of "logcounts".
#' @param Group Factor or character vector denoting group/condition for each sample.
#' @param title Character. Title for the PCA plot.
#' @param palette Character. Name of a discrete color palette from the "paletteer" package.
#' @param maxOverlaps Integer. Maximum number of overlapping labels allowed by "ggrepel".
#' @param sizeLabel Numeric. Font size for sample labels.
#' @param center Logical. If TRUE, center variables before PCA.
#' @param scale Logical. If TRUE, scale variables to unit variance before PCA.
#'
#' @return A "ggplot" object displaying the PCA scatter plot of PC1 vs PC2.
#'
#' @details
#' 1. Transposes the "logcounts" matrix so samples are rows.
#' 2. Runs PCA via "stats::prcomp()" with centering and scaling options.
#' 3. Calculates percent variance explained by PC1 and PC2.
#' 4. Builds a scatter plot with black‐bordered points and non‐overlapping labels.
#'
# pcainfo(logcounts = y$yPreLogcounts, center = TRUE, scale = FALSE)

pcaPlot <- function(logcounts, Sample, Group, title, palette, maxOverlaps, sizeLabel, center, scale){

  # 'logcounts' is the matrix from edgeR::cpm
  # Transpose the logcount matrix so that rows represent samples and columns represent genes.
  logcount_t <- t(logcounts)

  # Perform Principal Component Analysis (PCA) on the transposed matrix.
  # 'center = TRUE' centers the data by subtracting the mean of each gene.
  # 'scale. = FALSE' does not scale the variance of each gene.
  pca <- stats::prcomp(logcount_t, center = as.logical(center), scale. = as.logical(scale))

  # Extract the standard deviation of each principal component.
  sdev <- pca$sdev

  # Extract the coordinates for the first two principal components (PC1 and PC2).
  dim1 <- pca$x[,1]  # Principal Component 1
  dim2 <- pca$x[,2]  # Principal Component 2

  # Calculate the percentage of variance explained by the first two principal components.
  pcaVarPer <- round(sdev^2 / sum(sdev^2) * 100, 1)

  # Create a tibble to store the PCA dimensions along with sample and group information.
  data <- tibble::tibble(dim1 = dim1, dim2 = dim2, Sample = Sample, Group = Group)

  # Generate the PCA plot using ggplot2.
  ggplot2::ggplot(data, mapping = ggplot2::aes(
    x = dim1,   # X-axis represents Principal Component 1
    y = dim2,   # Y-axis represents Principal Component 2
    label = Sample,  # Labels are the sample names
    color = Group    # Color points by the group
  )) +

    # Add points representing samples, with black borders.
    ggplot2::geom_point(color = "black") +

    # Ensure that labels and points do not get clipped outside plot bounds.
    ggplot2::coord_cartesian(clip = "off") +

    # Use ggrepel to prevent label overlap and to adjust label appearance.
    ggrepel::geom_text_repel(
      fontface = "bold",          # Bold font for sample labels
      max.overlaps = as.numeric(maxOverlaps),  # Maximum allowed overlaps for labels
      size = as.numeric(sizeLabel),  # Size of the sample labels
      xlim = c(-Inf, Inf),  # No limits for the X-axis
      ylim = c(-Inf, Inf)   # No limits for the Y-axis
    ) +

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
}
