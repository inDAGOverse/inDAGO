#' CorrPlotHeatmaply
#'
#' Create an interactive correlation heatmap of top variable genes using Heatmaply.
#'
#' This function selects the highest-variance genes from a log-CPM matrix, computes
#' pairwise correlation coefficients (and p-values) with "Hmisc::rcorr()", and renders
#' an interactive correlation heatmap via "heatmaply::heatmaply_cor()", using clustering
#' and scaling options derived from "pheatmap" call.
#'
#' @param x Numeric matrix of log-CPM values (genes × samples), e.g., from "edgeR::cpm()".
#' @param Color Character. Name of a continuous palette from the "paletteer" package.
#' @param type Character. Correlation method passed to "Hmisc::rcorr()": "pearson", "spearman", or "kendall".
#' @param cluster Character or logical. Clustering option for dendrogram: "both", "row", "column", or "none".
#' @param scale Character. Scaling mode for the heatmap: "row", "column", or "none".
#' @param show_names Character. One of "both", "row", "column", or "none" to display row/column labels.
#' @param NumGenes Integer. Number of top-variance genes to include in the correlation.
#'
#' @return A Plotly object (heatmaply) representing the interactive correlation heatmap.
#'
#' @details
#' 1. Compute per-gene variance and select the top "NumGenes".
#' 2. Subset the matrix and compute correlations (and p-values) via "Hmisc::rcorr()".
#' 3. Generate a temporary static heatmap with "pheatmap" to extract dendrograms.
#' 4. Render an interactive heatmap with "heatmaply::heatmaply_cor()", passing in color,
#'    clustering, scaling, tick-label visibility, and point size based on -log10(p-value).
#'
CorrPlotHeatmaply <- function(x, Color, type, cluster, scale, show_names, NumGenes){

  # 'x' is the logcounts matrix from edgeR::cpm
  # Generate a color palette for the heatmap using the specified Color
  col <- paletteer::paletteer_c(Color, n = 50)

  # Calculate the variance for each gene across all samples
  var_genes <- apply(x, 1, stats::var)

  # Select the top NumGenes with the highest variance for further analysis
  select_var <- names(sort(var_genes, decreasing = TRUE))[1:NumGenes]

  # Subset the expression matrix to include only highly variable genes
  highly_variable_lcpm <- x[select_var,]


  # Compute the correlation coefficients and p-values for the selected genes
  rcor <-  Hmisc::rcorr(highly_variable_lcpm, type = type)
  p <- rcor$P  # Extract p-values for further analysis



  # Determine which tick labels to show based on the 'show_names' parameter
  if (show_names == "both") {
    showticklabels <- c(TRUE, TRUE)  # Show both row and column names
  } else if (show_names == "row") {
    showticklabels <- c(FALSE, TRUE)  # Show only column names
  } else if (show_names == "column") {
    showticklabels <- c(TRUE, FALSE)  # Show only row names
  } else if (show_names == "none") {
    showticklabels <- c(FALSE, FALSE)  # Show neither row nor column names
  }



  # Calculate the distance matrix for samples based on the correlation coefficients
  sampleDists <- stats::as.dist(1 - rcor$r)

  # create a tempfile to avoid autosave of pheatmap
  RemoveAutosave <- paste0(tempfile(),".png")

  # Create a heatmap using the pheatmap function, specifying various parameters
  CorrPlotPh <- pheatmap::pheatmap(
    rcor$r,                                  # Matrix of correlation coefficients
    scale = scale,                           # Scaling method for the heatmap
    clustering_distance_rows = sampleDists,  # Distance measure for clustering rows
    clustering_distance_cols = sampleDists,  # Distance measure for clustering columns
    filename = RemoveAutosave                # file path where to save the picture
  )

  unlink(RemoveAutosave)


  # Create a heatmap using the heatmaply function, specifying various parameters
  maply <- heatmaply::heatmaply_cor(
    rcor$r,                             # Matrix of correlation coefficients
    colors = col,                       # Color palette for the heatmap
    dendrogram = cluster,               # Type of dendrogram to display (e.g., "none", "row", "column", "both")
    scale = scale,                      # Scaling method for the heatmap
    Rowv = CorrPlotPh[[1]],             # Dendrogram for rows
    Colv = CorrPlotPh[[2]],             # Dendrogram for columns
    revC = TRUE,                        # Reverse the order of the columns
    showticklabels = showticklabels,    # Whether to show tick labels
    node_type = "scatter",              # Type of plot for points (scatter plot)

    # Point size based on the negative logarithm of p-values for better visualization
    point_size_mat = -log10(p),
    point_size_name = "-log10(p-value)", # Legend name for point size
    label_names = c("y", "x", "Correlation") # Custom labels for axes
  )

  return(maply)
}
