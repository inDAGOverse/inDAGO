#' HeatmapExpPlotly
#'
#' Create an interactive heatmap of top variable genes using Heatmaply.
#'
#' This function selects the highest-variance genes from a log-CPM matrix, transposes
#' the data, and renders an interactive heatmap via "heatmaply", using "pheatmap" call.
#'
#' @param x Numeric matrix of log-CPM values (genes × samples), e.g., from edgeR::cpm().
#' @param ColorPanel Character. Name of a continuous palette from the paletteer package.
#' @param scale Character. Scaling mode: "row", "column", or "none".
#' @param cluster Character or logical. Clustering option for dendrogram: "both", "row", "column", or "none".
#' @param show_names Character. One of "both", "row", "column", or "none" to display row/column labels.
#' @param NumGenes Integer. Number of top-variance genes to include in the heatmap.
#'
#' @return A Plotly object (heatmaply) representing the interactive heatmap.
#'
#' @details
#' 1. Compute per-gene variance and select the top NumGenes.
#' 2. Transpose the subsetted matrix so samples are rows.
#' 3. Generate a temporary static heatmap with pheatmap to extract dendrograms.
#' 4. Render an interactive heatmap with heatmaply::heatmaply().
#'
HeatmapExpPlotly <- function(x, ColorPanel, scale, cluster, show_names, NumGenes){


  # 'x' is the logcounts matrix from edgeR::cpm
  # Calculate the variance for each gene across samples
  var_genes <- apply(x, 1, stats::var)

  # Select the top NumGenes with the highest variance
  select_var <- names(sort(var_genes, decreasing = TRUE))[1:NumGenes]

  # Transpose the selected highly variable gene expression matrix
  highly_variable_lcpm_t <- magrittr::`%>%`(x[select_var,], t)

  # Set the color palette for the heatmap using the paletteer package
  col <- paletteer::paletteer_c(ColorPanel, n = 50)


  # Determine whether to show row and column names based on the 'show_names' parameter
  if (show_names == "both") {
    showticklabels <- c(TRUE, TRUE)    # Show both row and column labels
  } else if (show_names == "row") {
    showticklabels <- c(FALSE, TRUE)   # Show only column labels
  } else if (show_names == "column") {
    showticklabels <- c(TRUE, FALSE)   # Show only row labels
  } else if (show_names == "none") {
    showticklabels <- c(FALSE, FALSE)  # Show neither row nor column labels
  }

  # create a tempfile to avoid autosave of pheatmap
  RemoveAutosave <- paste0(tempfile(),".png")

  # Create the heatmap with specified parameters using the pheatmap package
  phtmap <- pheatmap::pheatmap(
    mat = highly_variable_lcpm_t,   # Transposed matrix of highly variable genes
    scale = scale,                  # Scale option: "row", "column", or "none"
    filename = RemoveAutosave       # file path where to save the picture
  )

  unlink(RemoveAutosave)

  # Create an interactive heatmap using heatmaply with specified parameters
  plotly <- heatmaply::heatmaply(
    x = highly_variable_lcpm_t,        # Matrix of highly variable genes (transposed)
    dendrogram = cluster,              # Clustering method for rows and/or columns
    scale = scale,                     # Scaling method: "none", "row", "column"
    Rowv = phtmap[[1]],                # Row dendrogram (optional)
    Colv = phtmap[[2]],                # Column dendrogram (optional)
    revC = TRUE,                       # Reverse the column order if TRUE
    colors = col,                      # Color palette for the heatmap
    showticklabels = showticklabels    # Control visibility of tick labels
  )

  return(plotly)
}
