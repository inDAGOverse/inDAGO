#' HeatmapExp
#'
#' Plot a heatmap of the top variable genes across samples.
#'
#' This function selects the highest-variance genes from a log-CPM matrix,
#' transposes the data, and renders a heatmap with customizable clustering,
#' scaling, and color palettes using pheatmap.
#'
#' @param x Numeric matrix of log-CPM values (genes × samples), e.g., from edgeR::cpm().
#' @param ColorPanel Character. Name of a continuous palette from the paletteer package.
#' @param scale Character. Scaling mode for heatmap: "row", "column", or "none".
#' @param cutree_rows Integer. Number of clusters for rows (genes).
#' @param cutree_cols Integer. Number of clusters for columns (samples).
#' @param cluster Character. One of "both", "row", "column", or "none" to specify clustering.
#' @param show_names Character. One of "both", "row", "column", or "none" to show row/col names.
#' @param NumGenes Integer. Number of top-variance genes to include in the heatmap.
#'
#' @return A "pheatmap" object containing the heatmap and clustering information.
#'
#' @details
#' 1. Compute per-gene variance and select the top "NumGenes".
#' 2. Transpose the subsetted matrix so samples are rows.
#' 3. Apply the specified color palette (n = 50) via paletteer::paletteer_c().
#' 4. Determine clustering and name-display options from "cluster" and "show_names".
#' 5. Render the heatmap with "pheatmap::pheatmap()", saving to a temporary file to suppress autosave.
#'
HeatmapExp <- function(x, ColorPanel, scale, cutree_rows, cutree_cols, cluster, show_names, NumGenes){

  # 'x' is the logcounts matrix from edgeR::cpm
  # Calculate the variance for each gene across samples
  var_genes <- apply(x, 1, stats::var)

  # Select the top NumGenes with the highest variance
  select_var <- names(sort(var_genes, decreasing = TRUE))[1:NumGenes]

  # Transpose the selected highly variable gene expression matrix
  highly_variable_lcpm_t <- magrittr::`%>%`(x[select_var,], t)

  # Set the color palette for the heatmap using the paletteer package
  col <- paletteer::paletteer_c(ColorPanel, n = 50)

  # Determine the clustering options for rows and columns
  if (cluster == "both") {
    cluster_rows <- TRUE   # Cluster both rows (genes) and columns (samples)
    cluster_cols <- TRUE
  } else if (cluster == "row") {
    cluster_rows <- TRUE   # Cluster only rows (genes)
    cluster_cols <- FALSE
  } else if (cluster == "column") {
    cluster_rows <- FALSE  # Cluster only columns (samples)
    cluster_cols <- TRUE
  } else if (cluster == "none") {
    cluster_rows <- FALSE  # Do not cluster either rows or columns
    cluster_cols <- FALSE
  }

  # Determine whether to display row names (genes) and column names (samples)
  if (show_names == "both") {
    show_rownames <- TRUE   # Show both row names and column names
    show_colnames <- TRUE
  } else if (show_names == "row") {
    show_rownames <- TRUE   # Show only row names (genes)
    show_colnames <- FALSE
  } else if (show_names == "column") {
    show_rownames <- FALSE  # Show only column names (samples)
    show_colnames <- TRUE
  } else if (show_names == "none") {
    show_rownames <- FALSE  # Show neither row names nor column names
    show_colnames <- FALSE
  }

  # create a tempfile to avoid autosave of pheatmap
  RemoveAutosave <- paste0(tempfile(),".png")

  # Create the heatmap with specified parameters using the pheatmap package
  plot <- pheatmap::pheatmap(
    mat = highly_variable_lcpm_t,   # Transposed matrix of highly variable genes
    scale = scale,                  # Scale option: "row", "column", or "none"
    cutree_rows = cutree_rows,      # Number of clusters to create in rows
    cutree_cols = cutree_cols,      # Number of clusters to create in columns
    cluster_rows = cluster_rows,    # Whether to cluster rows (genes)
    cluster_cols = cluster_cols,    # Whether to cluster columns (samples)
    show_rownames = show_rownames,  # Whether to display row names (genes)
    show_colnames = show_colnames,  # Whether to display column names (samples)
    fontsize_number = 5,            # Font size for the numbers in the heatmap
    main = "Expression heatmap",    # Title of the heatmap
    color = col,                    # Color palette for the heatmap
    angle_col = 45,                 # Angle of the column names (samples) on the X-axis
    fontsize = 8,                   # Font size for text in the heatmap
    filename = RemoveAutosave       # file path where to save the picture
  )
  unlink(RemoveAutosave)

  return(plot)
}
