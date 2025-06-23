#' CorrPlotHeatmap
#'
#' Plot a correlation heatmap of top variable genes across samples.
#'
#' This function selects the highest-variance genes from a log-CPM matrix, computes
#' pairwise correlation coefficients (or p-values) with "Hmisc::rcorr()", and renders
#' a heatmap via "pheatmap", with options for clustering, scaling, and number display.
#'
#' @param x Numeric matrix of log-CPM values (genes × samples), e.g., from "edgeR::cpm()".
#' @param scale Character. Scaling mode for the heatmap: "row", "column", or "none".
#' @param Color Character. Name of a continuous palette from the "paletteer" package.
#' @param type Character. Correlation method passed to "Hmisc::rcorr()": "pearson", "spearman", or "kendall".
#' @param display Character. Which matrix to display: "correlation" (coefficients) or "pvalue".
#' @param round_number Integer. Number of decimal places to round displayed numbers.
#' @param cutree_rows Integer. Number of clusters to cut for row dendrogram.
#' @param cutree_cols Integer. Number of clusters to cut for column dendrogram.
#' @param cluster Character. Clustering mode: one of "both", "row", "column", or "none".
#' @param show_names Character. One of "both", "row", "column", or "none" to display row/column labels.
#' @param NumGenes Integer. Number of top-variance genes to include in the correlation.
#'
#' @return A "pheatmap" object representing the correlation heatmap with clustering.
#'
#' @details
#' 1. Compute per-gene variance and select the top "NumGenes".
#' 2. Subset the matrix and compute correlations (and p-values) via "Hmisc::rcorr()".
#' 3. Choose to display correlation coefficients or p-values, rounded to "round_number".
#' 4. Determine clustering and label visibility from cluster and "show_names".
#' 5. Render the heatmap with "pheatmap::pheatmap()", passing in custom distance, color,
#'    clustering, and "display" number settings, saving to a temporary file to suppress autosave.
#'
CorrPlotHeatmap <- function(x, scale, Color, type, display, round_number, cutree_rows, cutree_cols, cluster, show_names, NumGenes){

  # 'x' is the logcounts matrix from edgeR::cpm
  # Generate a color palette for the heatmap using the specified Color
  col <- paletteer::paletteer_c(Color, n = 50)

  # Calculate the variance for each gene across all samples
  var_genes <- apply(x, 1, stats::var)

  # Select the top NumGenes with the highest variance for further analysis
  select_var <- names(sort(var_genes, decreasing = TRUE))[1:NumGenes]
  highly_variable_lcpm <- x[select_var,]  # Subset the expression matrix to include only highly variable genes

  # Compute the correlation coefficients and p-values for the selected genes
  rcor <- Hmisc::rcorr(highly_variable_lcpm, type = type)

  # Determine which values to display based on the 'display' parameter
  if (display == "correlation") {
    display_numbers <- rcor$r  # Use correlation coefficients for display
  } else if (display == "pvalue") {
    display_numbers <- rcor$P  # Use p-values for display
  }

  # Set clustering options based on the 'cluster' parameter
  if (cluster == "both") {
    cluster_rows <- TRUE
    cluster_cols <- TRUE
  } else if (cluster == "row") {
    cluster_rows <- TRUE
    cluster_cols <- FALSE
  } else if (cluster == "column") {
    cluster_rows <- FALSE
    cluster_cols <- TRUE
  } else if (cluster == "none") {
    cluster_rows <- FALSE
    cluster_cols <- FALSE
  }

  # Determine which names to show in the heatmap based on the 'show_names' parameter
  if (show_names == "both") {
    show_rownames <- TRUE
    show_colnames <- TRUE
  } else if (show_names == "row") {
    show_rownames <- TRUE
    show_colnames <- FALSE
  } else if (show_names == "column") {
    show_rownames <- FALSE
    show_colnames <- TRUE
  } else if (show_names == "none") {
    show_rownames <- FALSE
    show_colnames <- FALSE
  }

  # Calculate the distance matrix for samples based on the correlation coefficients
  sampleDists <- stats::as.dist(1 - rcor$r)

  # create a tempfile to avoid autosave of pheatmap
  RemoveAutosave <- paste0(tempfile(),".png")

  # Create a heatmap using the pheatmap function, specifying various parameters
  map <- pheatmap::pheatmap(
    rcor$r,                                  # Matrix of correlation coefficients
    color = col,                             # Color palette for the heatmap
    cutree_rows = cutree_rows,               # Number of clusters to cut for rows
    cutree_cols = cutree_cols,               # Number of clusters to cut for columns
    cluster_rows = cluster_rows,             # Whether to cluster rows
    cluster_cols = cluster_cols,             # Whether to cluster columns
    show_rownames = show_rownames,           # Whether to show row names
    show_colnames = show_colnames,           # Whether to show column names
    clustering_distance_rows = sampleDists,  # Distance measure for clustering rows
    clustering_distance_cols = sampleDists,  # Distance measure for clustering columns
    display_numbers = round(display_numbers, round_number),  # Round and display numbers
    fontsize_number = 5,                     # Font size for displayed numbers
    scale = scale,                           # Scaling method for the heatmap
    main = "Correlation heatmap",            # Main title of the heatmap
    angle_col = 45,                          # Angle for column names
    fontsize = 8,                            # Font size for axis labels
    filename = RemoveAutosave                # file path where to save the picture
  )

  unlink(RemoveAutosave)

  return(map)

}
