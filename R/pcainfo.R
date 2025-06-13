#' pcainfo
#'
#' Perform Principal Component Analysis (PCA) on log-expression data.
#'
#' This function transposes a log-count matrix (samples as columns, genes as rows)
#' and runs PCA using  "stats::prcomp() ", with options to center and scale variables.
#'
#' @param logcounts Numeric matrix. Log-CPM values (genes × samples), e.g., from edgeR::cpm..
#' @param center Logical. If TRUE, center variables by subtracting the mean (default: TRUE).
#' @param scale Logical. If TRUE, scale variables to unit variance (default: FALSE).
#'
#' @return An object of class  "prcomp " containing the PCA results, including loadings,
#'   scores, and explained variance.
#'
pcainfo <- function(logcounts, center, scale){

  # 'logcounts' is the matrix from edgeR::cpm
  # Transpose the logcount matrix so that rows represent samples and columns represent genes.
  logcount_t <- t(logcounts)

  # Perform Principal Component Analysis (PCA) on the transposed matrix
  # - 'center = TRUE' centers the data by subtracting the mean of each variable (gene)
  # - 'scale. = FALSE' does not scale the variance of each variable (no standardization)
  pca <- stats::prcomp(logcount_t, center = as.logical(center), scale. = as.logical(scale))

  # Return the PCA result
  return(pca)
}
