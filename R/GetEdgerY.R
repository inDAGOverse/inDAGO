#' GetEdgerY
#'
#' Calculate and return filtered DGEList object and log-CPM matrices using edgeR and optional HTSFilter
#'
#' @param gr Data frame with sample metadata, including sample names and group labels
#' @param WDpn Directory containing count files (*.tab)
#' @param colIDgene Column index of gene IDs in count files
#' @param colCounts Column index of counts in count files
#' @param skip_preN Number of header lines to skip in count files
#' @param filterMethod Either "filterByExpr" or "HTSFilter"
#' @param min_count Minimum count per gene (filterByExpr)
#' @param min_total_count Minimum total count per gene (filterByExpr)
#' @param large_n Number of samples per group to consider as "large" (filterByExpr)
#' @param min_prop Minimum proportion of samples with expression (filterByExpr)
#' @param normMethod Normalization method (e.g., "TMM", "RLE")
#'
#' @return A list with total/kept gene counts, filtered DGEList objects, and log-CPM matrices
GetEdgerY <- function(gr, WDpn, colIDgene, colCounts, skip_preN, filterMethod, min_count, min_total_count, large_n, min_prop, normMethod){

  # List all files in the specified working directory (WDpn)
  lpath <- list.files(file.path(WDpn), pattern =  "*.tab")
  l <- magrittr::`%>%`(list.files(file.path(WDpn), pattern = "*.tab"), tools::file_path_sans_ext(.))

  # Extract group information for each sample by matching file names from the group metadata (gr)
  Groups <- magrittr::`%>%`(gr[match(l, gr$Samples),],.$Groups)
  # Read in the DGE (differential gene expression) data using the edgeR package
  # - 'files' is the list of files
  # - 'labels' is the list of labels associated to the files
  # - 'columns' specifies the gene ID and count columns
  # - 'path' is the directory path where files are located
  # - 'group' assigns the group information (e.g., treatment or control) for each sample
  # - 'skip' is the number of lines to skip in each file (header rows)
  y <- edgeR::readDGE(files = lpath,  labels = l, columns = c(colIDgene, colCounts), path = WDpn, group = Groups, skip = skip_preN, header = FALSE)

  # change files names
  y$samples$files <- row.names(y$samples)


  # Filter lowly expressed genes based on specific thresholds using filterByExpr or HTSFilter functions:
  # - 'min.count' is the minimum count per gene to keep
  # - 'min.total.count' is the minimum total count across all samples
  # - 'large.n' number of samples per group that is considered to be “large”
  # - 'min.prop' in large sample situations, the minimum proportion of samples in a group that a gene needs to be expressed in

  # Using filterByExpr function
  if (filterMethod == "filterByExpr") {

    # Using filterByExpr function
    keep <- edgeR::filterByExpr(y, group = Groups, min.count = min_count, min.total.count = min_total_count, large.n = large_n, min.prop = min_prop)

    # Subset the DGE list to only keep the filtered genes
    # The option keep.lib.sizes=FALSE causes the library sizes to be recomputed after the filtering.
    ypre <- y[keep, , keep.lib.sizes=FALSE]

    # Normalize library sizes based on the provided normMethod (e.g., TMM, RLE)
    ypost <- edgeR::normLibSizes(ypre, method = normMethod)

    # Generate logcounts (log-CPM) for pre-normalization and post-normalization
    yPreLogcounts <- edgeR::cpm(ypre, log = TRUE)
    yPostLogcounts <- edgeR::cpm(ypost, log = TRUE)

    # check number of kept genes
    totalGenes <- format(dim(y)[[1]],big.mark=",",scientific=FALSE)
    keptGenesPre <-  format(dim(ypre)[[1]],big.mark=",",scientific=FALSE)
    keptGenesPost <- keptGenesPre[[1]]


    # Using HTSFilter function
  } else if (filterMethod == "HTSFilter") {

    if (normMethod == "RLE") {
      normMethodHTSFilter <- "DESeq"
    }else{
      normMethodHTSFilter <- normMethod
    }

    ## Without normalization


    # Filter by HTSFilter without normalization
    ypre <- HTSFilter::HTSFilter(y, conds = Groups, normalization = "none")$filteredData

    ## With normalization

    # Normalize data
    ypostNorm <- edgeR::normLibSizes(y, method = normMethod)


    # Filter by HTSFilter with normalization
    ypost <- HTSFilter::HTSFilter(ypostNorm, conds = Groups, normalization = normMethodHTSFilter, s.min = 1, s.max = 200, s.len = 100)$filteredData

    # Generate logcounts (log-CPM) for pre-normalization and post-normalization
    yPreLogcounts <- edgeR::cpm(ypre, log = TRUE)
    yPostLogcounts <- edgeR::cpm(ypost, log = TRUE)

    # check number of kept genes
    totalGenes <- format(dim(y)[[1]],big.mark=",",scientific=FALSE)
    keptGenesPre <- format(dim(ypre)[[1]],big.mark=",",scientific=FALSE)
    keptGenesPost <- format(dim(ypost)[[1]],big.mark=",",scientific=FALSE)
  }

  # Return a list containing the filtered DGE lists and log-transformed counts
  return(list(totalGenes = totalGenes, keptGenesPre = keptGenesPre, keptGenesPost= keptGenesPost, ypre = ypre, ypost = ypost, yPreLogcounts = yPreLogcounts, yPostLogcounts = yPostLogcounts))
}
