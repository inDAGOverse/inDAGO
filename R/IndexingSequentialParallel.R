#' Indexing sequential parallel
#'
#' @param basename output basename
#' @param reference reference genome
#' @param gappedIndex gapped structure
#' @param indexSplit split structure
#' @param memory handling memory
#' @param TH_subread threshold memory usage
IndexingSequentialParallel <- function (basename, reference,
                                        gappedIndex, indexSplit,
                                        memory, TH_subread) {
  Rsubread::buildindex(
    basename = basename,
    reference = reference,
    gappedIndex = gappedIndex,
    indexSplit = indexSplit,
    memory = memory,
    TH_subread = TH_subread
  )
} # close IndexingSequentialParallel function
