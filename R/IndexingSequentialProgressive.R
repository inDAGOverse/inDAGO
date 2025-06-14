#' Indexing sequential progressive
#'
#' @param outfolder1 first output folder
#' @param outfolder2 second output folder
#' @param refgen1 first reference genome
#' @param refgen2 second reference genome
#' @param gappedIndex gapped structure
#' @param indexSplit split structure
#' @param memory handling memory
#' @param TH_subread threshold memory usage
## indexing genomes one after the other
IndexingSequentialProgressive <- function(outfolder1, outfolder2, refgen1, refgen2,
                                          gappedIndex, indexSplit,
                                          memory, TH_subread) {
  Rsubread::buildindex(basename = outfolder1,
                       reference = refgen1,
                       gappedIndex = gappedIndex,
                       indexSplit = indexSplit,
                       memory = memory,
                       TH_subread = TH_subread
  )
  Rsubread::buildindex(basename = outfolder2,
                       reference = refgen2,
                       gappedIndex = gappedIndex,
                       indexSplit = indexSplit,
                       memory = memory,
                       TH_subread = TH_subread
  )
} # close IndexingSequentialProgressive
