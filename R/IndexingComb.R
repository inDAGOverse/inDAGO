#' Combined indexing
#'
#' @param basename output basename
#' @param reference reference genome
#' @param gappedIndex gapped structure
#' @param indexSplit split structure
#' @param memory handling memory
#' @param TH_subread threshold memory usage
#' @param gen1 first reference genome
#' @param gen2 second reference genome
#' @param outfolder output folder
#' @param tempfolder temporary folder
#' @param tag1 first genome label
#' @param tag2 second genome label
IndexingComb <- function (basename, reference, gappedIndex,
                          indexSplit, memory, TH_subread,
                          gen1, gen2, outfolder,
                          tempfolder = file.path(fs::path_temp(),"TempDirSum_3738"),
                          tag1, tag2
)
{
    ## importing, renaming, combininb genomes
    Genome1 <- seqinr::read.fasta(gen1, seqtype = "DNA", forceDNAtolower = FALSE,
                                  whole.header = TRUE)
    names(Genome1) <- paste(tag1, names(Genome1), sep = "_")
    Genome2 <- seqinr::read.fasta(gen2, seqtype = "DNA", forceDNAtolower = FALSE,
                                  whole.header = TRUE)
    names(Genome2) <- paste(tag2, names(Genome2), sep = "_")
    Combined <- c(Genome1, Genome2)
    seqinr::write.fasta(sequences = Combined, file.out = file.path(tempfolder, "CombinedGenome.fasta"),
                        names = names(Combined))
    ## index building
    Rsubread::buildindex(
        basename = basename,
        reference = reference,
        gappedIndex = gappedIndex,
        indexSplit = indexSplit,
        memory = memory,
        TH_subread = TH_subread)
}