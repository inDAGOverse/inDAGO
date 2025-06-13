#' Filtering
#'
#' Filter paired-end FASTQ files in parallel based on quality and adapter trimming criteria.
#'
#' This function processes raw paired-end FASTQ files to remove low-quality bases, trim adapters,
#' and filter out short reads. It supports quality-based end trimming, sliding window trimming,
#' and adapter removal. The processing is done in parallel across multiple nodes to enhance performance
#' when working with large datasets.
#'
#' @param Nodes Integer. Number of parallel processing nodes (e.g., CPU cores).
#' @param X List of character vectors. Each element is a character vector of paired file names (e.g., c("sample_1.fq", "sample_2.fq")).
#' @param UploadPath Character. Path to directory containing raw FASTQ files.
#' @param DownloadPath Character. Path to directory where filtered files will be saved.
#' @param qualityType Character. Type of quality score encoding, e.g., "Sanger" or "Illumina".
#' @param minLen Integer. Minimum length of reads to retain after filtering.
#' @param trim Logical. Whether to perform quality-based trimming of reads.
#' @param trimValue Integer. Minimum Phred score threshold for trimming.
#' @param n Integer. Number of reads to stream per chunk (default typically set to 1e6).
#' @param Adapters Logical. Whether to remove adapters from reads.
#' @param Lpattern Character. Adapter sequence to remove from the 5' end (left).
#' @param Rpattern Character. Adapter sequence to remove from the 3' end (right).
#' @param max.Lmismatch Integer. Maximum mismatches allowed for the left adapter.
#' @param max.Rmismatch Integer. Maximum mismatches allowed for the right adapter.
#' @param kW Integer. Minimum number of low-quality scores in a window to trigger trimming (sliding window analysis).
#' @param left Logical. Whether to allow trimming from the left end.
#' @param right Logical. Whether to allow trimming from the right end.
#' @param halfwidthAnalysis Logical. Whether to perform sliding window-based trimming.
#' @param halfwidth Integer. Half-width of the sliding window.
#' @param compress Logical. Whether to compress the output FASTQ files.
#'
#' @details
#' - Paired FASTQ files must be named consistently, distinguished by "_1" and "_2" for forward and reverse reads.
#' - This function uses the "ShortRead" and "Biostrings" packages for FASTQ processing and quality filtering.
#' - Filtered files are written in the format "Filtered_<original>_1.fq(.gz)" and "Filtered_<original>_2.fq(.gz)".
#' - Log files containing read counts before and after filtering are written per sample.
#'
#' @return Filtered FASTQ files written to "DownloadPath"; one log file per sample.
#'
Filtering <- function(Nodes,
                      X,
                      UploadPath,
                      DownloadPath,
                      qualityType,
                      minLen,
                      trim,
                      trimValue,
                      n,
                      Adapters,
                      Lpattern,
                      Rpattern,
                      max.Lmismatch,
                      max.Rmismatch,
                      kW,
                      left,
                      right,
                      halfwidthAnalysis,
                      halfwidth,
                      compress)
{
  parallel::parLapply(
    cl <- parallel::makeCluster(Nodes, type = "SOCK"),
    # Creates a set of copies of R running in parallel via sockets
    X,
    # Object obtained by ListFIlter()
    fun =  function(R,
                    #Object obtained by list component X
                    UploadPath,
                    DownloadPath,
                    qualityType,
                    minLen,
                    trim,
                    trimValue,
                    n,
                    Adapters,
                    Lpattern,
                    Rpattern,
                    max.Lmismatch,
                    max.Rmismatch,
                    kW,
                    left,
                    right,
                    halfwidthAnalysis,
                    halfwidth,
                    compress) {
      Read1Up <- paste0(UploadPath, R[[1]]) #Upload sample file R1
      Read2Up <- paste0(UploadPath, R[[2]]) #Upload sample file R2
      output <-
        paste0(sub(
          "_[1-2].fastq|_[1-2].fq|_[1-2].fastq.gz|_[1-2].fq.gz",
          "",
          R[[1]]
        ))
      #Draws successive subsets from a fq file
      Fr <- ShortRead::FastqStreamer(Read1Up, n = n) #Draws successive subsets from a fastq file R1
      Rv <- ShortRead::FastqStreamer(Read2Up, n = n) #Draws successive subsets from a fastq file R2
      while (base::length(FrY <-
                    suppressWarnings(ShortRead::yield(Fr, qualityType = qualityType))) &
             base::length(RvY <-
                    suppressWarnings(ShortRead::yield(Rv, qualityType = qualityType)))) {

        FrYStarAnalysed <- base::length(FrY)
        RvYStarAnalysed <- base::length(RvY)

        if (Adapters == "TRUE") {

          # Remove adapters
          FrY <-
            ShortRead::trimLRPatterns(
              Lpattern = Lpattern,
              Rpattern = Rpattern,
              subject = FrY,
              max.Lmismatch = max.Lmismatch,
              max.Rmismatch = max.Rmismatch,
              with.Lindels = FALSE,
              with.Rindels = FALSE,
              Lfixed = TRUE,
              Rfixed = TRUE,
              ranges = FALSE
            )
          RvY <-
            ShortRead::trimLRPatterns(
              Lpattern = Lpattern,
              Rpattern = Rpattern,
              subject = RvY,
              max.Lmismatch = max.Lmismatch,
              max.Rmismatch = max.Rmismatch,
              with.Lindels = FALSE,
              with.Rindels = FALSE,
              Lfixed = TRUE,
              Rfixed = TRUE,
              ranges = FALSE
            )

        }

        # Identify Phred quality score corresponding to trimLength value
        FrEnc <- Biostrings::encoding(Biostrings::quality(FrY))
        RvEnc <- Biostrings::encoding(Biostrings::quality(RvY))
        FrW <- which(FrEnc == trimValue - 1)
        RvW <- which(RvEnc == trimValue - 1)

        # Trim ends of reads based on qualities <= (FrW) & (RvW)
        # The trimEnds functions remove nucleotides from the beginning or ending of reads only, not from the middle.

        if (trim == "TRUE") {


          if (base::length(FrY) > 0) {
            FrYrangeTE <-
              ShortRead::trimEnds(
                FrY,
                a = names(FrW),
                # The letter at or below which a nucleotide is marked as failing
                ranges = TRUE,
                left = left,
                right = right,
                alphabet = FrEnc
              )
            FrY <- # Ranging forward string between start and end
              ShortRead::narrow(
                x = FrY,
                start = BiocGenerics::start(FrYrangeTE),
                end = BiocGenerics::end(FrYrangeTE)
              )
          }
          if (base::length(RvY) > 0) {
            RvYrangeTE <-
              ShortRead::trimEnds(
                RvY,
                a = names(RvW),
                ranges = TRUE,
                left = left,
                right = right,
                alphabet = FrEnc
              )
            RvY <- # Ranging reverse string between start and end
              ShortRead::narrow(
                x = RvY,
                start = BiocGenerics::start(RvYrangeTE),
                end = BiocGenerics::end(RvYrangeTE)
              )
          }
          keep <- (ShortRead::width(FrY) > 0 &
                     ShortRead::width(RvY) > 0)
          FrY <- FrY[keep]
          RvY <- RvY[keep]


        }

        # Remove low-quality reads from the right end using a sliding window
        # trimTailw starts at the left-most nucleotide, tabulating the number of cycles in a window of 2 *
        # halfwidth + 1 surrounding the current nucleotide with quality scores that fall at or below a. The
        # read is trimmed at the first nucleotide for which this number >= k. The quality of the first or last
        # nucleotide is used to represent portions of the window that extend beyond the sequence.


        if (halfwidthAnalysis == "TRUE") {

          if (base::length(FrY) > 0) {
            FrYrangeTW <-
              ShortRead::trimTailw(
                FrY,
                k = kW,
                a = names(FrW),
                halfwidth = halfwidth,
                ranges = TRUE
              )
            FrY <-
              ShortRead::narrow(
                x = FrY,
                start = BiocGenerics::start(FrYrangeTW),
                end = BiocGenerics::end(FrYrangeTW)
              )
          }

          if (base::length(RvY) > 0) {
            RvYrangeTW <-
              ShortRead::trimTailw(
                RvY,
                k = kW,
                a = names(RvW),
                halfwidth = halfwidth,
                ranges = TRUE
              )
            RvY <-
              ShortRead::narrow(
                x = RvY,
                start = BiocGenerics::start(RvYrangeTW),
                end = BiocGenerics::end(RvYrangeTW)
              )
          }
        }


        # Filter for min base::length
        keep <- ShortRead::width(FrY) >= minLen &
          ShortRead::width(RvY) >= minLen
        FrY <- FrY[keep]
        RvY <- RvY[keep]


        if (compress == "TRUE"){

          RecordSample <- c(paste0("Filtered_", output,"_1.fq.gz"),paste0("Filtered_", output,"_2.fq.gz"))

        }else{

          RecordSample <- c(paste0("Filtered_", output,"_1.fq"),paste0("Filtered_", output,"_2.fq"))
        }


        # #check reads left
        ReadLengthFrY <- base::length(FrY)
        ReadLengthRvY <- base::length(RvY)
        DiffLengthFrY <- FrYStarAnalysed - ReadLengthFrY
        DiffLengthRvY <- RvYStarAnalysed - ReadLengthRvY
        RecordLength <- c(ReadLengthFrY,ReadLengthRvY)
        Discarded_reads <-  c(DiffLengthFrY,DiffLengthRvY)
        Sub_sampled_reads <- c(FrYStarAnalysed,RvYStarAnalysed)
        Percent_Discarded_reads <-
          df <- data.frame(Samples = RecordSample, Sub_sampled_reads = Sub_sampled_reads, Retained_reads = RecordLength,Discarded_reads = Discarded_reads)

        ### salvare nella cartella del file temporaneo
        write.table(df, file = paste0(DownloadPath, "Filtered_", output,".log"), append = TRUE, row.names = FALSE,  col.names = FALSE)


        # Save filtered reads
        if (compress == "TRUE") {
          ShortRead::writeFastq(
            FrY,
            file = paste0(DownloadPath, "Filtered_", output, "_1.fq.gz"),
            mode = "a",
            compress = TRUE
          )
          ShortRead::writeFastq(
            RvY,
            file = paste0(DownloadPath, "Filtered_", output, "_2.fq.gz"),
            mode = "a",
            compress = TRUE
          )
        } else {
          ShortRead::writeFastq(
            FrY,
            file = paste0(DownloadPath, "Filtered_", output, "_1.fq"),
            mode = "a",
            compress = FALSE
          )
          ShortRead::writeFastq(
            RvY,
            file = paste0(DownloadPath, "Filtered_", output, "_2.fq"),
            mode = "a",
            compress = FALSE
          )
        }
      }
    }
    ,
    UploadPath,
    DownloadPath,
    qualityType,
    minLen,
    trim,
    trimValue,
    n,
    Adapters,
    Lpattern,
    Rpattern,
    max.Lmismatch,
    max.Rmismatch,
    kW,
    left,
    right,
    halfwidthAnalysis,
    halfwidth,
    compress
  )
  stopCluster(cl) # Stop Cluster generated by parallel::parLapply
}
