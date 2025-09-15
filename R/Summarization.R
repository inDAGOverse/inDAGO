#' Summarization
#'
#' Summarizes read counts from multiple BAM/SAM files in parallel using feature annotations.
#'
#' This function run `Rsubread::featureCounts()` on each input file,
#' capturing count statistics, annotation data, and per-sample summary logs. Results are
#' written to the specified output directory.
#'
#' @param NodesSum Integer. Number of parallel R nodes (e.g., CPU cores) to spawn.
#' @param Xsum Character vector. Filenames of BAM or SAM files to process.
#' @param UploadPathSum Character. Directory containing the raw input files.
#' @param DownloadPathSum Character. Directory into which all output files will be written.
#'
#' @param annot.ext Character. Path to an external annotation file (e.g., GTF/GFF).
#' @param isGTFAnnotationFile Logical. Should `annot.ext` be treated as a GTF file?
#' @param GTF.featureType Character. Feature type (e.g., "exon").
#' @param GTF.attrType Character. GTF attribute (e.g., "gene_id").
#'
#' @param useMetaFeatures Logical. Collapse sub-features into meta-features before counting.
#'
#' @param allowMultiOverlap Logical. Allow reads overlapping multiple features to be counted.
#' @param minOverlap Integer. Minimum number of overlapping bases to assign a read.
#' @param fracOverlap Numeric. Minimum fraction of read that must overlap a feature.
#' @param fracOverlapFeature Numeric. Minimum fraction of feature that must be covered by a read.
#' @param largestOverlap Logical. When overlapping multiple features, assign based on largest overlap.
#'
#' @param countMultiMappingReads Logical. Count reads that map to multiple locations.
#' @param fraction Logical. Distribute counts fractionally for multi-mapping reads.
#'
#' @param minMQS Integer. Minimum mapping quality score for reads to be counted.
#' @param primaryOnly Logical. Count only the primary alignments of multi-mapping reads.
#' @param ignoreDup Logical. Exclude PCR duplicates from counting.
#'
#' @param strandSpecific Integer. Strand-specific counting mode (0 = unstranded, 1 = stranded, 2 = reversely stranded).
#'
#' @param requireBothEndsMapped Logical. In paired-end mode, require both mates to map.
#' @param checkFragLength Logical. Enforce fragment length checks on paired-end reads.
#' @param minFragLength Numeric. Minimum fragment length to keep.
#' @param maxFragLength Numeric. Maximum fragment length to keep.
#' @param countChimericFragments Logical. Count discordant or chimeric read pairs.
#' @param autosort Logical. Automatically sort input files if not already sorted.
#'
#' @param nthreads Integer. Number of threads per featureCounts call.
#' @param tmpDir Character. Directory for temporary files (e.g., large intermediate files).
#' @param verbose Logical. Print verbose messages during execution.
#'
#' @details
#' 1. A socket cluster of `NodesSum` workers is created.
#' 2. Each worker invokes `featureCounts()` on one sample, using the annotation and counting parameters.
#' 3. Outputs per sample:
#'    - A text summary (`*_summary.txt`) capturing the console output.
#'    - A CSV of count statistics (`*_stat.csv`).
#'    - A CSV of feature annotations (`*_annotation.csv`).
#'    - A tab-delimited count matrix saved under `Counts/<sample>.tab`.
#' 4. The cluster is terminated once all samples complete.
#'
#' @return Writes files to `DownloadPathSum`.
#'
Summarization <- function(NodesSum,
                          Xsum,
                          UploadPathSum,
                          DownloadPathSum,
                          # annotation
                          annot.ext,
                          isGTFAnnotationFile,
                          GTF.featureType,
                          GTF.attrType,
                          # level of summarization
                          useMetaFeatures,
                          # overlap between reads and features
                          allowMultiOverlap,
                          minOverlap,
                          fracOverlap,
                          fracOverlapFeature,
                          largestOverlap,
                          # multi-mapping reads
                          countMultiMappingReads,
                          # fractional counting
                          fraction,
                          # read filtering
                          minMQS,
                          primaryOnly,
                          ignoreDup,
                          # strandness
                          strandSpecific,
                          # parameters specific to paired end reads
                          requireBothEndsMapped,
                          checkFragLength,
                          minFragLength,
                          maxFragLength,
                          countChimericFragments,
                          autosort,
                          # number of CPU threads
                          nthreads,
                          # miscellaneous
                          # maxMOp, # Removed the 'maxMOp' parameter from featureCounts in version 2.18.0
                          tmpDir,
                          verbose)
{
  parallel::parLapply(
    clSum <- parallel::makeCluster(NodesSum, type = "SOCK"),
    # Creates a set of copies of R running in parallel via sockets
    Xsum,
    # Object obtained by ListFIlter()
    fun =  function(Rsum,
                    UploadPathSum,
                    DownloadPathSum,
                    # annotation
                    annot.ext,
                    isGTFAnnotationFile,
                    GTF.featureType,
                    GTF.attrType,
                    # level of summarization
                    useMetaFeatures,
                    # overlap between reads and features
                    allowMultiOverlap,
                    minOverlap,
                    fracOverlap,
                    fracOverlapFeature,
                    largestOverlap,
                    # multi-mapping reads
                    countMultiMappingReads,
                    # # fractional counting
                    fraction,
                    # read filtering
                    minMQS,
                    primaryOnly,
                    ignoreDup,
                    # strandness
                    strandSpecific,
                    # parameters specific to paired end reads
                    requireBothEndsMapped,
                    checkFragLength,
                    minFragLength,
                    maxFragLength,
                    countChimericFragments,
                    autosort,
                    # number of CPU threads
                    nthreads,
                    # miscellaneous
                    #      maxMOp, # Removed the 'maxMOp' parameter from featureCounts in version 2.18.0
                    tmpDir,
                    verbose) {
      BamToAnalyze <- paste0(UploadPathSum, Rsum) #Upload samples
      CaptureSumm <- utils::capture.output(
        SummResult <- Rsubread::featureCounts(
          files = BamToAnalyze,
          annot.inbuilt = NULL,
          annot.ext = annot.ext,
          isGTFAnnotationFile = as.logical(isGTFAnnotationFile),
          GTF.featureType = as.character(GTF.featureType),
          GTF.attrType = as.character(GTF.attrType),
          GTF.attrType.extra = NULL,
          chrAliases = NULL,
          # level of summarization
          useMetaFeatures = as.logical(useMetaFeatures),
          # # overlap between reads and features
          allowMultiOverlap = as.logical(allowMultiOverlap),
          minOverlap = as.integer(minOverlap),
          fracOverlap = as.numeric(fracOverlap),
          fracOverlapFeature = as.numeric(fracOverlapFeature),
          largestOverlap = as.logical(largestOverlap),
          nonOverlap = NULL,
          nonOverlapFeature = NULL,
          # Read shift, extension and reduction
          readShiftType = "upstream",
          readShiftSize = 0,
          readExtension5 = 0,
          readExtension3 = 0,
          read2pos = NULL,
          # multi-mapping reads
          countMultiMappingReads = as.logical(countMultiMappingReads),
          # fractional counting
          fraction = as.logical(fraction),
          # long reads
          isLongRead = FALSE,
          # read filtering
          minMQS = as.integer(minMQS),
          splitOnly = FALSE,
          nonSplitOnly = FALSE,
          primaryOnly = as.logical(primaryOnly),
          ignoreDup = as.logical(ignoreDup),
          # strandness
          strandSpecific = as.integer(strandSpecific),
          # exon-exon junctions
          juncCounts = FALSE,
          genome = NULL,
          # parameters specific to paired end reads
          isPairedEnd = TRUE,
          countReadPairs = TRUE,
          requireBothEndsMapped = as.logical(requireBothEndsMapped),
          checkFragLength = as.logical(checkFragLength),
          minFragLength = as.numeric(minFragLength),
          maxFragLength = as.numeric(maxFragLength),
          countChimericFragments = as.logical(countChimericFragments),
          autosort = as.logical(autosort),
          # number of CPU threads
          nthreads = as.integer(nthreads),
          # read group
          byReadGroup = FALSE,
          # report assignment result for each read
          reportReads = NULL,
          reportReadsPath = NULL,
          # miscellaneous
          #    maxMOp = as.integer(maxMOp), # Removed the 'maxMOp' parameter from featureCounts in version 2.18.0
          tmpDir = tmpDir,
          verbose = as.logical(verbose)
        )
      )

      #Save result
      SampleName <- gsub(".bam|.sam", "", Rsum)
      write.table(
        CaptureSumm,
        file = paste0(DownloadPathSum, SampleName, "_summary.txt"),
        row.names = FALSE,
        col.names = FALSE
      )
      write.csv(
        SummResult$stat,
        file = paste0(DownloadPathSum, SampleName, "_stat.csv"),
        row.names = FALSE
      )
      write.csv(
        SummResult$annotation,
        file = paste0(DownloadPathSum, SampleName, "_annotation.csv"),
        row.names = FALSE
      )
      dir.create(paste0(DownloadPathSum,"/","Counts"))
      write.table(SummResult$counts,
                  file = paste0(DownloadPathSum,"/","Counts","/",SampleName,".tab"), sep="\t", col.names = FALSE)
    },
    UploadPathSum,
    DownloadPathSum,
    # annotation
    annot.ext,
    isGTFAnnotationFile,
    GTF.featureType,
    GTF.attrType,
    # level of summarization
    useMetaFeatures,
    # overlap between reads and features
    allowMultiOverlap,
    minOverlap,
    fracOverlap,
    fracOverlapFeature,
    largestOverlap,
    # multi-mapping reads
    countMultiMappingReads,
    # fractional counting
    fraction,
    # read filtering
    minMQS,
    primaryOnly,
    ignoreDup,
    # strandness
    strandSpecific,
    # parameters specific to paired end reads
    requireBothEndsMapped,
    checkFragLength,
    minFragLength,
    maxFragLength,
    countChimericFragments,
    autosort,
    # number of CPU threads
    nthreads,
    # miscellaneous
    #    maxMOp, # Removed the 'maxMOp' parameter from featureCounts in version 2.18.0
    tmpDir,
    verbose
  )

  parallel::stopCluster(clSum) # Stop Cluster generated by parallel::parLapply
}
