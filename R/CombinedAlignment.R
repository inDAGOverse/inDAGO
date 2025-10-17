#' Combined alignment for dual RNA-seq
#' @param lalista list of samples
#' @param nodes logic cores
#' @param readsPath sample folders
#' @param GenomeConcIndex genome index
#' @param outBam output folder
#' @param threads processes
#' @param phredScore quality score
#' @param maxExtractedSubreads number of subreads
#' @param consensusVote consensus
#' @param mismatchMax mismatch
#' @param maxMultiMapped multimapping
#' @param indelLength indel
#' @param fragmentMinLength fragment minumum length
#' @param fragmentMaxLength fragment maximum length
#' @param matesOrientation mate orientation
#' @param readOrderConserved read order
#' @param coordinatesSorting sorting
#' @param allJunctions junctions
#' @param tempfolder temporary folder
#' @param readsAlignedBlock chunks
CombinedAlignment <- function(lalista,
                              nodes,
                              readsPath,
                              GenomeConcIndex,
                              outBam,
                              threads,
                              # outFormat,
                              phredScore,
                              maxExtractedSubreads,
                              consensusVote,
                              mismatchMax,
                              # uniqueOnly,
                              maxMultiMapped,
                              indelLength,
                              fragmentMinLength,
                              fragmentMaxLength,
                              matesOrientation,
                              readOrderConserved,
                              coordinatesSorting,
                              allJunctions,
                              tempfolder,
                              readsAlignedBlock
) {
  ###### NESTED function
  DownstreamOperations <- function(
    temporarySource,
    outputBam,
    prefix,
    sampleBasename,
    pathToGenomeName,
    subsetting
  ) {

    # Read genome list and prefixes
    gn_tbl <- utils::read.table(file.path(paste0(pathToGenomeName, ".files")), header = FALSE, stringsAsFactors = FALSE)
    GenomeNames_raw <- gn_tbl[,1]
    GenomeNames <- unique(sub("^(.*?)_.*$", "\\1", GenomeNames_raw))
    if (length(GenomeNames) < 2) stop("At least two genome prefixes expected in the .files")

    # Paths for global bam file
    supp_file_base <- file.path(temporarySource, paste0(prefix, sampleBasename, "_global.bam"))

    # Temporary files to collect qnames that map to each genome (only RNEXT == "=" rows)
    temp_g1 <- file.path(temporarySource, paste0(prefix, "idMappedFiltered_", sampleBasename, "_", GenomeNames[1], ".txt"))
    temp_g2 <- file.path(temporarySource, paste0(prefix, "idMappedFiltered_", sampleBasename, "_", GenomeNames[2], ".txt"))
    if (file.exists(temp_g1)) file.remove(temp_g1)
    if (file.exists(temp_g2)) file.remove(temp_g2)
    file.create(temp_g1)
    file.create(temp_g2)

    # Final FASTQ files for unmapped reads (R1 / R2)
    fastq_R1_Unmapped  <- file.path(outputBam, paste0(prefix, sampleBasename, "_Unmapped_R1.fq.gz"))
    fastq_R2_Unmapped  <- file.path(outputBam, paste0(prefix, sampleBasename, "_Unmapped_R2.fq.gz"))

    # BAM reading parameters
    parametersBamId <- Rsamtools::ScanBamParam(what = c("qname","rname","mrnm","seq","qual","flag"))

    # write files to store qnames of unmapped reads
    written_file_R1 <- file.path(temporarySource, paste0(prefix, sampleBasename, "_written_qnames_R1.txt"))
    written_file_R2 <- file.path(temporarySource, paste0(prefix, sampleBasename, "_written_qnames_R2.txt"))
    if (!file.exists(written_file_R1)) file.create(written_file_R1)
    if (!file.exists(written_file_R2)) file.create(written_file_R2)

    # Read a chunk of qnames to see if it is already saved
    # TRUE if qname is already present in written_file
    # FALSE if it was not found in the file
    disk_contains <- function(written_file, qn, chunk_n = subsetting) {
      present <- rep(FALSE, length(qn)) # create a logical vector with the same length of qn_norm
      if (!file.exists(written_file) || file.info(written_file)$size == 0) return(present)
      con <- file(written_file, open = "r")
      on.exit(close(con), add = TRUE)
      repeat {
        lines <- readLines(con, n = chunk_n) # read the file in chunk
        if (length(lines) == 0) break
        mask <- qn %in% lines
        if (any(mask)) present[mask] <- TRUE # update present if it find qnames
        if (all(present)) break # if all the elements of qnames are present in the saved file break
      }
      present # returns a logical vector of TRUE if qnames are found
    }

    #write only the qnames not already written in temporaney files
    make_fastq_writer_disk <- function(outFile, written_file) {
      function(sb_env, idxs) {
        if (length(idxs) == 0) return(invisible(0L))
        idxs <- sort(unique(as.integer(idxs)))
        if (length(idxs) == 0) return(invisible(0L))

        qn <- as.character(sb_env$qname[idxs])

        # keep only first occurrence within the chunk
        keep_first <- !duplicated(qn)
        if (!all(keep_first)) {
          idxs <- idxs[keep_first]
          qn <- qn[keep_first]
        }
        if (length(idxs) == 0) return(invisible(0L))

        # check saved index for already written qnames
        present_mask <- disk_contains(written_file, qn)
        new_mask <- !present_mask
        if (!any(new_mask)) return(invisible(0L))

        idxs_new <- idxs[new_mask]
        qn_new_norm <- unique(qn[new_mask])

        # write ShortReadQ
        SRQ <- ShortRead::ShortReadQ(
          sread   = sb_env$seq[idxs_new],
          quality = sb_env$qual[idxs_new],
          id      = Biostrings::BStringSet(sb_env$qname[idxs_new])
        )
        ShortRead::writeFastq(SRQ, file = outFile, mode = "a", compress = TRUE)

        # append newly written qnames to saved files
        write(qn_new_norm, file = written_file, append = TRUE)

        invisible(length(idxs_new))
      }
    }


    # Create per-mate writers
    writer_R1_global <- make_fastq_writer_disk(fastq_R1_Unmapped, written_file_R1)
    writer_R2_global <- make_fastq_writer_disk(fastq_R2_Unmapped, written_file_R2)

    # Open the  BAM and process in chunks
    bf <- Rsamtools::BamFile(supp_file_base, yieldSize = subsetting)
    open(bf)
    on.exit({
      try(close(bf), silent = TRUE)
    }, add = TRUE)

    repeat {
      sb <- tryCatch(Rsamtools::scanBam(bf, param = parametersBamId)[[1]],
                     error = function(e) { message("scanBam error: ", e$message)
      return(list(qname = character(0))) })
      if (length(sb$qname) == 0) break

      sb_env <- list(seq = sb$seq, qual = sb$qual, qname = sb$qname)

      #retrieve flag information
      fm <- Rsamtools::bamFlagAsBitMatrix(sb$flag)
      isFirst         <- fm[,"isFirstMateRead"]
      isSecond        <- fm[,"isSecondMateRead"]
      isUnmapped      <- fm[,"isUnmappedQuery"]
      hasUnmappedMate <- fm[,"hasUnmappedMate"]

      rname_chr <- as.character(sb$rname)
      mrnm_chr  <- as.character(sb$mrnm)
      sameRef   <- !is.na(mrnm_chr) & (mrnm_chr == "=" | mrnm_chr == rname_chr)

      both_unmapped   <- isUnmapped & hasUnmappedMate
      mate_unmapped_A <- isUnmapped & !hasUnmappedMate
      mate_unmapped_B <- !isUnmapped & hasUnmappedMate
      unproperly      <- !isUnmapped & !hasUnmappedMate & !sameRef

      mask_R1 <- isFirst  & (both_unmapped | mate_unmapped_A | mate_unmapped_B | unproperly)
      mask_R2 <- isSecond & (both_unmapped | mate_unmapped_A | mate_unmapped_B | unproperly)

      idx_R1 <- which(mask_R1)
      idx_R2 <- which(mask_R2)


      # write unmapped
      written_R1_n <- tryCatch(writer_R1_global(sb_env, idx_R1), error = function(e) { message("writer R1 error: ", e$message)
        return(0L) })
      written_R2_n <- tryCatch(writer_R2_global(sb_env, idx_R2), error = function(e) { message("writer R2 error: ", e$message)
        return(0L) })


      # collect qnames for mapped reads with RNEXT == "=" to assign to genome-specific lists
      mapped_idx_eq <- which(!isUnmapped & !is.na(rname_chr) & rname_chr != "" & sameRef)
      if (length(mapped_idx_eq) > 0) {
        rnames_chunk <- rname_chr[mapped_idx_eq]
        qnames_chunk <- as.character(sb$qname[mapped_idx_eq])
        prefixes <- sub("^(.*?)_.*$", "\\1", rnames_chunk)
        for (g in unique(prefixes)) {
          sel <- which(prefixes == g)
          ids <- unique(qnames_chunk[sel])
          if (length(ids) == 0) next
          tgt <- if (g == GenomeNames[1]) temp_g1 else if (g == GenomeNames[2]) temp_g2 else NULL
          if (!is.null(tgt)) write(ids, file = tgt, append = TRUE)
        }
      }

      rm(sb, sb_env)
      base::gc(reset = TRUE)
    } # end repeat BamFile closed by on.exit

    # Ensure FASTQ files exist (create empty gz if nothing written)
    if (!file.exists(fastq_R1_Unmapped)) { con <- gzfile(fastq_R1_Unmapped, "w")
    close(con) }
    if (!file.exists(fastq_R2_Unmapped)) { con <- gzfile(fastq_R2_Unmapped, "w")
    close(con) }


    # Sort & index the BAM (needed for downstream filtering)
    Rsamtools::sortBam(file = supp_file_base, destination = file.path(temporarySource, paste0(prefix, sampleBasename, "_sorted")))
    Rsamtools::indexBam(files = file.path(temporarySource, paste0(prefix, sampleBasename, "_sorted.bam")))
    sorted_bam   <- file.path(temporarySource, paste0(prefix, sampleBasename, "_sorted.bam"))

    # Cross-mapped IDs file (IDs that appear mapped to both genomes)
    cross_ids_file <- file.path(temporarySource, paste0(prefix, "idCrossMapped_", sampleBasename, ".txt"))
    if (file.exists(cross_ids_file)) file.remove(cross_ids_file)
    file.create(cross_ids_file)

    # Intersect IDs between the two genome
    intersectFiles <- function(genomeA, genomeB, outFile) {
      if (!file.exists(genomeA) || !file.exists(genomeB)) return(invisible(NULL))
      con1 <- file(genomeA, "r")
      repeat {
        line1 <- readLines(con1, n = subsetting) # chunk size can be tuned
        if (length(line1) == 0) break
        con2 <- file(genomeB, "r")
        repeat {
          line2 <- readLines(con2, n = subsetting)
          if (length(line2) == 0) break
          commonLine <- intersect(line1, line2)
          if (length(commonLine) > 0) write(commonLine, file = outFile, append = TRUE)
        }
        close(con2)
      }
      close(con1)
    }
    # Run the intersection to find cross-mapped qnames
    intersectFiles(temp_g1, temp_g2, cross_ids_file)

    # Filter and extract cross-mapped reads (keeping only RNEXT == "=" rows)
    want_cross <- character(0)
    if (file.exists(cross_ids_file) && file.info(cross_ids_file)$size > 0) {
      want_cross <- unique(readLines(con = cross_ids_file))
      filter_cross_factory <- function(want_ids) list(
        KeepQnameSameRef = function(x) {
          q_in <- x$qname %in% want_ids
          rnm  <- as.character(x$rname)
          mrnm <- as.character(x$mrnm)
          same <- !is.na(mrnm) & (mrnm == "=" | mrnm == rnm)
          q_in & same
        }
      )
      filter_cross <- S4Vectors::FilterRules(filter_cross_factory(want_cross))
      Rsamtools::filterBam(
        file = sorted_bam,
        destination = file.path(temporarySource, paste0("pre_CrossMapping_", sampleBasename, ".bam")),
        filter = filter_cross,
        param = Rsamtools::ScanBamParam(what = c("qname","rname","mrnm", Rsamtools::scanBamWhat())),
        indexDestination = FALSE
      )
    } else {
      # create an empty file if no cross-mapped found
      file.create(file.path(temporarySource, paste0("pre_CrossMapping_", sampleBasename, ".bam")))
    }

    # Remove cross-mapped IDs from genome-specific ID lists (create depurated lists)
    crossMappingDeletion <- function(toProcess, crossMapped, outPathList) {
      if (!file.exists(toProcess)) { file.create(outPathList)
        return(invisible(NULL)) }
      if (!file.exists(crossMapped) || file.info(crossMapped)$size == 0) {
        file.copy(from = toProcess, to = outPathList, overwrite = TRUE)
        return(invisible(NULL))
      }
      cross_ids <- unique(readLines(con = crossMapped))
      con1 <- file(toProcess, "r")
      if (file.exists(outPathList)) file.remove(outPathList)
      file.create(outPathList)
      repeat {
        lines <- readLines(con1, n = subsetting)
        if (length(lines) == 0) break
        kept <- lines[!(lines %in% cross_ids)]
        if (length(kept) > 0) write(kept, file = outPathList, append = TRUE)
      }
      close(con1)
    }

    dep_g1 <- file.path(temporarySource, paste0(prefix, "id", GenomeNames[1], "Depurated_", sampleBasename, ".txt"))
    dep_g2 <- file.path(temporarySource, paste0(prefix, "id", GenomeNames[2], "Depurated_", sampleBasename, ".txt"))
    crossMappingDeletion(temp_g1, cross_ids_file, dep_g1)
    crossMappingDeletion(temp_g2, cross_ids_file, dep_g2)


    # Create filter rules per genome
    filter_factory_genome <- function(want_ids, genome_prefix) {
      list(KeepQnameRnameSame = function(x) {
        q_in <- x$qname %in% want_ids
        rnm  <- as.character(x$rname)
        mrnm <- as.character(x$mrnm)
        same <- !is.na(mrnm) & (mrnm == "=" | mrnm == rnm)
        r_ok <- grepl(paste0("^", genome_prefix, "_"), rnm)
        q_in & r_ok & same
      })
    }

    # Apply filter for Genome 1
    want_g1 <- if (file.exists(dep_g1) && file.info(dep_g1)$size > 0) unique(readLines(con = dep_g1)) else character(0)
    filterOne <- S4Vectors::FilterRules(filter_factory_genome(want_g1, GenomeNames[1]))
    Rsamtools::filterBam(
      file = sorted_bam,
      destination = file.path(temporarySource, paste0("pre_", prefix, sampleBasename, "_", GenomeNames[1], ".bam")),
      filter = filterOne,
      param = Rsamtools::ScanBamParam(what = c("qname","rname","mrnm", Rsamtools::scanBamWhat())),
      indexDestination = FALSE
    )

    # Apply filter for Genome 2
    want_g2 <- if (file.exists(dep_g2) && file.info(dep_g2)$size > 0) unique(readLines(con = dep_g2)) else character(0)
    filterTwo <- S4Vectors::FilterRules(filter_factory_genome(want_g2, GenomeNames[2]))
    Rsamtools::filterBam(
      file = sorted_bam,
      destination = file.path(temporarySource, paste0("pre_", prefix, sampleBasename, "_", GenomeNames[2], ".bam")),
      filter = filterTwo,
      param = Rsamtools::ScanBamParam(what = c("qname","rname","mrnm", Rsamtools::scanBamWhat())),
      indexDestination = FALSE
    )

    # Clean reference names for cross-mapped and genome-specific BAMs,
    # remove the genome prefix (e.g. "genomeA_chr1" -> "chr1") and export final BAMs
    cleaningReferenceNamesCrossMapped <- function(outputBam,
                                                  prefix,
                                                  sampleBasename,
                                                  subsetting,
                                                  temporarySource,
                                                  results,
                                                  GenomeNames) {
      bamToModify <- file.path(temporarySource, paste0("pre_CrossMapping_", sampleBasename, ".bam"))
      finalBam <- file.path(results, paste0(prefix, sampleBasename, ".bam"))

      if (!file.exists(bamToModify) || file.info(bamToModify)$size == 0) {
        file.create(finalBam)
        writeLines("No cross-mapped reads detected", con = file.path(results, paste0(prefix, sampleBasename, ".summary")))
        return(invisible(NULL))
      }

      doc <- Rsamtools::BamFile(bamToModify, yieldSize = subsetting)
      open(doc)
      tmpdir <- file.path(temporarySource, paste0("tempDir_CrossMapped_", sampleBasename))
      if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
      idx <- 1
      repeat {
        preBAM <- tryCatch(
          rtracklayer::import(con = doc, use.names = TRUE,
                              param = Rsamtools::ScanBamParam(what = c(
                                "qname","flag","rname","strand","pos","qwidth",
                                "mapq","cigar","mrnm","mpos","isize","seq","qual",
                                "groupid","mate_status"))),
          error = function(e) NULL
        )
        if (is.null(preBAM) || length(preBAM) == 0) break
        # remove genome prefix from seqnames and relevant metadata columns
        for (g in GenomeNames) {
          prefix_pat <- paste0("^", g, "_")
          preBAM@seqnames@values <- sub(prefix_pat, "", preBAM@seqnames@values)
          preBAM@elementMetadata@listData$rname <- sub(prefix_pat, "", preBAM@elementMetadata@listData$rname)
          preBAM@elementMetadata@listData$mrnm <- sub(prefix_pat, "", preBAM@elementMetadata@listData$mrnm)
          preBAM@seqinfo@seqnames <- sub(prefix_pat, "", preBAM@seqinfo@seqnames)
        }
        rtracklayer::export(preBAM, Rsamtools::BamFile(file.path(tmpdir, paste0("temp_", idx, ".bam"))))
        idx <- idx + 1
      }
      close(doc)

      # Merge partial BAMs written in tmpdir into final BAM
      temporaryBam <- list.files(tmpdir, pattern = "\\.bam$", full.names = TRUE)
      if (length(temporaryBam) == 0) file.create(finalBam)
      else if (length(temporaryBam) == 1) file.copy(from = temporaryBam, to = finalBam, overwrite = TRUE)
      else Rsamtools::mergeBam(files = temporaryBam, destination = finalBam)

      if (!file.exists(finalBam) || file.info(finalBam)$size == 0) {
        writeLines("No cross mapped reads detected", con = file.path(results, paste0(prefix, sampleBasename, ".summary")))
      } else {
        sink(file.path(results, paste0(prefix, sampleBasename, ".summary")))
        try(Rsamtools::quickBamFlagSummary(finalBam), silent = TRUE)
        sink()
      }
    }

    cleaningReferenceNamesCrossMapped(outputBam = outputBam, prefix = "CrossMapping_", sampleBasename = sampleBasename, subsetting = subsetting, temporarySource = temporarySource, results = outputBam, GenomeNames = GenomeNames)

    cleaningReferenceNamesGenomes <- function(outputBam, prefix, sampleBasename, GenomeNames, index, subsetting, temporarySource, results) {
      bamToModify <- file.path(temporarySource, paste0("pre_", prefix, sampleBasename, "_", GenomeNames[index], ".bam"))
      outfn <- file.path(results, paste0(prefix, sampleBasename, "_", GenomeNames[index], ".bam"))
      if (!file.exists(bamToModify) || file.info(bamToModify)$size == 0) { file.create(outfn)
        return(invisible(NULL)) }

      doc <- Rsamtools::BamFile(bamToModify, yieldSize = subsetting)
      open(doc)
      tmpdir <- file.path(temporarySource, paste0("tempDir_", sampleBasename,"_", GenomeNames[index]))
      if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
      idx <- 1
      repeat {
        preBAM <- tryCatch(
          rtracklayer::import(con = doc, use.names = TRUE,
                              param = Rsamtools::ScanBamParam(what = c(
                                "qname","flag","rname","strand","pos","qwidth",
                                "mapq","cigar","mrnm","mpos","isize","seq","qual",
                                "groupid","mate_status"))),
          error = function(e) NULL
        )
        if (is.null(preBAM) || length(preBAM) == 0) break
        prefix_pat <- paste0("^", GenomeNames[index], "_")
        preBAM@seqnames@values <- sub(prefix_pat, "", preBAM@seqnames@values)
        preBAM@elementMetadata@listData$rname <- sub(prefix_pat, "", preBAM@elementMetadata@listData$rname)
        preBAM@elementMetadata@listData$mrnm  <- sub(prefix_pat, "", preBAM@elementMetadata@listData$mrnm)
        preBAM@seqinfo@seqnames <- sub(prefix_pat, "", preBAM@seqinfo@seqnames)
        rtracklayer::export(preBAM, Rsamtools::BamFile(file.path(tmpdir, paste0("temp_", idx, ".bam"))))
        idx <- idx + 1
      }
      close(doc)

      # Merge/export into final genome-specific BAM
      temporaryBam <- list.files(tmpdir, pattern = "\\.bam$", full.names = TRUE)
      if (length(temporaryBam) == 0) file.create(outfn)
      else if (length(temporaryBam) == 1) file.copy(from = temporaryBam, to = outfn, overwrite = TRUE)
      else Rsamtools::mergeBam(files = temporaryBam, destination = outfn)

      if (!file.exists(outfn) || file.info(outfn)$size == 0) {
        writeLines("No mapped reads detected", con = file.path(results, paste0(prefix, sampleBasename, "_", GenomeNames[index], ".summary")))
      } else {
        sink(file.path(results, paste0(prefix, sampleBasename, "_", GenomeNames[index], ".summary")))
        try(Rsamtools::quickBamFlagSummary(outfn), silent = TRUE)
        sink()
      }
      base::gc(reset = TRUE)
    }

    # Produce cleaned BAMs for both genomes
    cleaningReferenceNamesGenomes(outputBam, prefix, sampleBasename, GenomeNames, 1, subsetting, temporarySource, outputBam)
    cleaningReferenceNamesGenomes(outputBam, prefix, sampleBasename, GenomeNames, 2, subsetting, temporarySource, outputBam)


  }     # End of DownstreamOperations

  ###### parallelization: run subjunc + downstream per sample in parallel workers
  clust <- parallel::makeCluster(spec = nodes, type = "PSOCK")
  # Ensure cluster is stopped on function exit or error
  base::on.exit(parallel::stopCluster(cl = clust), add = TRUE)

  parallel::parLapply(cl = clust,
                      X = lalista,
                      fun = function(x, readsPath, GenomeConcIndex, outBam, threads, phredScore, maxExtractedSubreads, consensusVote, mismatchMax, maxMultiMapped, indelLength, fragmentMinLength, fragmentMaxLength, matesOrientation, readOrderConserved, coordinatesSorting, allJunctions, tempfolder, readsAlignedBlock) {

                        # Run subjunc (Rsubread) to align reads against the concatenated genome index.
                        Rsubread::subjunc(
                          readfile1 = file.path(readsPath, list.files(path = readsPath, pattern = paste0(x, "_1.f"))),
                          readfile2 = file.path(readsPath, list.files(path = readsPath, pattern = paste0(x, "_2.f"))),
                          index = GenomeConcIndex,
                          output_file = file.path(tempfolder, paste0("Alignment_", x, "_global.bam")),
                          nthreads = threads,
                          # output_format = outFormat,
                          phredOffset = phredScore,
                          nsubreads = maxExtractedSubreads,
                          TH1 = consensusVote,
                          TH2 = consensusVote,
                          maxMismatches = mismatchMax,
                          # unique = uniqueOnly,
                          nBestLocations = maxMultiMapped,
                          indels = indelLength,
                          maxFragLength = fragmentMaxLength,
                          minFragLength = fragmentMinLength,
                          PE_orientation = matesOrientation,
                          keepReadOrder = readOrderConserved,
                          sortReadsByCoordinates = coordinatesSorting,
                          reportAllJunctions = allJunctions
                        ) ## end subjunc

                        # Downstream processing: extract unmapped reads, build genome-specific lists, filter cross-mapped, export cleaned BAMs
                        DownstreamOperations(temporarySource = tempfolder,
                                             outputBam = outBam,
                                             prefix = "Alignment_",
                                             sampleBasename = x,
                                             pathToGenomeName = GenomeConcIndex,
                                             subsetting = readsAlignedBlock
                        )

                        # Rescue other output (indel/junction files)
                        fileRescue <- c(".indel.vcf", ".junction.bed")
                        file.rename(
                          from = file.path(tempfolder,
                                           paste("Alignment_", x, "_global.bam", fileRescue, sep = "")),
                          to = file.path(outBam,
                                         paste("Alignment_", x, fileRescue, sep = ""))
                        )

                        # Explicit GC after heavy worker task
                        base::gc(reset = TRUE)
                      },
                      readsPath,
                      GenomeConcIndex,
                      outBam,
                      threads,
                      # outFormat,
                      phredScore,
                      maxExtractedSubreads,
                      consensusVote,
                      mismatchMax,
                     # uniqueOnly,
                      maxMultiMapped,
                      indelLength,
                      fragmentMinLength,
                      fragmentMaxLength,
                      matesOrientation,
                      readOrderConserved,
                      coordinatesSorting,
                      allJunctions,
                      tempfolder,
                      readsAlignedBlock
  ) ## end parLapply

  # Stop cluster (on.exit above already ensures stop on error/exit)
  parallel::stopCluster(cl = clust)

  # Remove temporary folder tree after run
  unlink(tempfolder, recursive = TRUE)
}
