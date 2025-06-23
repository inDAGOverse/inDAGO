#' Sequential alignment function
#'
#' @param lalista list of samples
#' @param nodes logic cores
#' @param readsPath sample folders
#' @param GenomeFirstIndex first genome index
#' @param GenomeSecondIndex second genome index
#' @param outBam1 first output folder
#' @param outBam2 second output folder
#' @param threads processes
#' @param outFormat BAM or SAM
#' @param phredScore quality score
#' @param maxExtractedSubreads number of subreads
#' @param consensusVote consensus
#' @param mismatchMax mismatch
#' @param uniqueOnly no multimapping
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
SequentialAlignment <- function(lalista, nodes,
                                readsPath,
                                GenomeFirstIndex,
                                GenomeSecondIndex,
                                outBam1,
                                outBam2,
                                threads,
                                outFormat,
                                phredScore,
                                maxExtractedSubreads,
                                consensusVote,
                                mismatchMax,
                                uniqueOnly,
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
    ###### nested function
    ## unmapped extraction to fastq and BAM/SAM depuration 
    UnmappedExtraction <- function(temporarySource, outputBam, prefix, sampleBasename, 
                                   subsetting = 1000000) {
        fromBamToFastq <- function(alignFile, Setting, OutPut) {
            SubsetImported <- Rsamtools::scanBam(
                file = alignFile,
                param = Setting
            )
            SubsetShortReads <- ShortRead::ShortReadQ(sread = SubsetImported[[1]]$seq,
                                                      quality = SubsetImported[[1]]$qual,
                                                      id = Biostrings::BStringSet(SubsetImported[[1]]$qname))
            ShortRead::writeFastq(object = SubsetShortReads,
                                  file = OutPut,
                                  mode = "a", compress = TRUE)
        }
        ##### R1 unmapped, mate unmapped
        ## parameters
        fieldR1unmap2 <- Rsamtools::ScanBamParam(
            flag =  Rsamtools::scanBamFlag(
                isFirstMateRead = TRUE,
                isUnmappedQuery = TRUE,
                hasUnmappedMate = TRUE),
            reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        ## PROCESSING
        #1.set number of BAM chunks
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")),
            yieldSize = subsetting))
        cycleBAM <- 0
        while(nrec <- length(Rsamtools::scanBam(docBam)[[1]][[1]])) {
            cat("records:", nrec, "\n")
            cycleBAM <- cycleBAM+1
            print(cycleBAM)
        }
        close(docBam)
        #2.empty object inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1unmap2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
            )
        )
        #3.iterating, changing class and recording
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")),
            yieldSize = subsetting))
        R1up <- 1
        while (R1up<=cycleBAM) {
            print(R1up)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR1unmap2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR1unmap2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
                           ))
            R1up <- R1up+1
        }
        close(docBam)
        ## category 2: unmapped R2 (mate unmapped)
        ## PARAMETERS
        fieldR2unmap2 <- Rsamtools::ScanBamParam(
            flag =  Rsamtools::scanBamFlag(
                isSecondMateRead = TRUE,
                isUnmappedQuery = TRUE,
                hasUnmappedMate = TRUE),
            reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        #2.empty object inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR2unmap2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
            )
        )
        #3.iterate, import, class, write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R2up <- 1
        while (R2up<=cycleBAM) {
            print(R2up)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR2unmap2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR2unmap2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
                           )
            )
            R2up <- R2up+1
        }
        close(docBam)
        ## "SPURIE": R1 unmapped e R2 mapped
        ## unmapped R1
        ## PARAMETERS
        fieldR1mateUnmapped2 <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isUnmappedQuery = TRUE,
                isFirstMateRead = TRUE,
                hasUnmappedMate = FALSE
            ), reverseComplement = TRUE, 
            what = c("qname", "seq", "qual")
        )
        #2.empty object inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1mateUnmapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
            )
        )
        #3.iterate, import, class, write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R1mu <- 1
        while (R1mu<=cycleBAM) {
            print(R1mu)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR1mateUnmapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR1mateUnmapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
                           ))
            R1mu <- R1mu+1
        }
        ## mapped R2, mate R1 unmapped
        ## PARAMETERS
        fieldR2mateMapped2 <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isUnmappedQuery = FALSE,
                isSecondMateRead = TRUE,
                hasUnmappedMate = TRUE
            ), reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        #2.empty object inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR2mateMapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
            )
        )
        #3.iterate, import, class, write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R2mm <- 1
        while (R2mm<=cycleBAM) {
            print(R2mm)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR2mateMapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR2mateMapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
                           ))
            R2mm <- R2mm+1
        }
        ## "SPURIE": R1 mapped e R2 unmapped
        ## mapped R1
        fieldR1mateMapped2 <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isUnmappedQuery = FALSE,
                isFirstMateRead = TRUE,
                hasUnmappedMate = TRUE
            ), reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        #2.empty object inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1mateMapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
            )
        )
        #3.iterate, import, class, write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R1mm <- 1
        while (R1mm<=cycleBAM) {
            print(R1mm)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR1mateMapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR1mateMapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
                           ))
            R1mm <- R1mm+1
        }
        ## unmapped R2, mate R1 mapped
        fieldR2mateUnmapped2 <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isUnmappedQuery = TRUE,
                isSecondMateRead = TRUE,
                hasUnmappedMate = FALSE
            ), reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        #2.empty object inizialization
        file.create(
            file.path(                                                                                        
                temporarySource, paste(
                    prefix, "FastqR2mateUnmapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
            )
        )
        #3.interate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R2mu <- 1
        while (R2mu<=cycleBAM) {
            print(R2mu)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR1mateMapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR2mateUnmapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
                           ))
            R2mu <- R2mu+1
        }
        ##### UNION
        ## UNION R1
        erreUno <- c(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1unmap2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")),
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1mateUnmapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")),
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1mateMapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = ""))
        )
        Rfastp::catfastq(
            output = 
                file.path(
                    outputBam, paste(
                        prefix, "R1_", sampleBasename, "_unmapped.fastq.gz", sep = "")),
            inputFiles = erreUno,
            append = FALSE,
            paired = FALSE
        )
        ## UNION R2
        erreDue <- c(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR2unmap2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")),
            file.path(
                temporarySource, paste(
                    prefix, "FastqR2mateMapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")),
            file.path(
                temporarySource, paste(
                    prefix, "FastqR2mateUnmapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = ""))
        )
        Rfastp::catfastq(
            output = file.path(
                outputBam, paste(
                    prefix, "R2_", sampleBasename, "_unmapped.fastq.gz", sep = "")),
            inputFiles = erreDue,
            append = FALSE,
            paired = FALSE)
        ## CLEAN BAM FROM UNMAPPED
        ## setting filter parameters
        parameters <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isPaired = TRUE,
                isUnmappedQuery = FALSE,
                hasUnmappedMate = FALSE
            ), 
            what = Rsamtools::scanBamWhat())
        ## bam ordering
        Rsamtools::sortBam(
            file = file.path(temporarySource,
                             paste(prefix, sampleBasename, "_supplementary", sep = "")),
            destination = file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted", sep = "")))
        ## bam indexing
        Rsamtools::indexBam(files = file.path(temporarySource, paste(
            prefix, sampleBasename, "_sorted", ".bam", sep = "")))
        ## bam filtering and writing
        Rsamtools::filterBam(
            file = file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted", ".bam", sep = "")),
            destination = 
                file.path(outputBam, paste(
                    prefix, sampleBasename, ".bam", sep = "")),
            param = parameters,
            indexDestination = FALSE)
    }
    ###### end nested function
    ###### parallelization
    parallel::parLapply(cl = clust <- parallel::makeCluster(spec = nodes, type = "PSOCK"), 
                        X = lalista,
                        fun = function(x,
                                       readsPath,
                                       GenomeFirstIndex,
                                       GenomeIndexSecond,
                                       outBam1,
                                       outBam2,
                                       threads,
                                       outFormat,
                                       phredScore,
                                       maxExtractedSubreads,
                                       consensusVote,
                                       mismatchMax,
                                       uniqueOnly,
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
                            ## first alignment
                            Rsubread::subjunc(
                                readfile1 = file.path(readsPath,
                                                      list.files(
                                                          path = readsPath,
                                                          pattern =
                                                              paste(x, 
                                                                    "_1.fastq$|_1.fq$|_1.fastq.gz$|_1.fq.gz$", 
                                                                    sep = "")
                                                      )
                                ),
                                readfile2 = file.path(readsPath,
                                                      list.files(
                                                          path = readsPath,
                                                          pattern =
                                                              paste(x, 
                                                                    "_2.fastq$|_2.fq$|_2.fastq.gz$|_2.fq.gz$", 
                                                                    sep = "")
                                                      )
                                ),
                                index = GenomeFirstIndex, 
                                output_file = file.path(tempfolder,
                                                        paste("FirstAlignment", x, "_supplementary", sep = "")
                                ),
                                nthreads = threads,
                                input_format = "gzFASTQ",
                                output_format = outFormat,
                                phredOffset = phredScore,
                                nsubreads = maxExtractedSubreads,
                                TH1 = consensusVote,
                                TH2 = consensusVote,
                                maxMismatches = mismatchMax,
                                unique = uniqueOnly,
                                nBestLocations = maxMultiMapped,
                                indels = indelLength,
                                maxFragLength = fragmentMinLength,
                                minFragLength = fragmentMaxLength,
                                PE_orientation = matesOrientation,
                                keepReadOrder = readOrderConserved,
                                sortReadsByCoordinates = coordinatesSorting,
                                reportAllJunctions = allJunctions
                            )## end of subjunc1
                            ## unmapped extraction
                            UnmappedExtraction(temporarySource = tempfolder,
                                               outputBam = outBam1,
                                               prefix = "FirstAlignment",
                                               sampleBasename = x, 
                                               subsetting = readsAlignedBlock)
                            ## second allignment
                            Rsubread::subjunc(
                                readfile1 = file.path(
                                    outBam1, paste("FirstAlignment", "R1_", x, "_unmapped.fastq.gz", sep = "")),
                                readfile2 = file.path(
                                    outBam1, paste("FirstAlignment", "R2_", x, "_unmapped.fastq.gz", sep = "")),
                                index = GenomeSecondIndex,
                                output_file = file.path(tempfolder,
                                                        paste("SecondAlignment", x, "_supplementary", sep = "")
                                                        
                                ),
                                nthreads = threads,
                                input_format = "gzFASTQ",
                                output_format = outFormat,
                                phredOffset = phredScore,
                                nsubreads = maxExtractedSubreads,
                                TH1 = consensusVote,
                                TH2 = consensusVote,
                                maxMismatches = mismatchMax,
                                unique = uniqueOnly,
                                nBestLocations = maxMultiMapped,
                                indels = indelLength,
                                maxFragLength = fragmentMinLength,
                                minFragLength = fragmentMaxLength,
                                PE_orientation = matesOrientation,
                                keepReadOrder = readOrderConserved,
                                sortReadsByCoordinates = coordinatesSorting,
                                reportAllJunctions = allJunctions
                            )## end of subjunc2
                            ## unmapped extraction
                            UnmappedExtraction(temporarySource = tempfolder,
                                               outputBam = outBam2,
                                               prefix = "SecondAlignment",
                                               sampleBasename = x, 
                                               subsetting = readsAlignedBlock)
                            ## rescuing indel.vcf
                            fileRescue <- c(".indel.vcf", ".junction.bed", ".summary")
                            file.rename(
                                from = file.path(tempfolder, 
                                                 paste("FirstAlignment", x, "_supplementary", fileRescue, sep = "")), 
                                to = file.path(outBam1, 
                                               paste("FirstAlignment", x, fileRescue, sep = ""))
                            )
                            file.rename(
                                from = file.path(tempfolder, 
                                                 paste("SecondAlignment", x, "_supplementary", fileRescue, sep = "")), 
                                to = file.path(outBam2, 
                                               paste("SecondAlignment", x, fileRescue, sep = ""))
                            )
                            ## eliciting RAM memory cleaning
                            base::gc(reset = TRUE)
                        }, ## end of the function body
                        readsPath,
                        GenomeFirstIndex,
                        GenomeSecondIndex,
                        outBam1,
                        outBam2,
                        threads,
                        outFormat,
                        phredScore,
                        maxExtractedSubreads,
                        consensusVote,
                        mismatchMax,
                        uniqueOnly,
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
    ) ## end of parlapply
    parallel::stopCluster(cl = clust) ## end of node communications
    unlink(tempfolder, recursive = TRUE)
}