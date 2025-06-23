#' Title
#'
#' @param lalista list of samples
#' @param nodes logic cores
#' @param readsPath sample folders
#' @param GenomeConcIndex genome index
#' @param outBam output folder
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
CombinedAlignment <- function(lalista, 
                              nodes,
                              readsPath,
                              GenomeConcIndex,
                              outBam,
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
    ###### NESTED function
    ## unmapped extraction
    ## filtering BAM from unmappeddalle medesime
    ## read discrimination
    ## intetifying cross-mapped
    DownstreamOperations <- function(
        temporarySource, #temporary folder
        outputBam, #result folder
        prefix, #prefix
        sampleBasename, #sample name
        pathToGenomeName, #index genome folder to extract basename
        subsetting # chunk division
    ) {
        ## nested function 1
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
        ###### nested function 2 #####
        fromBamToTextId <- function(
        alignFile,
        Setting,
        temp1,
        temp2
        ) {
            # importing
            SubsetImported <- Rsamtools::scanBam(
                file = alignFile,
                param = Setting
            )
            LikeATable <- data.frame(
                id = SubsetImported[[1]]$qname,
                genome = SubsetImported[[1]]$rname
            )
            LikeATable$genome <- 
                gsub(pattern = "_.*$",replacement = "",x = LikeATable$genome)
            print(unique(LikeATable$genome))
            ## in-silico discrimination
            if(length(unique(LikeATable$genome)) == 2) { ## double name
                LikeAList <- split(LikeATable, f = LikeATable$genome)
                if(unique(LikeAList[[1]]$genome) == GenomeNames[1]) { ## name list 1 == genome 1
                    GenomeOne <- LikeAList[[1]]$id
                    GenomeOne <- unique(GenomeOne)
                    GenomeTwo <- LikeAList[[2]]$id
                    GenomeTwo <- unique(GenomeTwo)
                    write(x = GenomeOne, file = temp1, append = TRUE)
                    write(x = GenomeTwo, file = temp2, append = TRUE)
                } else if (unique(LikeAList[[1]]$genome) == GenomeNames[2]) { # nome list 1 == genome 2
                    GenomeTwo <- LikeAList[[1]]$id
                    GenomeTwo <- unique(GenomeTwo)
                    GenomeOne <- LikeAList[[2]]$id
                    GenomeOne <- unique(GenomeOne)
                    write(x = GenomeOne, file = temp1, append = TRUE)
                    write(x = GenomeTwo, file = temp2, append = TRUE)
                }
            } else if(length(unique(LikeATable$genome)) == 1)  { ## single name
                LikeAList <- split(LikeATable, f = LikeATable$genome)
                if(unique(LikeAList[[1]]$genome) == GenomeNames[1]) { ## name object 1 == genome 1
                    GenomeOne <- LikeAList[[1]]$id
                    GenomeOne <- unique(GenomeOne)
                    write(x = GenomeOne, file = temp1, append = TRUE)
                } else if(unique(LikeAList[[1]]$genome) == GenomeNames[2]) { ## name object 1 == genome 2
                    GenomeTwo <- LikeAList[[1]]$id
                    GenomeTwo <- unique(GenomeTwo)
                    write(x = GenomeTwo, file = temp2, append = TRUE)
                }
            }
        }
        ##### unmapped R1, mate unmapped
        ## parameters
        fieldR1unmap2 <- Rsamtools::ScanBamParam(
            flag =  Rsamtools::scanBamFlag(
                isFirstMateRead = TRUE,
                isUnmappedQuery = TRUE,
                hasUnmappedMate = TRUE),
            reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        ## PROCESSing
        ## setting BAM chunck number
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")),
            yieldSize = subsetting))
        cycleBam <- 0
        while(nrec <- length(Rsamtools::scanBam(docBam)[[1]][[1]])) {
            cat("records:", nrec, "\n")
            cycleBam <- cycleBam+1
            print(cycleBam)
        }
        close(docBam)
        #1. BAM chunck number
        cycleR1unmapPaired <- cycleBam
        #2. inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1unmap2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
            )
        )
        #3. iterate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")),
            yieldSize = subsetting))
        R1up <- 1
        while (R1up<=cycleR1unmapPaired) {
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
        ## PARAMETers
        fieldR2unmap2 <- Rsamtools::ScanBamParam(
            flag =  Rsamtools::scanBamFlag(
                isSecondMateRead = TRUE,
                isUnmappedQuery = TRUE,
                hasUnmappedMate = TRUE),
            reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        ## PROCESSing
        #1. BAM chunk number
        cycleR2unmapPaired <- cycleBam
        #2.inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR2unmap2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
            )
        )
        #3.iterate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R2up <- 1
        while (R2up<=cycleR2unmapPaired) {
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
        ## "SPURIE": unmapped R1, R2 mapped
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
        ## PROCESSING
        #1.BAM chunck number
        cycleR1mateUnmapped <- cycleBam
        #2. inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1mateUnmapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
            )
        )
        #3. iterate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R1mu <- 1
        while (R1mu<=cycleR1mateUnmapped) {
            print(R1mu)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR1mateUnmapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR1mateUnmapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
                           ))
            R1mu <- R1mu+1
        }
        close(docBam)
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
        ## PROCESSing
        #1.bam chuncks number
        cycleR2mateMapped <- cycleBam
        #2. inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR2mateMapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
            )
        )
        #3. iterate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R2mm <- 1
        while (R2mm<=cycleR2mateMapped) {
            print(R2mm)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR2mateMapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR2mateMapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
                           ))
            R2mm <- R2mm+1
        }
        close(docBam)
        ## "SPURIE": mapped R1 and R2 unmapped
        ## mapped R1
        fieldR1mateMapped2 <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isUnmappedQuery = FALSE,
                isFirstMateRead = TRUE,
                hasUnmappedMate = TRUE
            ), reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        ## PROCESSing
        #1. bam chuncks number
        cycleR1mateMapped <- cycleBam
        #2.inizialization
        file.create(
            file.path(
                temporarySource, paste(
                    prefix, "FastqR1mateMapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
            )
        )
        #3. iterate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R1mm <- 1
        while (R1mm<=cycleR1mateMapped) {
            print(R1mm)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR1mateMapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR1mateMapped2_", sampleBasename, "_Unmapped_R1.fq.gz", sep = "")
                           ))
            R1mm <- R1mm+1
        }
        close(docBam)
        ## unmapped R2, mate R1 mapped
        fieldR2mateUnmapped2 <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isUnmappedQuery = TRUE,
                isSecondMateRead = TRUE,
                hasUnmappedMate = FALSE
            ), reverseComplement = TRUE,
            what = c("qname", "seq", "qual")
        )
        ## PROCESSING
        #1.BAM chuncks number
        cycleR2mateUnmapped <- cycleBam
        #2. inizialization
        file.create(
            file.path(                                                                                        
                temporarySource, paste(
                    prefix, "FastqR2mateUnmapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
            )
        )
        #3. iterate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource,
                      paste(prefix, sampleBasename, "_supplementary", sep = "")), 
            yieldSize = subsetting))
        R2mu <- 1
        while (R2mu<=cycleR2mateUnmapped) {
            print(R2mu)
            fromBamToFastq(alignFile = docBam,
                           Setting = fieldR1mateMapped2,
                           OutPut = file.path(
                               temporarySource, paste(
                                   prefix, "FastqR2mateUnmapped2_", sampleBasename, "_Unmapped_R2.fq.gz", sep = "")
                           ))
            R2mu <- R2mu+1
        }
        close(docBam)
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
        ## filtering BAM from unmapped
        ## setting filter parameters
        parameters <- Rsamtools::ScanBamParam(
            flag = Rsamtools::scanBamFlag(
                isPaired = TRUE,
                isUnmappedQuery = FALSE,
                hasUnmappedMate = FALSE
            ),
            what = Rsamtools::scanBamWhat())
        ## sorting bam
        Rsamtools::sortBam(
            file = file.path(temporarySource,
                             paste(prefix, sampleBasename, "_supplementary", sep = "")),
            destination = file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted", sep = "")))
        ## index bam
        Rsamtools::indexBam(files = file.path(temporarySource, paste(
            prefix, sampleBasename, "_sorted", ".bam", sep = "")))
        ## filter and write bam
        Rsamtools::filterBam(
            file = file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted", ".bam", sep = "")),
            destination =
                file.path(temporarySource, paste(
                    prefix, sampleBasename, ".bam", sep = "")),
            param = parameters,
            indexDestination = FALSE)
        ### BAM division/assignment
        ## extracting basename genomes
        GenomeNames <- utils::read.table(
            file.path(paste(pathToGenomeName, ".files", sep = "")),
            header = FALSE
        )
        GenomeNames <- GenomeNames[,1]
        GenomeNames <- base::gsub(pattern = "_.*$", replacement = "", x = GenomeNames)
        GenomeNames <- unique(GenomeNames)
        ## fields bam
        fieldsBam <- c("qname", "rname")
        ## parameters
        parametersBamId <- Rsamtools::ScanBamParam(
            what = fieldsBam
        )
        ## PROCESSING: eXTRACTING list id
        #1. setting chuncks number BAM
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource, paste(
                prefix, sampleBasename, ".bam", sep = "")),
            yieldSize = subsetting))
        cycleFinalBam <- 0
        while(nrec <- length(Rsamtools::scanBam(docBam)[[1]][[1]])) {
            cat("records:", nrec, "\n")
            cycleFinalBam <- cycleFinalBam+1
            print(cycleFinalBam)
        }
        close(docBam)
        #2. INIZIALIZATION
        file.create(
            file.path(temporarySource, paste(
                prefix, "idMappedFiltered_", sampleBasename, "_GenomeOne.txt", sep = ""))
        )
        file.create(
            file.path(temporarySource, paste(
                prefix, "idMappedFiltered_", sampleBasename, "_GenomeTwo.txt", sep = ""))
        )
        #3. iterate,import,class,write
        docBam <- open(Rsamtools::BamFile(
            file.path(temporarySource, paste(
                prefix, sampleBasename, ".bam", sep = "")),
            yieldSize = subsetting))
        R1up <- 1
        while (R1up<=cycleFinalBam) {
            print(R1up)
            fromBamToTextId(alignFile = docBam,
                            Setting = parametersBamId,
                            temp1 = file.path(temporarySource, paste(
                                prefix, "idMappedFiltered_", sampleBasename, "_GenomeOne.txt", sep = "")),
                            temp2 = file.path(temporarySource, paste(
                                prefix, "idMappedFiltered_", sampleBasename, "_GenomeTwo.txt", sep = "")))
            R1up <- R1up+1
        }
        close(docBam)
        ## INTERSECTION: indentify cross-mapping
        #1, inizialization
        file.create(
            file.path(temporarySource, paste(
                prefix, "idCrossMapped_", sampleBasename, ".txt", sep = "")
            )
        )
        #2: function intersection iterated
        intersectFiles <-  function(genomeOneId, genomeTwoId) {
            con1 = file(genomeOneId, "r")
            while (TRUE) {
                line1 = readLines(con1, n = 1000000)
                if ( length(line1) == 0 ) {
                    break
                } else {
                    con2 <- file(genomeTwoId, "r")
                    repeat {
                        line2 <- readLines(con2, n = 1000000)
                        if (length(line2) == 0) break  # Exit the loop if no more lines
                        # Process the chunk of lines
                        commonLine <- intersect(line1, line2)
                        commonLine <- unique(commonLine)
                        if (length(commonLine) != 0) {
                            write(x = commonLine, 
                                  file = file.path(
                                      temporarySource, paste(
                                          prefix, "idCrossMapped_", sampleBasename, ".txt", sep = "")
                                  ),
                                  append = TRUE)
                        }
                    }
                    close(con2)
                }
            }
            close(con1)
        }
        #3: intersection
        intersectFiles(
            genomeOneId = file.path(
                temporarySource, paste(
                    prefix, "idMappedFiltered_", sampleBasename, "_GenomeOne.txt", sep = "")
            ),
            genomeTwoId = file.path(
                temporarySource, paste(
                    prefix, "idMappedFiltered_", sampleBasename, "_GenomeTwo.txt", sep = "")
            ))
        ##### WRITING THE THRE FINAL BAM
        ## SORT bam
        Rsamtools::sortBam(
            file = file.path(temporarySource, paste(
                prefix, sampleBasename, ".bam", sep = "")),
            destination = file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted", sep = "")))
        ## index bam
        Rsamtools::indexBam(Rsamtools::BamFile(
            file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted.bam", sep = ""))
        ))
        ## CROSS-MAPPING: extracting and writing bam
        #1 importing cross-mapped
        want <- readLines(
            con = file.path(
                temporarySource, paste(
                    prefix, "idCrossMapped_", sampleBasename, ".txt", sep = "")
            ))
        want <- unique(want)
        #2 setting filter
        filter_factory <- function(want) {
            list(KeepQname = function(x) x$qname %in% want)
        }
        filter <- S4Vectors::FilterRules(filter_factory(want))
        #3 filter bam
        Rsamtools::filterBam(
            file = file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted.bam", sep = "")),
            destination = file.path(
                outputBam, paste(
                    "CrossMapping_", sampleBasename, ".bam", sep = "")
            ),
            filter=filter,
            param=Rsamtools::ScanBamParam(
                what=Rsamtools::scanBamWhat()),
            indexDestination = FALSE
        )
        #2 complement function iterated
        crossMappingDeletion <- function(toProcess, crossMapped, outPathList) {
            file.create(
                file.path(outPathList)
            )
            con1 = file(toProcess, "r")
            while (TRUE) {
                line1 = readLines(con1, n = 1000000)
                if ( length(line1) == 0 ) {
                    break
                } else {
                    con2 <- file(crossMapped, "r")
                    #inizialization
                    matriceLogica <- vector()
                    repeat {
                        line2 <- readLines(con2, n = 1000000)
                        if (length(line2) == 0) break  # Exit the loop if no more lines
                        matriceLogica <- line1 %in% line2
                        line1 <- line1[!matriceLogica]
                        write(x = line1,
                              file = file.path(outPathList),
                              append = TRUE)
                    } # close repeat loop
                    close(con2)
                }
            } #close while loop
            close(con1)
        }
        #2 complement of both lists
        ## genome 1
        crossMappingDeletion(
            toProcess = file.path(
                temporarySource, paste(
                    prefix, "idMappedFiltered_", sampleBasename, "_GenomeOne.txt", sep = "")
            ),
            crossMapped = file.path(
                temporarySource, paste(
                    prefix, "idCrossMapped_", sampleBasename, ".txt", sep = "")
            ),
            outPathList = file.path(
                temporarySource, 
                paste(prefix, "idGenomeOneDepurated_", sampleBasename, ".txt", sep = ""))
        )
        ## genome 2
        crossMappingDeletion(
            toProcess = file.path(
                temporarySource, paste(
                    prefix, "idMappedFiltered_", sampleBasename, "_GenomeTwo.txt", sep = "")
            ),
            crossMapped = file.path(
                temporarySource, paste(
                    prefix, "idCrossMapped_", sampleBasename, ".txt", sep = "")
            ),
            outPathList = file.path(
                temporarySource, 
                paste(prefix, "idGenomeTwoDepurated_", sampleBasename, ".txt", sep = ""))
        )
        ## FINAL BAM: writing distinct bam
        #1 genome 1
        wantGenomeOne <- readLines(con = file.path(
            temporarySource, 
            paste(prefix, "idGenomeOneDepurated_", sampleBasename, ".txt", sep = "")
        ))
        wantGenomeOne <- unique(wantGenomeOne)
        ##
        filter_factory_One <- function(wantGenomeOne) {
            list(KeepQname = function(x) x$qname %in% wantGenomeOne)
        }
        filterOne <- S4Vectors::FilterRules(filter_factory_One(wantGenomeOne))
        Rsamtools::filterBam(file = 
                                 file.path(temporarySource, paste(
                                     prefix, sampleBasename, "_sorted.bam", sep = "")),
                             destination = file.path(outputBam, paste(
                                 prefix, "_", sampleBasename, "_", GenomeNames[1], ".bam", sep = "")),
                             filter=filterOne,
                             param=Rsamtools::ScanBamParam(
                                 what=Rsamtools::scanBamWhat()),
                             indexDestination=FALSE)
        #2 genome 2
        wantGenomeTwo <- readLines(con = file.path(
            temporarySource, 
            paste(prefix, "idGenomeTwoDepurated_", sampleBasename, ".txt", sep = "")
        ))
        wantGenomeTwo <- unique(wantGenomeTwo)
        ##
        filter_factory_Two <- function(wantGenomeTwo) {
            list(KeepQname = function(x) x$qname %in% wantGenomeTwo)
        }
        filterTwo <- S4Vectors::FilterRules(filter_factory_Two(wantGenomeTwo))
        Rsamtools::filterBam(
            file = file.path(temporarySource, paste(
                prefix, sampleBasename, "_sorted.bam", sep = ""))
            , 
            destination = file.path(
                outputBam, paste(
                    prefix, "_", sampleBasename, "_", GenomeNames[2], ".bam", sep = "")
            ), 
            filter=filterTwo,
            param=Rsamtools::ScanBamParam(
                what=Rsamtools::scanBamWhat()),
            indexDestination=FALSE)
        ### removing the three lists
        rm(want,
           wantGenomeOne,
           wantGenomeTwo)
        ### end
    }
    ###### parallelization
    parallel::parLapply(cl = clust <- parallel::makeCluster(spec = nodes, type = "PSOCK"),
                        X = lalista,
                        fun = function(x,
                                       readsPath,
                                       GenomeConcIndex,
                                       outBam,
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
                            ## alignment
                            Rsubread::subjunc(
                                readfile1 = file.path(readsPath,
                                                      list.files(
                                                          path = readsPath,
                                                          pattern =
                                                              paste(x, "_1.f",
                                                                    sep = "")
                                                      )
                                ),
                                readfile2 = file.path(readsPath,
                                                      list.files(
                                                          path = readsPath,
                                                          pattern =
                                                              paste(x, "_2.f",
                                                                    sep = "")
                                                      )
                                ),
                                index = GenomeConcIndex,
                                output_file = file.path(tempfolder,
                                                        paste("Alignment", x, "_supplementary", sep = "")
                                ),
                                nthreads = threads,
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
                            )## end of subjunc
                            ## extracting unmapped sequences
                            ## assign sequences to each genome
                            DownstreamOperations(temporarySource = tempfolder,
                                                 outputBam = outBam,
                                                 prefix = "Alignment",
                                                 sampleBasename = x,
                                                 pathToGenomeName = GenomeConcIndex,
                                                 subsetting = readsAlignedBlock
                            )
                            ## rescue indel.vcf
                            fileRescue <- c(".indel.vcf", ".junction.bed", ".summary")
                            file.rename(
                                from = file.path(tempfolder,
                                                 paste("Alignment", x, "_supplementary", fileRescue, sep = "")),
                                to = file.path(outBam,
                                               paste("Alignment", "_", x, fileRescue, sep = ""))
                            )
                            ### elicit freeing memory RAM
                            base::gc(reset = TRUE)
                        }, ## end of the function body
                        readsPath,
                        GenomeConcIndex,
                        outBam,
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
    base::on.exit(parallel::stopCluster(cl = clust)) ## end of node communications
    unlink(tempfolder, recursive = TRUE)
}