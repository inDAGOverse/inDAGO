#' QUALITY CONTROL ANALYSIS
#'
#' @param directoryInput sample directory
#' @param inputFormat raw read format
#' @param Nodes cores
#' @param ReadsNumber chunk 
#' @param directoryOutput output folder
#' @param tempFolder temporary folder
QualityCheckAnalysis <- function(
    directoryInput, inputFormat, 
    Nodes,
    ReadsNumber,
    directoryOutput,
    tempFolder
) {
  ##### baseContent_length_Distribution #####
  baseContent_length_Distribution <- function (
    directoryOutput,
    Nodes,
    directoryInput,
    ReadsNumber,
    tempFolder
  ) {
    ## parallelization
    parallel::parLapply(
      ## argument 1: cluster
      cl =  cl <- parallel::makeCluster(Nodes, type = "SOCK"),
      ## argument 2: list
      X = lapply(
        1:length(list.files(directoryInput, 
                            pattern = ".fastq|.fastq.gz|.fq|.fq.gz")), 
        function (m) {
          list.files(directoryInput)[m]
        }),
      ## argument 3: function
      fun = function(
    x,
    directoryInput,
    ReadsNumber,
    tempFolder
      ) {
        ## sequence path
        ReadToAnalyze <- file.path(directoryInput, x)
        ## Load n records at a time.
        sampler <- ShortRead::FastqSampler(ReadToAnalyze,
                                           n = ReadsNumber
        )
        fq <- ShortRead::yield(sampler)
        ## length distribution
        linePlotLength <- as.data.frame(table(ShortRead::width(fq)))
        colnames(linePlotLength) <- c("width","count")
        linePlotLength$width <- as.factor(linePlotLength$width)
        filename <- as.factor(x)
        linePlotLength <- cbind(filename,linePlotLength)
        linePlotLength$percentage <- as.numeric(round((linePlotLength$count/length(fq))*100,digits = 2))
        ## write results
        write.table(
          x = linePlotLength,
          file = file.path(
            tempFolder, paste(
              gsub(pattern = ".fastq|.fastq.gz|.fq|.fq.gz",
                   replacement = "", x = x),
              "_sequence_length_distribution.tab", sep = "")),
          row.names = FALSE,
          quote = FALSE)
        ## base frequency
        linePlotBasi <-
          ShortRead::alphabetByCycle(ShortRead::sread(fq))
        linePlotBasi <-
          linePlotBasi[rowSums(linePlotBasi) != 0,]
        colnames(linePlotBasi) <- 1:ncol(linePlotBasi)
        linePlotBasi <- t(linePlotBasi)
        cycle <- row.names(linePlotBasi)
        linePlotBasi <- cbind(cycle, linePlotBasi)
        linePlotBasi <- as.data.frame(linePlotBasi)
        linePlotBasi <-
          tidyr::pivot_longer(linePlotBasi, cols = !cycle, names_to = "variable", values_to = "value")
        filename <- x
        linePlotBasi <- cbind(filename, linePlotBasi)
        linePlotBasi$value <- as.numeric(linePlotBasi$value)
        linePlotBasi$value <- round((linePlotBasi$value/length(fq))*100,digits = 2)
        linePlotBasi$filename <- as.factor(linePlotBasi$filename)
        linePlotBasi$variable <- as.factor(linePlotBasi$variable)
        linePlotBasi$cycle <- as.factor(linePlotBasi$cycle)
        write.table(
          x = linePlotBasi,
          file = file.path(
            tempFolder, paste(
              gsub(pattern = ".fastq|.fastq.gz|.fq|.fq.gz",
                   replacement = "", x = x),
              "_ATCGNcontent.tab", sep = "")),
          row.names = FALSE,
          quote = FALSE)
        ## cleaning RAM
        rm(fq,linePlotLength,linePlotBasi)
        base::gc(reset = TRUE)
      }, # end argument fun
    directoryInput,
    ReadsNumber,
    tempFolder
    ) ## close parlapply
    parallel::stopCluster(cl = parallel::makeCluster(Nodes, type = "SOCK"))
    ###### NOT PARALLELIZED CODE
    ##### BASE COMPOSITION #####
    ## paths
    ATGCN_distributionPaths <-
      file.path(tempFolder, list.files(tempFolder, pattern = "_ATCGNcontent.tab"))
    ## importing
    list.atgcmContentDistribution <- list()
    for (i in ATGCN_distributionPaths) {
      table <- read.table(
        file = i, header = TRUE
      )
      list.atgcmContentDistribution[[i]] <- table
    }        
    ## one table
    df.atgcnContentDistribution <- 
      do.call(rbind, list.atgcmContentDistribution)
    ## writing results
    write.csv(x = df.atgcnContentDistribution,
              file = file.path(directoryOutput, "ATCGN_content_per_base.csv"),
              quote = FALSE, row.names = FALSE)
    ## cleaning RAM
    rm(list.atgcmContentDistribution,df.atgcnContentDistribution)
    ##### LENGTH COMPOSITION #####
    ## paths
    length_distributionPaths <-
      file.path(tempFolder, list.files(tempFolder, pattern = "_sequence_length_distribution.tab"))
    ## importing
    list.lengthDistribution <- list()
    for (i in length_distributionPaths) {
      table <- read.table(
        file = i, header = TRUE
      )
      list.lengthDistribution[[i]] <- table
    }        
    ## one table
    df.lengthDistribution <- 
      do.call(rbind, list.lengthDistribution)
    ## writing results
    write.csv(x = df.lengthDistribution,
              file = file.path(directoryOutput, "Sequence_length_distribution.csv"),
              quote = FALSE, row.names = FALSE)
    ## cleaning RAM
    rm(list.lengthDistribution,df.lengthDistribution)
  } # close baseContent_length_Distribution function
  
  ##### GCcontentDistribution FUNCTION #####
  GCcontentDistribution <- function (
    Nodes,
    directoryInput,
    ReadsNumber,
    directoryOutput,
    tempFolder
  ) {
    TheFinalList <-
      parallel::parLapply(
        cl <- parallel::makeCluster(Nodes, type = "SOCK"),
        X = lapply(
          1:magrittr::'%>%'(list.files(
            directoryInput, 
            pattern = ".fastq|.fastq.gz|.fq|.fq.gz"), 
            length()), 
          function (x) {
            list.files(directoryInput)[x]
          }),
        fun = function(
    x,
    directoryInput,
    ReadsNumber,
    tempFolder
        ) {
          ## empty file generation
          file.create(file.path(
            tempFolder, paste(
              gsub(pattern = ".fastq|.fastq.gz|.fq|.fq.gz",
                   replacement = "", x = x),
              "_GCcontent.tab", sep = "")))
          ## path
          ReadToAnalyze <- XVector::open_input_files(
            file.path(directoryInput, x)
          )
          ## Load n records at a time.
          reads <- Biostrings::readDNAStringSet(
            ReadToAnalyze, 
            format = "fastq",
            nrec = ReadsNumber)
          ## gc frequency
          gc_frequency <- 
            round(
              Biostrings::letterFrequency(reads, letters = "GC")/
                BiocGenerics::width(reads),
              digits = 2)
          ## writing results
          write.table(
            x = gc_frequency, 
            file = file.path(
              tempFolder, paste(
                gsub(pattern = ".fastq|.fastq.gz|.fq|.fq.gz",
                     replacement = "", x = x),
                "_GCcontent.tab", sep = "")), 
            append = TRUE,
            row.names = FALSE,
            col.names = FALSE, 
            quote = FALSE)
          ## cleaning RAM
          rm(reads,gc_frequency)
          base::gc(reset = TRUE)
        } # end argument fun
    ,
    directoryInput,
    ReadsNumber,
    tempFolder
      ) ## close parlapply
    parallel::stopCluster(cl = parallel::makeCluster(Nodes, type = "SOCK"))
    ### NOT PARALLELIZED CODE
    ## paths
    GCdistributionPaths <- 
      file.path(tempFolder, list.files(tempFolder, pattern = "_GCcontent.tab"))
    ## importing
    list.gcContentDistribution <- list()
    for (i in 1:length(GCdistributionPaths)) {
      df.gcContentDistribution <- data.frame(
        Sample = 
          gsub(pattern = paste(tempFolder, "/", sep = ""), replacement = "", 
               x = GCdistributionPaths[i]), 
        GCpercentage = as.numeric(readLines(con = file.path(GCdistributionPaths[i])))
      )
      df.gcContentDistribution$Sample <-
        gsub(pattern = "_GCcontent.tab$", replacement = "",
             x = df.gcContentDistribution$Sample)
      list.gcContentDistribution[[i]] <- df.gcContentDistribution
      names(list.gcContentDistribution)[i] <-
        gsub(pattern = tempFolder, replacement = "",
             x = GCdistributionPaths[i])
      names(list.gcContentDistribution)[i] <-
        gsub(pattern = "_GCcontent.tab$", replacement = "",
             x = names(list.gcContentDistribution[i]))
    }
    ## one table
    df.gcContentDistribution <- do.call(what = rbind, args = list.gcContentDistribution)
    ## writing results
    write.csv(x = df.gcContentDistribution, 
              file = file.path(directoryOutput, "GC_content_distribution.csv"), 
              quote = FALSE, row.names = FALSE)
  } # close function
  
  ##### baseQuality_distribution_average FUNCTION #####
  baseQuality_distribution_average <- function(
    Nodes,
    directoryInput,
    ReadsNumber,
    tempFolder,
    directoryOutput
  ) {
    ## parallelization
    parallel::parLapply(
      ##### ARGUMENT 1: cluster
      cl =  cl <- parallel::makeCluster(Nodes, type = "SOCK"),
      ##### ARGUMENT 2: list
      X = lapply(
        1:length(list.files(directoryInput, 
                            pattern = ".fastq|.fastq.gz|.fq|.fq.gz")), 
        function (m) {
          list.files(directoryInput)[m]
        }),
      ##### ARGUMENT 3: function
      fun = function (
    x,
    directoryInput,
    ReadsNumber,
    tempFolder
      ) {
        # sequence path
        ReadToAnalyze <- file.path(directoryInput, x)
        ## Load n records at a time.
        sampler <- ShortRead::FastqSampler(ReadToAnalyze,
                                           n = ReadsNumber
        )
        fq <- ShortRead::yield(sampler)
        ## quality value distribution
        ## values list
        qualityPerCycle <-
          ShortRead::alphabetByCycle(Biostrings::quality(fq))
        ## deleting
        qualityPerCycle <-
          qualityPerCycle[rowSums(qualityPerCycle) != 0,]
        ## t
        qualityPerCycle <- t(qualityPerCycle)
        ## decoding
        decodifica <-
          ShortRead::encoding(Biostrings::quality(fq))
        decodifica <- data.frame(
          value=names(decodifica),
          decoding=decodifica
        )
        columnNames <- vector()
        for (i in colnames(qualityPerCycle)) {
          if(i == decodifica[i,]$value) {
            columnNames[i] <- decodifica[i,]$decoding
          }
        }
        if(identical(names(columnNames),colnames(qualityPerCycle))) {
          colnames(qualityPerCycle) <- columnNames
        }
        ## quality value list
        ## clustering per position
        qualityValueList <-
          lapply(1:nrow(qualityPerCycle), function(x) {
            rep(
              as.integer(colnames(qualityPerCycle)[1:ncol(qualityPerCycle)]), 
              qualityPerCycle[x,1:ncol(qualityPerCycle)]
            )
          })
        ## summary() applied to the distribution
        qualityValueList <- 
          lapply(qualityValueList, summary)
        ## from vector to table
        qualityValueList <- 
          lapply(qualityValueList, function(x) {
            tableDistQ <- data.frame(
              "Min." = ".",
              "1st Qu." = ".",
              "Median" = ".",
              "Mean" = ".",
              "3rd Qu." = ".",
              "Max." = "."
            )
            tableDistQ[1,] <- x
          })
        ## ECCE TABELLA!
        qualityValueTable <- 
          as.data.frame(
            do.call(rbind, qualityValueList)
          )
        ### data for boxplot
        qualityDistributionTable <-
          qualityValueTable[,-grep("Mean",colnames(qualityValueTable))]
        colnames(qualityDistributionTable) <-
          c("ymin","lower","middle","upper","ymax")
        filename <- as.factor(x)
        cycle <- as.factor(rownames(qualityDistributionTable))
        qualityDistributionTable <-
          cbind(filename, cycle, qualityDistributionTable)
        ## writing results
        write.table(
          x = qualityDistributionTable,
          file = file.path(
            tempFolder, paste(
              gsub(pattern = ".fastq|.fastq.gz|.fq|.fq.gz",
                   replacement = "", x = x),
              "_qualityDistributionTable.tab", sep = "")),
          row.names = FALSE,
          quote = FALSE)
        ### quality value average
        averageQualityTable <-
          data.frame(
            cycle = as.numeric(rownames(qualityValueTable)),
            quality = as.numeric(round(qualityValueTable$Mean,digits = 6)))
        filename <- as.factor(x)
        averageQualityTable <-
          cbind(filename, averageQualityTable)
        averageQualityTable$cycle <- as.factor(averageQualityTable$cycle)
        ## writing table
        write.table(
          x = averageQualityTable,
          file = file.path(
            tempFolder, paste(
              gsub(pattern = ".fastq|.fastq.gz|.fq|.fq.gz",
                   replacement = "", x = x),
              "_averageQualityTable.tab", sep = "")),
          row.names = FALSE,
          quote = FALSE)
        ## cleaning RAM
        base::rm(fq,
                 qualityPerCycle,qualityValueList,qualityValueTable,qualityDistributionTable,
                 averageQualityTable)
        base::gc(reset = TRUE)
      }, # end argument fun
    directoryInput,
    ReadsNumber,
    tempFolder
    ) ## close parlapply
    parallel::stopCluster(cl = parallel::makeCluster(Nodes, type = "SOCK"))
    ## not parallelized code
    ##### QUALITY DISTRIBUTION #####
    ## paths
    QUALITY_distributionPaths <-
      file.path(tempFolder, list.files(tempFolder, pattern = "_qualityDistributionTable.tab"))
    ## importing
    list.qualityValueDistribution <- list()
    for (i in QUALITY_distributionPaths) {
      table <- read.table(
        file = i, header = TRUE
      )
      list.qualityValueDistribution[[i]] <- table
    }        
    ## one table
    df.qualityValueDistribution <- 
      do.call(rbind, list.qualityValueDistribution)
    ## writing results
    write.csv(x = df.qualityValueDistribution,
              file = file.path(directoryOutput, "Quality_value_distribution.csv"),
              quote = FALSE, row.names = FALSE)
    ##### QUALITY DISTRIBUTION #####
    ## paths
    QUALITY_averagePaths <-
      file.path(tempFolder, list.files(tempFolder, pattern = "_averageQualityTable.tab"))
    ## importing
    list.qualityAverage <- list()
    for (i in QUALITY_averagePaths) {
      table <- read.table(
        file = i, header = TRUE
      )
      list.qualityAverage[[i]] <- table
    }        
    ## one table
    df.qualityAverageValue <- 
      do.call(rbind, list.qualityAverage)
    ## writing results
    write.csv(x = df.qualityAverageValue,
              file = file.path(directoryOutput, "Average_quality_table.csv"),
              quote = FALSE, row.names = FALSE)
    
  } ## close baseQuality_distribution_average function
  
  ##### quality control analysis execution #####
  baseContent_length_Distribution(
    directoryOutput = directoryOutput,
    Nodes = Nodes,
    directoryInput = directoryInput,
    ReadsNumber = ReadsNumber,
    tempFolder = tempFolder)
  GCcontentDistribution(
    Nodes = Nodes,
    directoryInput = directoryInput,
    ReadsNumber = ReadsNumber,
    directoryOutput = directoryOutput,
    tempFolder = tempFolder)
  baseQuality_distribution_average(
    Nodes = Nodes,
    directoryInput = directoryInput,
    ReadsNumber = ReadsNumber,
    tempFolder = tempFolder,
    directoryOutput = directoryOutput)
  ## deleting temporary folder
  unlink(tempFolder, recursive = TRUE)
} ## close unique function
