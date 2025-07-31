#' COUNTING SEQUENCES
#'
#' @param input_data sample folder
counting_Reads <- function(input_data) {
  theCount <- 
    ShortRead::countFastq(
      dirPath = file.path(input_data),
      pattern = ".fastq|.fastq.gz|.fq|.fq.gz")
  files <- rownames(theCount)
  theCount <- cbind(files,theCount)
  theCount <- theCount[,-4]
  return(theCount)
}