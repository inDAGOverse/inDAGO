#' checkMetadata
#'
#' Validate and extract non-empty annotation fields from a GTF file.
#'
#' This function imports a GTF file, filters entries by a specified feature type,
#' and identifies metadata columns that contain at least one non-missing value.
#'
#' @param gtfPath Character. Path to the directory or file location of the GTF file.
#' @param typeFilter Character. The feature type to filter on (e.g., "gene", "exon").
#'
#' @return Character vector of column names in the GTF annotation that are not entirely NA or empty.
#'
#' @details
#' 1. Imports the GTF into a data frame via "rtracklayer::import()".
#' 2. Filters rows by "type" == typeFilter.
#' 3. Tests each column for all-NA or empty-string entries.
#' 4. Returns names of columns with at least one non-missing, non-empty value.
#'
checkMetadata <- function(gtfPath, typeFilter) {

  # Import the GTF file as a dataframe
  Aa_gtf <- as.data.frame(rtracklayer::import(file.path(gtfPath)))

  # Filter the dataframe for the specified type
  Aa_gene <- dplyr::filter(Aa_gtf, type == typeFilter)

  # Identify columns that do not consist entirely of NA values
  keep <- apply(Aa_gene, 2, function(x) !all(is.na(x) | x == '' ))

  # Get the names of the columns to keep (those that are not all NA)
  chooseName <- colnames(Aa_gene[, keep])

  # Return the names of the selected columns
  return(chooseName)
}
