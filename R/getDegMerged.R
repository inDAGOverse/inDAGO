#' getDegMerged
#'
#' Merge multiple DEG result CSVs with GTF annotations into a single data frame.
#'
#' This function reads all CSV files in a directory, validates presence of required
#' columns ("ID", and optionally "diffExp"), filters for up/down regulated genes if requested,
#' extracts annotation fields from a GTF, and returns a merged table of selected annotation
#' columns alongside all DEG metrics (with optional file-based column prefixes).
#'
#' @param path Character. Directory containing DEG result CSV files.
#' @param gtfPath Character. Path to the GTF annotation file.
#' @param columns Character vector. Names of annotation columns to include from the GTF.
#' @param collapseName Logical. If TRUE, strip method/model prefixes from file names when prefixing columns.
#' @param typeFilter Character. GTF feature type to filter (e.g., "gene" or "transcript").
#' @param selectUpDown Logical. If TRUE, only include IDs with "diffExp" == UP or DOWN.
#'
#' @return A combined data frame
#'
getDegMerged <- function(path,  gtfPath, columns, collapseName, typeFilter, selectUpDown) {

  # Set the directory path for input files
  path <- file.path(path)

  # List all CSV files in the specified directory
  files <- list.files(path, pattern = "*.csv")

  # Read each CSV file into a list of dataframes
  df_list <- sapply(files, function(i) {
    file <- read.csv(paste0(path, "/", i))
    assign(i, file)
  }, simplify = FALSE, USE.NAMES = TRUE)


  # Check each dataframe for the presence of the 'ID' column and for duplicates
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]

    # Stop execution if 'ID' column is missing
    if (!"ID" %in% colnames(df)) {
      stop(paste("The file", names(df_list)[i], "doen not have ID column"))
    }

    # Stop execution if there are duplicate IDs
    if (any(duplicated(df[["ID"]]))) {
      stop(paste("The file", names(df_list)[i], "has dupliated value in ID"))
    }
  }

  # Check each dataframe for the presence of the 'diffExp' column
  if (selectUpDown == "TRUE") {

    for (i in seq_along(df_list)) {
      df <- df_list[[i]]

      # Stop execution if 'diffExp' column is missing
      if (!"diffExp" %in% colnames(df)) {
        stop(paste("The file", names(df_list)[i], "doen not have diffExp column"))
      }
    }
    Vector <- magrittr::`%>%`(lapply(df_list, function(i) {
      dplyr::filter(i, diffExp == "UP" | diffExp == "DOWN")[["ID"]]
    }), do.call(base::c, .))  # Combine results into a single vector
  }else {
    Vector <- magrittr::`%>%`(lapply(df_list, function(i) {
      i[["ID"]]
    }), do.call(base::c, .))  # Combine results into a single vector
  }


  # Import the GTF file as a dataframe
  Aa_gtf <- as.data.frame(rtracklayer::import(file.path(gtfPath)))
  Aa_gene <- dplyr::filter(Aa_gtf, type == typeFilter)  # Filter GTF by type

  # Select unique IDs and convert to dataframe
  masterVector <- magrittr::`%>%`(as.data.frame(unique(Vector)), `names<-`("ID"))

  # get column name in which are stored the 'ID' used
  name <- sapply(1:ncol(Aa_gene), function(i){
    check  <- all(masterVector$ID %in% Aa_gene[[i]])
    if (check == "TRUE") {
      colnames(Aa_gene)[i]
    } else {print("no")}
  })
  IDCol <- colnames(Aa_gene)[!unlist(Map(is.null, name))][1]

  # Select unique IDs and convert to dataframe
  colnames(masterVector) <- IDCol

  # Select from 'GTF' file chosen columns unique and merge with column ID
  dfMaster <-  magrittr::`%>%`(dplyr::left_join(masterVector,Aa_gene, by = IDCol), dplyr::select(all_of(IDCol), dplyr::all_of(columns)))

  # Rename columns of each dataframe, prefixing with the dataframe name
  df_list_renamed <- lapply(seq_along(df_list), function(i) {
    df <- df_list[[i]]

    # Optionally collapse file names by removing specific patterns
    if (collapseName == "TRUE") {
      df_name <- magrittr::`%>%`(magrittr::`%>%`(names(df_list)[i], gsub("filterByExpr_|HTSFilter_|exactTest_|glmQLFTest_|glmLRT_", "", .)), tools::file_path_sans_ext(.))
    } else {
      df_name <- magrittr::`%>%`(names(df_list)[i], tools::file_path_sans_ext(.))
    }

    # Rename columns, keeping 'ID' unchanged
    colnames(df)[-1] <- paste(df_name, colnames(df)[-1], sep = "_")
    return(df)  # Return the renamed dataframe
  })

  # Merge all dataframes in the list based on 'ID'
  merged_df <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), df_list_renamed)

  # change the 'ID' column name with corresponding name used in 'GTF'
  names(merged_df)[1] <- IDCol

  # Join with the master dataframe based on 'ID'
  merged_df <- dplyr::left_join(dfMaster, merged_df, by = IDCol)

  # Return the merged and ordered dataframe
  return(merged_df)
}
