#' Saturation
#'
#' Generate a saturation curve plot showing gene detection versus sequencing depth.
#'
#' This function estimates how many genes are detected at increasing read depths using
#' a rarefaction-based approach ( "estimate_saturation() from RNAseQC"), and plots the
#' saturation curves for each sample. It supports two estimation methods: “division”
#' for a fast analytic approximation and “sampling” for more realistic approach.
#'
#' @param matrix Numeric matrix or object coercible to matrix (genes × samples), e.g., log-counts or raw counts.
#'   Genes are rows; samples are columns.
#' @param method Character. Estimation method: "division" or "sampling".
#' @param max_reads Numeric. Maximum number of reads to include in the rarefaction (default: Inf).
#' @param palette Character. Name of a discrete color palette from the  "paletteer " package for curve colors.
#'
#' @return A  "ggplot " object showing saturation (genes detected) versus sequencing depth for each sample.
#'
#' @details
#' 1. Internally,  "extract_counts() " (from countSubsetNorm) extracts a counts matrix from
#'    various input classes (matrix, DGEList, EList, ExpressionSet).
#' 2.  "estimate_saturation() " (from RNAseQC) rarefies each library at multiple depths:
#'    - “division” divides counts by scale factors;
#'    - “sampling” performs repeated random sampling to simulate read down sampling.
#' 3. The resulting data frame contains one row per sample per depth, with the number of
#'    detected genes ( "sat ") and, for sampling, its variance ( "sat.var ").
#' 4. The function then plots gene saturation curves ( "sat" vs.  "depth") colored by sample.
#'
Saturation <- function(matrix, method, max_reads, palette){

  # Loads the estimate_saturation() function and its dependency, used to perform the saturation plot.
  # The estimate_saturation() function is available with the RNAseQC package
  # As a dependency, the extract_counts() function is available with the countSubsetNorm package

  ### extract_counts()

  #' Extract counts matrix from different types of expression objects
  #'
  extract_counts <-
    function(counts, return_class=NULL) {
      if (inherits(counts, "EList")) {
        counts <- counts[["E"]]
      } else if (inherits(counts, "DGEList")) {
        counts <- counts[["counts"]]
      } else if (inherits(counts, "ExpressionSet") | inherits(counts, "eSet")) {
        if (is.environment(counts@assayData)) {
          counts <- counts@assayData[["exprs"]]
        } else if (is.matrix(counts@assayData)) {
          counts <- counts@assayData
        }
      } else if (!(is.matrix(counts) | is.data.frame(counts)))
        stop("Class of input \"counts\" object not recognized. Please check object class for compatability with this function.")

      if (!is.null(return_class)) counts <- as(counts, return_class)

      counts
    }


  ### estimate_saturation()

  #' Estimate saturation of genes based on rarefaction of reads
  #'
  estimate_saturation <-
    function(counts, max_reads = Inf,
             method = "sampling",
             ndepths = 6, nreps = 5,
             min_counts = 1, min_cpm = NULL,
             verbose = FALSE) {

      # extract counts and/or convert to matrix
      counts <-
        extract_counts(counts, return_class = "matrix") # from countSubsetNorm package

      # check inputs
      checkmate::assert_array(counts)
      checkmate::assert_numeric(counts)
      if (nrow(counts) < ncol(counts))
        warning("The input counts object has more columns than rows. ",
                "Are you sure the rows are genes and the columns are samples?")

      checkmate::assert_numeric(max_reads, lower = 0)
      checkmate::assert_character(method)
      method <- match.arg(method, choices = c("division", "sampling"))

      checkmate::assert_integerish(ndepths, lower = 1)

      checkmate::assert_numeric(min_counts, null.ok = TRUE)
      checkmate::assert_numeric(min_cpm, null.ok = TRUE)
      if (sum(!is.null(min_counts), !is.null(min_cpm)) != 1)
        stop("One of min_counts or min_cpm must be specified, but not both.")

      if (!is.null(min_cpm) && method == "division")
        stop("Thresholding by min_cpm is only relevant with method = 'sampling'.")

      checkmate::assert_logical(verbose)

      # calculate characteristics of libraries and calculations
      readsums <- colSums(counts)
      max_reads <- min(max(readsums), max_reads)
      depths <- round(seq(from = 0, to = max_reads, length.out = ndepths+1))

      # generate empty data frame to store results
      saturation <-
        data.frame(sample = as.vector(sapply(colnames(counts), FUN = rep, times = ndepths+1)),
                   depth = rep(depths, time = ncol(counts)))
      sat_estimates <- as.numeric(rep(NA, ncol(counts) * length(depths)))

      # create an iterator for rows of the data frame
      counter <- 0
      if (method == "sampling") {
        checkmate::assert_integerish(nreps, lower = 1)
        sat_var_estimates <- as.numeric(rep(NA, ncol(counts) * length(depths)))
      }

      # iterate over each column in counts, and each depth
      for (lib_current in seq_len(ncol(counts))) {
        if (verbose) cat("Working on library", lib_current, "of", ncol(counts), "\n")

        # calculate gene probabilities for the focal library
        probs <- counts[, lib_current, drop = TRUE] / readsums[lib_current]
        probs <- probs[probs > 0] # zero counts add nothing but computational time!
        ngenes <- length(probs)

        # calculate for each depth in the focal library
        for (depth_current in depths) {
          counter <- counter + 1
          if (depth_current == 0) {
            sat_estimates[counter] <- 0
            if (method == "sampling")
              sat_var_estimates[counter] <- 0
          } else if (depth_current > readsums[lib_current]) {
            sat_estimates[counter] <- NA
            if (method == "sampling")
              sat_var_estimates[counter] <- NA
          } else if (method == "division") {
            sat_estimates[counter] <- sum((probs * depth_current) >= min_counts)
          } else if (method == "sampling") {
            sat_estimate_lib_depth_current <- as.numeric(rep(NA, nreps))

            if (!is.null(min_cpm)) {
              min_counts_lib_current <- depth_current * min_cpm / 1E6
            } else min_counts_lib_current <- min_counts

            for (rep_current in seq_len(nreps)) {
              reads <- as.matrix(sample.int(n = ngenes, size = depth_current, replace = TRUE, prob = probs))
              sat_estimate_lib_depth_current[rep_current] <-
                sum(bigtabulate::bigtable(reads, ccol = 1) >= min_counts_lib_current)
            }
            sat_estimates[counter] <- mean(sat_estimate_lib_depth_current)
            sat_var_estimates[counter] <- var(sat_estimate_lib_depth_current)
          }
        }
      }
      saturation$sat <- sat_estimates
      if (method == "sampling")
        saturation$sat.var <- sat_var_estimates

      return(saturation)
    }


  ### Applying function estimate_saturation()


  # 'matrix' is the logcounts matrix from edgeR::cpm
  # 'estimate_saturation()' function from RNAseQC package https://github.com/BenaroyaResearch/RNAseQC.git
  Es <- estimate_saturation(counts = as.matrix(matrix), method = method, max_reads = max_reads)
  # 'method' specifies the estimation method
  # 'max_reads' sets the maximum number of reads to consider for saturation estimation

  # Create a saturation plot using ggplot2
  p <- ggplot2::ggplot(data = Es, mapping = ggplot2::aes(x = depth, y = sat, group = sample, color = sample)) +

    # Add lines to represent saturation across different samples
    ggplot2::geom_line(linewidth = 1, alpha = 1, linetype = 1, na.rm = TRUE) +

    # Label the x-axis as "Depth"
    ggplot2::xlab("Depth") +

    # Label the y-axis as "Saturation"
    ggplot2::ylab("Saturation") +

    # Set the title of the plot
    ggplot2::ggtitle("Saturation Plot") +

    # Apply a color palette to differentiate between samples
    paletteer::scale_color_paletteer_d(palette) +

    # Use a minimal theme for the plot
    ggplot2::theme_minimal() +

    # Customize the plot's appearance
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "black", size = 20, face = "bold.italic", hjust = 0.5), # Centered bold title
      axis.title.x = ggplot2::element_text(color = "black", size = 14, face = "bold"), # Bold x-axis title
      axis.title.y = ggplot2::element_text(color = "black", size = 14, face = "bold")  # Bold y-axis title
    )
  return(p)
}
