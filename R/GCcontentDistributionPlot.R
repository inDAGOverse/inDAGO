#' GCcontentDistributionPlot
#'
#' @param input_data samples folder
GCcontentDistributionPlot <- function(input_data) {
  ## importing
  df.gcContentDistribution <- 
    read.csv(file = file.path(
      input_data, "GC_content_distribution.csv"
    ))
  ## plot
  plot <- 
    ggplot2::ggplot(
      data = df.gcContentDistribution,
      ggplot2::aes(x = GCpercentage, colour = Sample)) +
    ggplot2::geom_density() +
    ggplot2::theme_bw()
  return(plot)
} #close funzione