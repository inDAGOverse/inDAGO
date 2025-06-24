#' interactive GCcontentDistributionPlot
#'
#' @param input_data samples folder
GCcontentDistributionPlotly <- function(input_data) {
  df.gcContentDistribution <- 
    read.csv(file = file.path(
      input_data, "GC_content_distribution.csv"
    ))
  ## importing
  df.gcContentDistribution <- 
    read.csv(
      file = file.path(input_data, "GC_content_distribution.csv"))
  ## plot
  plot <- 
    plotly::ggplotly(
      ggplot2::ggplot(
        data = df.gcContentDistribution,
        ggplot2::aes(x = GCpercentage, colour = Sample)) +
        ggplot2::geom_density() +
        ggplot2::theme_bw()
    ) ## close ggplotly
  return(plot)
} #close funzione