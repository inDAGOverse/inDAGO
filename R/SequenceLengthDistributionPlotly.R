#' interactive SequenceLengthDistributionPlot
#'
#' @param input_data result tables folder
SequenceLengthDistributionPlotly <- function(input_data) {
  df.seq.read.length <-
    read.csv(file = file.path(
      input_data, "Sequence_length_distribution.csv"))
  plotly::ggplotly(
    ggplot2::ggplot(data=df.seq.read.length,
                    ggplot2::aes_string(x="width", y="percentage", group = "filename")) +
      ggplot2::geom_line(mapping = ggplot2::aes_string(colour = "filename")) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "position in read (bp)", y = "Read (%)") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Sequence length distribution") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  )
}