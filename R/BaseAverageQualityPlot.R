#' BaseAverageQualityPlot
#'
#' @param input_data folder containing data
BaseAverageQualityPlot <- function(input_data) {
  df.base.average.quality <-        
    read.csv(file = file.path(
      input_data, "Average_quality_table.csv"),
      colClasses = 
        c("factor", "numeric", "numeric"))
  df.base.average.quality$cycle <- as.factor(df.base.average.quality$cycle)
  len <- max(as.integer(df.base.average.quality$cycle))
  ggplot2::ggplot(
    data = df.base.average.quality,
    ggplot2::aes_string(x = "cycle", y = "quality", colour = "filename")) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes_string(group = "filename")) +
    ggplot2::labs(x = "position in read (bp)", 
                  y = "Average quality", 
                  colour = "Filename") +
    ggplot2::scale_x_discrete(breaks = seq(from = 1, 
                                           to = len, by = len%/%20)) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Base average quality") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}