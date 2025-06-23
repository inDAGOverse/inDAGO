#' BaseCompositionLinePlot
#'
#' @param input_data folder containing data
BaseCompositionLinePlot <- function(input_data) {
  df.base.composition.area.chart <-
    read.csv(file = file.path(
      input_data, "ATCGN_content_per_base.csv"), 
      colClasses = c("factor","numeric","character","numeric"))
  df.base.composition.area.chart$cycle <- as.factor(df.base.composition.area.chart$cycle)
  base.composition <- df.base.composition.area.chart 
  colnames(base.composition) <- c("sample", "position in read (bp)", "nucleotide", "occurence (%)")
  file.names <- as.vector(unique(df.base.composition.area.chart$filename))
  files.list <- list()
  for (i in 1:length(file.names)) {
    tmp <- df.base.composition.area.chart[df.base.composition.area.chart$filename == file.names[i],]
    files.list[[i]] <- tmp
  }
  plots <- list()
  for (i in 1:length(files.list)) {
    len <- max(as.integer(files.list[[i]]$cycle))
    box <- ggplot2::ggplot(files.list[[i]], ggplot2::aes_string(x = "cycle", y = "value", colour = "variable")) + 
      ggplot2::geom_line(ggplot2::aes_string(group = "variable")) + 
      ggplot2::scale_x_discrete(breaks = seq(from = 1, to = len, by = len%/%10)) + 
      ggplot2::labs(x = "position in read (bp)", y = "Percentage", colour = "Base Call") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste("Base composition",
                             as.vector(unique(files.list[[i]]$filename)),
                             sep = ": ")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    plots[[i]] <- box
    names(plots)[i] <- file.names[i]
  }
  return(plots)
}