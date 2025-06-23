#' BaseQualityBoxplotPlot
#'
#' @param input_data folder containing data
BaseQualityBoxplotPlot <- function(input_data) {
  df.sequence.quality.boxplot <-
    read.csv(file = file.path(
      input_data, "Quality_value_distribution.csv"),
      colClasses = 
        c("factor","numeric", "numeric",
          "numeric", "numeric", "numeric", "numeric"))
  df.sequence.quality.boxplot$cycle <- as.factor(df.sequence.quality.boxplot$cycle)
  file.names <- as.vector(unique(df.sequence.quality.boxplot$filename))
  files.list <- list()
  for (i in 1:length(file.names)) {
    tmp <- df.sequence.quality.boxplot[df.sequence.quality.boxplot$filename == file.names[i],]
    files.list[[i]] <- tmp
  }
  plots <- list()
  for (i in 1:length(files.list)) {
    len <- max(as.integer(files.list[[i]]$cycle))
    box <- ggplot2::ggplot(data = files.list[[i]], ggplot2::aes_string(x = "cycle", ymin = "ymin", lower = "lower",
                                                                       middle = "middle", upper = "upper", ymax = "ymax")) +
      ggplot2::geom_rect(data=NULL,ggplot2::aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=20),
                         fill="#FC9272") +
      ggplot2::geom_rect(data = NULL, ggplot2::aes(xmin=-Inf,xmax=Inf,ymin=20,ymax=28),
                         fill = "#FFEDA0") +
      ggplot2::geom_rect(data=NULL, ggplot2::aes(xmin=-Inf,xmax=Inf,ymin=28,ymax=Inf),
                         fill="#D9F0A3") +
      ggplot2::geom_boxplot(stat = "identity", fill = "darkgrey") +
      ggplot2::scale_x_discrete(breaks = seq(from = 1, to = len, by = len%/%10)) +
      ggplot2::labs(x = "position in read (bp)", y = "Average quality") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste("Base quality", as.vector(unique(files.list[[i]]$filename)),
                             sep = ": ")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    plots[[i]] <- box
    names(plots)[i] <- file.names[i]
  }
  return(plots)
}