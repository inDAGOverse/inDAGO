% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volcanoPlot.R
\name{volcanoPlot}
\alias{volcanoPlot}
\title{volcanoPlot}
\usage{
volcanoPlot(
  x,
  palettePoint,
  maxOverlaps,
  sizeLabel,
  Th_logFC,
  Th_Pvalue,
  subsetGenes,
  st_significance
)
}
\arguments{
\item{x}{Character. File path to a CSV containing DEG results, with at least columns "ID", "logFC", and one of "PValue", "FDR", or "FWER".}

\item{palettePoint}{Character. Name of a discrete palette from the "paletteer" package, supplying colors for "UP", "DOWN", and "NO".}

\item{maxOverlaps}{Integer. Maximum allowed label overlaps passed to "ggrepel::geom_text_repel()".}

\item{sizeLabel}{Numeric. Font size for gene labels in the plot.}

\item{Th_logFC}{Numeric. Absolute log2 fold-change threshold to call a gene "UP" or "DOWN".}

\item{Th_Pvalue}{Numeric. P-value threshold to call significance (uses "FDR"/"FWER" if "st_significance = "adjustPvalue"", otherwise raw "PValue").}

\item{subsetGenes}{Integer or "Inf". If numeric, only the top "subsetGenes" genes by p-value are shown and labeled.}

\item{st_significance}{Character. Which p-value column to use: "adjustPvalue" (FDR or FWER) or "PValue".}
}
\value{
A "ggplot" object displaying the volcano plot.
}
\description{
Create a volcano plot of differential expression results.
}
\details{
This function reads a CSV of DEGs, classifies genes as up/down/no change based on
log-fold change and p-value thresholds, and plots –log10(p-value) versus log-FC using ggplot2.
\enumerate{
\item Reads the input CSV and checks for duplicate IDs.
\item Standardizes columns to "ID", "logFC", and "adjustPvalue" or "PValue".
\item Optionally subsets to the top N genes by p-value.
\item Classifies each gene as "UP", "DOWN", or "NO" based on thresholds.
\item Plots points with manual fill, size, and alpha scales, adds threshold lines,
and repels labels using "ggrepel".
}
}
