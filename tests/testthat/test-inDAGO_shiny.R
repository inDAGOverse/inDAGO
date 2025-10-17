# Install Bioconductor dependencies if you don't have them yet
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
bioc_pac <- c(
  "XVector",
  "ShortRead",
  "S4Vectors",
  "rtracklayer",
  "Rsubread",
  "Rsamtools",
  "limma",
  "HTSFilter",
  "edgeR",
  "Biostrings",
  "BiocGenerics"
)
for (pac in bioc_pac) {
  if (!requireNamespace(pac, quietly = TRUE))
    BiocManager::install(pac)
}

library("inDAGO")


test_that("Shiny app is generated", {
  expect_s3_class(inDAGO(), "shiny.appobj")
})
