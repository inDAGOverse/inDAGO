## File: R/zzz.R

.onAttach <- function(libname, pkgname) {
  ## list of package to check
  needed <- c(
    "BiocManager", "XVector", "ShortRead", "S4Vectors", "rtracklayer",
    "Rsubread", "Rsamtools", "Rfastp", "limma", "HTSFilter",
    "edgeR", "Biostrings", "BiocGenerics"
  )
  
  ## check
  missing <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = FALSE)]
  
  if (length(missing)) {
    msg <- paste0(
      "The following packages are not installed: ",
      paste(missing, collapse = ", "), ".\n",
      "To enable all the features of ", "inDAGO", ", install with:\n",
      "  if (!requireNamespace('BiocManager', quietly=TRUE))\n",
      "    install.packages('BiocManager')\n",
      "  BiocManager::install(c(",
      paste0("\"", missing, "\"", collapse = ", "), "))\n"
    )
    packageStartupMessage(msg)
  }
}