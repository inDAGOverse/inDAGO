# .onLoad
.onLoad <- function(libname, pkgname) {
  required_pkgs <- c(
    "BiocManager", "XVector", "ShortRead", "S4Vectors", "rtracklayer",
    "Rsubread", "Rsamtools", "Rfastp", "limma", "HTSFilter",
    "edgeR", "Biostrings", "BiocGenerics"
  )

  missing_pkgs <- c()

  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, pkg)
    }
  }

  if (length(missing_pkgs) > 0) {
    message <- paste0(
      "The following Bioconductor packages are missing:\n",
      paste(missing_pkgs, collapse = ", "), "\n\n",
      "To install them, run:\n",
      "if (!requireNamespace(\"BiocManager\", quietly = TRUE)) install.packages(\"BiocManager\")\n",
      "BiocManager::install(c(\"", paste(missing_pkgs, collapse = "\", \""), "\"))"
    )
    packageStartupMessage(message)
  }
}
