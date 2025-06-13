.onAttach <- function(libname, pkgname) {
  # Required Bioconductor packages
  required_pkgs <- c(
    "BiocManager", "XVector", "ShortRead", "S4Vectors", "rtracklayer",
    "Rsubread", "Rsamtools", "Rfastp", "limma", "HTSFilter",
    "edgeR", "Biostrings", "BiocGenerics"
  )

  # Find those not installed
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    msg <- paste0(
      "Missing Bioconductor packages: ",
      paste(missing_pkgs, collapse = ", "),
      ".\nInstall them via:\n",
      "if (!requireNamespace(\"BiocManager\", quietly = TRUE))\n",
      "  install.packages(\"BiocManager\")\n",
      "BiocManager::install(c(\"",
      paste(missing_pkgs, collapse = "\", \""),
      "\"))"
    )
    packageStartupMessage(msg)
  }
}
