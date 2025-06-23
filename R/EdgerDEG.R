#' EdgerDEG
#'
#' Perform differential expression analysis on RNA-seq count data using edgeR.
#'
#' This function reads raw count tables, applies expression filtering (via "filterByExpr" or "HTSFilter"),
#' normalizes library sizes, estimates dispersion, fits statistical models ("exactTest", "glmQLFTest", or "glmLRT"),
#' and writes per-contrast results and diagnostic plots.
#'
#' @param gr Data frame. Sample metadata with columns Samples and Groups.
#' @param WD_samples Character. Directory containing raw count .tab files.
#' @param WD_DEGs Character. Directory in which to write results and logs.
#' @param colIDgene Integer. Column index in each count file for gene IDs.
#' @param colCounts Integer. Column index in each count file for raw counts.
#' @param skip_preN Integer. Number of header lines to skip when reading count files.
#' @param grContrast Data frame. Two-column table with Test and Baseline group names for contrasts.
#' @param filter Character. Filtering method: "filterByExpr" or "HTSFilter".
#' @param model Character. Statistical test: "exactTest", "glmQLFTest", or "glmLRT".
#' @param normMethod Character. Normalization method for edgeR (e.g., "TMM", "RLE").
#' @param min_count Numeric. Minimum count per gene for "filterByExpr".
#' @param min_total_count Numeric. Minimum total count per gene for "filterByExpr".
#' @param large_n Integer. Sample size threshold for "filterByExpr".
#' @param min_prop Numeric. Proportion threshold for "filterByExpr".
#' @param adjustPvalue Character. P-value adjustment method (e.g., "fdr", "holm", "none").
#' @param Th_logFC Numeric. Absolute log-fold-change threshold to call differential expression.
#' @param Th_Pvalue Numeric. Adjusted p-value threshold to call differential expression.
#'
#' @details
#' 1. Reads in per-sample count files and generate a DGEList.
#' 2. Builds the design matrix and contrast definitions from "grContrast".
#' 3. Filters lowly expressed genes, normalizes library sizes, and logs filtering summary.
#' 4. Estimates dispersion (standard or quasi-likelihood).
#' 5. Runs chosen differential test per contrast, annotates each gene as "UP", "DOWN", or "NO",
#'    and writes CSV output files named by filter, model, and contrast.
#' 6. Captures and saves BCV and QL dispersion plots as SVGs in WD_DEGs.
#'
#' @return A list invisibly returned containing any captured plots and log messages;
#'   primary results are written to CSV files in "WD_DEGs".
#'
EdgerDEG <- function(gr, WD_samples, WD_DEGs, colIDgene, colCounts, skip_preN, grContrast, filter, model, normMethod, min_count, min_total_count, large_n, min_prop, adjustPvalue, Th_logFC, Th_Pvalue){


  # Load sample files and match group information from 'gr' (group metadata) based on sample file names
  lpath <- list.files(file.path(WD_samples), pattern =  "*.tab")
  l <- magrittr::`%>%`(list.files(file.path(WD_samples), pattern =  "*.tab"), tools::file_path_sans_ext(.))

  Groups <- magrittr::`%>%`(gr[match(l,gr$Samples),], .$Groups)
  # Set the output path for saving DEG results
  pathDE <- file.path(WD_DEGs)
  # Read the count data using edgeR's readDGE function
  y <- edgeR::readDGE(files = lpath, labels = l, columns = c(colIDgene,colCounts), path = file.path(WD_samples), group = Groups, skip = skip_preN, header = FALSE)

  # change files names
  y$samples$files <- row.names(y$samples)



  # check number of kept genes
  totalGenes <- format(dim(y)[[1]],big.mark=",",scientific=FALSE)
  readr::write_lines(paste("Total genes:",totalGenes,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
  #save log for shiny view
  totalGenes <- paste("Total genes:",totalGenes,"(","running time:",Sys.time(),")")

  # Generate the design matrix for the analysis
  design <- stats::model.matrix(~0+group, data=y$samples)
  colnames(design) <- levels(y$samples$group)


  # Construct contrast matrix based on the comparisons in 'grContrast'
  Contrast <- sapply(1:nrow(grContrast), function(i){
    test <- grContrast$Test[i] # Extract test group name
    baseline <- grContrast$Baseline[i] # Extract baseline group name
    paste0(test,"-",baseline) # Construct contrast string
  })
  my.contrasts <- limma::makeContrasts(contrasts = Contrast, levels=design)

  # initialize list to save contrasts
  d <- list()

  # Apply filtering based on the filter parameter (either "filterByExpr" or "HTSFilter")
  if (filter == "filterByExpr") {


    # Filter lowly expressed genes based on specific thresholds:
    # - 'min.count' is the minimum count per gene to keep
    # - 'min.total.count' is the minimum total count across all samples
    # - 'large.n' number of samples per group that is considered to be “large”
    # - 'min.prop' in large sample situations, the minimum proportion of samples in a group that a gene needs to be expressed in
    keep <- edgeR::filterByExpr(y, group = Groups, min.count = min_count, min.total.count = min_total_count, large.n = large_n, min.prop = min_prop)

    # Subset the DGE list to only keep the filtered genes
    y <- y[keep, , keep.lib.sizes=FALSE]

    # Normalize library sizes based on the provided normMethod (e.g., TMM, RLE)
    y <- edgeR::normLibSizes(y, method=normMethod)


    # check number of kept genes
    keptGenes <- format(dim(y)[[1]],big.mark=",",scientific=FALSE)
    readr::write_lines(paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
    #save log for shiny view
    keptGenes <- paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")")


    # Estimate dispersion for the data set, required for statistical models.  Note that this NB dispersion estimation step is now optional as all the NB dispersion estimates
    # will not be used further under the latest quasi-likelihood (QL) pipeline.
    y <- edgeR::estimateDisp(y, design, robust = TRUE)

    # create file where save info
    readr::write_lines(paste("Common dispersion calculation","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
    readr::write_lines(paste("Common dispersion:", y$common.dispersion), file = paste0(pathDE,"/","info.log"), append = TRUE) # Display the calculated common dispersion value

    #save log for shiny view
    a <- paste("Common dispersion calculation","(","running time:",Sys.time(),")")
    b <- paste("Common dispersion:", y$common.dispersion)


    # plot BCV (biological coefficient of variation)
    record_plotBCV <-  R.devices::capturePlot(edgeR::plotBCV(y),res = 200)
    grDevices::svg(paste0(pathDE,"/","plotBCV.svg"))
    grDevices::replayPlot(record_plotBCV)
    grDevices::dev.off()



    # Check which model is selected for DEG analysis
    if (model == "exactTest") {

      # Perform the exactTest method for differential expression
      readr::write_lines(paste("filterByExpr - exactTest mode is performing","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
      c <- paste("filterByExpr - exactTest mode is performing","(","running time:",Sys.time(),")")

      num_rows <- nrow(grContrast)
      lapply(1:num_rows, function(i) {
        test <- grContrast$Test[[i]] # Extract test group name
        baseline <- grContrast$Baseline[[i]] # Extract baseline group name
        readr::write_lines(paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE) # Display the process message
        testVsBaseline <- paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")")

        #Append testVsBaseline to d list
        d <<- c(d, list(testVsBaseline))

        # Perform exact test for differential expression between test and baseline
        exactDEG <- edgeR::exactTest(y, pair= c(baseline,test))
        # Get the top DEGs, adjusting p-values as specified (e.g., FDR, Bonferroni)
        top <- magrittr::`%>%`(edgeR::topTags(exactDEG, n = Inf, adjust.method = adjustPvalue, sort.by = "PValue"), as.data.frame(.))

        # Label genes as up regulated, down regulated, or no differential expression based on thresholds
        if (adjustPvalue != "none") {
          if (adjustPvalue %in% c("holm", "hochberg", "hommel", "bonferroni")) {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FWER < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FWER < Th_Pvalue] <- "DOWN"
          }         else if (adjustPvalue == "fdr") {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FDR < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FDR < Th_Pvalue] <- "DOWN"
          }
        }

        # Add gene IDs to the results table
        top <- tibble::rownames_to_column(top, "ID")
        # Save the DEG results to a CSV file
        utils::write.csv(x = top, file = paste0(pathDE,"/","filterByExpr_","exactTest_",test,"vs",baseline,".csv"), row.names = FALSE)

        # Display message after processing all contrasts
        if (num_rows == i) {
          readr::write_lines(paste("The process has been completed","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
          # Add final message
          finishMessage <- paste("The process has been completed","(","running time:",Sys.time(),")")
          d <<- c(d, list(finishMessage))

        }
      })

      # Return the BCV plot for visualization
      log <- list(totalGenes = totalGenes, keptGenes = keptGenes, a = a, b = b, c = c, d = d)
      return(list(record_plotBCV = record_plotBCV, log = log))

    }else if (model == "glmQLFTest") {


      # Perform the quasi-likelihood F-test for differential expression
      readr::write_lines(paste("filterByExpr - glmQLFTest mode is performing","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
      c <- paste("filterByExpr - glmQLFTest mode is performing","(","running time:",Sys.time(),")")

      # Fit the quasi-likelihood model to the data
      fit_glmQL <- edgeR::glmQLFit(y, design, robust = TRUE)

      # plot the QL dispersion
      record_plotQLDisp <-  R.devices::capturePlot(edgeR::plotQLDisp(fit_glmQL), res = 200)
      grDevices::svg(paste0(pathDE,"/","plotQLDisp.svg"))
      grDevices::replayPlot(record_plotQLDisp)
      grDevices::dev.off()



      num_rows <- nrow(grContrast)
      lapply(1:num_rows, function(i) {
        test <- grContrast$Test[[i]] # Extract test group name
        baseline <- grContrast$Baseline[[i]] # Extract baseline group name

        readr::write_lines(paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        testVsBaseline <- paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")")

        #Append testVsBaseline to d list
        d <<- c(d, list(testVsBaseline))

        # Perform glmQLFTest for differential expression between test and baseline
        glmQLFDEG <- edgeR::glmQLFTest(fit_glmQL, contrast = my.contrasts[,paste0(test,"-",baseline)])


        # Get the top DEGs, adjusting p-values as specified (e.g., FDR, Bonferroni)
        top <- magrittr::`%>%`(edgeR::topTags(glmQLFDEG, n = Inf, adjust.method = adjustPvalue, sort.by = "PValue"), as.data.frame(.))

        # Label genes based on logFC and p-value thresholds
        if (adjustPvalue != "none") {
          if (adjustPvalue %in% c("holm", "hochberg", "hommel", "bonferroni")) {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FWER < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FWER < Th_Pvalue] <- "DOWN"
          }         else if (adjustPvalue == "fdr") {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FDR < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FDR < Th_Pvalue] <- "DOWN"
          }
        }

        # Add gene IDs and save results
        top <- tibble::rownames_to_column(top, "ID")
        write.csv(x = top, file = paste0(pathDE,"/","filterByExpr_","glmQLFTest_",test,"vs",baseline,".csv"),row.names = FALSE)

        if (num_rows == i) {
          readr::write_lines(paste("The process has been completed","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)

          # Add final message
          finishMessage <- paste("The process has been completed","(","running time:",Sys.time(),")")
          d <<- c(d, list(finishMessage))
        }
      })


      # Return BCV and QLDisp plots for visualization

      log <- list(totalGenes = totalGenes, keptGenes = keptGenes, a = a, b = b, c = c, d = d)
      return(list(record_plotBCV = record_plotBCV, record_plotQLDisp = record_plotQLDisp, log = log))

    }else if (model == "glmLRT") {

      # Perform likelihood ratio test for differential expression
      readr::write_lines(paste("filterByExpr - glmLRT mode is performing","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
      c <- paste("filterByExpr - glmLRT mode is performing","(","running time:",Sys.time(),")")

      # Fit the GLM model to the data
      fit_glm <- edgeR::glmFit(y, design, robust=TRUE)

      num_rows <- nrow(grContrast)
      lapply(1:num_rows, function(i) {
        test <- grContrast$Test[[i]] # Extract test group name
        baseline <- grContrast$Baseline[[i]] # Extract baseline group name
        readr::write_lines(paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        testVsBaseline <- paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")")

        #Append testVsBaseline to d list
        d <<- c(d, list(testVsBaseline))

        # Perform glmLRT for differential expression between test and baseline
        glmLRTDEG <- edgeR::glmLRT(fit_glm, contrast = my.contrasts[,paste0(test,"-",baseline)])
        # Get the top DEGs, adjusting p-values as specified (e.g., FDR, Bonferroni)
        top <- magrittr::`%>%`(edgeR::topTags(glmLRTDEG, n = Inf, adjust.method = adjustPvalue, sort.by = "PValue"), as.data.frame(.))

        # Label genes based on thresholds
        if (adjustPvalue != "none") {
          if (adjustPvalue %in% c("holm", "hochberg", "hommel", "bonferroni")) {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FWER < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FWER < Th_Pvalue] <- "DOWN"
          }         else if (adjustPvalue == "fdr") {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FDR < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FDR < Th_Pvalue] <- "DOWN"
          }
        }
        # Add gene IDs and save results
        top <- tibble::rownames_to_column(top, "ID")
        utils::write.csv(x = top,file = paste0(pathDE,"/","filterByExpr_","glmLRT_",test,"vs",baseline,".csv"),row.names = FALSE)

        if (num_rows == i) {
          readr::write_lines(paste("The process has been completed","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)

          # Add final message
          finishMessage <- paste("The process has been completed","(","running time:",Sys.time(),")")
          d <<- c(d, list(finishMessage))

        }
      })

      # Return BCV plot
      log <- list(totalGenes = totalGenes, keptGenes = keptGenes, a = a, b = b, c = c, d = d)
      return(list(record_plotBCV = record_plotBCV, log = log))


    }

  } else if (filter == "HTSFilter") {

    # Change the parameter of normalization

    if (normMethod == "RLE") {
      normMethodHTSFilter <- "DESeq"
    }else{
      normMethodHTSFilter <- normMethod
    }

    # Normalize library sizes based on the provided normMethod (e.g., TMM, RLE)
    y <- edgeR::normLibSizes(y,method = normMethod)

    # Estimate dispersion for the dataset, required for statistical models.  Note that this NB dispersion estimation step is now optional as all the NB dispersion estimates
    # will not be used further under the latest quasi-likelihood (QL) pipeline.
    y <- edgeR::estimateDisp(y, design, robust = TRUE)

    readr::write_lines(paste("Common dispersion calculation","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
    readr::write_lines(paste("Common dispersion:", y$common.dispersion), file = paste0(pathDE,"/","info.log"), append = TRUE) # Display the calculated common dispersion value

    #save log for shiny view
    a <- paste("Common dispersion calculation","(","running time:",Sys.time(),")")
    b <- paste("Common dispersion:", y$common.dispersion)

    # plot BCV (biological coefficient of variation)
    record_plotBCV <-  R.devices::capturePlot(edgeR::plotBCV(y), res = 200)
    grDevices::svg(paste0(pathDE,"/","plotBCV.svg"))
    grDevices::replayPlot(record_plotBCV)
    grDevices::dev.off()


    # Check which model is selected for DEG analysis
    if (model == "exactTest") {

      # Perform the exactTest method for differential expression
      readr::write_lines(paste("HTSFilter - exactTest mode is performing","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
      c <- paste("HTSFilter - exactTest mode is performing","(","running time:",Sys.time(),")")

      num_rows <- nrow(grContrast)
      lapply(1:num_rows, function(i) {
        test <- grContrast$Test[[i]] # Extract test group name
        baseline <- grContrast$Baseline[[i]]# Extract baseline group name
        readr::write_lines(paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        testVsBaseline <- paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")")

        #Append testVsBaseline to d list
        d <<- c(d, list(testVsBaseline))

        # Perform exact test for differential expression between test and baseline
        exactDEG <- edgeR::exactTest(y, pair= c(baseline,test))
        # Perform filtering using HTSFilter function
        exact_HTSFilterDEG <- HTSFilter::HTSFilter(exactDEG, DGEList=y, plot=FALSE, normalization = normMethodHTSFilter, s.min = 1, s.max = 200, s.len = 100)$filteredData

        # check number of kept genes
        keptGenes <- format(dim(exact_HTSFilterDEG$table)[[1]],big.mark=",",scientific=FALSE)
        readr::write_lines(paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        #save log for shiny view
        keptGenes <- paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")")

        # Get the top DEGs, adjusting p-values as specified (e.g., FDR, Bonferroni)
        top <- magrittr::`%>%`(edgeR::topTags(exact_HTSFilterDEG, n = Inf, adjust.method = adjustPvalue, sort.by = "PValue"), as.data.frame(.))

        # Label genes as upregulated, downregulated, or no differential expression based on thresholds
        if (adjustPvalue != "none") {
          if (adjustPvalue %in% c("holm", "hochberg", "hommel", "bonferroni")) {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FWER < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FWER < Th_Pvalue] <- "DOWN"
          }         else if (adjustPvalue == "fdr") {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FDR < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FDR < Th_Pvalue] <- "DOWN"
          }
        }
        # Add gene IDs to the results table
        top <- tibble::rownames_to_column(top, "ID")
        # Save the DEG results to a CSV file
        utils::write.csv(x = top, file = paste0(pathDE,"/","HTSFilter_","exactTest_",test,"vs",baseline,".csv"), row.names = FALSE)

        if (num_rows == i) {
          readr::write_lines(paste("The process has been completed","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
          # Add final message
          finishMessage <- paste("The process has been completed","(","running time:",Sys.time(),")")
          d <<- c(d, list(finishMessage))
        }

        # report filtered genes message
        keptGenes <<- keptGenes
      })

      # Return the BCV plot for visualization
      log <- list(totalGenes = totalGenes, keptGenes = keptGenes, a = a, b = b, c = c, d = d)
      return(list(record_plotBCV = record_plotBCV, log = log))

    }else if (model == "glmQLFTest") {

      # Perform the glmQLFTest method for differential expression
      readr::write_lines(paste("HTSFilter - glmQLFTest mode is performing","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
      c <- paste("HTSFilter - glmQLFTest mode is performing","(","running time:",Sys.time(),")")

      # Fit the quasi-likelihood model to the data
      fit_glmQL <- edgeR::glmQLFit(y,design,robust = TRUE)

      # plot the QL dispersion
      record_plotQLDisp <-  R.devices::capturePlot(edgeR::plotQLDisp(fit_glmQL), res = 200)
      grDevices::svg(paste0(pathDE,"/","plotQLDisp.svg"))
      grDevices::replayPlot(record_plotQLDisp)
      grDevices::dev.off()


      num_rows <- nrow(grContrast)
      lapply(1:num_rows, function(i) {
        test <- grContrast$Test[[i]] # Extract test group name
        baseline <- grContrast$Baseline[[i]] # Extract baseline group name
        readr::write_lines(paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        testVsBaseline <- paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")")

        #Append testVsBaseline to d list
        d <<- c(d, list(testVsBaseline))

        # Perform glmQLFTest for differential expression between test and baseline
        glmQLFDEG <- edgeR::glmQLFTest(fit_glmQL, contrast = my.contrasts[,paste0(test,"-",baseline)])
        # Perform filtering using HTSFilter function
        glmQLF_HTSFilterDEG <- HTSFilter::HTSFilter(glmQLFDEG, DGEGLM=fit_glmQL, plot=FALSE, normalization = normMethodHTSFilter, s.min = 1, s.max = 200, s.len = 100)$filteredData

        # check number of kept genes
        keptGenes <- format(dim(glmQLF_HTSFilterDEG$table)[[1]],big.mark=",",scientific=FALSE)
        readr::write_lines(paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        #save log for shiny view
        keptGenes <- paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")")

        # Get the top DEGs, adjusting p-values as specified (e.g., FDR, Bonferroni)
        top <-  magrittr::`%>%`(edgeR::topTags(glmQLF_HTSFilterDEG, n = Inf, adjust.method = adjustPvalue, sort.by = "PValue"), as.data.frame(.))

        # Label genes as upregulated, downregulated, or no differential expression based on thresholds
        if (adjustPvalue != "none") {
          if (adjustPvalue %in% c("holm", "hochberg", "hommel", "bonferroni")) {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FWER < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FWER < Th_Pvalue] <- "DOWN"
          }         else if (adjustPvalue == "fdr") {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FDR < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FDR < Th_Pvalue] <- "DOWN"
          }
        }
        # Add gene IDs to the results table
        top <- tibble::rownames_to_column(top, "ID")
        # Save the DEG results to a CSV file
        utils::write.csv(x = top,file = paste0(pathDE,"/","HTSFilter_","glmQLFTest_",test,"vs",baseline,".csv"),row.names = FALSE)

        if (num_rows == i) {
          readr::write_lines(paste("The process has been completed","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)

          # Add final message
          finishMessage <- paste("The process has been completed","(","running time:",Sys.time(),")")
          d <<- c(d, list(finishMessage))

        }
        # report filtered genes message
        keptGenes <<- keptGenes

      })

      # Return the BCV and QLDisp plots for visualization
      log <- list(totalGenes = totalGenes, keptGenes = keptGenes, a = a, b = b, c = c, d = d)
      return(list(record_plotBCV = record_plotBCV, record_plotQLDisp = record_plotQLDisp, log = log))

    }else if (model == "glmLRT") {


      # Perform the glmLRT method for differential expression
      readr::write_lines(paste("HTSFilter - glmLRT mode is performing","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
      c <- paste("HTSFilter - glmLRT mode is performing","(","running time:",Sys.time(),")")

      # Fit the glmFit model to the data
      fit_glm <-  edgeR::glmFit(y,design)

      num_rows <- nrow(grContrast)
      lapply(1:num_rows, function(i) {
        test <- grContrast$Test[[i]]
        baseline <- grContrast$Baseline[[i]]
        readr::write_lines(paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        testVsBaseline <- paste("The process is ongoing - analysing contrast:",test,"vs",baseline,"(","running time:",Sys.time(),")")

        #Append testVsBaseline to d list
        d <<- c(d, list(testVsBaseline))

        # Perform glmLRT for differential expression between test and baseline
        glmLRTDEG <-  edgeR::glmLRT(fit_glm, contrast = my.contrasts[,paste0(test,"-",baseline)])
        # Perform filtering using HTSFilter function
        glmLRT_HTSFilterDEG <- HTSFilter::HTSFilter(glmLRTDEG, DGEGLM=fit_glm, plot=FALSE, normalization = normMethodHTSFilter, s.min = 1, s.max = 200, s.len = 100)$filteredData

        # check number of kept genes
        keptGenes <- format(dim(glmLRT_HTSFilterDEG$table)[[1]],big.mark=",",scientific=FALSE)
        readr::write_lines(paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)
        #save log for shiny view
        keptGenes <- paste("Kept genes:",keptGenes,"(","running time:",Sys.time(),")")

        # Get the top DEGs, adjusting p-values as specified (e.g., FDR, Bonferroni)
        top <-  magrittr::`%>%`(edgeR::topTags(glmLRT_HTSFilterDEG, n = Inf, adjust.method = adjustPvalue, sort.by = "PValue"), as.data.frame(.))

        # Label genes as upregulated, downregulated, or no differential expression based on thresholds
        if (adjustPvalue != "none") {
          if (adjustPvalue %in% c("holm", "hochberg", "hommel", "bonferroni")) {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FWER < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FWER < Th_Pvalue] <- "DOWN"
          }         else if (adjustPvalue == "fdr") {

            top$diffExp <- "NO"
            top$diffExp[top$logFC >= Th_logFC & top$FDR < Th_Pvalue] <- "UP"
            top$diffExp[top$logFC <= -Th_logFC & top$FDR < Th_Pvalue] <- "DOWN"
          }
        }
        # Add gene IDs to the results table
        top <- tibble::rownames_to_column(top, "ID")
        # Save the DEG results to a CSV file
        utils::write.csv(x = top, file = paste0(pathDE,"/","HTSFilter_","glmLRT_",test,"vs",baseline,".csv"),row.names = FALSE)

        if (num_rows == i) {
          readr::write_lines(paste("The process has been completed","(","running time:",Sys.time(),")"), file = paste0(pathDE,"/","info.log"), append = TRUE)

          # Add final message
          finishMessage <- paste("The process has been completed","(","running time:",Sys.time(),")")
          d <<- c(d, list(finishMessage))

        }

        # report filtered genes message
        keptGenes <<- keptGenes
      })

      # Return the BCV plot for visualization
      log <- list(totalGenes = totalGenes, keptGenes = keptGenes, a = a, b = b, c = c, d = d)
      return(list(record_plotBCV = record_plotBCV, log = log))

    }
  }

}
