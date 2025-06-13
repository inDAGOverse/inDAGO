# inDAGO
![favicon-96x96](https://github.com/user-attachments/assets/4d325017-c452-4e8d-95a7-12f1f7ccd6f7)

A Shiny app for dual and bulk RNA‑sequencing analysis

## 🚀 Overview

**inDAGO** supports both dual and bulk RNA‑seq workflows in a single, user-friendly Shiny interface. For dual RNA‑seq, you can choose between:

- **Sequential mapping**: map reads separately to each reference genome  
- **Combined mapping**: align reads to a single, merged reference genome

The interface walks you step‑by‑step through the entire analysis, from raw reads to publication‑ready plots, and lets you:

- Download intermediate results at each step  
- Export high‑quality figures directly for your manuscript  

Thanks to optimized, parallelized code, inDAGO runs efficiently on a standard laptop (16 GB RAM), so you don’t need access to a high‑performance cluster.

## 🔧 Key Modules

1. **Quality Control**  
   Generating quality control metrics and graphical plots.
1. **Sequence Pre‑processing**  
   Read trimming, low‑quality filtering, and adapter removal
2. **Reference‑based Alignment**  
   Align reads according to the selected approach (bulk or dual RNA‑seq) 
3. **Read Count Summarization**  
   Generate gene or transcript level count matrices  
4. **Exploratory Data Analysis**  
   PCA, clustering, heatmaps and more  
5. **Differential Expression**  
   Identify differentially expressed genes/transcripts across any comparison

## 📦 Installation

```r
# Install devtools if you don’t have it yet
install.packages("devtools")     

# Install inDAGO from GitHub
devtools::install_github("inDAGOverse/inDAGO")

# Load and launch the app
library(inDAGO)
inDAGO::inDAGO()
