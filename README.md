# inDAGO
![favicon-96x96](https://github.com/user-attachments/assets/4d325017-c452-4e8d-95a7-12f1f7ccd6f7)

A Shiny app for dual and bulk RNA‑sequencing analysis

## 👀 Overview

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
2. **Sequence Pre‑processing**  
   Read trimming, low‑quality filtering, and adapter removal
3. **Genome indexing**  
   Index genome or genomes according to the selected approach (bulk or dual RNA‑seq)
4. **Reference‑based Alignment**  
   Align reads according to the selected approach (bulk or dual RNA‑seq) 
5. **Read Count Summarization**  
   Generate gene or transcript level count matrices  
6. **Exploratory Data Analysis**  
   PCA, MDS, heatmaps and more  
7. **Differential Expression Gene (DEG) analysis**  
   Identify differentially expressed genes/transcripts across comparisons

<details>
  <summary><strong>💻 INSTALLATION GUIDE: R AND RSTUDIO</strong> ▸</summary>

### 📦 1. Install R

**Official site:** [CRAN R Project](https://cran.r-project.org/)

| OS | Command or Link |
|----|-----------------|
| **Windows** | [Download R for Windows](https://cran.r-project.org/bin/windows/base/) and run the `.exe` installer.<br>*(Optional)*: [Install Rtools](https://cran.r-project.org/bin/windows/Rtools/) if you plan to compile packages. |
| **macOS** | [Download R for macOS](https://cran.r-project.org/bin/macosx/) and run the `.pkg` installer.<br>**Homebrew alternative:**<br>`brew install r` |
| **Ubuntu / Debian** | ```bash<br>sudo apt update && sudo apt install --no-install-recommends software-properties-common dirmngr<br>wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc<br>sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"<br>sudo apt update && sudo apt install r-base r-base-dev<br>``` |

---

### 🧭 2. Install RStudio (Posit Desktop)

**Official site:** [Posit RStudio Desktop](https://posit.co/download/rstudio-desktop/)

| OS | Command or Link |
|----|-----------------|
| **Windows** | Download the `.exe` installer and run it. |
| **macOS** | Download the `.dmg` installer and drag RStudio into Applications.<br>**Homebrew alternative:** `brew install --cask rstudio` |
| **Ubuntu / Debian** | ```bash<br>wget https://download1.rstudio.org/electron/jammy/amd64/rstudio-latest-amd64.deb<br>sudo dpkg -i rstudio-latest-amd64.deb || sudo apt -f install<br>``` |

---

### 🧪 3. Verify installation
```bash
R --version
Rscript -e 'cat(R.version.string, "\n")'
   
</details>

<details>
   <summary><strong>💻 INSTALLATION GUIDE: INDAGO</strong> ▸</summary>


## How to install inDAGO from CRAN or GitHub

### Install the Bioconductor dependencies

```r

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
  "Rfastp",
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

```

### Install inDAGO from GitHub

```r
#Install devtools if you don’t have it yet
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")

# Install inDAGO
devtools::install_github("inDAGOverse/inDAGO")

```

### Install inDAGO from CRAN (https://cran.r-project.org/web/packages/inDAGO/index.html)

```r

# Install inDAGO
install.packages("inDAGO")

```
</details>
<details>
   <summary><strong>🚀 HOW TO LOAD AND LAUNCH THE APP</strong> ▸</summary>

```r
# Load and launch the app
library(inDAGO)
inDAGO::inDAGO()

```
</details>
<details>
   <summary><strong>⚙️ TIPS FOR A SEAMLESS EXECUTION</strong> ▸</summary>
   
To ensure execution during long time-consuming steps such as reference‑based alignment:

💤 Disable sleep mode to keep your system active.

💡 Reduce screen brightness to save power.

These simple precautions can help avoid incomplete runs and unnecessary power consumption.

</details>
<details>
   <summary><strong>👥 AUTHORS & ACKNOWLEDGEMENTS</strong> ▸</summary>

- **Authors / Creators**  
  - Carmine Fruggiero (c.fruggiero@tigem.it)  
  - Gaetano Aufiero (gaetano.aufiero@unina.it)

- **Designated maintainer for CRAN Repository**  
  - Carmine Fruggiero (c.fruggiero@tigem.it)

- **Project Supervisor**  
  - Nunzio D'Agostino (nunzio.dagostino@unina.it)
</details>
