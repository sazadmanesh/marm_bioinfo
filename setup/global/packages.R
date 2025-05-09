install_missing_packages <- function(required_packages) {
  installed_packages <- rownames(installed.packages())
  missing_packages   <- dplyr::setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  } else {
    message("All required packages are already installed.")
  }
}


required_packages <- c(
  "knitr",
  "ape",
  "bibtex",
  "BiocManager",
  "bookdown",
  "bsicons",
  "bslib",
  "config",
  "conflicted",
  "crosstalk",
  "data.table",
  "devtools",
  "fontawesome",
  "glue",
  "ggtext",
  "gt",
  "gtExtras",
  "gtable",
  "htmltools",
  "htmlwidgets",
  "kableExtra",
  "MASS",
  "lmerTest",
  "lubridate",
  "paletteer",
  "pander",
  "pandoc",
  "philr",
  "phyloseq",
  "png",
  "rcompanion",
  "reactable",
  "rmarkdown",
  "sass",
  "scales",
  "shinydashboard",
  "shinyjs",
  "shinyMatrix",
  "shinyTime",
  "showtext",
  "thematic",
  "tidyverse",
  "usethis",
  "utf8",
  "rmdformats"
)

if (!exists("packages_checked")) {
  install_missing_packages(required_packages)
  packages_checked <- TRUE
}
# Global
library(knitr)
library(ape)
library(bibtex)
library(BiocManager)
library(bookdown)
library(bsicons)
library(bslib)
library(config)
library(conflicted)
library(crosstalk)
library(data.table)
library(devtools)
library(fontawesome)
library(forcats)
library(glue)
library(ggtext)
library(gt)
library(gtExtras)
library(gtable)
library(htmltools)
library(htmlwidgets)
library(kableExtra)
library(MASS)
library(lmerTest)
library(lubridate)
library(magrittr)
library(paletteer)
library(pander)
library(pandoc)
library(philr)
library(phyloseq)
library(png)
library(rcompanion)
library(reactable)
library(rmarkdown)
library(sass)
library(scales)
library(seqinr)
library(shinydashboard)
library(shinyjs)
library(shinyMatrix)
library(shinyTime)
library(showtext)
library(thematic)
library(tidyverse)
library(usethis)
library(utf8)
library(rmdformats)

# Microbiome
library(agricolae)
library(aplot)
library(Boruta)
library(brms)
library(caret)
library(DESeq2)
library(edgeR)
library(file2meco)
library(FSA)
library(ggalluvial)
library(GGally)
library(ggdendro)
library(ggtern)
library(ggnested)
library(ggradar)
library(ggtree)
library(glmmTMB)
library(GUniFrac)
library(igraph)
library(job)
library(MASS)
library(mecodev)
library(mecoturn)
library(metagenomeSeq)
library(MicrobiomeStat)
library(microeco)
library(multiROC)
library(NetCoMi)
library(networkD3)
library(NST)
library(parallel)
library(patchwork)
library(performance)
library(picante)
library(png)
library(poweRlaw)
library(randomForest)
library(rfPermute)
library(rgexf)
library(rsample)
library(seqinr)
library(SpiecEasi)
library(SPRING)
library(SRS)
library(TreeTools)
library(WGCNA)