global             <- config::get(config = "default")
swan               <- config::get(config = "swan")
microbiome         <- config::get(config = "microbiome")
modules            <- config::get(config = "modules")
widgets            <- config::get(config = "widgets")
marmoset           <- config::get(config = "marmoset")
methods_16s_r9     <- config::get(config = "methods_16s_r9")
methods_16s_r10    <- config::get(config = "methods_16s_r10")
sample_sheets      <- config::get(config = "sample_sheets")
abund_wf16s_files  <- config::get(config = "abund_wf16s_files")
barcode_alignments <- config::get(config = "barcode_alignments")

source(paste0(global$packages))
source(paste0(global$conflicts))
source(paste0(global$functions))
source(paste0(global$inputs))

opts_chunk$set(message = FALSE,
               warning = FALSE,
               echo    = TRUE,
               include = TRUE,
               eval    = TRUE,
               comment = "")

setwd("~/work/marm_bioinfo")

