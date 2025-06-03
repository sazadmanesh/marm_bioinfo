global             <- config::get(config = "default")
swan               <- config::get(config = "swan")
microbiome         <- config::get(config = "microbiome")
modules            <- config::get(config = "modules")
widgets            <- config::get(config = "widgets")
marmoset           <- config::get(config = "marmoset")
methods_16s        <- config::get(config = "methods_16s")
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

theme_set(theme_classic())
thematic_rmd()
thematic_on(accent = "#8785B2FF", fg = "black")

font_add_google("Noto Sans Symbols", family = "NotoSym")
font_add_google("Six Caps", family = "Six Caps")
font_add_google("Gafata", family = "Gafata")
showtext_auto()

setwd("~/work/marm_bioinfo")

