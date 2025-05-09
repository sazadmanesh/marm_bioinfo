knitr::knit_engines$set(terminal = function(options) {
  code <- paste(options$code, collapse = "\n")
  
  params <- map(params, function(x) if (is.atomic(x)) list(x) else x) %>%
    list_flatten()
  
  # Define placeholder groups
  patterns <- list(
    global        = global,
    params        = params,
    methods_16s_r9  = methods_16s_r9,
    methods_16S_r10 = methods_16s_r10,
    swan          = swan,
    sample_sheets = sample_sheets,
    microbiome         = microbiome,
    marmoset          = marmoset,
    abund_wf16s_files  = abund_wf16s_files,
    barcode_alignments = barcode_alignments
  )
  
  # Replace placeholders group by group
  for (group in names(patterns)) {
    placeholder_list <- patterns[[group]]
    for (name in names(placeholder_list)) {
      placeholder <- paste(group, name, sep = "\\$") # Match exact placeholder
      value <- placeholder_list[[name]]
      
      # Replace placeholders exactly and avoid breaking suffixes
      code <- gsub(placeholder, value, code, perl = TRUE)
    }
  }
  
  options$warning <- FALSE
  knitr::engine_output(options, code, out = code)
})
