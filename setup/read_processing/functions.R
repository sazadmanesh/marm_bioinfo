read_alignment_file  <- function(file) {
  read.csv(file, stringsAsFactors = FALSE, fill = TRUE)
}

export_table <- function(df, path) {
  write.table(df,
              path,
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}


export_extracts <- function(path) {
  write.table(blank.extracts,
              path,
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}

export_libraries <- function(path) {
  write.table(blank.libraries,
              path,
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}



read_extracts <- function(path) {
  tibble <- read.table(path, sep = "\t", header = TRUE) %>% 
    mutate(CollectionDate = ymd(CollectionDate), 
           ExtractDate    = ymd(ExtractDate))
  
  return(tibble)
}


read_libraries <- function(path) {
  tibble <- read.table(path, sep = "\t", header = TRUE) %>% 
    mutate(LibPrepDate = ymd(LibPrepDate))
  
  return(tibble)
}

sort.taxa <- function(df) {
  df <- df %>%
    arrange(superkingdom,
            phylum,
            class,
            order,
            family,
            genus,
            species)
  return(df)
}

group.taxonomy <- function(df) {
  group_by(.data = df,
           superkingdom,
           phylum,
           class,
           order,
           family,
           genus,
           species)
}

fix.strings <- function(df) {
  df <- df %>%
    mutate(across(where(is.character), ~ .x %>%
                    str_remove_all(fixed("'")) %>%
                    str_remove_all(fixed("[")) %>%
                    str_remove_all(fixed("]")) %>%
                    str_trim("both") %>%
                    str_squish()))
  return(df)
}
