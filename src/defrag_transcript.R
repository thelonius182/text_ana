defrag_trs <- function(t_new) {
  
  ### TST ###
  # t_new <- available_transcripts
  ### TST ###
  
  data(stopwords_en)
  swe_added <- tibble(value = c("ve", "ll", "re", "am"))
  swe <- stopwords_en %>% as_tibble() %>% bind_rows(swe_added)
  
  for (a_transcript in t_new$value) {
    
    ### TST ###
    # a_transcript <- t_new$value[[1]]
    ### TST ###
    
    acg_01 <- read_delim(file = a_transcript,
                         delim = "^",
                         escape_double = FALSE,
                         col_names = FALSE,
                         locale = locale(),
                         trim_ws = T,
                         show_col_types = F
    )
    
    acg_01.1 <- str_flatten(acg_01$X1, collapse = "¶") %>% as_tibble()
    
    acg02 <- str_split(acg_01.1, pattern = "\\.¶", simplify = F) %>% 
      as_tibble(.name_repair = "unique") %>% 
      rename(line = `...1`) %>% 
      mutate(line = str_replace_all(line, "¶", replacement = " "))
    
    transcript_as_lines_file <- str_replace(a_transcript, pattern = "\\.txt", replacement = " LINES\\.txt") %>% 
      str_replace(pattern = "acg_files", replacement = "acg_files/summaries")
    
    write_delim(x = acg02, file = transcript_as_lines_file)
    
    index_source <- tibble(source = str_replace(a_transcript, pattern = "/home/lon/Documents/acg_files/", ""))

    acg_03 <- suppressWarnings(separate(data = acg02, col = "line", into = paste0("W", 1:50), sep = "[ ']"))

    acg_03.1 <- acg_03 %>%
      pivot_longer(cols = starts_with("W"), values_to = "words") %>%
      mutate(words = str_replace_all(words, "[,.]", "") %>% str_to_lower()) %>%
      filter(!is.na(words) & str_length(words) > 0) %>%
      filter(!words %in% swe$value) %>%
      select(words) %>%
      arrange(words) %>% distinct() %>% bind_cols(index_source)
    
    index_file <- str_replace(a_transcript, pattern = "\\.txt", replacement = " INDEX\\.tsv") %>% 
      str_replace(pattern = "acg_files", replacement = "acg_files/summaries")
    
    write_delim(x = acg_03.1, file = index_file, delim = "\t")
  }
}
