library(lsa)
library(readr)
library(tidyverse)
library(fs)

source(file = "src/defrag_transcript.R", encoding = "UTF-8")

processed_transcripts_rds <- "~/Documents/acg_files/transcripts.RDS"
available_transcripts <- dir_ls("~/Documents/acg_files/", regexp = "\\d\\.\\d - .*\\.txt") %>% as_tibble()

if (file_exists(processed_transcripts_rds)) {
  
  processed_transcripts <- read_rds(file = processed_transcripts_rds) %>% as_tibble()
  new_transcripts <- available_transcripts %>% anti_join(processed_transcripts)
  
  defrag_trs(new_transcripts)
  
  processed_transcripts <- processed_transcripts %>% bind_rows(new_transcripts)
  
} else {
  
  defrag_trs(available_transcripts)
  
  processed_transcripts <- available_transcripts
}

write_rds(x = processed_transcripts, file = processed_transcripts_rds)
