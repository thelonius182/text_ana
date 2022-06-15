library(lsa)
library(readr)
library(tidyverse)

acg_01 <- read_delim(
  "~/Documents/acg_files/acg_01.txt",
  delim = "^",
  escape_double = FALSE,
  col_names = FALSE,
  locale = locale(),
  trim_ws = T,
  show_col_types = F
)

data(stopwords_en)
swe_added <- tibble(value = c("ve", "ll", "re", "am"))
swe <- stopwords_en %>% as_tibble() %>% bind_rows(swe_added)

acg_01.1 <- str_flatten(acg_01$X1, collapse = "¶") %>% as_tibble()

acg02 <- str_split(acg_01.1, pattern = "\\.¶", simplify = F) %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename(line = `...1`) %>% 
  mutate(line = str_replace_all(line, "¶", replacement = " "))

acg_03 <- suppressWarnings(separate(data = acg02, col = "line", into = paste0("W", 1:50), sep = "[ ']"))

acg_03.1 <- acg_03 %>% 
  pivot_longer(cols = starts_with("W"), values_to = "words") %>% 
  mutate(words = str_replace_all(words, "[,.]", "") %>% str_to_lower()) %>% 
  filter(!is.na(words) & str_length(words) > 0) %>% 
  filter(!words %in% swe$value) %>% 
  select(words) %>% 
  arrange(words) %>% distinct()
