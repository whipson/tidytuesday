library(tidyverse)
library(tidytext)

wine <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

tasting_notes_raw <- read.delim("tasting_notes_raw.txt", header = FALSE, stringsAsFactors = FALSE) %>%
  data.frame() %>%
  rename(tasting_note = V1)

tasting_notes <- data.frame(str_remove(tasting_notes_raw$tasting_note, '\\* ')) %>%
  rename(word = 1) %>%
  separate(word, into = c("word", "label"), sep = ":") %>%
  mutate(word = ifelse(str_detect(word, "Bouquet"), "Bouquet", word)) %>%
  mutate(word = str_to_lower(word))

wines_unnest <- wine %>%
  unnest_tokens(word, description) %>%
  inner_join(tasting_notes, by = "word")

