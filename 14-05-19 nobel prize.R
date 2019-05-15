library(tidyverse)
library(tidytext)
library(ggraph)
library(tidygraph)

nobel_pubs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv") %>%
  mutate(journal = str_to_title(journal),
         journal = ifelse(str_detect(journal, "National Academy Of"), "PNAS", journal))

nobel_pub_words <- nobel_pubs %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c(0:100000, "ii", "und", "der", "von")) %>%
  select(-paper_id, doi, affiliation)

nobel_pubs_bigrams <- nobel_pubs %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  filter(!word1 %in% c(0:100000, "cheminform", "abstract", "uber", "die", "zur", "der"),
         !word2 %in% c(0:100000, "cheminform", "abstract", "uber", "die", "zur", "der"),
         str_length(word1) >= 3,
         str_length(word2) >= 3) 

pubs_bigram_counts <- nobel_pubs_bigrams %>%
  count(word1, word2, category, sort = TRUE) %>%
  top_n(50)

bigram_graph <- pubs_bigram_counts %>%
  as_tbl_graph(directed = FALSE)

layout <- create_layout(bigram_graph, layout = "nicely")

ggraph(layout) +
  geom_edge_link(aes(edge_color = category), edge_width = 1.25) +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), size = 4.75, repel = TRUE) +
  scale_edge_color_manual(values = c("#008B00", "#104E8B", "#FF7F00"), labels = c("Chemistry", "Medicine", "Physics")) +
  labs(edge_color = "Category",
       title = "Word Co-occurrences in Nobel Prize Winner Publication Titles",
       caption = "Source: https://www.kaggle.com/devisangeetha/nobel-prize-winners-story") +
  theme_void() +
  theme(plot.background = element_rect(fill = "lightgrey"),
        plot.title = element_text(size = 18, hjust = .85, face = "bold"),
        legend.title = element_text(size = 16, hjust = .5),
        legend.text = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 8, hjust = 1.35))
                 