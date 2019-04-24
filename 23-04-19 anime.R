library(tidyverse)

anime <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv") %>%
  select(-title_japanese, -title_english, -title_synonyms, -producers, -studio, -airing, -duration,
         -broadcast, -related) %>%
  distinct(animeID, .keep_all = TRUE)

library(tidytext)

anime_words <- anime %>%
  select(animeID, name, synopsis) %>%
  unnest_tokens(word, synopsis) %>%
  anti_join(stop_words, by = "word")

valence <- read.delim("~/R/Sentiment Analysis/Lexicon/v.scores", header = FALSE) %>%
  rename(word = V1,
         valence = V2)

anime_valence <- anime_words %>%
  group_by(animeID) %>%
  mutate(word_count = n()) %>%
  ungroup() %>%
  inner_join(valence) %>%
  group_by(animeID, word_count) %>%
  summarize(valence = mean(valence))

anime_valence_full <- anime %>%
  mutate(rating = case_when(str_detect(rating, "R") ~ "R",
                            str_detect(rating, "All") ~ "G",
                            str_detect(rating, "13") ~ "PG13",
                            str_detect(rating, "Children") ~ "PG")) %>%
  inner_join(anime_valence, by = "animeID") %>%
  filter(word_count >= 15)

library(ggridges)
library(viridis)

windowsFonts(`TT Courier New` = windowsFont("TT Courier New"))

anime_valence_full %>%
  filter(!is.na(rating),
         type == "TV") %>%
  ggplot(aes(x = valence, y = rating, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2,
                               rel_min_height = .001,
                               panel_scaling = FALSE,
                               size = 1.25,
                               show.legend = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(option = "B") +
  labs(x = "Valence (higher = more positive)",
       y = NULL,
       title = "Does Anime for Older Audiences\nContain More Negative Words?",
       subtitle = "TV shows with synopses containing at least 15 words.\nValence scores obtained from NRC VAD lexicon\nhttp://saifmohammad.com/WebPages/nrc-vad.html") +
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(hjust = .5, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.subtitle = element_text(size = 13),
        text = element_text(family = "TT Courier New"))
