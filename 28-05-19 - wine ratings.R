library(tidyverse)
library(tidytext)
library(RColorBrewer)

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
  inner_join(tasting_notes, by = "word") %>%
  filter(!word %in% c("aroma", "nose", "finesse", "finish", "bouquet", "transparency", "fruit", "depth", "body"))

plot_data <- wines_unnest %>%
  group_by(word) %>%
  count() %>%
  filter(n >= 1500) %>%
  ungroup() %>%
  mutate(id = row_number())

word_number <- nrow(plot_data)
angle <- 90 - 360 * (plot_data$id - 0.5) / word_number

plot_data <- plot_data %>%
  mutate(hjust = ifelse(angle < -90, 1, 0),
         angle = ifelse(angle < -90, angle + 180, angle))

palette <- c("#8B2323", "#8B1A1A", "#B22222", "#CD0000", "#EE0000",
             "#FF0000", "#EE3B3B", "#CD4F39", "#CD5555", "#B03060",
             "#CD3278", "#D02090", "#CD1076", "#C71585", "#D02090",
             "#C71585", "#8B1C62", "#8B1C62", "#8B0A50", "#8B2252",
             "#8B3A62", "#B03060", "#CD6889", "#CD6090", "#CD6889",
             "#DB7093", "#F08080", "#CD7054", "#CD8162", "#BC8F8F",
             "#CD9B9B", "#CD919E", "#CD8C95", "#EEB4B4", "#EEA9B8")

library(extrafont)
loadfonts(device = "win")

plot <- plot_data %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_bar(stat = "identity", size = 1) +
  ylim(-5000, 20500) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  labs(title = "Wine Tasting Wheel",
       subtitle = "Which tasting notes are used most to describe wine?",
       caption = "Tasting notes from https://en.wikipedia.org/wiki/Wine_tasting_descriptors") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(vjust = -40, hjust = .50, face = "bold", size = 18),
        plot.subtitle = element_text(vjust = -40.25, hjust = .50, face = "bold", size = 14),
        plot.caption = element_text(vjust = 40, hjust = .95, face = "bold", size = 10),
        plot.margin = unit(rep(-3, 4), "cm"),
        legend.position = "none",
        text = element_text(family = "Verdana")) +
  coord_polar(start = 0, clip = "off") +
  geom_text(aes(x = word, y = n + 400 * nchar(word), label = word), size = 4.5,
            angle = plot_data$angle, fontface = "bold", nudge_x = .05)

ggsave("28-05-19 wine.png", plot, scale = 1.5)
