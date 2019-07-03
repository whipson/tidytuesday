library(tidyverse)
library(ggalluvial)
library(extrafont)

media <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

media_clean <- media %>%
  mutate(original_media_lumped = fct_lump(original_media, 8),
         revenue_category = case_when(revenue_category == "Book sales" ~ "Books",
                                      revenue_category == "Box Office" ~ "Box Office",
                                      revenue_category == "Comic or Manga" ~ "Comics",
                                      revenue_category == "Home Video/Entertainment" ~ "Home Video",
                                      revenue_category == "Merchandise, Licensing & Retail" ~ "Merchandise",
                                      revenue_category == "Music" ~ "Music",
                                      revenue_category == "TV" ~ "TV",
                                      revenue_category == "Video Games/Games" ~ "Video Games")) 

media_grouped <- media_clean %>%
  group_by(original_media_lumped, revenue_category) %>%
  summarize(total = n(),
            average_revenue = mean(revenue))

media_lodes <- to_lodes_form(media_grouped, axes = 1:2)

library(RColorBrewer)

number_colors <- 17
my_colors <- colorRampPalette(brewer.pal(7, "Set1"))(number_colors)

media_lodes %>%
  ggplot(aes(x = x, y = total, stratum = stratum, alluvium = alluvium,
             fill = stratum, label = stratum)) +
  scale_x_discrete(expand = c(.05, .05), labels = c("Original Format", "Revenue Streams")) +
  scale_fill_manual(values = my_colors) +
  geom_flow(width = 1/6) +
  geom_stratum(alpha = .5, width = 2/12, size =  1, fill = NA, color = "grey") +
  geom_text(stat = "stratum", size = 4, family = "Rockwell") +
  labs(title = "The Evolution of Popular Media",
       subtitle = "This plot combines data from over 300 popular media franchises. On the left are the original media,
on the left are extant revenue streams. Box size and line width is proportional to total franchises in that category.",
       caption = "Source: Wikipedia") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(vjust = 5, size = 14),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Rockwell"),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 12))

ggsave("popular_media.png", width = 9.97)
