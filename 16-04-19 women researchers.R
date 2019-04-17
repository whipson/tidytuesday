library(tidyverse)

theme_set(theme_bw())

women_research <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

windowsFonts(`TT Courier New` = windowsFont("TT Courier New"))

p1 <- women_research %>%
  mutate(field = ifelse(field == 'Women inventores', 'Inventor *patent filed', field),
         country = ifelse(country == 'United States', 'US', country),
         country = ifelse(country == 'United Kingdom', 'UK', country)) %>%
  filter(country != "EU28") %>%
  group_by(country) %>%
  mutate(mean = mean(percent_women)) %>%
  ungroup() %>%
  group_by(field) %>%
  mutate(total = sum(percent_women)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = reorder(country, mean), y = percent_women, fill = reorder(field, total)), position = "dodge") +
  labs(x = NULL, y = NULL,
       fill = "Field",
       title = "Women among Researchers with Published Papers") +
  coord_flip() +
  scale_fill_manual(values = c("#458B74", "#4169E1", "#EE9A00", "#00C5CD", "#EE6363")) +
  scale_y_continuous(limits = c(0, .65), expand = c(0, 0), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(color = "black", size = 14),
        plot.title = element_text(hjust = .5, size = 22, face  = "bold"),
        legend.background = element_rect(color = "black", fill = "lightgrey", linetype = "solid", size = 1),
        legend.position = c(.85, .165),
        legend.text = element_text(size = 10, face = "bold"),
        legend.title.align = .5,
        legend.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "TT Courier New")) +
  guides(fill = guide_legend(reverse = TRUE))

p1

p2 <- women_research %>%
  mutate(field = ifelse(field == 'Women inventores', 'Inventor', field),
         country = ifelse(country == 'United States', 'US', country),
         country = ifelse(country == 'United Kingdom', 'UK', country)) %>%
  filter(country != "EU28") %>%
  group_by(field) %>%
  mutate(total = sum(percent_women)) %>%
  ungroup() %>%
  group_by(field) %>%
  mutate(mean = mean(percent_women)) %>%
  ungroup() %>%
  ggplot(aes(x = mean, y = "Average  ", color = reorder(field, total))) +
  geom_point(shape = "square", size = 5, alpha = .5) +
  scale_color_manual(values = c("#458B74", "#4169E1", "#EE9A00", "#00C5CD", "#EE6363")) +
  labs(y = NULL, x = NULL) +
  scale_x_continuous(limits = c(0, .65), expand = c(0, 0), labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none",
        axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_text(size = 14),
        text = element_text(family = "TT Courier New"))

library(grid)
library(gtable)
library(gridExtra)

layout <- rbind(c(1, 1, 1),
                c(1, 1, 1),
                c(1, 1, 1),
                c(1, 1, 1),
                c(1, 1, 1),
                c(1, 1, 1),
                c(1, 1, 1),
                c(2, 2, 2))

grid.arrange(p1, p2, layout_matrix = layout)
