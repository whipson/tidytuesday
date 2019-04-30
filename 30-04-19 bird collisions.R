library(tidyverse)
library(lubridate)
library(ggecho)

windowsFonts(`TT Courier New` = windowsFont("TT Courier New"))

birds <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

birds_trend <- birds %>%
  mutate(year = year(date),
           month = month(date, label = TRUE)) %>%
  group_by(year, month) %>%
  summarize(count = n()) %>%
  ungroup()

birds_trend %>%
  ggplot(aes(x = month, y = count)) +
  geom_line(aes(group = year, color = year), alpha = .25, size = 1.75, stat = 'echo') +
  scale_color_gradient(low = "#FF7256", high = "#00CD00") +
  stat_summary(fun.data = mean_cl_normal, geom = 'line', group = 1, size = 2.5, alpha = .80) +
  stat_summary(fun.data = mean_cl_normal, geom = 'point', group = 1, size = 4, alpha = .80) +
  stat_boxplot(geom = 'errorbar', width = .25, size = 1) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = NULL,
       y = "Number of Collisions",
       color = "Year",
       title = "Yearly and overall trends for 40 years of\nChicago bird collision data.",
       subtitle = "Years 1978-2016. Spring and Fall data available.\nBlack represents average yearly trend.",
       caption = "Source: Winger et al. (2019)") +
  theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        plot.caption = element_text(size = 10, hjust = 1.35),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        text = element_text(family = "TT Courier New"))

