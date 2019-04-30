library(tidyverse)
library(lubridate)
library(forecast)
library(ggfortify)
library(timeSeries)

birds <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

View(birds)

birds_trend <- birds %>%
  mutate(year = year(date),
           month = month(date, label = TRUE)) %>%
  group_by(year, month) %>%
  summarize(count = n()) %>%
  ungroup()

birds_trend %>%
  ggplot(aes(x = month, y = count)) +
  geom_line(aes(group = year, color = year), alpha = .25, size = 1.75) +
  stat_summary(fun.data = mean_cl_normal, geom = 'line', group = 1, size = 2.5, alpha = .80) +
  stat_summary(fun.data = mean_cl_normal, geom = 'point', group = 1, size = 4, alpha = .80) +
  stat_boxplot(geom = 'errorbar', width = .25, size = 1) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = NULL,
       y = "Number of Collisions",
       color = "Year",
       title = "Which months are worse for bird collisions?",
       subtitle = "Black line represents overall trend. Faded lines are individual years.") +
  theme_bw(base_size = 16) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

