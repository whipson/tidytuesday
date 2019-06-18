library(tidyverse)
library(widyr)
library(forecast)
library(timetk)
library(lubridate)
library(sweep)
library(extrafont)

bird_counts_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

bird_counts <- bird_counts_raw %>%
  select(-species_latin) %>%
  filter(year >= 1960) %>%
  group_by(species) %>%
  mutate(total = sum(how_many_counted)) %>%
  filter(total >= 1000)

bird_counts_nest <- bird_counts %>%
  select(-total_hours, -total, -how_many_counted) %>%
  mutate(year = as.Date(ISOdate(year, 1, 1))) %>%
  group_by(species) %>%
  nest(.key = "data.tbl")

bird_counts_ts <- bird_counts_nest %>%
  mutate(data.ts = map(.x = data.tbl,
                       .f = tk_ts,
                       select = -year,
                       start = 1960,
                       freq = 1))

bird_counts_fit <- bird_counts_ts %>%
  mutate(fit.ets = map(data.ts, ets))

bird_counts_aug <- bird_counts_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "year")) %>%
  unnest(augment, .drop = TRUE) %>%
  mutate(year = year(year))

worst_fitting <- bird_counts_aug %>%
  mutate(residual = abs(.resid)) %>%
  group_by(species) %>%
  summarize(mean_residual = mean(residual)) %>%
  ungroup() %>%
  top_n(4, mean_residual) %>%
  select(species) %>%
  distinct()

bird_counts_aug %>%
  inner_join(worst_fitting, by = "species") %>%
  gather(parameter, value, -year, -species, -.resid) %>%
  ggplot(aes(x = year, y = value, color = parameter)) +
  geom_line(size = 1.45, alpha = .75) +
  scale_color_manual(labels = c("Observed", "Predicted"), values = c("black", "red")) +
  labs(x = "Year",
       y = "Number of Birds Observed per Hour",
       color = "",
       title = "Bird Species that Fluctuate Most Over Time",
       subtitle = "*based on average absolute residual.\n*limited to species that appear at least 1,000 times from 1960-2017.",
       caption = "Data courtesy of @Bird Studies Canada") +
  facet_wrap(~species) +
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = "Rockwell"),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(hjust = .5, size = 22, face = "bold"))
