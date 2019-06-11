library(tidyverse)
library(ggmap)
library(maptools)
library(maps)
library(gganimate)
library(extrafont)

loadfonts()

meteors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>%
  filter(long < 300,
         year < 2020 & year > 1950,
         fall == "Fell")

world_map <- map_data("world")

big_meteors <- meteors %>%
  group_by(year) %>%
  top_n(1, mass)

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = group), show.legend = FALSE) +
  geom_text(data = big_meteors, aes(x = long, y = lat, label = name), nudge_x = 5, nudge_y = 5, color = "orange", fontface = "bold", alpha = .75) +
  geom_point(data = big_meteors, aes(x = long, y = lat), color = "orange", size = 1.25, alpha = .75) +
  scale_fill_gradient(low = "#262626", high = "black") +
  scale_colour_manual(values = c("#FF4500", "orange"), labels = c("Found", "Impact")) +
  theme_void() +
  labs(title = 'Meteorite Impacts',
       caption = "Source: https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh/data") +
  theme(plot.background = element_rect(fill = "#27408B"),
        plot.title = element_text(hjust = .5, face = "bold", size = 20),
        legend.text = element_text(size = 14, color = "white"),
        legend.position = "bottom",
        text = element_text(family = "Rockwell")) +
  guides(size = FALSE)

p <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = group), show.legend = FALSE) +
  geom_text(data = big_meteors, aes(x = long, y = lat, label = name), nudge_x = 5, nudge_y = 5, color = "orange", fontface = "bold", alpha = .75) +
  geom_point(data = big_meteors, aes(x = long, y = lat), color = "orange", size = 1.25, alpha = .75) +
  scale_fill_gradient(low = "#262626", high = "black") +
  scale_colour_manual(values = c("#FF4500", "orange"), labels = c("Found", "Impact")) +
  theme_void() +
  labs(title = "Meteorite Impacts",
       subtitle = "Year: {closest_state}",
       caption = "Source: https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh/data",
       color = "") +
  transition_states(year, transition_length = 4, state_length = 1) +
  shadow_mark(alpha = .25) +
  theme(plot.background = element_rect(fill = "#27408B"),
        plot.title = element_text(hjust = .5, face = "bold", size = 20),
        plot.subtitle = element_text(size = 16, color = "white", face = "bold", hjust = .05),
        legend.text = element_text(size = 14),
        plot.caption = element_text(vjust = .05),
        text = element_text(family = "Rockwell"))

p <- animate(p, nframes = 150, fps = 5, width = 600)

p


