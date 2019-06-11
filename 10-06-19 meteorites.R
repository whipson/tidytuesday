library(tidyverse)
library(ggmap)
library(maptools)
library(maps)
library(extrafont)
library(RColorBrewer)
library(cowplot)

loadfonts()

meteors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>%
  filter(long < 300,
         long != 0 & lat != 0,
         fall == "Fell") 

meteors_1960_1979 <- meteors %>%
  filter(year %in% c(1960:1979))

meteors_1980_1999 <- meteors %>%
  filter(year %in% c(1980:1999))

meteors_2000_present <- meteors %>%
  filter(year %in% c(2000:2019))

meteor_map_1960_1979 <- ggmap(get_map(location = c(mean(meteors_1960_1979$long), mean(meteors_1960_1979$lat)), zoom = 3))
meteor_map_1980_1999 <- ggmap(get_map(location = c(mean(meteors_1980_1999$long), mean(meteors_1980_1999$lat)), zoom = 3))
meteor_map_2000_present <- ggmap(get_map(location = c(mean(meteors_2000_present$long), mean(meteors_2000_present$lat)), zoom = 3))

map_1960 <- meteor_map_1960_1979 +
  stat_density2d(data = meteors_1960_1979, aes(x = long, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_alpha_continuous(range = c(.1, .40)) +
  geom_point(data = meteors_1960_1979, aes(x = long, y = lat), color = "red", alpha = .50, size = 4) +
  scale_fill_gradientn(colors = rev(brewer.pal(7, "Spectral"))) +
  theme_void() +
  labs(title = '1960 - 1979') +
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 16),
        legend.position = "none",
        text = element_text(family = "Rockwell"))

map_1960

map_1980 <- meteor_map_1980_1999 +
  stat_density2d(data = meteors_1980_1999, aes(x = long, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_alpha_continuous(range = c(.1, .40)) +
  geom_point(data = meteors_1980_1999, aes(x = long, y = lat), color = "red", alpha = .50, size = 4) +
  scale_fill_gradientn(colors = rev(brewer.pal(7, "Spectral"))) +
  theme_void() +
  labs(title = '1980 - 1999') +
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 16),
        legend.position = "none",
        text = element_text(family = "Rockwell"))

map_1980

map_2000 <- meteor_map_2000_present +
  stat_density2d(data = meteors_2000_present, aes(x = long, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_alpha_continuous(range = c(.1, .40)) +
  geom_point(data = meteors_2000_present, aes(x = long, y = lat), color = "red", alpha = .50, size = 4) +
  scale_fill_gradientn(colors = rev(brewer.pal(7, "Spectral"))) +
  theme_void() +
  labs(title = '2000 - present') +
  theme(plot.title = element_text(hjust = .5, face = "bold", size = 16),
        legend.position = "none",
        text = element_text(family = "Rockwell"))

map_2000

fallen_meteors <- meteors %>%
  filter(year %in% c(1960:2019)) %>%
  group_by(year) %>%
  count() %>%
  ungroup() %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(size = 1.25, color = "red") +
  labs(x = "Year",
       y = "# of Meteorites Fallen",
       title = "Fallen Meteorites from 1960 - present") +
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = "Rockwell"),
        plot.title = element_text(hjust = .5))

fallen_meteors

plot <- ggdraw() +
  draw_plot(map_1960, x = 0, y = 0, width = .35, height = .5) +
  draw_plot(map_1980, x = .3333, y = 0, width = .35, height = .5) +
  draw_plot(map_2000, x = .6668, y = 0, width = .35, height = .5) +
  draw_plot(fallen_meteors, x = 0, y = .5, width = 1, height = .5) +
  annotate(geom = "text", x = .325, y = .51, label = "*Maps are centered at the most active location for each time period", family = "Rockwell")

ggsave("11-06-19 meteorites.png")
