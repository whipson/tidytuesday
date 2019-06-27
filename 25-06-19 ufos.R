library(tidyverse)
library(tidytext)
library(lubridate)
library(ggimage)
library(extrafont)

airports <- read.delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", sep = ",", header = FALSE) %>%
  rename(id = V1,
         name = V2,
         city = V3,
         country = V4,
         IATA = V5,
         ICAO = V6,
         lat = V7,
         long = V8) %>%
  select(1:8)

airports_us <- airports %>%
  filter(country == "United States",
         long < -50 & long > -130,
         lat > 20) %>%
  mutate(image = "https://iconsplace.com/wp-content/uploads/_icons/ff0000/256/png/airplane-mode-on-icon-14-256.png")
  
ufos <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufos_us <- ufos %>%
  mutate(date_time = as.Date(date_time, format = "%m/%d/%Y"),
         year = year(date_time)) %>%
  filter(country == "us",
         state != "ak",
         state != "hi",
         state != "pr",
         year >= 1960)

us_map <- map_data("state", region = NULL)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), color = "black", fill = "#303030", size = 1.15) +
  geom_point(data = ufos_us, aes(x = longitude, y = latitude), size = .95, alpha = .075, color = "#7CFC00") +
  geom_image(data = airports_us, aes(x = long, y = lat, image = image), size = .0155, alpha = .50) +
  labs(title = "Do UFO sightings cluster around airports?",
       caption = "Answer: Yes, but maybe not why you think.") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#2B2342"),
        text = element_text(family = "Rockwell"),
        plot.title = element_text(hjust = .5, size = 24, color = "#7CFC00"),
        plot.caption = element_text(color = "#7CFC00", size = 14))

ggsave("ufo-airports.png")
