library(tidyverse)
library(janitor)
library(ggmap)
library(maptools)
library(maps)
library(viridis)
library(extrafont)

font_import()
loadfonts(device = "win")

mismanaged_vs_gdp <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

mismanaged <- mismanaged_vs_gdp %>%
  clean_names() %>%
  rename(region = entity) %>%
  mutate(region = ifelse(region == "United States", "USA", region),
         region = ifelse(region == "United Kingdom", "UK", region)) %>%
  filter(year == 2010)

map_world <- map_data("world", region = NULL) %>%
  select(-group, -order)

world_map <- map_data("world") %>%
  filter(region != "Antarctica")

waste_map <- world_map %>%
  left_join(mismanaged, by = "region") %>%
  rename(mismanaged = 9)

p <- ggplot(waste_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mismanaged), color = "#EEEED1") +
  scale_fill_viridis(option = "C") +
  labs(title = "Global Plastic Mismanagement",
       subtitle = "Per capita plastic mismanagement in kilograms per person per day",
       fill = "Kilograms per person\nper day",
       caption = "Data available for 2010") +
  theme_void(base_size = 14) +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        legend.position = "bottom",
        legend.title.align = .5,
        text = element_text(family = "Verdana"),
        plot.background = element_rect(fill = "#EEEED1"))

ggsave(p, file = "21-05-19-plastic.png")
