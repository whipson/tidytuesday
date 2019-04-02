library(tidyverse)
library(lubridate)
library(gganimate)
library(reshape2)

theme_set(theme_bw())

bikes_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

bikes <- bikes_raw %>%
  mutate(date = mdy_hms(date),
         year = year(date),
         month = month(date),
         day = day(date),
         hour = hour(date))

running_count <- bikes %>%
  filter(!is.na(bike_count)) %>%
  group_by(date = floor_date(date, "day"), crossing) %>%
  summarize(bike_count = sum(bike_count)) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  spread(crossing, bike_count) %>%
  arrange(date) %>%
  rename(greenway62 = '39th Ave NE Greenway at NE 62nd St',
         broadway = 'Broadway Cycle Track North Of E Union St',
         burke = 'Burke Gilman Trail',
         elliot = 'Elliot Bay Trail',
         mts = 'MTS Trail',
         greenway22 = 'NW 58th St Greenway at 22nd Ave',
         sealth = 'Sealth Trail') %>%
  mutate('39th Greenway' = ifelse(is.na(greenway62), 0, greenway62),
         'Broadway Cycle' = ifelse(is.na(broadway), 0, broadway),
         'Burke Gilman' = ifelse(is.na(burke), 0, burke),
         'Elliot Bay' = ifelse(is.na(elliot), 0, elliot),
         'MTS Trail' = ifelse(is.na(mts), 0, mts),
         '58th Greenway' = ifelse(is.na(greenway22), 0, greenway22),
         'Sealth Trail' = ifelse(is.na(sealth), 0, sealth)) %>%
  select(-id) %>%
  mutate_if(is.numeric, cumsum)

time_plot <- running_count %>%
  select(-c(2:8)) %>%
  gather(key = "crossing", value = "count", -date)

library(ggthemr)

p <- time_plot %>%
  ggplot(aes(date, count, group = crossing, color = fct_reorder(crossing, -count))) +
  geom_line(size = 1.25) +
  labs(x = "Date",
       y = "Cumulative Sum of Bicycle Crossings (in millions)",
       color = "Location",
       title = "Total Bicycle Crossings over time") +
  scale_y_continuous(labels = scales::comma_format(scale = 1 / 1000000, accuracy = .5)) +
  theme(axis.text = element_text(size = 12, color = "white"),
        legend.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(hjust = .5))

ggthemr('flat dark', type = 'outer')

p

p + transition_reveal(date) +
  labs(subtitle = "Date: {frame_along}")
