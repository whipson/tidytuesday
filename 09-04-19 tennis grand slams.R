library(tidyverse)
library(lubridate)

slams <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv") %>%
  mutate(grand_slam = recode(grand_slam, australian_open = "Australian Open",
                             wimbledon = "Wimbledon",
                             us_open = "US Open",
                             french_open = "French Open"))

timeline_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv") %>%
  rename(name = player,
         grand_slam = tournament)

players <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv") %>%
  select(-age)

tournament_dates <- slams %>%
  select(year, grand_slam, tournament_date)

player_birthdays <- players %>%
  select(name, date_of_birth) %>%
  unique(.)

timeline <- timeline_raw %>%
  inner_join(tournament_dates) %>%
  unique(.) %>%
  inner_join(player_birthdays) %>%
  filter(!is.na(outcome),
         outcome != "Absent") %>%
  mutate(age = year(tournament_date) - year(date_of_birth),
         win = ifelse(outcome == "Won", 1, 0))

ggplot(timeline, aes(age, win)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), size = 1.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Player Age (in years)",
       y = "Odds of Winning",
       title = "Odds of Winning Tennis Grand Slam Decrease with Age",
       subtitle = "Data limited to players who won at least one grand slam during career") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = .5, size = 22),
        axis.text = element_text(size = 18))

