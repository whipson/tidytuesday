library(tidyverse)
library(extrafont)
library(ggimage)

loadfonts()

df_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv")

df <- df_raw %>%
  mutate(mission_title_mod = str_remove_all(mission_title, "-.*|\\s.*|[0-9]"),
         mission_title_mod = str_to_lower(trimws(mission_title_mod)),
         mission_title_mod = ifelse(str_starts(mission_title_mod, "sts"), "STS", mission_title_mod),
         mission_title_mod = ifelse(str_starts(mission_title_mod, "ast"), "ASTP", mission_title_mod),
         mission_title_mod = ifelse(nchar(mission_title_mod) < 2, "#", mission_title_mod),
         nationality = case_when(nationality == "U.S.S.R/Russia" ~ "USS/RUS",
                                 nationality == "U.S." ~ "USA",
                                 nationality == "China" ~ "CHN",
                                 TRUE ~ nationality)) %>%
  group_by(mission_title_mod) %>%
  filter(n() >= 3) %>%
  select(nationality, year_of_mission, mission_title, mission_title_mod)

predom_nat <- df %>%
  count(nationality, mission_title_mod) %>%
  group_by(mission_title_mod) %>%
  slice_max(n) %>%
  rename(predom_nat = nationality)

plot_df <- df %>%
  left_join(predom_nat, by = "mission_title_mod") %>%
  mutate(mission_title_mod = str_to_upper(mission_title_mod),
         mission_title_nat = paste0(mission_title_mod, " (", predom_nat, ")"))

ship_df <- plot_df %>%
  group_by(mission_title_mod) %>%
  slice_max(year_of_mission, with_ties = FALSE) %>%
  mutate(image = "space-ship.png")

plot_df %>%
  ggplot(aes(x = year_of_mission, y = reorder(mission_title_nat, -year_of_mission))) + 
  geom_line(size = 2, color = "white", lineend = "round", alpha = 0.10) +
  geom_line(size = 1.60, color = "white", lineend = "round", alpha = 0.45) +
  geom_line(size = 0.80, color = "seagreen2", lineend = "round", alpha = 0.95) +
  geom_image(data = ship_df, aes(image = image), color = "gray81", size = 0.06) +
  labs(x = NULL,
       y = NULL,
       title = "Space Mission Series Timeline",
       subtitle = "Piloted space missions from 1961 to 2016.\nNationality indexed by nation running most missions under that series.",
       caption = "'#' refers to mission series identified only by numbers.\nCreated by @whipson3") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Lucida Sans Unicode"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray12", size = 0.75),
        panel.grid.major.y = element_line(color = "gray6", size = 0.75),
        axis.text.x = element_text(color = "seagreen2", size = 14),
        axis.text.y = element_text(color = "slategray3", face = "bold"),
        plot.caption = element_text(color = "slategray3", size = 11, face = "italic"),
        plot.background = element_rect(fill = "black"),
        panel.border = element_rect(color = "slategray3", fill = NA),
        plot.title = element_text(color = "seagreen2", face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(color = "seagreen3", size = 11))

ggsave("14-07-20-astronauts.png")
