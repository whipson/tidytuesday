library(tidyverse)
library(broom)
library(lme4)
library(gghighlight)

class_size <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv") %>%
  filter(str_detect(country_code, "[:alpha:]"),
         !indicator %in% c("Post-Secondary Non-Tertiary Education", "Tertiary Education")) %>%
  select(-edulit_ind, -flag_codes, -flags, -country_code) %>%
  mutate(country = ifelse(country == "China, Macao Special Administrative Region", "Macao", country))

class_size_diff <- class_size %>%
  filter(indicator == "Upper Secondary Education",
         year <= 2017) %>%
  select(-indicator) %>%
  group_by(country) %>%
  filter(year == min(year) | year == max(year)) %>%
  group_by(country) %>%
  mutate(diff = student_ratio - lag(student_ratio)) %>%
  select(-year, -student_ratio) %>%
  na.omit(.)
  
class_size_plot <- class_size %>%
  filter(indicator == "Upper Secondary Education",
         year <= 2017,
         student_ratio < 40) %>%
  left_join(class_size_diff) %>%
  group_by(country) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 4)

hi <- class_size_plot %>%
  ggplot() +
  geom_line(aes(x = year, y = student_ratio, color = country, group = country), size = 1.5) +
  gghighlight(max(-diff), max_highlight = 5L, unhighlighted_colour = ggplot2::alpha("grey", 0.25),
              label_params = list(size = 5)) +
  scale_x_continuous(expand = c(0, 0.15)) +
  scale_color_manual(values = c("#0000EE", "#CD0000", "#FF7F00", "#8B1C62", "#2E8B57")) +
  labs(y = "Student-Teacher Ratio",
       x = "Year") +
  theme_minimal(base_size = 16)

lo <- class_size_plot %>%
  ggplot() +
  geom_line(aes(x = year, y = student_ratio, color = country, group = country), size = 1.5) +
  gghighlight(max(diff), max_highlight = 5L, unhighlighted_colour = ggplot2::alpha("grey", 0.25),
              label_params = list(size = 5)) +
  scale_x_continuous(expand = c(0, 0.15)) +
  scale_color_manual(values = c("#0000EE", "#CD0000", "#FF7F00", "#8B1C62", "#2E8B57")) +
  labs(y = "Student-Teacher Ratio",
       x = "Year") +
  theme_minimal(base_size = 16)

library(ggpubr)

figure <- ggarrange(hi, lo,
          labels = c("Decreasing Ratio (Good)", "Increasing Ratio (Bad)"),
          font.label = list(size = 14, face = "plain"),
          nrow = 1, ncol = 2)

annotate_figure(figure,
                top = text_grob(label = "Which countries are most/least improved in student-teacher ratio for Upper Secondary Education?", size = 18, face = "bold"),
                bottom = text_grob(label = "One outlier (Eritrea) removed to better show differences between countries.",
                                   hjust = 1, x = 1, face = "italic"))
