# Employed Status
# 02/23/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-23/readme.md

# Setup----
library(tidyverse)
library(patchwork)

# Theme for plotting
theme_dubois <- function() {
  theme_minimal() %+replace%
    theme(
      plot.background = element_rect(fill = "#DFD4C7", color = "#DFD4C7"),
      panel.grid = element_line(color = "#DFD4C7"),
      panel.background = element_rect(fill = "#DFD4C7", color = "#DFD4C7"),
      plot.title = element_text(family = "B52-ULCW00-ULC"),
      text = element_text(family = "Vasarely-Regular"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(hjust = .5),
      plot.caption = element_text(size = 9)
    ) 
}
# Get data
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

employed_data <-employed %>% 
  filter(year == "2020" & race_gender %in% c("Asian", "Black or African American", "White")) %>% 
  group_by(race_gender) %>% 
  mutate(total_employed = sum(employ_n, na.rm = TRUE)) %>% 
  group_by(major_occupation, .add = TRUE) %>% 
  mutate(per = round(100 * employ_n/total_employed, 2)) %>% 
  group_by(race_gender, major_occupation) %>% 
  summarise(per_employed = round(sum(per, na.rm = TRUE), 0)) %>% 
  mutate(major_occupation = case_when(
    major_occupation == "Management, professional, and related occupations" ~ "Management",
    major_occupation == "Sales and office occupations" ~ "Sales and office",
    major_occupation == "Service occupations" ~ "Service",
    major_occupation == "Production, transportation, and material moving occupations" ~ "Production and transportation",
    major_occupation == "Natural resources, construction, and maintenance occupations" ~ "Natural resources, construction, and maintenance",
    TRUE ~ major_occupation
  ))

# Plots----

p1 <- employed_data %>% 
  filter(race_gender == "Black or African American") %>% 
  mutate(end = ifelse(per_employed >= 20, 20, per_employed)) %>% 
  ggplot(aes(x = reorder(major_occupation, per_employed), y = end)) +
  geom_text(y = -1, aes(label = paste0(per_employed, "%")), family = "Vasarely-Regular") +
  geom_col(width = .15, fill = "#DC354A") +
  geom_segment(x = 4.5, xend = 4.5, y = 9, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 3.5, xend = 3.5, y = 17, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 2.5, xend = 2.5, y = 19, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 5, xend = 4.5, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 3.5, xend = 4, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 2.5, xend = 3, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_point(x = 5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 3.5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 3, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 2.5, y = 20, size = 2.25, color = "#DC143C") +
  scale_x_discrete(labels = scales::wrap_format(10)) +
  coord_flip() +
  expand_limits(y = -1.5) +
  labs(title = "Black.") +
  theme_dubois()

p2 <- employed_data %>% 
  filter(race_gender == "Asian") %>% 
  mutate(end = ifelse(per_employed >= 20, 20, per_employed)) %>% 
  ggplot(aes(x = reorder(major_occupation, per_employed), y = end)) +
  geom_text(y = -1, aes(label = paste0(per_employed, "%")), family = "Vasarely-Regular") +
  geom_col(width = .15, fill = "#DC354A") +
  geom_segment(x = 4.75, xend = 4.75, y = 0, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.5, xend = 4.5, y = 0, yend = 13, size = 3, color = "#DC143C") +
  geom_segment(x = 5, xend = 4.75, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.75, xend = 4.5, y = 0, yend = 0, size = 3, color = "#DC143C") +
  geom_point(x = 5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.75, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.75, y = 0, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.5, y = 0, size = 2.25, color = "#DC143C") +
  scale_x_discrete(labels = scales::wrap_format(10)) +
  coord_flip() +
  expand_limits(y = -1.5) +
  labs(title = "Asian.") +
  theme_dubois()

p3 <- employed_data %>% 
  filter(race_gender == "White") %>% 
  mutate(end = ifelse(per_employed >= 20, 20, per_employed)) %>% 
  ggplot(aes(x = reorder(major_occupation, per_employed), y = end)) +
  geom_text(y = -1, aes(label = paste0(per_employed, "%")), family = "Vasarely-Regular") +
  geom_col(width = .15, fill = "#DC354A") +
  geom_segment(x = 4.5, xend = 4.5, y = 1, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 3.5, xend = 3.5, y = 17, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 5, xend = 4.5, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 3.5, xend = 4, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_point(x = 5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 3.5, y = 20, size = 2.25, color = "#DC143C") +
  scale_x_discrete(labels = scales::wrap_format(10)) +
  coord_flip() +
  expand_limits(y = -1.5) +
  labs(title = "White.") +
  annotate("text", 
           x = 1, y = 15,
           label = "2020.",
           family = "B52-ULCW00-ULC",
           size = 10
  ) +
  theme_dubois()


p1/p2/p3 + plot_annotation(title = "Occupations in the US.",
                           subtitle = "In the style of W.E.B. Dubois's Data Portraits",
                        caption = "Created by @kllycttn | Data from US Bureau of Labor Statistics | #TidyTuesday",
                        theme = theme_dubois())   

ggsave("employment.png", height = 10, width = 6, units = "in")
