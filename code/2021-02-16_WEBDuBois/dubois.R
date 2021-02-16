# WEB DuBois
# 02/16/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-16/readme.md

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
    
    
# Read data
occupation <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/occupation.csv')

# Create Plot 1 (very messily...it works but I don't recommend)
p1 <- occupation %>%
  filter(Group == "Negroes") %>% 
  mutate(end = ifelse(Percentage >= 20, 20, Percentage)) %>% 
  ggplot(aes(x = reorder(Occupation, Percentage), y = end)) +
  geom_text(y = -1, aes(label = paste0(Percentage, "%")), family = "Vasarely-Regular") +
  geom_col(width = .15, fill = "#DC354A") +
  geom_segment(x = 4.75, xend = 4.75, y = 0, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.5, xend = 4.5, y = 0, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.25, xend = 4.25, y = 18, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 3.5, xend = 3.5, y = 12, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 5, xend = 4.75, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.75, xend = 4.5, y = 0, yend = 0, size = 3, color = "#DC143C") +
  geom_segment(x = 4.5, xend = 4.25, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 3.5, xend = 4, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_point(x = 5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.75, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.75, y = 0, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.5, y = 0, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.25, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 3.5, y = 20, size = 2.25, color = "#DC143C") +
  scale_x_discrete(labels = scales::wrap_format(10)) +
  coord_flip() +
  expand_limits(y = -1.5) +
  labs(title = "Negroes.") +
  theme_dubois()

# Create Plot 2 (see above)
p2 <- occupation %>%
  filter(Group == "Whites") %>% 
  mutate(end = ifelse(Percentage >= 20, 20, Percentage)) %>% 
  ggplot(aes(x = reorder(Occupation, Percentage), y = end)) +
  geom_text(y = -1, aes(label = paste0(Percentage, "%")), family = "Vasarely-Regular") +
  geom_col(width = .15, fill = "#DC354A") +
  geom_segment(x = 4.75, xend = 4.75, y = 0, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.5, xend = 4.5, y = 0, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.25, xend = 4.25, y = 16, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 5, xend = 4.75, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_segment(x = 4.75, xend = 4.5, y = 0, yend = 0, size = 3, color = "#DC143C") +
  geom_segment(x = 4.5, xend = 4.25, y = 20, yend = 20, size = 3, color = "#DC143C") +
  geom_point(x = 5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.75, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.75, y = 0, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.5, y = 0, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.5, y = 20, size = 2.25, color = "#DC143C") +
  geom_point(x = 4.25, y = 20, size = 2.25, color = "#DC143C") +
  scale_x_discrete(labels = scales::wrap_format(10)) +
  coord_flip() +
  expand_limits(y = -1.5) +
  labs(title = "Whites.") +
  annotate("text", 
           x = 1, y = 15,
           label = "1900.",
           family = "B52-ULCW00-ULC",
           size = 10
           ) +
  theme_dubois()


p1/p2 + plot_annotation(title = "Occupations of Negroes and Whites in Georgia.",
                        caption = "Created by @kllycttn | Data from W.E.B. DuBois's Data Portraits | #TidyTuesday, #DuBoisChallenge",
                        theme = theme_dubois()) 

ggsave("dubois.png", height = 15)

