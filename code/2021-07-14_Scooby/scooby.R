# Scooby-Doo
# 7/13/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-13/readme.md

# Setup----
library(tidyverse)

# Read data 
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

# Colors
pal <- c("#2596be", "#f89a1e","#64cbc2", "#7969af", "#c1d82f")

# Set arrow coordinates
arrows <- 
  tibble(
    x1 = c(1979, 2000), 
    x2 = c(1980, 1995),
    y1 = c(4.9, 7.4),
    y2 = c(3.7, 7)
  )

catchphrases <- scoobydoo %>% 
  select(index, date_aired, jeepers, jinkies, zoinks, groovy, rooby_rooby_roo) %>% 
  mutate(date_aired = format(date_aired, format = "%Y")) %>% 
  pivot_longer(cols = -c(index, date_aired),
               names_to = "phrase",
               values_to = "count") %>% 
  mutate(decade = round(as.numeric(date_aired) %/% 10)*10,
         count = ifelse(count == "NULL", 0, count)) %>% 
  group_by(decade, phrase) %>% 
  summarize(per_ep = mean(as.numeric(count)))

ggplot(catchphrases, aes(x = decade, y = per_ep, fill = phrase)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal, 
                    labels = c("Groovy", "Jeepers", "Jinkies", "Rooby Rooby Roo", "Zoinks"),
                    name = NULL) +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
                     name = NULL,
                     expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  ylab("Average Times Said per Episode") +
  annotate("text",
           label = "\"Jinkies\" had the highest mean count: 3.7 times per episode in the 90s.",
           x = 2008,
           y = 7.5,
           family = "Scooby Doo"
  ) +
  annotate("text",
           label = "In the 80s, \"Groovy\" and \"Jeepers\" were never said.",
           x = 1973,
           y = 5,
           family = "Scooby Doo"
  ) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3, 
    inherit.aes = FALSE
  ) +
  labs(title = "If it wasn't for those meddling catchphrases...", 
       subtitle = "", 
       caption = "Created by @kllycttn | Data from Kaggle & plummye | #TidyTuesday") +
  theme_light() +
  theme(
    plot.title = element_text(family = "Flowers Kingdom", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(family = "Scooby Doo"),
    legend.position = c(.85, .77),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) 
  


