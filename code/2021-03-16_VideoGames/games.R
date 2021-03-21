# Video Games + Sliced
# 3/16/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-16/readme.md

# Setup----
library(tidyverse)
library(shadowtext)

theme_set(theme_void())

# Read data
games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Only Sims data, rearrange months in order (not alphabetical)
sims <- games %>% 
  filter(gamename == "The Sims(TM) 3") %>% 
  filter(!year %in% c("2021", "2012")) %>% 
  mutate(month = factor(month, levels = month.name)) %>% 
  arrange(month)

# Create plot
ggplot(sims, aes(month, year, fill= peak)) + 
  geom_tile(color = "#0870B6", size = .5) +
  geom_text(data = filter(sims, (peak > 10000 | peak < 1540)), aes(label = scales::comma(peak)), 
            family = "The Sims Sans Bold", color = "white") +
  scale_fill_gradient(low="#135B56", high="#C8D572",
                      name = "Number of \nplayers",
                      labels = scales::comma) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(n.breaks = 8, expand = c(0.01, 0.01)) +
  labs(title = "Who still plays The Sims?",
       subtitle = "Highest number of simultaneous players on The Sims 3 per month, 2013 - 2020",
       caption = "Created by @kllycttn | Data from SteamCharts | #TidyTuesday") +
  theme(
    plot.title = element_shadowtext(color = "white",
                              hjust = 0,vjust = 0, 
                              size = 30, family = "immajer-Simlish"),
    axis.text = element_text(color = "white", family = "The Sims Sans Bold"),
    plot.subtitle = element_text(color = "white",
                                 hjust = 0, size = 10, family = "The Sims Sans Bold"),
    plot.caption = element_text(color = "white",
                                size = 9, family = "The Sims Sans Bold"),
    legend.title = element_shadowtext(color = "white",
                                      hjust = 0,vjust = 0, 
                                      size = 13, family = "SimLLHP"),
    legend.text = element_text(family = "The Sims Sans Bold", color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = "#0870B6", color = "#0870B6"),
    panel.background = element_rect(fill = "#0870B6", color = "#0870B6"),
    legend.background = element_rect(fill = "#0870B6", color = "#0870B6"),
    legend.key = element_rect(fill = "#0870B6", color = "#0870B6"),
    plot.margin = unit(c(.25, .1, .1, .1), "in")
  )

ggsave("sims.png", width = 10.5, height = 6, units = "in")
