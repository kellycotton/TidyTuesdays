# Transit Costs
# 01/05/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-05/readme.md

# Setup----
library(tidyverse)
library(hrbrthemes)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost %>% 
  filter_at(vars(city, length, tunnel_per, start_year), all_vars(!is.na(.))) %>%
  mutate(tunnel_per = as.numeric(gsub("%", "", tunnel_per))) %>% 
  filter(tunnel_per > 0) %>% # Unclear if 0% actually means none of it has been completed (seems unlikely)
  mutate(complete_per = (tunnel_per/100 * length)) %>% 
  slice_max(order_by = length, n = 20) %>% 
  ggplot(aes(x = reorder(as.factor(e), length), y = length)) +
  geom_col(fill = "#CDE77F") +
  geom_col(aes(y = complete_per), fill = "#709176") +
  geom_text(aes(label = paste0(round(tunnel_per,0), "%")), nudge_y = 5) +
  geom_text(aes(label = paste0("Project began in ", start_year)), y = 28, 
            nudge_x = -.25, 
            size = 3, 
            fontface = "italic") +
  geom_text(aes(label = paste0(city,": ", line)), y = 28, fontface = "bold") +
  coord_flip() +
  ylab("Length of proposed line (km)") +
  labs(
    title = "Longest transit projects worldwide: How much is done?",
    subtitle = "Percent of line length completed for the top 20 longest transit projects.",
    caption = "Created by @kllycttn, Data from The Transit Costs Project, #TidyTuesday"
  ) +
  theme_ipsum() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("transit.png", height = 11, width = 10)
