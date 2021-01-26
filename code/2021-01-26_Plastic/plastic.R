# Plastic Pollution
# 01/26/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-26/readme.md

# Setup----
library(tidyverse)
library(rnaturalearth)
library(sf)

showtext::sho
# Read data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
world <- ne_countries(scale = "medium", returnclass = "sf") 

totals <- plastics %>% 
  mutate(country = trimws(country)) %>% 
  mutate(country = case_when(
    country == "United Kingdom of Great Britain & Northern Ireland" ~ "United Kingdom",
    country == "United States of America" ~ "United States",
    TRUE ~ country
  )) %>% 
  mutate(parent_company = tolower(str_replace_all(parent_company, '[[:punct:] ]+', ' '))) %>% 
  filter(year == 2020 & !parent_company %in% c("unbranded", "null", "grand total")) %>% 
  group_by(country) %>% 
  slice_max(order_by = grand_total, n = 1) %>% 
  mutate(cc_or_no = ifelse(parent_company == "the coca cola company", "The Coca-Cola Company", "All Others")) 

totals %>% 
  group_by(parent_company) %>% 
  count(sort = TRUE)

totals %>% 
  left_join(world, ., by = c("name" = "country")) %>% # join with the world data for plotting
  filter(name != "Antarctica") %>% 
  ggplot() +
  geom_sf(aes(fill = cc_or_no), color = NA) +
  scale_fill_manual(values = c("#F4AB3D", "#3A8C9E"), 
                    breaks = c("The Coca-Cola Company", "All Others"),
                    na.value = "grey50",
                    name = NULL) +
  labs(title = "Where does all this plastic pollution come from?",
       subtitle = "Brand audits in 2020 revealed that out of 53 participating countries, the top plastic pollution source was The Coca-Cola 
Company in 18 countries. The second highest source (Philip Morris International) was the top source in 5 countries.",
       caption = "Created by @kllycttn | Data from Break Free From Plastic | #TidyTuesday") +
  ggthemes::theme_map() +
  theme(
    plot.background = element_rect(fill = "#ADD8E6", color = "#ADD8E6"),
    panel.background = element_rect(fill = "#ADD8E6", color = "#ADD8E6"),
    legend.background = element_rect(fill = "#ADD8E6", color = "#ADD8E6"),
    legend.key = element_rect(fill = "#ADD8E6", color = "#ADD8E6"),
    text = element_text(family = "AvenirNext LT Pro Regular", size = 13),
    plot.title = element_text(color = "#083742", hjust = 0.5, size = 23, family = "AvenirNext LT Pro Bold"),
    plot.subtitle = element_text(hjust = 0.5,  size = 11),
    plot.caption = element_text(size = 9, hjust = 1)
  )

ggsave("plastic.png")

