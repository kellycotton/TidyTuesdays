# Setup----
library(tidyverse)
library(tidytuesdayR)
library(ggbump)
library(nycpalettes)
library(lubridate)
library(hrbrthemes)

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 39)
expeditions <- tuesdata$expeditions

expeditions$decade <- parse_date_time(expeditions$year,"y")
expeditions$decade <- floor_date(expeditions$decade, years(10))
expeditions$decade <- gsub("-.*","", expeditions$decade)

exp <- expeditions %>% 
  filter(year >= 1980) %>% 
  group_by(peak_name, year) %>% 
  summarise(decade = decade, number = n()) %>% 
  distinct(peak_name, decade, number) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(rank = rank(-number, ties.method = "random")) %>% 
  ungroup() %>% 
  group_by(peak_name) %>% 
  mutate(meanrank = mean(rank)) %>% 
  ungroup()   

exp2 <- exp %>% 
  filter(peak_name %in% (exp %>% 
                           distinct(peak_name, meanrank) %>% 
                           top_n(5, -meanrank) %>% 
                           pull(peak_name))) %>% 
  group_by(year) %>% 
  mutate(rank = rank(-number, ties.method = "random")) %>% 
  ungroup() %>%
  select(-meanrank)

endpoints <- exp2 %>% 
  group_by(peak_name, decade) %>% 
  filter(year %in% c("1980", "1990", "2000", "2010") | row_number() > (n() - 1))

ggplot(exp2, aes(year, rank, group = peak_name, color = peak_name)) +
  geom_bump(size = 1.5, lineend = "round") +
  geom_point(data = endpoints, size = 2.5) + 
  scale_y_reverse() +
  scale_x_continuous(labels = scales::number_format(1, big.mark = ""),
                     breaks = scales::breaks_extended(10)) +
  scale_color_manual(values = nyc_palette("BrooklynBridge")) +
  facet_wrap(~decade, scales = "free_x") +
  theme_ft_rc() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.text=element_text(size=10)
  ) +
  labs(title = "Himalayan Expeditions 1980 - 2019",
       subtitle = "Ranking of the top peaks by total number of expeditions per year",
       caption = "Created by @kllycttn, Data from The Himalayan Database, #TidyTuesday")
