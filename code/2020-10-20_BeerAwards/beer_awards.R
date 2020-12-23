# Setup----
library(tidyverse)
library(tidytuesdayR)
library(ggchicklet)
library(ggdark)
library(showtext)

font_add_google("Bebas Neue","Bebas Neue")
showtext_auto()

theme_set(dark_theme_minimal(base_family = "Bebas Neue"))

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 43)
beer_awards <- tuesdata$beer_awards

beer_awards$medal <- factor(beer_awards$medal, levels = c("Gold", "Silver", "Bronze"))

top_breweries <- beer_awards %>% 
  group_by(brewery) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  slice_max(order_by = n, n = 10) %>% 
  pull(brewery)
  
beer_awards %>% 
  filter(brewery %in% top_breweries) %>% 
  group_by(brewery, medal) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(brewery, n), y = n, fill = medal)) +
  geom_chicklet(show.legend = FALSE, width = .95) + 
  coord_flip() +
  scale_fill_manual(values = c("#FFD700", "#C0C0C0", "#CD7F32")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 12, margin = margin(r = -20)),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 13)
  ) +
  labs(title = "Award-winning Breweries",
       subtitle = "Number of Gold, Silver, and Bronze Medals for Top 10 Breweries, 1987 - 2020",
       caption = "Created by @kllycttn, Data from Great American Beer Festival, #TidyTuesday")

ggsave("Plots/top_beers.png")
