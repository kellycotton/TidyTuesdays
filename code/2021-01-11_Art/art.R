# Art Collections
# 01/11/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md

# Setup----
library(tidyverse)
library(showtext)

font_add_google("Cinzel","Cinzel")
font_add_google("Noto Serif","Noto Serif")
showtext_auto()

# Get data----
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

art <- left_join(artwork, artists, by = c("artistId" = "id"))

# Build theme----
theme_art <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#381f21", color = "#381f21"),
          panel.grid = element_line(color = "#381f21"),
          panel.background = element_rect(fill = "#381f21", color = "#381f21"),
          text = element_text(color = "#c39ca4", family = "Cinzel"), 
          plot.title = element_text(color = "#c39ca4", hjust = 0.5, size = 22, family = "Cinzel", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, color = "#c39ca4", size = 9, family = "Noto Serif"),
          plot.caption = element_text(color = "#c39ca4", size = 6, hjust = 1, family = "Noto Serif"),
          axis.title = element_blank(),
          axis.text = element_text(color = "#c39ca4", size = 7),
          axis.ticks = element_blank())
}

# Prepare data----
art_age <- art %>% 
  filter_at(vars(artist, acquisitionYear, yearOfBirth, gender), all_vars(!is.na(.))) %>% 
  filter(artistRole == "artist" & !str_detect(dates, "established")) %>% 
  mutate(acquistion_age = acquisitionYear - yearOfBirth) %>% 
  filter(acquisitionYear >= 1914) %>% 
  filter((acquisitionYear < yearOfDeath | is.na(yearOfDeath))) %>% 
  group_by(artist) %>% 
  arrange(acquistion_age) %>% 
  filter(row_number() == 1)

# Plot----
ggplot(data = art_age, aes(x = acquisitionYear, y = acquistion_age)) +
  geom_point(alpha = 0.2, color = "#c39ca4") +
  geom_line(stat = "smooth", method = "gam", se = FALSE, color = "#e05959", alpha = 0.6) +
  stat_summary(geom = "point", fun = mean, size = 3, color = "#e05959", alpha = 0.6) +
  scale_x_continuous(expand = c(.02, .02)) +
  scale_y_continuous(expand = c(.02, .02), breaks = seq(20, 100, by = 10)) +
  theme_art() +
  labs(title = "100 Years of Art at the Tate Museum", 
       subtitle = "From 1914-2014, over 2000 artists had at least one piece of art acquired by the museum while they were still alive.
Each small dot below represents an artist whose art was acquired by the Tate, the year and the artist's age at 
acquisition. Each larger dot represents the average age of all artists for that year.", 
       caption = "Created by @kllycttn | Data from Tate Art Museum | #TidyTuesday")

ggsave("art.png")
  
  