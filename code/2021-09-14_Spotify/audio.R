# Billboard Top 100
# 9/14/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-14/readme.md

# Setup----
library(tidyverse)
library(gghighlight)

theme_set(theme_minimal(base_family = "Lora"))
palette <- c("#171A21", "#DA9F93", "#B6465F", "#890620", "#2C0703", "#88AB75", "#DBD56E", "#2D93AD", "#96C5B0")

# Read data 
audio <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

audio_genre <- audio %>% 
  mutate(spotify_genre = strsplit(as.character(spotify_genre), ", ")) %>% # split the ingredients in each cell
  unnest(spotify_genre) %>% 
  mutate(spotify_genre = gsub("[[:punct:]]","" , spotify_genre)) %>%  # remove punctuation
  na.omit()

top_dance_genres <- audio_genre %>% 
  filter(!is.na(danceability)) %>% 
  filter(!is.na(energy)) %>% 
  group_by(spotify_genre) %>% 
  summarise(count = n()) %>% 
  slice_max(order_by = count, n = 200) %>% 
  pull(spotify_genre)

valence_dance <- audio_genre %>% 
  filter(spotify_genre %in% top_dance_genres) %>% 
  group_by(spotify_genre) %>% 
  summarise(dance = mean(danceability), valence = mean(valence), count = n())

ggplot(valence_dance, aes(x = dance, y = valence, color = spotify_genre)) +
  geom_point() +
  coord_flip() +
  scale_color_manual(values = palette) +
  gghighlight(max(dance) > .8 | min(dance) < .48 | max(valence) > .78 | min(valence) < .4, 
              label_params = list(size = 4)) +
  labs(title = "Is happier music more danceable?", 
       subtitle = "Danceability represents how suitable a track is for dancing by combining 
musical elements including tempo, rhythm stability, 
<br>beat strength, and overall regularity. A higher value means the track is more danceable. Valence describes the musical
<br> positiveness conveyed by the track, with a higher valence indicating more positivity. Some genres like <span style = 'color:#88AB75;'>**old school hip hop**</span> are 
<br>both highly danceable and pretty positive. Other genres are highly positive but less danceable, 
like <span style = 'color:#DBD56E;'>**postpunk**</span>, or less positive 
<br>and less danceable, like <span style = 'color:#2D93AD;'>**vocal jazz**</span>.", 
       caption = "Created by @kllycttn | Data from Spotify | #TidyTuesday") +
  xlab("Danceability") + ylab("Valence") +
  theme(
    plot.title = element_text(family = "Lato", face = "bold", size = 25),
    plot.subtitle = ggtext::element_markdown(lineheight = .2),
    plot.caption = element_text(size = 7),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank()
  )

ggsave(here::here("code", "2021-09-14_Spotify", "danceable.png"), width = 9, height = 6.5, units = "in", bg = "white")

