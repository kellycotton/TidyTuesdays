# Setup----
library(tidyverse)
library(tidytext)
library(beyonce)
library(ggpubr)
library(hrbrthemes)

# Get data
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')


# Manually assign songs to albums
DangerLove <- c("Crazy in Love (Ft. JAY-Z)", "Naughty Girl", "That's How You Like It (Ft. JAY-Z)", "Baby Boy (Ft. Sean Paul)", "Hip Hop Star (Ft. Big Boi & Sleepy Brown)", "Be With You", "Me, Myself, and I", "Yes", "Signs (Ft. Missy Elliott)", "Speechless", "The Closer I Get to You (Ft. Luther Vandross)", "Dangerously in Love 2", "Beyoncé Interlude", "Gift From Virgo", "Daddy")
Bday <- c("Déjà Vu (Ft. JAY-Z)", "Get Me Bodied", "Suga Mama", "Upgrade U (Ft. JAY-Z)", "Ring the Alarm", "Kitty Kat", "Freakum Dress", "Green Light", "Irreplaceable", "Resentment", "Check On It (Ft. Bun B & Slim Thug)", "Encore for the Fans", "Listen" )
Sasha <- c("If I Were a Boy", "Halo", "Disappear", "Broken-hearted Girl", "Ave Maria", "Smash Into You", "Satellites", "That's Why You're Beautiful", "Save the Hero", "Single Ladies (Put a Ring on It)", "Radio", "Diva", "Sweet Dreams", "Video Phone", "Hello", "Ego")
Four <- c("1+1", "I Care", "I Miss You", "Best Thing I Never Had", "Party (Ft. André 3000)", "Rather Die Young", "Start Over", "Love on Top", "Countdown", "End of Time", "I Was Here", "Run the World (Girls)", "Lay Up Under Me", "Schoolin' Life", "Dance for You")
Beyonce <- c("Pretty Hurts", "Haunted", "Drunk in Love (Ft. JAY-Z)", "Blow", "No Angel", "Partition", "Jealous", "Rocket", "Mine (Ft. Drake)", "XO", "***Flawless (Ft. Chimamanda Ngozi Adichie)", "Superpower (Ft. Frank Ocean)", "Heaven", "Blue (Ft. Blue Ivy Carter)")

beyonce_lyrics_albums <- beyonce_lyrics %>% 
  filter(song_name %in% c(DangerLove, Bday, Sasha, Four, Beyonce)) %>% 
  mutate(album = case_when(
    song_name %in% DangerLove ~ "Dangerously in Love",
    song_name %in% Bday ~ "B'Day",
    song_name %in% Sasha ~ "I Am... Sasha Fierce",
    song_name %in% Four ~ "4",
    song_name %in% Beyonce ~ "Beyoncé"
  ))

# Lyric analysis
stop_words <- stop_words
tidy_b_lyrics <- beyonce_lyrics_albums %>%
  unnest_tokens(word, line)  %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!word %in%
           c("ooh", "yeah", "hey", "uh", "ya", "na", "em", "yo"))

tidy_b_lyrics %>% 
  count(word, sort = TRUE) %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(reorder(word, n), n)) + 
  geom_col() + 
  coord_flip()

# Sentiment analysis
lyrics_sentiments <- tidy_b_lyrics %>%
  inner_join(get_sentiments("nrc"), by = "word")

# Reorder sentiments and albums for plotting
lyrics_sentiments$sentiment <- factor(lyrics_sentiments$sentiment, levels = c("negative", "disgust", "anger", "fear", "sadness", "trust", "anticipation", "joy", "positive"))
lyrics_sentiments$album <- factor(lyrics_sentiments$album, levels = c("Dangerously in Love", "B'Day", "I Am... Sasha Fierce", "4", "Beyoncé"))

lyrics_sentiments %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  filter(sentiment != "NA") %>% 
  group_by(album) %>% 
  count(sentiment) %>% 
  mutate(percent = round(n/sum(n)*100, digits = 0)) %>% 
  select(-"n") %>% 
  ggballoonplot(fill = "percent", size = 20, color = "percent") +
  scale_fill_gradientn(colours = beyonce_palette(11)) +
  scale_color_gradientn(colours = beyonce_palette(11)) +
  scale_x_discrete(limits = rev(levels(lyrics_sentiments$album))) +
  coord_flip() + guides(size = FALSE) +
  theme_ft_rc() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    legend.title = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
    ) +
  labs(title = "Sentiment Analysis of Beyoncé",
       subtitle = "Percent Sentiment per Album",
       caption = "Created by @kllycttn, #TidyTuesday")

ggsave("Plots/beyonce.png")  
  
