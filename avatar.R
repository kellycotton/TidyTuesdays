# Setup----
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(tvthemes)
library(scales)

import_avatar()

tuesdata <- tidytuesdayR::tt_load(2020, week = 33)

avatar <- tuesdata$avatar
stop_words <- stop_words

avatar$character <- gsub("[^A-z]","",avatar$character)

# Tokenize data (individual words)
token.avatar <- avatar %>%
  filter(character != "SceneDescription") %>%
  unnest_tokens(word, character_words) %>%
  anti_join(stop_words, by = "word")

# Word frequency----
token.avatar %>%
  filter(character != "SceneDescription") %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  ggplot(aes(x = reorder(word,n), y = n)) +
  geom_col(fill = "#a10000") + coord_flip() +
  labs(x = element_blank(), y = element_blank(),
       title = "Word Frequency of Avatar: The Last Airbender",
       subtitle = "Most frequent words across all seasons",
       caption = "Created by @kllycttn, Data from Avatar Wiki, #TidyTuesday") +
  scale_x_discrete(expand = expansion(mult = c(0, .1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     breaks = scales::breaks_pretty(n = 5)) +
  theme_avatar(legend.position = "none",
               title.font = "Slayer",
               text.font = "Slayer", 
               subtitle.size = 10, 
               text.size = 8, 
               title.size = 15) +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank())

# Top characters
characters.top8 <- token.avatar %>%
  count(character, sort = TRUE) %>%
  top_n(8) %>%
  pull(character)

# Number of lines per character
avatar %>% 
  filter(character != "SceneDescription") %>%
  group_by(character) %>% 
  count(sort=TRUE) %>%
  filter(character %in% characters.top8) %>%
  mutate(nation = ifelse(character == "Aang", "Air", 
                         ifelse(character %in% c("Sokka", "Katara"), "Water", 
                                ifelse(character %in% c("Zuko", "Iroh", "Azula", "Zhao"), "Fire", "Earth")))) %>%
  ggplot(aes(x = reorder(character,n), y = n, fill = nation)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("#ff9933", "#4C7022", "#a10000",  "#174D79")) +
  labs(x = element_blank(), y = element_blank(),
       title = "Word Frequency of Avatar: The Last Airbender",
       subtitle = "Most frequent words by character",
       caption = "Created by @kllycttn, Data from Avatar Wiki, #TidyTuesday") +
  scale_x_discrete(expand = expansion(mult = c(0, .1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     breaks = scales::breaks_pretty(n = 5)) +
  theme_avatar(legend.position = "none",
               title.font = "Slayer",
               text.font = "Slayer", 
               subtitle.size = 10, 
               text.size = 8, 
               title.size = 15) +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank())

# By book
avatar %>% 
  filter(character != "SceneDescription") %>%
  group_by(book_num,character) %>% 
  count(sort=TRUE) %>%
  ungroup() %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  filter(character %in% characters.top8) %>%
  mutate(nation = ifelse(character == "Aang", "Air", 
                         ifelse(character %in% c("Sokka", "Katara"), "Water", 
                                ifelse(character %in% c("Zuko", "Iroh", "Azula", "Zhao"), "Fire", "Earth")))) %>%
  ggplot(aes(x = factor(book_num), y = percent, group = character, color = nation)) +
  geom_point() + geom_line() +
  scale_color_manual(values = c("#ff9933", "#4C7022", "#a10000",  "#174D79")) +
  labs(x = element_blank(), y = element_blank(),
       title = "Character Lines in Avatar: The Last Airbender",
       subtitle = "Number of lines per character per season",
       caption = "Created by @kllycttn, Data from Avatar Wiki, #TidyTuesday") +
  scale_x_discrete(expand = expansion(mult = c(0, .1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     breaks = scales::breaks_pretty(n = 5)) +
  theme_avatar(legend.position = "none",
               title.font = "Slayer",
               text.font = "Slayer", 
               subtitle.size = 10, 
               text.size = 8, 
               title.size = 15) +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank())

# n grams----
trigram.avatar <- avatar %>%
  filter(character != "SceneDescription") %>%
  unnest_tokens(trigram, character_words, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram))

trigrams <- trigram.avatar %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE) %>%
  slice_max(order_by = n, n = 7) %>%
  unite(trigram, word1, word2, word3, sep = " ")

ggplot(trigrams, aes(x = reorder(trigram, n), y = n, fill = trigram)) +
  geom_col() + coord_flip()+
  scale_fill_avatar(palette = "AirNomads") +
  labs(x = element_blank(), y = element_blank(),
       title = "Trigrams of Avatar: The Last Airbender",
       subtitle = "There is no war in Ba Sing Se",
       caption = "Created by @kllycttn, Data from Avatar Wiki, #TidyTuesday") +
  scale_x_discrete(expand = expansion(mult = c(0, .1))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     breaks = scales::breaks_pretty(n = 5)) +
  theme_avatar(legend.position = "none",
               title.font = "Slayer",
               text.font = "Slayer", 
               subtitle.size = 10, 
               text.size = 8, 
               title.size = 15) +
  theme(plot.caption = element_text(family = "sans"),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank())
  