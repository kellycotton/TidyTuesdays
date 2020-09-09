# Setup----
library(tidyverse)
library(tidytuesdayR)
library(stringr)
library(gender)
library(lubridate)
library(hrbrthemes)
library(RColorBrewer)

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 37)

friends <- tuesdata$friends
emotions <- tuesdata$friends_emotions
info <- tuesdata$friends_info


director_info <- info %>%
  mutate(director = strsplit(directed_by, " & ")) %>%
  unnest(cols = c(director)) %>% 
  mutate(director_fname = str_remove_all(director, " .*")) 

director_gender <- gender(unique(director_info$director_fname), method = "ssa")

director_info <- left_join(director_info, director_gender, by = c("director_fname" = "name"))

director_info %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ggplot(aes(x = gender, y = n)) +
  geom_col() + coord_flip()

director_info %>% 
  group_by(directed_by) %>% 
  count(directed_by) %>% 
  ggplot(aes(x = reorder(directed_by, n), y = n)) +
  geom_col() + coord_flip()


writer_info <- info %>%
  mutate(written_by = str_remove_all(written_by, "Story by : ")) %>%
  mutate(written_by = str_replace_all(written_by, "Teleplay by : ", " & ")) %>% 
  mutate(written_by = str_replace_all(written_by, "McC", "Mcc")) %>% 
  mutate(written_by = str_replace_all(written_by, "([:upper:]{1})([:lower:]+)([:upper:]{1})", "\\1\\2 & \\3")) %>% 
  mutate(writer = str_split(written_by, "& ")) %>%
  unnest(cols = c(writer)) %>% 
  mutate(writer_fname = str_remove_all(writer, " .*")) %>% 
  mutate(writer = str_trim(writer))

writer_gender <- gender(unique(writer_info$writer_fname), method = "ssa")

writer_info <- left_join(writer_info, writer_gender, by = c("writer_fname" = "name"))

# Manually enter the NAs
writer_info$gender[writer_info$writer == "Pang-Ni Landrum "] <- "female"
writer_info$gender[writer_info$writer == "R. Lee Fleming Jr. "] <- "male"
writer_info$gender[writer_info$writer == "R. Lee Fleming Jr."] <- "male"
#writer_info$gender[writer_info$writer_fname == "	Jill"] <- "female"

writer_info %>% 
  group_by(gender) %>% 
  count(gender) %>% 
  ggplot(aes(x = gender, y = n)) +
  geom_col(na.rm = TRUE) + coord_flip()

writer_info %>% 
  group_by(writer) %>% 
  count(writer) %>% 
  ggplot(aes(x = reorder(writer, n), y = n)) +
  geom_col() + coord_flip()

writer_info %>% 
  group_by(gender) %>% 
  summarise(mean_views = mean(us_views_millions), mean_ratings = mean(imdb_rating), n = n()) %>% 
  ggplot(aes(x = gender, y = mean_views)) +
  geom_point()

writer_info <- writer_info %>%
  mutate(year = year(air_date),
         month = month(air_date, label=TRUE))
  
date_gender <- writer_info %>% 
  select(year, month, gender) %>%
  group_by(year, month, gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  filter(gender == "female")
  
ggplot(date_gender, aes(x = year, y = month, fill = freq)) +
  geom_tile(color= "white",size=0.1)
  

season_gender <- writer_info %>% 
  select(season, gender, us_views_millions, imdb_rating) %>%
  group_by(season, gender) %>%
  summarise(n = n(), mean_views = mean(us_views_millions), mean_rating = mean(imdb_rating)) %>%
  mutate(freq = n / sum(n)) %>%
  filter(gender != "NA")

ggplot(season_gender, aes(x = season, y = gender, fill = freq)) +
  geom_tile(data = filter(season_gender, gender == "female"), color= "white") +
  theme_ipsum() +
  scale_fill_gradient(low = "#77519e", high = "#097977")

ggplot(season_gender, aes(x = season, y = n, fill = gender)) +
  geom_col(position = position_dodge2()) +
  theme_ipsum() +
  scale_fill_manual(values = c("#097977", "#77519e"))

season_writer <- writer_info %>% 
  select(season, writer, gender, us_views_millions, imdb_rating) %>%
  group_by(season, writer) %>%
  summarise(n = n(), mean_views = mean(us_views_millions), mean_rating = mean(imdb_rating)) 

 
