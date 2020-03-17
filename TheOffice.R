# Setup----
library(schrute)
library(tidyverse)
library(tidytext)
library(viridis)
library(ggrepel)

# IMDB rating data
office_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
office_ratings$season <- sprintf("%02d",office_ratings$season)
office_ratings$episode <- sprintf("%02d",office_ratings$episode)

# Transcript data
mydata <- schrute::theoffice
# Remove "" from the character column
mydata$character <- gsub("\"","",mydata$character)

# Tokenize the data (individual words)
token.mydata <- mydata %>%
  unnest_tokens(word, text) 

# Remove frequent words (the, it, etc.)
stop_words <- stop_words
tidy.token.mydata <- token.mydata %>%
  anti_join(stop_words, by = "word")

# Word frequency----
# Calculate word frequency (above 400)
word.freq <- tidy.token.mydata %>%
  count(word, sort = TRUE) %>%
  filter(n>400)

# Plot most frequent words 
ggplot(word.freq, aes(x=reorder(word,n),y=n)) +
  geom_bar(stat="identity") + coord_flip() +
  labs(x = "Word", y ="Frequency of Word" ,title ="Most Frequently Used Words")

# Episode ratings----
# Plot IMDB ratings over episodes 
#png("rating.png",units="in",width=5,height=7,res=300)
#print (
ggplot(office_ratings, aes(season, episode, fill = imdb_rating)) + 
  geom_tile(colour = "white") + 
  scale_fill_viridis(option="magma") +
  labs(x = "Season", y ="Episode" ,title ="IMDB Ratings of The Office",fill="Rating")
#)
#dev.off()

# Word sentiment----
# Calculate sentiment of words based on the Bing lexicon
office.sentiment <- tidy.token.mydata %>%
  inner_join(get_sentiments("bing")) %>%
  count(season, index = episode, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
colnames(office.sentiment)[2] <- "episode"

# Combine sentiment and rating data
ratings.sentiment <- merge(office.sentiment, office_ratings, by = c("season","episode"))

# Plot sentiment across seasons
ggplot(office.sentiment, aes(sentiment)) +
  geom_density(aes(color=season)) +
  scale_color_viridis(option="magma",discrete = TRUE) +
  labs(x = "Mean sentiment rating across season", y ="Density", title ="Sentiment Ratings Across Seasons", color="Season")

# Plot sentiment vs. IMDB rating
#png("ratingsentiment.png",units="in",width=7,height=5,res=300)
#print (
ggplot(ratings.sentiment, aes(x=sentiment,y=imdb_rating,group=season,color=season)) +
  geom_jitter() +
  scale_color_viridis(option="magma",discrete = TRUE) +
  labs(x = "Sentiment rating", y ="IMDB Rating" ,title ="Sentiment vs. IMDB Rating",color="Season") +
  geom_label_repel(aes(label = title),
                  data = subset(ratings.sentiment, (sentiment <= -50 | sentiment >= 35)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') 
#)
#dev.off()

# Characters line frequency----
# Find Top 20 characters by number of lines
characters.top20 <- tidy.token.mydata %>%
  count(character, sort = TRUE) %>%
  top_n(20) %>%
  pull(character)

# Calculate number of lines per character
characters <- mydata %>% 
  group_by(season, episode) %>% 
  ungroup() %>% 
  group_by(character) %>% 
  count(sort=TRUE)  

# Plot most-speaking characters
ggplot(subset(characters, character %in% characters.top20),aes(x=reorder(character,n),y=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x = "Character", y ="Number of Lines" ,title ="Number of Lines by Character") 

# Character sentiment----
# Calculate average sentiment for each character
character.sentiment <- tidy.token.mydata %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(character) %>%
  filter(is.element(character,characters.top20)) %>%
  count(character, index = season, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Plot sentiment for each character (average per season)
ggplot(character.sentiment, aes(x=reorder(character,sentiment),y=sentiment)) +
  geom_boxplot() + 
  coord_flip() +
  labs(x = "Character", y ="Sentiment Rating" ,title ="Character Sentiments") 

# Angela's love life
# Calculate number of times the State Senator is mentioned
angelas.senator <- tidy.token.mydata %>%
  filter(str_detect(word, "senator")) %>%
  count(character)

# Plot who talks about the State Senator
ggplot(angelas.senator,aes(x=reorder(character,n),y=n)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  labs(x = "Character", y ="Number of Senator Mentions" ,title ="Who talks about the state senator?") 

# Calculate who Angela talks about: the State Senator or Dwight
angela.love <-  tidy.token.mydata %>%
  subset(character=="Angela") %>%
  filter(str_detect(word, "dwight$|senator$")) 

# Plot who Angela talks about
ggplot(angela.love, aes(word)) +
  geom_bar() + 
  labs(x = "Angela's Lover", y ="Number of Mentions" ,title ="Who does Angela talk about?") 


