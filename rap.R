# Setup----
library(tidyverse)
library(tidytext)
library(genius)
library(ggrepel)
library(bbplot)
library(viridis)
library(tm)

# Get data
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Scores over time----
# Plot overall scores over time for each song
#png("rapratings.png",units="in",width=8,height=7,res=300)
#print (
ggplot(rankings) + aes(x=year,y=points) +
  geom_point(color="#1380A1",alpha=.6,size=3) + 
  labs(title="Most Critically-Acclaimed Rap Songs") +
  geom_text_repel(aes(label = paste(title,artist, sep=" - ")),
                  data = subset(rankings, (points > 50 | (points > 20 & year >= 2010))),
                  box.padding   = 0.3, 
                  point.padding = 0.7,
                  segment.color = 'grey50',
                  color="black") +
  bbc_style()
#)
#dev.off()

# Plotting sum of total scores for each year
rankings %>%
  group_by(year) %>%
  summarise(points = sum(points)) %>%
  ggplot(aes(x=year,y=points)) +
  geom_bar(stat="identity",fill="#FAAB18") +
  labs(title = "Total Score for All Songs") +
  bbc_style()

# Featured artists----
# Plotting the difference in scores for songs with features and without  
rankings %>%
  mutate(feature = str_detect(artist," ft. ")) %>%
  group_by(feature,year) %>%
  summarise(points=sum(points)) %>%
  ggplot() + aes(x=year,y=points,fill=feature) +
  geom_col(position="dodge") +
  scale_fill_viridis(discrete=TRUE,
                     labels=c("No Feature", "Feature")) +
  labs(title="Scores for Songs With and Without Features") +
  guides(fill = guide_legend(reverse = TRUE)) +
  bbc_style()

# Female artists----
# Plotting gender data, wanted to do something with female artists over time but I'm disappointed with this plot
# There aren't many female artists to begin with!
ggplot(subset(rankings,gender=="female"), aes(x=year, y=reorder(artist,points), fill = points)) + 
  geom_tile() + 
  scale_fill_viridis() +
  labs(title = "Scores for Female Artists Over Time") +
  bbc_style()

# Get and clean lyric data----
# Gathering lyrics for the top 50 songs and cleaning the input because the Genius package 
# looks for fairly specific input (like Fugees, not The Fugees!)
# This is a pretty messy section, I would really like to clean it up
artist_songs <- rankings[c(3,2)] 
artist_songs$artist <- str_replace(artist_songs$artist,"&","and") 
artist_songs$artist <- str_remove(artist_songs$artist,"ft.*") # Remove the ft artists so the songs are found on Genius
artist_songs$artist <- removePunctuation(artist_songs$artist)
artist_songs$title <- removePunctuation(artist_songs$title)
artist_songs$title <- removePunctuation(artist_songs$title)
artist_songs$title <- str_replace(artist_songs$title,"’","") # This apostrophe is different than what my keyboard produces! I had to directly copy and paste it from the file...
artist_songs[artist_songs$artist=="WuTang Clan","artist"] <- "Wu Tang Clan"
artist_songs[artist_songs$artist=="The Fugees","artist"] <- "Fugees"
artist_songs[artist_songs$title=="Country Grammar","title"] <- "Country Grammar Hot Shit" # Apparently this is the real name of the song

# This next line takes awhile to run, actually getting the lyrics using the Genius package
lyrics <- artist_songs %>%
  add_genius(artist,title,type="lyrics")

# Tokenizing the lyrics 
token.lyrics <-  lyrics %>%
  unnest_tokens(word, lyric) 

token.lyrics$artist <- str_squish(token.lyrics$artist) # Remove the space after a few names

# Word frequency----
# Calculating word counts by song
word.count <- token.lyrics %>%
  count(title, word, sort = TRUE) %>%
  subset(word!="NA") 

# Calculating word counts by artist
artist.count <- token.lyrics %>%
  count(artist, word, sort = TRUE) %>%
  subset(word!="NA") 

# Calculating total word counts for each song
totalword.count <- word.count %>%
  group_by(title) %>%
  summarise(total=sum(n))

# Calculating total word counts for each artist. Unforunately this is biased by artists who have multiple songs in the Top 50 (Tupac)
totalartist.count <- artist.count %>%
  group_by(artist) %>%
  summarise(total=sum(n))

# Plot the songs with the highest word counts
#png("longsongs.png",units="in",width=9,height=7,res=300)
#print (
ggplot(top_n(totalword.count,50), aes(x=reorder(title,total),y=total)) +
  geom_bar(stat="identity", fill="#1380A1") + coord_flip() +
  labs(x = "Song", y ="Number of Words" ,title ="Word Count of 50 Longest Songs") +
  bbc_style() +
  theme(axis.text=element_text(size=9)) 
#)
#dev.off()

# Plot the songs with the lowest word counts
#png("shortsongs.png",units="in",width=8.5,height=7,res=300)
#print (
  ggplot(top_n(totalword.count,-50), aes(x=reorder(title,total),y=total)) +
    geom_bar(stat="identity", fill="#1380A1") + coord_flip() +
    labs(x = "Song", y ="Number of Words" ,title ="Word Count of 50 Shortest Songs") +
    bbc_style() +
    theme(axis.text=element_text(size=9)) 
#)
#dev.off()

# Plot the term frequency - inverse document frequency (TF-IDF) for each song and artist
word.count <- left_join(word.count,totalword.count)
artist.count <- left_join(artist.count, totalartist.count)

word.count <- word.count %>%
  bind_tf_idf(word,title,n)

artist.count <- artist.count %>%
  bind_tf_idf(word,artist,n)

#png("tfidflong.png",units="in",width=12,height=9,res=300)
#print (
word.count %>%
  subset(total>1330) %>% # Plot only the songs with > 1330 words
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(title) %>% 
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word,tf_idf), tf_idf, fill = title)) + # I wasn't able to get this ordered very wellc
  geom_col(show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = NULL, title = "Most Unique Words of Longest Songs", subtitle = "TF-IDF for songs with > 1330 words") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip() +
  bbc_style() +
  theme(axis.text=element_text(size=10)) 
#)
#dev.off()

#png("tfidfshort.png",units="in",width=11,height=13,res=300)
#print (
word.count %>%
  subset(total<335) %>% # Plot only the songs with < 335 words
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(title) %>% 
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(reorder(word,tf_idf), tf_idf, fill = title)) + # I wasn't able to get this ordered very wellc
  geom_col(show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = NULL, title = "Most Unique Words of Shortest Songs", subtitle = "TF-IDF for songs with < 335 words") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip() +
  bbc_style() +
  theme(axis.text=element_text(size=10)) 
#)
#dev.off()

#png("tfidfartist.png",units="in",width=9,height=8,res=300)
#print (
artist.count %>%
  subset(total>4000) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(artist) %>% 
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = artist)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = NULL, title = "Most Unique Words by Artist", subtitle = "TF-IDF for artists with > 4000 words") +
  facet_wrap(~artist, ncol = 2, scales = "free") +
  coord_flip() +
  bbc_style() +
  theme(axis.text=element_text(size=8), strip.text.x = element_text(size = 15),
        plot.title=element_text(size=20), plot.subtitle = element_text(size=17)) 
#)
#dev.off()

# Sentiment analysis----
lyrics.sentiment <- token.lyrics %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(artist, index = title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Plot sentiment for each artist
#png("sentimentartist.png",units="in",width=10,height=19,res=300)
#print (
lyrics.sentiment %>% 
  group_by(artist) %>%
  summarise(sentiment = sum(sentiment)) %>%
  ggplot(aes(x=reorder(artist,sentiment),y=sentiment,label=sentiment)) +
  geom_point(stat="identity",color="black",size=6) + 
  geom_segment(aes(y = 0, 
                   x = artist, 
                   yend = sentiment, 
                   xend = artist), 
                  color = "black") +
  geom_text(color="white", size=3) + coord_flip() +
  labs(title="Overall Sentiment by Artist",subtitle="Most Artists Tend to be Negative") +
  bbc_style() +
  theme(axis.text=element_text(size=10)) 
#)
#dev.off()

# Plot sentiment for each song
#png("sentimentsong.png",units="in",width=13,height=30,res=300)
#print (
lyrics.sentiment %>% 
  group_by(index) %>%
  summarise(sentiment = sum(sentiment)) %>%
  ggplot(aes(x=reorder(index,sentiment),y=sentiment,label=sentiment)) +
  geom_point(stat="identity",fill="black",size=6) + 
  geom_segment(aes(y = 0, 
                   x = index, 
                   yend = sentiment, 
                   xend = index), 
               color = "black") +
  geom_text(color="white", size=3) + coord_flip() +
  labs(title="Overall Sentiment by Song",subtitle="Most Songs Are Negative") +
  bbc_style() +
  theme(axis.text=element_text(size=10)) 
#)
#dev.off()
#png("sentimentsong.png",units="in",width=13,height=10,res=300)
#print (
lyrics.sentiment %>% 
  group_by(index) %>%
  summarise(sentiment = sum(sentiment)) %>%
  ggplot(aes(x=reorder(index,sentiment),y=sentiment,label=sentiment)) +
  geom_point(stat="identity",fill="black") + 
  labs(title="Overall Sentiment by Song",subtitle="Most Songs Are Negative") +
  geom_text_repel(aes(label = paste(index,artist, sep=" - ")),
                  data = subset(lyrics.sentiment, (sentiment < -70 | sentiment > 25 | sentiment == 0)),
                  box.padding   = 1, 
                  point.padding = .8,
                  segment.color = 'grey50',
                  color="black") + 
  bbc_style() +
  theme(axis.text.x = element_blank()) 
#)
#dev.off()


  