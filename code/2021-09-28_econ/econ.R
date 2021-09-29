# NBER Papers
# 9/28/2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-28/readme.md

# Setup----
library(tidyverse)
library(tidytext)

theme_set(theme_minimal(base_family = "OldSansBlack"))

# Read data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')

# Text analysis and add decade info
stop_words <- stop_words
paper_words <- papers %>%
  unnest_tokens(word, title)  %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(word != "19") %>% 
  mutate(year = format(year, format = "%Y")) %>% 
  mutate(decade = round(as.numeric(year) %/% 10)*10)

word_counts <- paper_words %>% 
  group_by(decade, year, word) %>% 
  summarise(total = n()) %>% 
  group_by(year) %>% 
  slice_max(order_by = total, n = 2)

# Find top terms of the decade
top_decade <- paper_words %>% 
  group_by(decade, word) %>% 
  summarise(decade_total = n()) %>% 
  slice_max(order_by = decade_total, n = 1) %>% 
  mutate(word = ifelse(decade == 1970, "analysis & labor", word), # would be better if this was more automatic but..
           labels = paste("Most common term of the decade:", word)) %>% 
  distinct(decade, labels) %>% 
  pull(labels)
names(top_decade) <- c("1970", "1980", "1990", "2000", "2010", "2020") # see note above..

# And plot it
ggplot(word_counts, aes(x = year, y = total, fill = word, group = word)) +
  geom_col(position = position_dodge2()) +
  geom_text(aes(label = word, y = (total * .65)), 
            position = position_dodge(width = 1),
            size = 3.5, angle = 90,
            family = "OldSansBlack",
            color = "white") +
  facet_wrap(~decade, scales = "free", 
             labeller = labeller(decade = top_decade)) +
  scale_fill_manual(values = wesanderson::wes_palette("IsleofDogs1", 29, type = "continuous")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "What do economists write about?", 
       subtitle = "Since 1973, more than 29,000 working papers have been distributed by the National Bureau for Economic Research. Throughout the years, the focus of 
<br>these papers have shifted significantly. The 1970s mostly discussed <span style = 'color:#595135;'>labor</span> and <span style = 'color:#837292;'>analysis</span>, while the 1980s changed to <span style = 'color:#4a4749;'>policy</span>. Eventually, in the 1990s and 
<br>beyond, the focus shifted to <span style = 'color:#734d35;'>evidence</span>, though the second most common terms varied each year. For exampe, <span style = 'color:#79736e;'>trade</span> was prominent from 1994 - 1996, <span style = 'color:#baa867;'>health</span> 
<br>in 2014 and 2015, and <span style = 'color:#755b67;'>COVID</span> in 2020 and 2021.", 
       caption = "Created by @kllycttn | Data from National Bureau of Economic Research & Ben Davies | #TidyTuesday") +
  theme(legend.position = "none",
        plot.title = element_text(family = "Arvo", face = "bold", size = 30),
        plot.subtitle = ggtext::element_markdown(lineheight = .2),
        axis.title = element_blank(),
        panel.grid.minor.y = element_blank()
        )

ggsave(here::here("code", "2021-09-28_econ", "terms.png"), width = 11, height = 6.5, units = "in", bg = "white")

