# Setup----
library(tidyverse)
library(tidytext)
library(hrbrthemes)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

# Get individual words in trail names 
names <- hike_data %>% 
  mutate(name = gsub("[[:punct:]]", "", as.character(name))) %>% #remove punctuation
  mutate(name = strsplit(as.character(name), " ")) %>% #split trail names into individual words
  unnest(name) %>% 
  filter(!str_detect(name, "[[:digit:]]")) %>% #remove numbers
  filter(name != "") %>% #remove space names
  anti_join(stop_words, by = c("name" = "word")) %>% #remove common words like and, the, etc.
  count(name)

# Create plot
names %>% 
  slice_max(order_by = n, n = 25) %>%  #only top 25 (actually 26) most common
  ggplot(aes(x = name, y = n, color = name, group = 1)) +
  geom_line(size = 1) + geom_point(size = .3) + #add point to make it more rounded at the top
  geom_segment(aes(xend = name, y = 0, yend = n), linetype = "dotdash", alpha = .25) + #geom_segment adds the dashed lines
  scale_color_manual(values = calecopal::cal_palette("arbutus", n = 26, type = "continuous")) +
  theme_ft_rc() +
  scale_y_continuous(breaks = c(50, 100, 150, 200, 250, 300, 350), name = "Number of Occurrences") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7)) +
  labs(title = "Hiking the Creek Lake Mountain Trail",
       subtitle = "Most common words in Washington State trail names",
       caption = "Created by @kllycttn, Data from Washington Trails Association, #TidyTuesday")
  
#ggsave("Plots/trailnames.png", dpi = 300, width = 10)
  
# Generate trail name
name_generator <- function(n_words) {
  names <- names %>% filter(!name %in% c("Trail", "Trails"))
  words <- sample_n(names, n_words)[1]
  new_name <- paste("Your personalized Washington State trail name is: ", sapply(words, paste, collapse = " "), "Trail.")
  return (new_name)
}

# Enter your desired number of words 
name_generator(4)
