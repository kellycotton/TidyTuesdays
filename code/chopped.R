# Setup----
library(tidyverse)
library(tidytuesdayR)
library(ggalt)
library(hrbrthemes)
library(wesanderson)
library(patchwork)

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 35)
chopped <- tuesdata$chopped

# Separate ingredients
appetizers <- chopped %>% 
  mutate(appetizer = gsub("\\s*\\([^\\)]+\\)","",as.character(appetizer))) %>% # some extra info within parantheses removed
  mutate(ingredient = strsplit(as.character(appetizer), ", ")) %>% # split the ingredients in each cell
  unnest(ingredient) %>%
  mutate(ingredient = gsub("[[:punct:]]","" , ingredient)) # remove punctuation

entrees <- chopped %>% 
  mutate(entree = gsub("\\s*\\([^\\)]+\\)","",as.character(entree))) %>%
  mutate(ingredient = strsplit(as.character(entree), ", ")) %>% 
  unnest(ingredient) %>% 
  mutate(ingredient = gsub("[[:punct:]]","" , ingredient)) 

desserts <- chopped %>% 
  mutate(dessert = gsub("\\s*\\([^\\)]+\\)","",as.character(dessert))) %>%
  mutate(ingredient = strsplit(as.character(dessert), ", ")) %>% 
  unnest(ingredient) %>% 
  mutate(ingredient = gsub("[[:punct:]]","" , ingredient)) 

# Top 50 (ish) ingredients by round
top_apps <- appetizers %>% 
  count(ingredient) %>%
  slice_max(order_by = n, n = 50)

top_entree <- entrees %>% 
  count(ingredient) %>%
  slice_max(order_by = n, n = 50)

top_dessert <- desserts %>% 
  count(ingredient) %>%
  slice_max(order_by = n, n = 50)

# Combine all the ingredients into one dataframe
all_ingredients <- bind_rows(appetizers[c(4, 22)], entrees[c(4, 22)], desserts[c(4, 22)])

# Top 50 ingredients across all rounds
top_ingredients <- all_ingredients %>% 
  count(ingredient) %>%
  slice_max(order_by = n, n = 50)

# Number of unique ingredients
length(unique(all_ingredients$ingredient))


# Plotting

# Lollipop plot top 20 ingredients of all rounds
p1 <- top_ingredients %>%
  slice_max(order_by = n, n = 20) %>% 
  ggplot(aes(x = reorder(ingredient, n), y = n)) +
  geom_lollipop(point.colour = "#5BBCD6", color = "#5BBCD6", point.size = 5) + 
  coord_flip() +
  theme_ipsum_es(grid="") +
  labs(title = "All Rounds") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.text.y = element_text(size = 11, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = .5, size = 10),
    plot.margin = unit(c(0,0,0,0), "cm")
  )

# Lollipop plot top 10 ingredients of appetizer round
p2 <- top_apps %>%
  slice_max(order_by = n, n = 10) %>% 
  ggplot(aes(x = reorder(ingredient, n), y = n)) +
  geom_lollipop(point.colour = "#F98400", color = "#F98400", point.size = 3.5) + coord_flip() +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 15)) +
  theme_ipsum_es(grid="") +
  labs(title = "Appetizer Round") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = .5, size = 10),
    plot.margin = unit(c(0,0,.5,0), "cm")
  )

# Lollipop plot top 10 ingredients of entree round
p3 <- top_entree %>%
  slice_max(order_by = n, n = 10) %>% 
  ggplot(aes(x = reorder(ingredient, n), y = n)) +
  geom_lollipop(point.colour = "#00A08A", color = "#00A08A", point.size = 3.5) + coord_flip() +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 15)) +
  theme_ipsum_es(grid="") +
  labs(title = "Entree Round") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = .5, size = 10),
    plot.margin = unit(c(0,0,.5,0), "cm")
  )

# Lollipop plot top 10 ingredients of dessert round
p4 <- top_dessert %>%
  slice_max(order_by = n, n = 10) %>% 
  ggplot(aes(x = reorder(ingredient, n), y = n)) +
  geom_lollipop(point.colour = "#FF0000", color = "#FF0000", point.size = 3.5) + coord_flip() +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 15)) +
  theme_ipsum_es(grid="") +
  labs(title = "Dessert Round") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = .5, size = 10),
    plot.margin = unit(c(0,0,.5,0), "cm")
  )

# Put all plots together 
p1/(p2 + p3 + p4) + 
  plot_layout(heights = c(2,1)) + 
  plot_annotation(
  title = "Chopped: Most Popular Mystery Ingredients",
  subtitle = "In 567 episodes over 45 seasons, 4173 unique ingredients have been featured.",
  caption = "Created by @kllycttn, Data from Kaggle, #TidyTuesday",
  theme = theme_ipsum_es()) 
