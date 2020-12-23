# Big Mac index
# 12/22/2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-22/readme.md

# Setup----
library(tidyverse)
library(ggalt)
library(showtext)

font_add_google("Roboto","Roboto")
showtext_auto()

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

dat <- big_mac %>% 
  mutate(name = case_when(
    name == "UAE" ~ "United Arab Emirates",
    TRUE ~ as.character(name)
  )) %>% 
  group_by(name) %>% 
  summarise(first = first(dollar_price),
         first_date = first(date),
         last = last(dollar_price),
         last_date = last(date),
         difference = last - first,
         percent_difference = ((last - first)/first)*100,
         difference_label = case_when(
           percent_difference < 0 ~ "Decrease", 
           percent_difference == 0 ~ "No change",
           percent_difference <= 50 ~ "1-50% Increase",
           percent_difference <= 100 ~ "50-100% Increase",
           percent_difference > 100 ~ ">100% Increase"))

ggplot(dat, aes(x = first, xend = last, y = reorder(name, first), group = name)) +
  geom_dumbbell(colour_x ="#DA291C",
                size_x = 2.5,
                size = 0.75, 
                color = "#888888",
                colour_xend ="#FFC72C",
                size_xend = 2.5,
                dot_guide = TRUE,
                dot_guide_size = 0.25) + 
  geom_rect(aes(xmin = 7, xmax = 7.6, ymin = -.5, ymax = 59), fill = "grey") +
  geom_text(aes(label = paste0(round(percent_difference, 0), "%"), y = name, x = 7.3), size = 3) +
  geom_text(data = filter(dat, name == "Lebanon"), 
            aes(x = 7.3, y = name, label = "% Change"),
            color = "black", size = 3.1, vjust = -2, fontface = "bold") +
  geom_text(data = filter(dat, name == "Lebanon"), 
            aes(x = first, y = name, label = "Initial Price"),
            color = "black", size = 3.1, vjust = -1, fontface = "bold") +
  geom_text(data = filter(dat, name == "Lebanon"), 
            aes(x = last, y = name, label = "2020 Price"),
            color = "black", size = 3.1, vjust = -1, fontface = "bold") +
  scale_x_continuous(labels = scales::label_dollar(prefix = "$",
                                                   accuracy = .01),
                     breaks = scales::breaks_pretty(n = 5),
                     name = "Price of a Big Mac, USD") +
  #scale_y_discrete(expand = c(0.1, 0)) +
  labs(title = "How much has the price of a Big Mac changed?",
  subtitle = "A comparison of the initial price of Big Mac to the price in 2020. Most countries have an initial Big Mac price recorded in 2000-2005. 
However, a number of countries were not included until later.

2011: India, 2014: Vietnam, 2018: Lebanon, UAE, Honduras, Kuwait, Guatemala, Croatia, Qatar, Nicaragua, Bahrain, Jordan, Oman, 
Moldova, Azerbaijan, Romania",
  caption = "Created by @kllycttn, Data from TheEconomist, #TidyTuesday") +
  theme_minimal() +
  theme(
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 18, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0)
  )

ggsave("bigmac.png", width = 10, height = 8.5, units = "in")
       