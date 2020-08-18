# Setup----
library(tidyverse)
library(tidytuesdayR)
library(waffle)
library(hrbrthemes)
library(futurevisions)
library(patchwork)


# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
plants <- tuesdata$plants
actions <- tuesdata$actions

plants <- plants %>%
  mutate(across(where(is.character), as.factor))

actions <- actions %>%
  mutate(across(where(is.character), as.factor))

plants$year_last_seen <- factor(plants$year_last_seen, 
                                levels = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"))

p1 <- ggplot(plants, aes(x = year_last_seen, fill = year_last_seen)) +
  geom_bar(na.rm = TRUE, color = "black", size = .2) + coord_flip() +
  facet_wrap(~continent) +
  scale_x_discrete(na.translate = FALSE) +
  theme_ft_rc(grid="") +
  scale_fill_manual(values = futurevisions("grand_tour"), name = "Year Last Seen", limits = rev(levels(plants$year_last_seen))) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_text(hjust = .5),    
    plot.margin = unit(c(0,0,.5,0), "cm"),
    legend.key.width = unit(.4,"cm"), 
    legend.key.height = unit(.4,"cm"),
    legend.text = element_text(size = 9),
    panel.spacing.x = unit(0, "lines"),
    panel.spacing.y = unit(.5, "lines")
  ) + labs(subtitle = "When?")

p2 <- plants %>%
  count(continent) %>%
  ggplot(aes(fill = continent, values = n)) +
  geom_waffle(n_rows = 25, size = 0.25, colour = "black", flip = TRUE) +
  coord_equal() +
  scale_fill_manual(values = futurevisions("grand_tour"), name = element_blank(), limits = rev(levels(plants$continent))) +
  theme_ft_rc(grid="") +
  theme_enhance_waffle() + 
  theme(
    plot.subtitle = element_text(hjust = .5),     
    plot.margin = unit(c(0,0,.5,0), "cm"),
    legend.key.width = unit(.4,"cm"), 
    legend.key.height = unit(.4,"cm"),
    legend.text = element_text(size = 9)
  ) + labs(subtitle = "Where?")

p3 <- actions %>%
  filter(action_taken == 1 & action_type != "Unknown" ) %>%
  count(continent, action_type) %>%
  ggplot(aes(x = action_type, y = n, fill = action_type)) +
  geom_col(color = "black", size = .2) + coord_flip() +
  facet_wrap(~continent) +
  theme_ft_rc(grid="") +
  scale_fill_manual(values = futurevisions("grand_tour"), name = "Strategy", limits = c("Species Management", "Research & Monitoring", "Law & Policy", "Land & Water Protection", "Education & Awareness")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_text(hjust = .5),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.key.width = unit(.4,"cm"), 
    legend.key.height = unit(.4,"cm"),
    legend.text = element_text(size = 9),
    panel.spacing.x = unit(0, "lines"),
    panel.spacing.y = unit(.5, "lines")
    ) + labs(subtitle = "What is being done?")

(p2+p1)/p3 + plot_annotation(
    title = "Global Biodiversity Loss",
    subtitle = "As of 2020, 500 plant species are considered extinct",
    caption = "Created by @kllycttn, Data from International Union for Conservation of Nature, #TidyTuesday",
    theme = theme_ft_rc()) 
      
      
      
      
