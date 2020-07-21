# Setup----

library(tidyverse)
library(tidytuesdayR)
library(scales)
library(gganimate)
library(RColorBrewer)


# Get data
tuesdata <- tidytuesdayR::tt_load('2020-07-21')
tuesdata <- tidytuesdayR::tt_load(2020, week = 30)

animal_outcomes <- tuesdata$animal_outcomes

# Plots----
# Plot stacked bar chart for all animals. Adding the last line to animate makes a wild gif
ggplot(animal_outcomes, aes(x = outcome, y = Total, fill = animal_type, group = animal_type)) +
  geom_col() +
  scale_y_log10() +
  scale_fill_brewer(palette = "Dark2") 
  #+ transition_time(year) 

# Plot outcomes over time for only cats and dogs
# Taking out the commented portions will animate this by year
ggplot(subset(animal_outcomes, animal_type == "Dogs" | animal_type == "Cats"), aes(x = year, y = Total, color = outcome, group = outcome)) +
  geom_point() + geom_line() +
  facet_wrap(~animal_type) +
  scale_color_brewer(palette = "Dark2", name = element_blank()) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold.italic", size = 12))+
  #transition_reveal(year) +
  labs(
    title = "Animal Complaint Outcomes in Australia", 
    #subtitle = "Year: {as.integer(frame_along)}", 
    x = element_blank() , 
    y = element_blank())

#anim_save("pets.gif")

# Plot outcomes over time for all animals
# Taking out the commented portions will animate this by year
ggplot(subset(animal_outcomes, outcome != "Other"), aes(x = year, y = Total, color = outcome, group = outcome)) +
  geom_point() + geom_line() +
  scale_y_log10() + # because of the difference in numbers between cats/dogs and all other animals, transform the y axis
  facet_wrap(~animal_type) +
  scale_color_brewer(palette = "Dark2", name = element_blank()) +
  theme_minimal() +
  #transition_reveal(year) +
  labs(
    title = "Animal Outcomes in Australia", 
    #subtitle = "Year: {as.integer(frame_along)}", 
    x = element_blank() , 
    y = element_blank())
#anim_save("animal_outcomes.gif")


# Pivot the data to plot by region
animals_region <- animal_outcomes %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "region",
    values_to = "count"
    )

# Plot outcomes by region, looks better when animated by year, but still visually overwhelming
ggplot(animals_region, aes(x = animal_type, y = count, fill = outcome, group = outcome)) +
  geom_bar(stat="identity", position = position_dodge2()) +
  facet_wrap(~region, nrow = 4) +
  scale_y_log10() 
  #+ transition_time(year)

  
