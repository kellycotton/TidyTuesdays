# Setup----
library(tidyverse)
library(tidytuesdayR)
library(patchwork)
library(wesanderson)
library(showtext)
# Get code for creating half violin plot for rain cloud plot
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Get custom fonts
font_add_google("Abel","Abel")
showtext_auto()
theme_set(theme_light(base_family = "Abel"))

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 31)
penguins <- tuesdata$penguins

p1 <- ggplot(penguins, aes(x=species, y=body_mass_g, fill = species)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = body_mass_g, color = species), 
             position = position_jitter(width = .15), size = 1.5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 4) +
  guides(fill = FALSE, color = FALSE) +
  scale_color_manual(values = wes_palette("GrandBudapest2")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2")) +
  labs(y = "Body Mass (g)", x = element_blank()) +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"),
    panel.border = element_blank()) +
  coord_flip()

p2 <- ggplot(penguins, aes(x=species, y=flipper_length_mm, fill = species)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = flipper_length_mm, color = species), 
             position = position_jitter(width = .15), size = 1.5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 4) +
  guides(fill = FALSE, color = FALSE) +
  scale_color_manual(values = wes_palette("GrandBudapest2")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2")) +
  labs(y = "Flipper Length (mm)", x = element_blank()) +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"),
    panel.border = element_blank()) +
  coord_flip()

p3 <- ggplot(penguins, aes(x = as.factor(year), y = body_mass_g, color = species, group = species)) +
  geom_point(aes(y = body_mass_g, color = species), 
             position = position_jitter(width = .15), size = 2, alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", size = .5, alpha = .5) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  guides(fill = FALSE, color = FALSE) +
  scale_color_manual(values = wes_palette("GrandBudapest2")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2")) +
  labs(y = "Body Mass (g)", x = element_blank()) +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"),
    panel.border = element_blank()
    )

p4 <- ggplot(penguins, aes(x = as.factor(year), y = flipper_length_mm, color = species, group = species)) +
  geom_point(aes(y = flipper_length_mm, color = species), 
             position = position_jitter(width = .15), size = 2, alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", size = .5, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  guides(fill = FALSE, color = FALSE) +
  scale_color_manual(values = wes_palette("GrandBudapest2")) +
  scale_fill_manual(values = wes_palette("GrandBudapest2")) +
  labs(y = "Flipper Length (mm)", x = element_blank()) +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"),
    panel.border = element_blank()
  )

# Put final plot together and save
((p1+p3)/(p2+p4)) + plot_annotation(title = 'Comparing Penguin Species of Antarctica')
ggsave("penguins.png")
