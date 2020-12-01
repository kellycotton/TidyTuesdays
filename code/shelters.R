# Setup----
library(tidyverse)
library(lubridate)
library(gridExtra)
library(patchwork)

theme_set(theme_classic(base_family = "Noto Serif"))

# Theme for table
TSpecial <- ttheme_minimal(
  base_family = "Noto Serif",
  core = list(bg_params = list(fill = "white", col = NA),
              fg_params = list(fontsize = 11)),
  colhead = list(bg_params = list(fill = "white"),
                 fg_params = list(fontface = "plain")),
  rowhead = list(bg_params = list(fill = c("white", wesanderson::wes_palette("Moonrise2"))),
                 fg_params = list(fontface = "plain")))

# Read data
shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

# Extract Month/Year, summarize number of occupants
shelters_total <- shelters %>% 
  filter(shelter_city == "Toronto") %>% 
  mutate(month = factor(month(occupancy_date),
                        levels = as.character(1:12),
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                        ordered = TRUE)) %>% 
  mutate(year = year(occupancy_date)) %>% 
  group_by(occupancy_date) %>% 
  summarise(month = month, year = year, sum_total = sum(occupancy)) %>% 
  group_by(month, year) %>% 
  summarise(mean_sum = round(mean(sum_total), 0))

# Plot 
p <- ggplot(shelters_total, aes(x = month, y = mean_sum, fill = as.factor(year))) +
  geom_col(position = position_dodge2()) +
  scale_y_continuous(breaks = seq(1000, 7000, by = 1000)) +
  coord_cartesian(ylim = c(1000, 7000)) +
  scale_fill_manual(values = wesanderson::wes_palette("Moonrise2")) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line = element_line(size = .2),
    plot.margin = unit(c(0,0,0,0), "cm")
  ) 

# Table
shelter_table <- shelters_total %>%
  pivot_wider(names_from = month, 
              values_from = mean_sum) %>% 
  rename("Year" = "year") 

# Combine
p/tableGrob(shelter_table[2:13], 
                       rows = shelter_table$Year, 
                       theme = TSpecial) +
  plot_layout(widths = c(1, 5)) +
  plot_annotation(title = "Occupancy in Toronto Shelters",
                  subtitle = paste("Average number of nightly occupants in Toronto's shelter system,",
                                   "\nacross five types of shelters: families, women, men, co-ed, and youth."), 
                  caption = "Created by @kllycttn, Data from OpenDataToronto, #TidyTuesday",
                  theme = theme(plot.title = element_text(face = "bold"),
                                plot.subtitle = element_text(size = 9),
                                plot.caption = element_text(size = 6))) 
  
ggsave("Plots/shelters.png")




