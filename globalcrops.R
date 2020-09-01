# Setup----
library(tidyverse)
library(tidytuesdayR)
library(rnaturalearth)
library(sf)
library(hrbrthemes)
library(rcartocolor)
library(patchwork)

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

key_crop_yield <- tuesdata$key_crop_yield
colnames(key_crop_yield) <- gsub(" \\(tonnes per hectare\\)","",colnames(key_crop_yield))

world <- ne_countries(scale = "medium", returnclass = "sf") 

top_crops <- key_crop_yield %>%
  group_by(Entity) %>%
  slice(c(1,n())) %>% # get only first and last for each "entity"
  ungroup() %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop",
               values_to = "crop_production") %>% 
  group_by(Entity, Year) %>%
  filter(crop_production == max(crop_production, na.rm = TRUE)) %>% # find max crop yield for each country
  filter(!is.na(Code)) %>% # remove non-countries
  rename(name = Entity) %>%
  mutate(
    name = case_when(
      name == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
      name == "Central African Republic" ~ "Central African Rep.",
      name == "Cote d'Ivoire" ~ "Côte d'Ivoire",
      name == "Democratic Republic of Congo" ~ "Dem. Rep. Congo",
      name == "Czech Republic" ~ "Czech Rep.",
      name == "Dominican Republic" ~ "Dominican Rep.",
      name == "Equatorial Guinea" ~ "Eq. Guinea",
      name == "Laos" ~ "Lao PDR",
      name == "North Korea" ~ "Dem. Rep. Korea",
      name == "Western Sahara" ~ "W. Sahara",
      name == "South Sudan" ~ "S. Sudan",
      name == "Solomon Islands" ~ "Solomon Is.",
      name == "Timor" ~ "Timor-Leste",
      TRUE ~ name
    ))

top_crops_1961 <- top_crops %>% 
  filter(Year == "1961") %>% # chose to only include data from 1961 rather than the first datapoint for each country, so some will be missing
  left_join(world, ., by = "name") %>% # join with the world data for plotting
  filter(name != "Antarctica")

top_crops_2018 <- top_crops %>% 
  filter(Year == "2018") %>%
  left_join(world, ., by = "name") %>% 
  filter(name != "Antarctica")

colors <- carto_pal(9, "Prism")

p1 <- ggplot(top_crops_1961) +
  geom_sf(aes(fill = crop), color = NA) +
  scale_fill_manual(values = colors, 
                    breaks = c("Bananas", "Cassava", "Maize", "Potatoes", "Rice", "Wheat"),
                    na.value = "grey50") +
  theme_ft_rc() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  labs(subtitle = "1961")

p2 <- ggplot(top_crops_2018) +
  geom_sf(aes(fill = crop), color = NA) +
  scale_fill_manual(values = colors, 
                    breaks = c("Bananas", "Cassava", "Maize", "Potatoes", "Rice", "Wheat", "Barley", "Beans", "Soybeans"),
                    na.value = "grey50") +
  theme_ft_rc() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.title = element_blank()
  ) +
  labs(subtitle = "2018")

p1 / p2 + plot_layout(guides = 'collect') + 
  plot_annotation(
    title = "Global Crop Yields: Top Crop by Country",
    subtitle = "Highest crop yield in 1961 vs. 2018",
    caption = "Created by @kllycttn, Data from Our World in Data, #TidyTuesday",
    theme = theme_ft_rc()
  )
