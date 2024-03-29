# Setup----
library(tidyverse)
library(treemap)
library(ggfittext)

# Get data
allCategories <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')

# Format data-----

# Find the top foundation names
top_names <- allCategories %>% 
  group_by(name) %>% 
  summarise(count = n()) %>% 
  slice_max(n = 20, order_by = count) %>% 
  pull(name)

# Mode shade ----
# Find the most common Hex code associated with the top names
top_shades <- allCategories %>% 
  filter(name %in% top_names) %>% 
  filter(hex != "#FDFDFD") %>%  # I think this is an error - this is a shade of gray (almost white). It doesn't look like the associated image, so I removed it.
  group_by(name, hex) %>% 
  summarise(count = n()) %>% 
  slice_max(n = 1, order_by = count)

# Combine original data with top shade data, add subgroups if multiple hex codes 
plot_data <- allCategories %>% 
  group_by(name) %>% 
  count() %>% 
  right_join(., top_shades, by = "name") %>% 
  mutate(subgroup = seq_along(name), 
         prop = n/max(subgroup)) # Because some have multiple shades, make the size proportional to the whole group

# Make treemap
# Shoutout to Yobanny Sámano https://github.com/ysamano/TidyTuesday/blob/master/2021/week_03/TT_2021_03.R
data_tree <- treemap(plot_data,
                     index=c("name", "subgroup"),
                     vSize="prop",
                     type="color",
                     vColor = "hex",
                     algorithm = "pivotSize",
                     border.lwds = 0.7,
                     aspRatio = 6/3)

# Convert to ggplot object for better plotting
data_ggplot <- data_tree[["tm"]] %>% 
  as_tibble() %>% 
  arrange(desc(vSize)) %>% 
  mutate(rank = row_number(),
         xmax = x0 + w,
         ymax = y0 + h,
         label_name = str_glue("{name}\n({vSize})"))

# Plotting -----  
ggplot(data_ggplot) +
  geom_rect(aes(xmin = x0,
                ymin = y0,
                xmax = xmax,
                ymax= ymax,
                fill = color),
            size = 0.1,
            colour = "#727272",
            alpha = 0.9) +
  geom_fit_text(data = data_ggplot %>% filter(is.na(subgroup)),
                aes(xmin = x0, 
                    xmax = xmax, 
                    ymin = y0,
                    ymax = ymax,
                    label = label_name),
                colour = "black",
                family = "Titillium Web SemiBold",
                min.size = 4,
                reflow = TRUE) +
  scale_fill_identity() +
  labs(title = "What are the most common foundation names?",
       subtitle = "And what color best represents them? Each box represents a foundation name, larger boxes indicate more foundations with that name. 
The color represents the most common shade associated with that name. Some foundation names have multiple representative shades.",
       caption = "Created by @kllycttn | Data from The Pudding | #TidyTuesday") +
  theme_void() +
  theme(
    plot.title = element_text(family = "Titillium Web SemiBold", size = 20),
    plot.subtitle = element_text(family = "Titillium Web Light"),
    plot.margin = (unit(c(.1, .1, .1, .1), "cm"))
  )

ggsave("foundation.png")


# Median shade -----

# Re-do with average shade instead of "most representative shade"! Shoutout to @MStrasiotto and Stack Overflow
top_shades <- allCategories %>% 
  filter(name %in% top_names) %>% 
  filter(hex != "#FDFDFD") %>%  # I think this is an error - this is a shade of gray (almost white). It doesn't look like the associated image, so I removed it.
  rowwise() %>% 
  mutate(decimal = paste(as.vector(col2rgb(hex)), collapse = " "),
         r = col2rgb(hex)[1],
         g = col2rgb(hex)[2],
         b = col2rgb(hex)[3]
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  summarise(median_r = round(median(r), 0),
            median_g = round(median(g), 0),
            median_b = round(median(b),0)
  ) %>% 
  rowwise() %>% 
  mutate(hex = rgb(median_r, median_g, median_b, maxColorValue=255))


# Combine original data with top shade data, add subgroups if multiple hex codes 
plot_data <- allCategories %>% 
  group_by(name) %>% 
  count() %>% 
  right_join(., top_shades, by = "name") 

# Make treemap
# Shoutout to Yobanny Sámano https://github.com/ysamano/TidyTuesday/blob/master/2021/week_03/TT_2021_03.R
data_tree <- treemap(plot_data,
                     index=c("name"),
                     vSize="n",
                     type="color",
                     vColor = "hex",
                     algorithm = "pivotSize",
                     border.lwds = 0.7,
                     aspRatio = 6/3)

# Convert to ggplot object for better plotting
data_ggplot <- data_tree[["tm"]] %>% 
  as_tibble() %>% 
  arrange(desc(vSize)) %>% 
  mutate(rank = row_number(),
         xmax = x0 + w,
         ymax = y0 + h,
         label_name = str_glue("{name}\n({vSize})"))

# Plotting -----  

ggplot(data_ggplot) +
  geom_rect(aes(xmin = x0,
                ymin = y0,
                xmax = xmax,
                ymax= ymax,
                fill = color),
            size = 0.1,
            colour = "#727272",
            alpha = 0.9) +
  geom_fit_text(data = data_ggplot,
                aes(xmin = x0, 
                    xmax = xmax, 
                    ymin = y0,
                    ymax = ymax,
                    label = label_name),
                colour = "black",
                family = "Titillium Web SemiBold",
                min.size = 4,
                reflow = TRUE) +
  scale_fill_identity() +
  labs(title = "What are the most common foundation names?",
       subtitle = "And what color best represents them? Each box represents a foundation name, larger boxes indicate more foundations with that name. 
The color represents the median shade of that name.",
       caption = "Created by @kllycttn | Data from The Pudding | #TidyTuesday") +
  theme_void() +
  theme(
    plot.title = element_text(family = "Titillium Web SemiBold", size = 20),
    plot.subtitle = element_text(family = "Titillium Web Light"),
    plot.margin = (unit(c(.1, .1, .1, .1), "cm"))
  )

ggsave("foundation2.png")

# Mean shades ----

# Since the first was really the mode and the second was the median, why not do the mean for all measures of central tendency
top_shades <- allCategories %>% 
  filter(name %in% top_names) %>% 
  filter(hex != "#FDFDFD") %>%  # I think this is an error - this is a shade of gray (almost white). It doesn't look like the associated image, so I removed it.
  rowwise() %>% 
  mutate(decimal = paste(as.vector(col2rgb(hex)), collapse = " "),
         r = col2rgb(hex)[1],
         g = col2rgb(hex)[2],
         b = col2rgb(hex)[3]
  ) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  summarise(mean_r = round(mean(r), 0),
            mean_g = round(mean(g), 0),
            mean_b = round(mean(b),0)
  ) %>% 
  rowwise() %>% 
  mutate(hex = rgb(mean_r, mean_g, mean_b, maxColorValue=255))


# Combine original data with top shade data, add subgroups if multiple hex codes 
plot_data <- allCategories %>% 
  group_by(name) %>% 
  count() %>% 
  right_join(., top_shades, by = "name") 

# Make treemap
# Shoutout to Yobanny Sámano https://github.com/ysamano/TidyTuesday/blob/master/2021/week_03/TT_2021_03.R
data_tree <- treemap(plot_data,
                     index=c("name"),
                     vSize="n",
                     type="color",
                     vColor = "hex",
                     algorithm = "pivotSize",
                     border.lwds = 0.7,
                     aspRatio = 6/3)

# Convert to ggplot object for better plotting
data_ggplot <- data_tree[["tm"]] %>% 
  as_tibble() %>% 
  arrange(desc(vSize)) %>% 
  mutate(rank = row_number(),
         xmax = x0 + w,
         ymax = y0 + h,
         label_name = str_glue("{name}\n({vSize})"))

# Plotting -----  

ggplot(data_ggplot) +
  geom_rect(aes(xmin = x0,
                ymin = y0,
                xmax = xmax,
                ymax= ymax,
                fill = color),
            size = 0.1,
            colour = "#727272",
            alpha = 0.9) +
  geom_fit_text(data = data_ggplot,
                aes(xmin = x0, 
                    xmax = xmax, 
                    ymin = y0,
                    ymax = ymax,
                    label = label_name),
                colour = "black",
                family = "Titillium Web SemiBold",
                min.size = 4,
                reflow = TRUE) +
  scale_fill_identity() +
  labs(title = "What are the most common foundation names?",
       subtitle = "And what color best represents them? Each box represents a foundation name, larger boxes indicate more foundations with that name. 
The color represents the mean shade of that name.",
       caption = "Created by @kllycttn | Data from The Pudding | #TidyTuesday") +
  theme_void() +
  theme(
    plot.title = element_text(family = "Titillium Web SemiBold", size = 20),
    plot.subtitle = element_text(family = "Titillium Web Light"),
    plot.margin = (unit(c(.1, .1, .1, .1), "cm"))
  )

ggsave("foundation3.png")

